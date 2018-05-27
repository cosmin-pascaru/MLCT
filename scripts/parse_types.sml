datatype Token = TSelect
               | TInsert
               | TUpdate
               | TDelete
               | TWhere
               | TInto
               | TFrom
               | TSet
               | TIdentifier of string
               | TReal of real
               | TInt of int
               | TString of string
               | TAnd
               | TOr
               | TLess
               | TGreater
               | TNotEqual
               | TEqual
               | TInvalid of string
               | TOpeningParanthesis
               | TClosingParanthesis
               | TOperatorStar;

fun ParseIntLeft value [] = (SOME value)
  | ParseIntLeft leftVal (ch::x) = if (Char.isDigit ch)
                                   then case (ParseIntLeft (leftVal * 10 + ((ord ch) - 48)) x)
                                          of NONE => NONE
                                           | (SOME value) => (SOME value)
                                   else NONE;

fun ParseInt "" = NONE
  | ParseInt value = (ParseIntLeft 0 (String.explode value));

fun subunitary value = if value >= 1.0 then subunitary (value / 10.0) else value;

fun ParseRealLeft value [] = (SOME value)
  | ParseRealLeft leftVal (ch::x) = if (Char.isDigit ch)
                                    then case (ParseRealLeft (leftVal * 10.0 + real((ord ch) - 48)) x)
                                           of NONE => NONE
                                            | (SOME value) => (SOME value)
                                    else if (Char.contains "." ch)
                                         then case (ParseIntLeft 0 x)
                                                of NONE => NONE
                                                 | (SOME value) => (SOME (leftVal + (subunitary (real(value)))))
                                         else NONE;

fun ParseReal "" = NONE
  | ParseReal value = (ParseRealLeft 0.0 (String.explode value));

  
fun ParseString "" = NONE
  | ParseString value = if (String.isPrefix "'" value) andalso (String.isSuffix "'" value)
                        then case (String.tokens (fn c => c = #"'" ) value)
                               of (token::[]) => (SOME token)
                                | _ => NONE
                        else NONE;

fun ParseIdentifierChr [] = NONE
  | ParseIdentifierChr (ch::x) = if (Char.isAlpha ch) then (SOME (String.implode (ch::x))) else NONE;

fun ParseIdentifier "" = NONE
  | ParseIdentifier value = (ParseIdentifierChr (String.explode value));

fun ParseDynamic tokenStr = case (ParseInt tokenStr)
                              of (SOME value) => (TInt value)
                               | NONE => case (ParseReal tokenStr)
                                           of (SOME value) => (TReal value)
                                            | NONE => case ParseString tokenStr
                                                        of (SOME value) => (TString value)
                                                         | NONE => case ParseIdentifier tokenStr 
                                                                     of (SOME value) => (TIdentifier value)
                                                                      | NONE => (TInvalid tokenStr);

fun lower str = String.translate (fn c => String.str (Char.toLower c)) str

fun ParseToken tokenStr = case lower tokenStr
                            of "<"      => TLess
                             | ">"      => TGreater
                             | "="      => TEqual
                             | "~"      => TNotEqual
                             | "("      => TOpeningParanthesis
                             | ")"      => TClosingParanthesis
                             | "or"     => TOr
                             | "and"    => TAnd
                             | "select" => TSelect
                             | "update" => TUpdate
                             | "insert" => TInsert
                             | "delete" => TDelete
                             | "where"  => TWhere
                             | "into"   => TInto
                             | "from"   => TFrom
                             | "set"    => TSet
                             | "*"      => TOperatorStar
                             | value    => ParseDynamic tokenStr


fun SplitByDelimitersLeft f (lastPos, lastToken) (currentPos, []) = (lastPos, lastToken)::nil
  | SplitByDelimitersLeft f (lastPos, lastToken) (currentPos, (ch::x)) = if (f ch)
                                           then [(lastPos, lastToken)] @
                                                [(currentPos, ch::nil)] @
                                                (SplitByDelimitersLeft f (currentPos + 1, []) ((currentPos + 1), x))
                                           else (SplitByDelimitersLeft f (lastPos, (lastToken @ (ch::nil))) ((currentPos + 1), x)); 

fun SplitByDelimiters f "" = []
  | SplitByDelimiters f s = map (fn (pos, token) => (pos, (String.implode token))) (SplitByDelimitersLeft f (0, []) (0, (String.explode s)))

fun isNotNonOperatorDelimiter s "" = false
  | isNotNonOperatorDelimiter s delimiters = case String.tokens (fn c => Char.contains delimiters c) s
                                               of [] => false
                                                | _ => true;

fun SplitBySpecialTokens statement = SplitByDelimiters (fn c => Char.contains " ,=<>~()" c) statement;

fun SplitTokens statement = List.filter (fn (pos, token) => isNotNonOperatorDelimiter token " ,") (SplitBySpecialTokens statement);

fun Tokenize statement = map (fn (pos, token) => (pos, ParseToken token)) (SplitTokens statement);

(*SELECT col1 FROM table WHERE ;*)
(*INSERT INTO table(col1, col2) VALUES 1, '2';*)
(*UPDATE table SET col1=a WHERE ;*)
(*DELETE FROM table WHERE ;*)

datatype 'a ParseState = ParseError of int * string
                       | Result of 'a

infixr 9 >>= 
fun a >>= f = case a
                of (ParseError (lineNo, msg)) => ParseError (lineNo, msg)
                 | (Result result) => f result;

fun return a = Result a;

fun ParseColNameList ((_, (TIdentifier col))::rest) = (ParseColNameList rest)
                                                  >>= (fn (cols, rest) => return ((col::cols), rest))
  | ParseColNameList tokens = return ([], tokens);

fun ParseTable ((_, TFrom)::(_, TIdentifier tableName)::rest) = return (tableName, rest)
  | ParseTable ((_, TFrom)::(lineNo, _)::rest) = ParseError (lineNo, "Expected table name")
  | ParseTable ((lineNo, _)::rest) = ParseError (lineNo, "Expected keyword FROM")
  | ParseTable [] = ParseError (0, "Unexpected EOF!");


datatype Value = Int of int
               | Real of real
               | String of string
               | Identifier of string;

datatype Expr = Equal of Value * Value
              | Greater of Value * Value
              | Less of Value * Value
              | NotEqual of Value * Value;

datatype WhereCondition = Single of Expr
                        | And of WhereCondition * WhereCondition
                        | Or of WhereCondition * WhereCondition;


fun ParseValue [] = ParseError (0, "Unexpected EOF")
  | ParseValue (token::rest) = 
    case token
      of (_, (TInt value))        => return (Int value, rest)
       | (_, (TReal value))       => return (Real value, rest)
       | (_, (TString value))     => return (String value, rest)
       | (_, (TIdentifier value)) => return (Identifier value, rest)
       | (lineNo, _)              => ParseError (lineNo, "Expected value or identifier.");

fun ParseExpr [] = ParseError (0, "Unexpected EOF")
  | ParseExpr tokens = (ParseValue tokens)
                   >>= (fn (rvalue, rest) =>
        (case rest
           of ((_, TEqual)::rest)    => (ParseValue rest) >>= (fn (lvalue, rest) => return (Equal (rvalue, lvalue), rest)) 
            | ((_, TGreater)::rest)  => (ParseValue rest) >>= (fn (lvalue, rest) => return (Greater (rvalue, lvalue), rest))
            | ((_, TLess)::rest)     => (ParseValue rest) >>= (fn (lvalue, rest) => return (Less (rvalue, lvalue), rest))
            | ((_, TNotEqual)::rest) => (ParseValue rest) >>= (fn (lvalue, rest) => return (NotEqual (rvalue, lvalue), rest))
            | ((lineNo, _)::rest) => ParseError (lineNo, "Expected operator")
            | [] => ParseError (0, "Unexpected EOF")));

fun ParseWhereNextCondition ParseWhereConditions (expr, []) = return (expr, [])
  | ParseWhereNextCondition ParseWhereConditions (expr, (token::rest)) = 
    case token
        of (_, TAnd) => (ParseWhereConditions rest) >>= (fn (condition, rest) => return (And (expr, condition), rest))
         | (_, TOr) => (ParseWhereConditions rest) >>= (fn (condition, rest) => return (Or (expr, condition), rest))
         | _ => return (expr, (token::rest));

fun ParseWhereConditions ((_, TOpeningParanthesis)::tokens) = (ParseWhereConditions tokens) >>= (fn (condition, rest) =>
    case rest
      of [] => ParseError (0, "Unexpected EOF")
       | ((_, TClosingParanthesis)::[]) => return (condition, [])
       | ((_, TClosingParanthesis)::rest) => (ParseWhereNextCondition ParseWhereConditions (condition, rest))
       | ((lineNo, _)::_) => ParseError (lineNo, "Expected closing paranthesis")
   )
  | ParseWhereConditions tokens = (ParseExpr tokens) >>= (fn (expr, tokens) => (ParseWhereNextCondition ParseWhereConditions) (Single expr, tokens));

fun ParseWhere ((_, TWhere)::tokens) = ParseWhereConditions tokens
  | ParseWhere [] = Result (Single (Equal (Int 1, Int 1)), [])
  | ParseWhere ((lineNo, _)::_) = ParseError (lineNo, "Expected WHERE keyword.")

fun ParseSelectFromTokens ((lineNo, TSelect)::tokens) = (ParseColNameList tokens)
                                                    >>= (fn (colList, rest) => (ParseTable rest)
                                                    >>= (fn (tableName, rest) => (ParseWhere rest)
                                                    >>= (fn (conditions, rest) => return (colList, tableName, conditions))))
  | ParseSelectFromTokens ((lineNo, _)::tokens) = ParseError (lineNo, "Expected SELECT keyword.");

(*
fun ParseQueryFromTokens (head:tokens) = case head
                                          of TSelect => ParseSelectFromTokens
                                           | TUpdate => ParseUpdateFromTokens
                                           | TInsert => ParseInsertFromTokens
                                           | TDelete => ParseDeleteFromTokens;
*)
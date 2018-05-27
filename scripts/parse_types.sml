fun ParseSelect stmt = 1
fun ParseInsert stmt = 2
fun ParseUpdate stmt = 3
fun ParseDelete stnt = 4

fun SkipWhitespace [] = []
  | SkipWhitespace (ch::next) = if ch = " " then (SkipWhitespace next) else (ch::next)

datatype ColName = string
datatype ColValue = Int of int
                  | Real of real
                  | String of string

datatype Expr = Int of int;

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
               | TExpr of string;

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
                                                                      | NONE => (TExpr tokenStr);

fun lower str = String.translate (fn c => String.str (Char.toLower c)) str

fun ParseToken tokenStr = case lower tokenStr
                            of "<"      => TLess
                             | ">"      => TGreater
                             | "="      => TEqual
                             | "~"      => TNotEqual
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
                             | value    => ParseDynamic tokenStr


fun SplitByDelimitersLeft f lastToken [] = lastToken::nil
  | SplitByDelimitersLeft f lastToken (ch::x) = if (f ch)
                                           then [lastToken] @ [ch::nil] @ (SplitByDelimitersLeft f [] x)
                                           else (SplitByDelimitersLeft f (lastToken @ (ch::nil)) x); 

fun SplitByDelimiters f "" = []
  | SplitByDelimiters f s = map String.implode (SplitByDelimitersLeft f [] (String.explode s))

fun isNonOperatorDelimiter s "" = false
  | isNonOperatorDelimiter s delimiters = case String.tokens (fn c => Char.contains delimiters c) s
                                               of [] => true
                                                | _ => false;

fun Tokenize statement = map ParseToken (List.filter (fn s => if isNonOperatorDelimiter s " ," then false else true) (SplitByDelimiters (fn c => Char.contains " ,=" c) statement));

(*
fun ParseQueryFromTokens (head:tokens) = case head 
                                          of TSelect => ParseSelectFromTokens
                                           | TUpdate => ParseUpdateFromTokens
                                           | TInsert => ParseInsertFromTokens
                                           | TDelete => ParseDeleteFromTokens*)
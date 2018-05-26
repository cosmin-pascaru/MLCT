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
               | TCol of string
               | TInt of int
               | TString of int
               | TAnd
               | TOr
               | TLess
               | TGreater
               | TLessEqual
               | TGreaterEqual
               | TIsEqual
               | TNotEqual
               | TEqual;

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

fun ParseColNameChr [] = NONE
  | ParseColNameChr (ch::x) = if (Char.isAlpha ch) then (SOME (String.implode (ch::x))) else NONE;

fun ParseColName "" = NONE
  | ParseColName value = (ParseColNameChr (String.explode value));

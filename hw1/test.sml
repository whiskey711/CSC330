(* Assignment 1 Simple Test *)

(* These are basic test cases.  *)

(* loads the bindings from the file with your solutions *)

(* All the tests ld evaluate to true. For example, the REPL should say: val test1 = true : bool *)

fun reasonable_date(date: DATE) =
    let
      fun leapYear(year: int) =
          year mod 400 = 0 orelse year mod 4 = 0 andalso not (year mod 100 = 0)
      fun rightDate(month: int, day: int) =

          if month < 1 orelse month > 12
          then false
          else if month = 1 orelse month = 3 orelse month = 5 orelse month = 7 orelse month = 8 orelse month = 10 orelse month = 12
          then day > 0 andalso day < 32
          else if month = 2
          then day > 0 andalso day < 29
          else day > 0 andalso day < 31

      fun rightDateinLeap(month: int, day: int) =
          if month < 1 orelse month > 12
          then false
          else if month = 1 orelse month = 3 orelse month = 5 orelse month = 7 orelse month = 8 orelse month = 10 orelse month = 12
          then day > 0 andalso day < 32
          else if month = 2
          then day > 0 andalso day < 30
          else day > 0 andalso day < 31
    in
      if leapYear(#1 date)
      then rightDateinLeap(#2 date, #3 date)
      else rightDate(#2 date, #3 date)
    end


val test= reasonable_date( 2014,  3,  31);

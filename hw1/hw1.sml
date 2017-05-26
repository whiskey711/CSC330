(*  Assignment #1 *)

type DATE = (int * int * int)
exception InvalidParameter

(* This file is where your solutions go *)

fun is_older(d1: DATE, d2: DATE): bool =
    if (#1 d1) > (#1 d2)
    then false
    else if (#1 d1) < (#1 d2)
    then true
    else if (#2 d1) > (#2 d2)
    then false
    else if (#2 d1) < (#2 d2)
    then true
    else if (#3 d1) >= (#3 d2)
    then false
    else true


fun number_in_month(list: DATE list, month: int): int =
    let

      fun comp(list: DATE list, count: int) =
          if null (tl list)
          then count
          else if #2(hd (tl list)) = month
          then comp(tl list, count+1)
          else comp(tl list, count)
    in
      if #2(hd list) = month
      then comp(list, 1)
      else comp(list, 0)
    end

fun number_in_months(list: DATE list, months: int list) =


    if null (tl months)
    then number_in_month(list, hd months)
    else number_in_month(list, hd months) + number_in_months(list, tl months)


fun dates_in_month(list: DATE list, month: int) =
    let

      fun comp(list: DATE list, dateList: DATE list) =
          if null (tl list)
          then dateList
          else if #2(hd (tl list)) = month
          then hd (tl list) :: comp(tl list, dateList)
          else comp(tl list, dateList)
    in
      if #2(hd list) = month
      then hd list :: comp(list, [])
      else comp(list, [])
    end


fun dates_in_months(list: DATE list, months: int list) =
    if null (tl months)
    then dates_in_month(list, hd months)
    else dates_in_month(list, hd months) @ dates_in_months(list, tl months)


fun get_nth(list: string list, pos: int) =

      let
        val length = length list
        fun dec(list: string list, position: int) =
            if position = 1
            then hd list
            else dec(tl list, position - 1)
      in
        if pos = 0 orelse length < pos
        then raise InvalidParameter
        else dec(list, pos)
      end



fun date_to_string(date: DATE) =
    let
      val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
      val year = Int.toString(#1(date))
      val month = get_nth(months, #2(date))
      val day = Int.toString(#3(date))
    in
      month ^ " " ^ day ^ ", " ^ year
    end


fun number_before_reaching_sum(sum: int, list: int list) =
    let
      fun addOne(list: int list, sumNumber: int, count: int) =
          if sumNumber + (hd list) < sum
          then addOne(tl list, sumNumber + (hd list), count + 1)
          else count
    in
      if hd list < sum
      then addOne(tl list, hd list, 1)
      else 0
    end


fun what_month(day: int) =
    number_before_reaching_sum(day, [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31])



fun month_range(day1: int, day2: int) =
    let

      fun loop(day: int) =
          if day = day2
          then [what_month(day)]
          else [what_month(day)] @ loop(day + 1)
    in
      if day1 > day2
      then []
      else loop(day1)
    end


fun oldest(list: DATE list) =
    if null list
    then NONE
    else
      let
        val tl_ans = oldest(tl list)
      in
        if isSome tl_ans andalso is_older(valOf tl_ans, hd list)
        then tl_ans
        else SOME(hd list)
      end


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

     (* replace with your solution *)

(* Add your other functions here *)

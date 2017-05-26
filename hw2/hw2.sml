(* if you use this function to compare two strings (returns true if the same
   string), then you avoid some warning regarding polymorphic comparison  *)

fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for Part 1 here *)

fun all_except_option(str, list) =
    case list of
        [] => NONE
      | (head :: tail) => if same_string(head, str)
                          then SOME tail
                          else case all_except_option(str, tail) of
                                  NONE => NONE
                                | SOME x => SOME(head :: x)


fun get_substitutions1(list, str) =
    case list of
        [] => []
      | (head :: tail) => case all_except_option(str, head) of
                              NONE => get_substitutions1(tail, str)
                            | SOME x => x @ get_substitutions1(tail, str)


fun get_substitutions2(list, str) =
    let
      fun helper(xs, s, ls) =
          case xs of
              [] => ls
            | (head :: tail) => case all_except_option(s, head) of
                                    NONE => helper(tail, s, ls)
                                  | SOME x => helper(tail, s, ls @ x)
    in
      helper(list, str, [])
    end


fun similar_names(list, name) =
    let
      fun helper(name, ls, namels) =
          case ls of
              [] => namels
            | (head :: tail) => case name of
                                    {first, middle, last} => helper(name, tail, namels @ [{first = head, last = last, middle =middle}])

    in
      case name of
          {first, middle, last} => helper(name, first :: get_substitutions2(list, first), [])
    end

(************************************************************************)
(* Game  *)

(* you may assume that Num is always used with valid values 2, 3, ..., 10 *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw


exception IllegalMove

(* put your solutions for Part 2 here *)
fun card_color(card) =
    case card of
        (Clubs, _) => Black
      | (Spades, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red


fun card_value(card) =
    case card of
        (_, Num n) => n
      | (_, Ace) => 11
      | (_, _) => 10


fun remove_card(cardls, theCard, exc) =
    let
      fun helper(cardls, theCard) =
          case cardls of
              [] => []
            | (head :: tail) => case head = theCard of
                                    true => tail
                                  | false => case helper(tail, theCard) of
                                                  ls => head :: ls
    in
      case helper(cardls, theCard) of
          xs => case xs = cardls of
                    true => raise exc
                  | false => xs
    end


fun all_same_color(cardls) =
    case cardls of
        [] => true
      | [card] => true
      | (head :: neck :: tail) => case card_color(head) = card_color(neck) of
                                      true => all_same_color(neck :: tail)
                                    | false => false


fun sum_cards(cardls) =
    let
        fun helper(cardls, sum) =
            case cardls of
                [] => sum
              | (head :: tail) => helper(tail, card_value(head) + sum)
    in
        helper(cardls, 0)
    end


fun score(cardls, goal) =
    let
      val sum = sum_cards(cardls)
    in
      if sum > goal
      then case all_same_color(cardls) of
                true => 2 * (sum - goal) div 2
              | false => 2 * (sum - goal)
      else case all_same_color(cardls) of
                true => (goal - sum) div 2
              | false => goal - sum
    end


fun officiate(cardls, movels, goal) =
    let
      fun play(cardls, movels, goal, heldls) =
          case sum_cards(heldls) > goal of
              true => score(heldls, goal)
            | false => case movels of
                          [] => score(heldls, goal)
                        | (mhead :: mtail) => case mhead of
                                                  Draw => (case cardls of
                                                              [] => score(heldls, goal)
                                                            | (chead :: ctail) => play(ctail, mtail, goal, chead :: heldls))
                                                | Discard(card) => play(cardls, mtail, goal, remove_card(heldls, card, IllegalMove))
    in
      play(cardls, movels, goal, [])
    end

(*    fun officiate(cs,m,goal)=
     let
         fun held_list(cs,m,held_cards : card list,goal:int)=
          case m of
            [] => score(held_cards,goal)
              | x::xs=> case x of
                           Discard y => held_list(cs,xs,remove_card(held_cards,y,IllegalMove),goal)
                         | DRAW =>case cs of
                                        [] => score( held_cards,goal)
                                        | y::ys=> if (sum_cards(y::held_cards)>goal)
                                        then score( y::held_cards,goal)
                                        else held_list(ys,xs,y::held_cards,goal)
      in
            held_list(cs,m,[],goal)
      end
*)

val test1_0=all_except_option("9",["4","9","10"]) = SOME["4", "10"];
val test2_0=get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
                               "Betty")
           = ["Elizabeth"];
val test3_0=get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
                                "Betty")
            = ["Elizabeth"];
val test4_0=similar_names([
                                         ["Thomas", "Neo"],
                                         ["Batman", "Hulk","Bruce"],
                                         ["Spiderman", "Peter"]
                                     ], {first="Batman", middle = "(whoknows)", last="Wayne"}) =
                        [{first="Batman",last="Wayne",middle="(whoknows)"},
                         {first="Hulk",last="Wayne",middle="(whoknows)"},
                         {first="Bruce",last="Wayne",middle="(whoknows)"}];

val HeartsQ = (Hearts, Queen);
val test5_0= card_color(HeartsQ) = Red;
val test6_0= card_value(HeartsQ) = 10;
exception notFound
val cards0 = [(Clubs, Ace), (Diamonds, King), (Spades, Queen), (Clubs, Jack)];
val test7_0 = remove_card(cards0, (Clubs, Ace), notFound) = [(Diamonds, King), (Spades, Queen), (Clubs, Jack)];
val test8_0 = all_same_color(cards0) = false;
val test9_0 = sum_cards(cards0) = 41;
val test10_0 = score([(Hearts, Queen)], 28) = 9;
val test11_1 = officiate(cards0, [], 12) = 6;

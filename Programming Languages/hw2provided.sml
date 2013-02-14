(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
  s1 = s2


(* Inputs:  A string str, A string list strlist
   Outputs: If str is in strlist then return string list option where str is removed from strlist
            else return NONE *)
fun all_except_option(str, strlist) = 
  let 
    fun aux(str, strlist, acc) = 
      case strlist of 
        []          => []
      | head::tail  => if same_string(head, str) then acc @ tail else aux(str,tail,acc@[head])
  in
    let
      val resultList = aux(str, strlist, [])
    in
      case (resultList,strlist) of
        ([],head::[]) => SOME([])
      | ([],_)        => NONE   
      | _             => SOME(resultList)
    end
  end 


(* Inputs:  A list of list of strings strlistlist, A string str
   Outputs: Returns list of strings composed of strings in lists where str was present, without str *)
fun get_substitutions1(strlistlist, str) = 
  case strlistlist of 
    []         => []
  | head::tail => let
                    val strOpt = all_except_option(str, head)
                  in
                    case strOpt of
                      SOME(strExcept) =>  if not (head = strExcept)
                                          then strExcept @ get_substitutions1(tail,str)
                                          else get_substitutions1(tail,str)
                      | _ => get_substitutions1(tail,str)
                  end

(* Inputs:  A list of list of strings strlistlist, A string str
   Outputs: Returns list of strings composed of strings in lists where str was present, without str *)
fun get_substitutions2(strlistlist, str) = 
  let 
    fun aux(strlistlist, str, acc) = 
      case strlistlist of 
        []         => acc
      | head::tail => let 
                        val strOpt    = all_except_option(str, head)
                      in
                        case strOpt of 
                          SOME(strExcept) =>  if not (head = strExcept)
                                              then aux(tail, str, acc @ strExcept)
                                              else aux(tail, str, acc)
                        | _ => aux(tail, str, acc)
                      end
  in
    aux(strlistlist, str, [])
  end

(* Inputs:  A list of list of strings strlistlist, A record with first, middle, last (names)
   Outputs: A list of records listing the original name, along with the alternative names *)
fun similar_names(strlistlist, {first=fname,middle=mname,last=lname}) = 
  let
    val oname = {first=fname,middle=mname,last=lname}
    val subs  = get_substitutions2(strlistlist, fname)
    fun getSubs(strlist) = 
      case strlist of
        []         => []
      | head::tail => {first=head,middle=mname,last=lname}::getSubs(tail)
  in
    oname::getSubs(subs)
  end

(* you may assume that Num is always used with values 2, 3, ..., 9
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* Inputs:  A card (suit,rank)
   Outputs: A color associated with the card's suit *)
fun card_color(card) = 
  case card of
    (Clubs, _)    => Black
  | (Spades, _)   => Black
  | (Diamonds, _) => Red
  | (Hearts, _)   => Red

(* Inputs:  A card (suit,rank)
   Outputs: A value associated with the card's rank (11 for Ace, 10 for J/Q/K, input integer for Num) *)
fun card_value(card) = 
  case card of 
    (_, Ace)    => 11
  | (_, Num(i)) => i
  | _           => 10

(* Inputs:  A list of cards cs, A card c, An exception e 
   Outputs: If c is in cs, return cs without c, else raise exception e  *)
fun remove_card(cs, c, e) = 
  case cs of
    []         => raise e
  | head::tail => if c = head then tail else head::remove_card(tail, c, e)

(* Inputs:  A list of cards cs 
   Outputs: Returns true if all cards have the same color, else return false *)
fun all_same_color(cs) = 
  case cs of 
    []                 => true 
  | head::[]           => true
  | head::(neck::[])   => card_color(head) = card_color(neck)
  | head::(neck::tail) => card_color(head) = card_color(neck) andalso (all_same_color(neck::tail))

(* Inputs:  A list of cards cs
   Outputs: An integer which is the sum of the ranks of all the cards in cs *)
fun sum_cards(cs) = 
  let
    fun aux(cs, acc) = 
      case cs of 
      []         => acc
    | head::tail => aux(tail, card_value(head) + acc)
  in
    aux(cs, 0)
  end

(* Inputs:  A list of cards cs, An integer goal
   Outputs: An integer which is the score of the list of cards cs as specified in the rules *)
fun score(cs, goal) = 
  let
    val sameColors = all_same_color(cs)
    val sumOfCards = sum_cards(cs)
    val prelimScore = if sumOfCards > goal then 3 * (sumOfCards - goal) else goal - sumOfCards
  in
    if sameColors then prelimScore div 2 else prelimScore
  end

(* Inputs:  A list of cards cardList, A list of moves moveList, An integer goal
   Outputs: The outcome/score of the game as per the cardList, moveList, and goal *)
fun officiate(cardList, moveList, goal) = 
  let 
    fun aux(cardList, moveList, acc) =
      let 
        val handTotal = sum_cards(acc)
      in
        if handTotal > goal then acc 
        else 
          case (moveList,cardList) of 
            ([],_)                                => acc
          | (_,[])                                => acc
          | (curMove::otherMoves, head_c::tail_c) => case curMove of
                                                        Draw => aux(tail_c, otherMoves, head_c::acc)
                                                      | Discard(i) => aux(tail_c, otherMoves, remove_card(acc, i, IllegalMove))
      end
  in 
    score(aux(cardList, moveList, []), goal)
  end











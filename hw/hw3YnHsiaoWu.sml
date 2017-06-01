exception NoAnswer

datatype pattern = Wildcard
    | Variable of string
    | UnitP
    | ConstP of int
    | TupleP of pattern list
    | ConstructorP of string * pattern

datatype valu = Const of int
    | Unit
    | Tuple of valu list
    | Constructor of string * valu

fun g f1 f2 p =
    let 
        val r = g f1 f2 
    in
        case p of
            Wildcard => f1 ()
            | Variable x => f2 x
            | TupleP ps => List.foldl (fn (p,i) => (r p) + i) 0 ps
            | ConstructorP(_,p) => r p
            | _ => 0
    end

datatype typ = Anything
    | UnitT
    | IntT
    | TupleT of typ list
    | Datatype of string

val only_capitals = List.filter (fn s => (Char.isUpper o String.sub) (s, 0))

val longest_string1 = foldl
    (fn (ns, ps) => if String.size ns > String.size ps then ns else ps)
    ""

val longest_string2 = foldl
    (fn (ns, ps) => if String.size ns >= String.size ps then ns else ps)
    ""

fun longest_string_helper f = foldl
    (fn (ns, ps) => if f (String.size ns, String.size ps) then ns else ps)
    ""

val longest_string3 = longest_string_helper (fn (a, b) => a > b)

val longest_string4 = longest_string_helper (fn (a, b) => a >= b)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = implode o rev o explode

fun first_answer f [] = raise NoAnswer
| first_answer f (x :: xs) =
    case f x of
        NONE => first_answer f xs
        | SOME y => y

fun all_answers f ls =
    let fun loop ([], acc) = SOME acc
        | loop (x :: xs, acc) =
            case f x of
                NONE => NONE
                | SOME y => loop (xs, acc @ y)
    in loop (ls, []) end

val count_wildcards = g (fn _ => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn _ => 1) String.size

fun count_some_var (s, p) = g (fn _ => 0) (fn v => if v = s then 1 else 0) p

fun check_pat p =
    let fun get_strings (Variable x) = [x]
        | get_strings (TupleP ps) = List.foldl (fn (p,i) => i @ get_strings p) [] ps
        | get_strings (ConstructorP(_,p)) = get_strings p
        | get_strings _ = []
        fun check_repeats [] = true
        | check_repeats (x :: xs) = 
            if List.exists (fn s : string => s = x) xs
            then false
            else check_repeats (xs)
    in (check_repeats o get_strings) p end

fun match (_, Wildcard) = SOME []
| match (v, Variable s) = SOME [(s, v)]
| match (Unit, UnitP) = SOME []
| match (Const v, ConstP p) = if p = v then SOME [] else NONE
| match (Constructor (s2, v), ConstructorP (s1, p)) = 
    if s1 = s2 then match (v, p) else NONE
| match (Tuple vs, TupleP ps) =
    if length ps = length vs
    then all_answers match (ListPair.zip (vs, ps))
    else NONE
| match _ = NONE

fun first_match v ps =
    SOME (first_answer (fn p => match (v, p)) ps)
    handle NoAnswer => NONE

fun typecheck_patterns (ts, ps) =
    let
        fun zip (f, []) = SOME []
        | zip (f, p :: ps) =
            case f p of
                NONE => NONE
                | SOME t => case zip (f, ps) of
                    NONE => NONE
                    | SOME ts => SOME (t :: ts)

        fun check (Anything, t) = SOME t
        | check (t, Anything) = SOME t
        | check (UnitT, UnitT) = SOME UnitT
        | check (IntT, IntT) = SOME IntT
        | check (Datatype s, Datatype t) = if s = t then SOME (Datatype s) else NONE
        | check (TupleT ts, TupleT tt) = 
            (case zip (check, ListPair.zipEq (ts, tt)) handle ListPair.UnequalLengths => NONE of
                NONE => NONE
                | SOME ts => SOME (TupleT ts))
        | check (_, _) = NONE

        fun loop Wildcard = SOME Anything
        | loop (Variable s) = SOME Anything
        | loop (UnitP) = SOME UnitT
        | loop (ConstP c) = SOME IntT
        | loop (TupleP ps) =
            (case zip (loop, ps) of
                NONE => NONE
                | SOME ts => SOME (TupleT ts))
        | loop (ConstructorP (s, p)) =
            case List.find (fn (c, d, t) => c = s) ts of
                NONE => NONE
                | SOME (c, d, t) =>
                    case loop p of
                        NONE => NONE
                        | SOME rt =>
                            case check (rt, t) of
                                NONE => NONE
                                | SOME t => SOME (Datatype d)

        fun merge (f, ans, []) = SOME ans
        | merge (f, ans, t :: ts) = case f (t, ans) of
            NONE => NONE
            | SOME ret => merge (f, ret, ts)
    in
        case List.find (fn p => not (check_pat p)) ps of
            SOME p => NONE
            | NONE =>
                (case zip (loop, ps) of
                    NONE => NONE
                    | SOME [] => SOME Anything
                    | SOME (t :: ts) => merge (check, t, ts))
    end

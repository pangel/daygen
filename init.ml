Random.init 0

(* A person's state *)
type time = int
type plan = { date: time }

type location = Home | Work | Bar | Cinema
type activity = Working | Eating | Drinking | Resting | Canceling of plan | Void
type mood = Happy | Sad
type hunger = Full | Hungry | Starving
type friend = int


type recent = {
  eat: time;
  move: time;
  talk: time
}

type state = {
  name: string;
  location: location;
  activity: activity;
  mood: mood;
  hunger: hunger;
  plans: plan list;
  friends: friend list;
  recent: recent }

(* Return the first image of f which isn't None *)
let rec first f default = function
  | [] -> default
  | x::xs -> match f x with
    | None -> first f default xs
    | Some r -> r

(* Run the first action with a non-None result *)
let choice state time actions =
  first (fun action -> action state time) {state with activity = Void} actions



let str_of_location = function
  | Home -> "Home"
  | Work -> "Work"
  | Bar -> "Bar"
  | Cinema -> "Cinema"

let str_of_activity = function
  | Working -> "Working"
  | Eating -> "Eating"
  | Drinking -> "Drinking" 
  | Resting -> "Resting"
  | Canceling _ -> "Canceling"
  | Void -> "Void"

let str_of_mood = function
  | Happy -> "Happy"
  | Sad -> "Sad"

let str_of_hunger = function
  | Full -> "Full"
  | Hungry -> "Hungry"
  | Starving -> "Starving"

let str_of_state state = 
  Printf.sprintf
  (*"{ nm: %s, act: %s, loc: %s, mod: %s, hgr: %s, pln: %s, frd: %s, rct: %s}"*)
  "{ %7s  %7s  %7s  %7s  %8s %7s  %7s  %7s }"
  state.name
  (str_of_activity state.activity)
  (str_of_location state.location)
  (str_of_mood state.mood)
  (str_of_hunger state.hunger)
  "<plans>"
  "<friends>"
  "<recent>"

let find_soon time plans =
  let matcher plan = plan.date > time && plan.date - time < 2
  in match List.partition matcher plans with
  | plan::plans, rest -> (Some plan, (plans @ rest))
  | [], rest -> (None, rest)

(* Probabilistic methods *)

let roll p = Random.float 1. <= p

let chance_execute state =
  let mood_factor = match state.mood with
  | Happy -> 0.95
  | Sad -> 0.7
  and hunger_factor = match state.hunger with
  | Full -> 0.95
  | Hungry -> 0.8
  | Starving -> 0.7
  and activity_factor = match state.activity with
  | Resting -> 0.95
  | Working -> 0.5
  | _ -> 0.8
  in roll (mood_factor *. hunger_factor *. activity_factor)

let chance_eat state =
  print_endline (str_of_hunger state.hunger);
  let hunger_factor = match state.hunger with
  | Full -> 0.01
  | Hungry -> 0.70
  | Starving -> 0.99
  and activity_factor = match state.activity with
  | Working -> 0.90
  | Eating -> 0.1
  | Drinking -> 0.8
  | Resting -> 0.95
  | Canceling _ -> 0.90
  | Void -> 1.
  in roll (hunger_factor *. activity_factor)

let mediate state activity = ()

(* Execute an upcoming plan *)
(* Unlike other choices, which can do nothing, an upcoming plan either results
 * in an execution or a cancellation.
 *)
let execute state time =
  match find_soon time state.plans with
  | (None, _) -> None
  | (Some plan,rest) ->
      let state = {state with plans = rest} in
      if chance_execute state then
        Some {state with activity = Eating}
      else
        Some {state with activity = Canceling plan}

(* Eat if hungry enough *)
let eat state time =
  if chance_eat state 
  then
    (print_endline "eating";
      Some 
      {state with 
        hunger = Full; 
        recent = {state.recent with eat = time};
        activity = Eating }
    )
  else
    None

(*let positive n = if n < 0 then 0 else n*)
let reduce_mood = function
  | Happy -> Sad
  | Sad -> Sad

(* Update mood etc *)
let update state time =

  let hunger =
    print_endline (string_of_int (time - state.recent.eat));
    if (time - state.recent.eat) > 4
    then match state.hunger with
    | Full -> Hungry
    | Hungry -> Starving
    | Starving -> Starving
    else state.hunger

  in let mood = match state.hunger, hunger with
  | Full, _ 
  | Hungry, Hungry -> state.mood
  | _,_ -> reduce_mood state.mood

  in {state with hunger = hunger ; mood = mood}

let rec step state time =
  (*print_endline ("before: "^str_of_state state);*)
  (*let state = *)
  update (choice state time 
  [
    execute;
    eat;
    (*move;*)
    (*talk;*)
    (*plan;*)
    (*meet;*)
  ]) time
  (*in print_endline ("after: "^str_of_state state);*)
  (*state*)


let () =
  let state =
    {
      name = "John";
      location = Home;
      activity = Resting;
      mood = Happy;
      hunger = Starving;
      plans = [];
      friends = [];
      recent = { eat = 0; move = 0; talk = 0 }
    }
  in
  let r = ref 0 in
  let rec ff s t =
    Printf.printf "%2d %s\n" t (str_of_state s);
    let s' = step s (t+1)
    in
    incr r;
    if !r < 15 then ff s' (t+1) else ()
  in ff state 0





(*
 * eat
 *  what
 *  when
 *  image
 *  description
 *  recipe
 * entertain
 *   movie
 *   theater
 *   book
 *   source
 * work
 *   where
 *   feeling
 *
 * talk
 *   media
 *   people
 *   tone
 * meet
 *   who
 *   where
 *   for what
 * plan
 *   all the above
 * move
 *
 *
 * twitter
 * facebook
 * email
 * calendar
 * gps
 * whatsapp
 * bank
 * qself
 *
 * parking stuff
 * phone transcripts
 * browsing history
 *)

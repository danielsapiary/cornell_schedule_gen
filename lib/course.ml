open Yojson.Basic

type interval = int * int
(** AF: The tuple [(st,en)] represents the time interval between [st] and [en],
    including [st] but not [en]. Times are integers representing minutes since
    midnight on Monday morning. Ex: 12:00 PM Monday is [720]. 11:59 PM Monday is
    [1439]. 12:00 AM Tuesday is [1440]. 11:59 PM Sunday is [10079]

    RI: The ints must be between [0] and [10079]. *)

type t = {
  title : string;
  lectures : (string * interval list) list option;
  discussions : (string * interval list) list option;
  labs : (string * interval list) list option;
}
(** AF: The record [t] represents the class with the title [title], all offered
    class times named given by the [fst] elements of [lectures] and the
    corresponding times by [snd] elements, all offered dicussion times named
    given by the [fst] elements of [discusssions] and the corresponding times by
    [snd] elements, all offered class times named given by the [fst] elements of
    [labs] and the corresponding times by [snd] elements

    RI: A course must have a name/title and all possible times for its
    lectures/labs/discussion sections. If a course does not have one of these
    (ie. no lab), then that option must be [None] or [[]]. *)

type times = {
  lecture : (string * interval list) option;
  discussion : (string * interval list) option;
  lab : (string * interval list) option;
}
(** AF: The record [times] represents the available meeting times for a course,
    where [lecture], [discussion], and [lab] fields are optional lists of
    intervals representing possible times for each component. [None] indicates
    that there are no available times for that component.

    RI: Each list of intervals, if present, must represent valid,
    non-overlapping time slots for the respective course component. The
    intervals must be in the valid range of times and should not overlap with
    each other. *)

exception PreconditionViolated

let create_interval (start_time, end_time) : interval = (start_time, end_time)

let create_course name (lec, dis, la) =
  {
    title = name;
    lectures =
      (match lec with
      | Some (h :: t) -> Some (h :: t)
      | _ -> None);
    discussions =
      (match dis with
      | Some (h :: t) -> Some (h :: t)
      | _ -> None);
    labs =
      (match la with
      | Some (h :: t) -> Some (h :: t)
      | _ -> None);
  }

let day_to_time = function
  | "Monday" -> 0
  | "Tuesday" -> 1440
  | "Wednesday" -> 2880
  | "Thursday" -> 4320
  | "Friday" -> 5760
  | "Saturday" -> 7200
  | "Sunday" -> 8640
  | _ -> raise PreconditionViolated [@coverage off]

let time_of_day time =
  match List.of_seq (String.to_seq time) with
  | [ h; hh; hhh; hhhh ] ->
      let ret_num =
        (600 * (int_of_char h - 48))
        + (60 * (int_of_char hh - 48))
        + (10 * (int_of_char hhh - 48))
        + int_of_char hhhh - 48
      in
      ret_num
  | _ -> raise PreconditionViolated [@coverage off]

let get_line line =
  match line with
  | [] -> raise PreconditionViolated [@coverage off]
  | name :: data ->
      let rec remain = function
        | h :: t -> (
            match String.split_on_char ' ' h with
            | [ a; b; c; d ] ->
                create_interval
                  (day_to_time a + time_of_day b, day_to_time c + time_of_day d)
                :: remain t
            | _ -> raise PreconditionViolated [@coverage off])
        | [] -> []
      in
      (name, remain data)

let create_course2 lst_lst =
  let rec helper lst =
    match lst with
    | [ "Discussions" ] :: t | [ "Labs" ] :: t -> ([], t)
    | h :: t ->
        let temp = helper t in
        (get_line h :: fst temp, snd temp)
    | _ -> ([], lst)
  in
  match lst_lst with
  | h :: hh :: t ->
      let lec = helper t in
      let dis = helper (snd lec) in
      let la = helper (snd dis) in
      {
        title = List.hd h;
        lectures =
          (match fst lec with
          | [] -> None
          | x -> Some x);
        discussions =
          (match fst dis with
          | [] -> None
          | x -> Some x);
        labs =
          (match fst la with
          | [] -> None
          | x -> Some x);
      }
  | _ -> raise PreconditionViolated [@coverage off]

let get_title course = course.title

let get_lectures course =
  match course.lectures with
  | Some [] -> None
  | x -> x

let get_discussions course =
  match course.discussions with
  | Some [] -> None
  | x -> x

let get_labs course =
  match course.labs with
  | Some [] -> None
  | x -> x

let create_times (lec, dis, la) =
  {
    lecture =
      (match lec with
      | Some (_, []) -> None
      | _ -> lec);
    discussion =
      (match dis with
      | Some (_, []) -> None
      | _ -> dis);
    lab =
      (match la with
      | Some (_, []) -> None
      | _ -> la);
  }

let get_lecture (t : times) = t.lecture
let get_discussion (t : times) = t.discussion
let get_lab (t : times) = t.lab
let get_interval time_interval = (fst time_interval, snd time_interval)

let interval_overlap interval1 interval2 =
  if fst interval1 > snd interval1 && fst interval2 > snd interval2 then true
  else if fst interval1 > snd interval1 then
    if fst interval2 < snd interval1 || snd interval2 > fst interval1 then true
    else false
  else if fst interval2 > snd interval2 then
    if fst interval1 < snd interval2 || snd interval1 > fst interval2 then true
    else false
  else if
    (fst interval1 >= fst interval2 && fst interval1 <= snd interval2)
    || (fst interval2 >= fst interval1 && fst interval2 <= snd interval1)
  then true
  else false

let length_overlap interval1 interval2 =
  if fst interval1 > snd interval1 && fst interval2 > snd interval2 then
    min (snd interval1) (snd interval2)
    + 10080
    - max (fst interval1) (fst interval2)
  else if fst interval1 > snd interval1 then
    if fst interval2 < snd interval1 then snd interval2 - fst interval2
    else if snd interval2 > fst interval1 then
      min
        (abs (snd interval2 - fst interval2))
        (abs (snd interval2 - fst interval1))
    else 0
  else if fst interval2 > snd interval2 then
    if fst interval1 < snd interval2 then snd interval1 - fst interval1
    else if snd interval1 > fst interval2 then
      min
        (abs (snd interval1 - fst interval1))
        (abs (snd interval1 - fst interval2))
    else 0
  else if
    (fst interval1 >= fst interval2 && fst interval1 <= snd interval2)
    || (fst interval2 >= fst interval1 && fst interval2 <= snd interval1)
  then
    min
      (min
         (abs (fst interval1 - snd interval1))
         (abs (fst interval2 - snd interval2)))
      (min
         (abs (snd interval1 - fst interval2))
         (abs (snd interval1 - fst interval2)))
  else 0

let times_overlap times1 times2 =
  (* Turn None into Nil *)
  let collect_intervals = function
    | Some (_, intervals) -> intervals
    | None -> []
  in
  let intervals1 =
    List.concat
      [
        collect_intervals times1.lecture;
        collect_intervals times1.discussion;
        collect_intervals times1.lab;
      ]
  in
  let intervals2 =
    List.concat
      [
        collect_intervals times2.lecture;
        collect_intervals times2.discussion;
        collect_intervals times2.lab;
      ]
  in
  List.exists
    (fun i1 -> List.exists (fun i2 -> interval_overlap i1 i2) intervals2)
    intervals1

(**[interval_list_overlap lst1 lst2] is whether any [interval] in [lst1]
   overlaps with any [interval] in [lst2]. Useful as a helper function*)
let rec interval_list_overlap lst1 lst2 =
  let rec overlap_exist elem = function
    | [] -> false
    | h :: t -> if interval_overlap elem h then true else overlap_exist elem t
  in
  match lst1 with
  | [] -> false
  | h :: t ->
      if overlap_exist h lst2 then true else interval_list_overlap t lst2

let configs course =
  (* Function to merge lecture, discussion, and lab times into combinations *)
  let merge_times lectures discussions labs =
    let map_none_to_empty v =
      match v with
      | None -> []
      | Some t -> t
    in
    let new_lec = map_none_to_empty lectures in
    let new_dis = map_none_to_empty discussions in
    let new_labs = map_none_to_empty labs in
    match (new_lec, new_dis, new_labs) with
    | [], [], [] -> [ { lecture = None; discussion = None; lab = None } ]
    | h :: t, [], [] ->
        List.map
          (fun elem -> { lecture = Some h; lab = None; discussion = None })
          (h :: t)
    | [], h :: t, [] ->
        List.map
          (fun elem -> { lecture = None; lab = None; discussion = Some h })
          (h :: t)
    | [], [], h :: t ->
        List.map
          (fun elem -> { lecture = None; lab = Some h; discussion = None })
          (h :: t)
    | h :: t, hh :: tt, [] ->
        List.flatten
          (List.map
             (fun outer ->
               List.fold_left
                 (fun acc inner ->
                   if not (interval_list_overlap (snd outer) (snd inner)) then
                     {
                       lecture = Some outer;
                       discussion = Some inner;
                       lab = None;
                     }
                     :: acc
                   else acc)
                 [] (hh :: tt))
             (h :: t))
    | h :: t, [], hh :: tt ->
        List.flatten
          (List.map
             (fun outer ->
               List.fold_left
                 (fun acc inner ->
                   if not (interval_list_overlap (snd outer) (snd inner)) then
                     {
                       lecture = Some outer;
                       discussion = None;
                       lab = Some inner;
                     }
                     :: acc
                   else acc)
                 [] (hh :: tt))
             (h :: t))
    | [], h :: t, hh :: tt ->
        List.flatten
          (List.map
             (fun outer ->
               List.fold_left
                 (fun acc inner ->
                   if not (interval_list_overlap (snd outer) (snd inner)) then
                     {
                       lecture = None;
                       discussion = Some outer;
                       lab = Some inner;
                     }
                     :: acc
                   else acc)
                 [] (hh :: tt))
             (h :: t))
    | h :: t, hh :: tt, hhh :: ttt ->
        List.flatten
          (List.map
             (fun elem ->
               List.flatten
                 (List.map
                    (fun elem2 ->
                      List.fold_left
                        (fun acc elem3 ->
                          if
                            not
                              (interval_list_overlap (snd elem) (snd elem2)
                              || interval_list_overlap (snd elem2) (snd elem3)
                              || interval_list_overlap (snd elem) (snd elem3))
                          then
                            {
                              lecture = Some elem;
                              lab = Some elem3;
                              discussion = Some elem2;
                            }
                            :: acc
                          else acc)
                        [] (hhh :: ttt))
                    (hh :: tt)))
             (h :: t))
  in
  (course.title, merge_times course.lectures course.discussions course.labs)

let interval_to_string inte =
  let string_of_time num =
    let day_of_week =
      [|
        "Monday";
        "Tuesday";
        "Wednesday";
        "Thursday";
        "Friday";
        "Saturday";
        "Sunday";
      |]
    in
    let day = day_of_week.(num / 1440) in
    let mins = num mod 1440 in
    let hours = mins / 60 in
    let mins =
      if mins mod 60 < 10 then "0" ^ string_of_int (mins mod 60)
      else string_of_int (mins mod 60)
    in
    let am_pm = if hours < 12 then "AM" else "PM" in
    let display_hour =
      if hours = 0 then 12 else if hours > 12 then hours - 12 else hours
    in
    day ^ " " ^ string_of_int display_hour ^ ":" ^ mins ^ " " ^ am_pm
  in
  string_of_time (fst inte) ^ " - " ^ string_of_time (snd inte)
[@@coverage off]

let course_to_string course =
  let rec group_to_string =
    let rec str_of_str_lst lst =
      match lst with
      | [] -> ""
      | [ h ] -> h
      | h :: t -> h ^ ", " ^ str_of_str_lst t
    in
    function
    | _, [] -> " "
    | name, inte_lst ->
        name ^ " " ^ str_of_str_lst (List.map interval_to_string inte_lst)
  in
  let rec get_all data =
    match data with
    | [] -> ""
    | h :: t -> group_to_string h ^ "\n" ^ get_all t
  in
  let le =
    match get_lectures course with
    | None | Some [] -> "No Lectures\n"
    | Some lec -> "Lectures:\n" ^ get_all lec
  in
  let di =
    match get_discussions course with
    | None | Some [] -> "No Discussions\n"
    | Some dis -> "Discussions:\n" ^ get_all dis
  in
  let la =
    match get_labs course with
    | None | Some [] -> "No Labs\n"
    | Some lab -> "Labs:\n" ^ get_all lab
  in
  get_title course ^ "\n" ^ le ^ di ^ la
[@@coverage off]

let times_to_string choice =
  let rec str_of_str_lst lst =
    match lst with
    | [] -> ""
    | [ h ] -> h
    | h :: t -> h ^ ", " ^ str_of_str_lst t
  in
  let lec =
    match get_lecture choice with
    | None | Some (_, []) -> "No Lecture\n"
    | Some (name, meetings) ->
        name ^ ": "
        ^ str_of_str_lst (List.map interval_to_string meetings)
        ^ "\n"
  in
  let dis =
    match get_discussion choice with
    | None | Some (_, []) -> "No Discussion\n"
    | Some (name, meetings) ->
        name ^ ": "
        ^ str_of_str_lst (List.map interval_to_string meetings)
        ^ "\n"
  in
  let lab =
    match get_lab choice with
    | None | Some (_, []) -> "No Lab\n"
    | Some (name, meetings) ->
        name ^ ": "
        ^ str_of_str_lst (List.map interval_to_string meetings)
        ^ "\n"
  in
  lec ^ dis ^ lab
[@@coverage off]

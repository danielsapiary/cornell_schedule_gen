open Yojson.Basic

type t = (string * Course.times) list
(** AF: The list of tuples represents a schedule, where each tuple contains a
    name along with specific times assigned for its lecture, discussion, and
    lab. Each course is associated with intervals that indicate when those
    components occur, and [None] values indicate that the component (lecture,
    discussion, or lab) is not present for that course The name is the name of
    course from where the times have been chosen.

    RI: The schedule must not contain any overlapping intervals for any of the
    assigned times of courses. Each assigned interval (lecture, discussion, or
    lab) must be a valid choice from the respective course's available options.
    No course should appear more than once in the schedule. *)

exception InvalidSchedule

let empty = []
let schedule_times (sched : t) : Course.times list = List.map snd sched
let is_empty (sched : t) : bool = sched = empty

let has_no_conflict sched times =
  let schedule_intervals = schedule_times sched in
  not
    (List.exists
       (fun existing_times -> Course.times_overlap times existing_times)
       schedule_intervals)

let add_course sched name times : t =
  if not (has_no_conflict sched times) then raise InvalidSchedule
  else if List.exists (fun (title, _) -> title = name) sched then
    raise InvalidSchedule
  else (name, times) :: sched

let remove_course (sched : t) (name : string) : t =
  List.filter (fun (c, _) -> c <> name) sched

let course_titles (sched : t) : string list = List.map fst sched
let total_courses (sched : t) : int = List.length sched

(** [interval_to_json] is the JSON representation of an interval *)
let interval_to_json (interval : Course.interval) =
  `Assoc
    [
      ("start", `Int (fst (Course.get_interval interval)));
      ("end", `Int (snd (Course.get_interval interval)));
    ]

(** [optional_intervals_to_json] is the JSON representation of an interval list
    option *)
let optional_intervals_to_json (lst : Course.interval list option) =
  match lst with
  | None -> `Null
  | Some intervals -> `List (List.map interval_to_json intervals)

let json (sched : t) : Yojson.Basic.t =
  let courses_json =
    List.map2
      (fun course_title times ->
        `Assoc
          [
            ("title", `String course_title);
            ( "lecture_name",
              match Course.get_lecture times with
              | None -> `Null
              | Some (name, _) -> `String name );
            ( "lecture",
              optional_intervals_to_json
                (match Course.get_lecture times with
                | None -> None
                | Some (_, lst) -> Some lst) );
            ( "discussion_name",
              match Course.get_discussion times with
              | None -> `Null
              | Some (name, _) -> `String name );
            ( "discussion",
              optional_intervals_to_json
                (match Course.get_discussion times with
                | None -> None
                | Some (_, lst) -> Some lst) );
            ( "lab_name",
              match Course.get_lab times with
              | None -> `Null
              | Some (name, _) -> `String name );
            ( "lab",
              optional_intervals_to_json
                (match Course.get_lab times with
                | None -> None
                | Some (_, lst) -> Some lst) );
          ])
      (course_titles sched) (schedule_times sched)
  in
  `List courses_json

let all_schedules courses =
  let rec generate_schedules courses current_schedule =
    match courses with
    | [] -> [ current_schedule ] (* Base case: return the current schedule *)
    | course :: rest ->
        let _, configs = Course.configs course in
        List.fold_left
          (fun valid_schedules config ->
            if has_no_conflict current_schedule config then
              let new_schedule =
                add_course current_schedule (Course.get_title course) config
              in
              valid_schedules @ generate_schedules rest new_schedule
            else valid_schedules)
          [] configs
  in
  generate_schedules courses empty

let priority_func bad_times instance =
  List.fold_left
    (fun acc cur_interval ->
      acc
      + List.fold_left
          (fun acc2 times_interval ->
            Course.length_overlap cur_interval times_interval)
          0
          (match Course.get_lecture (snd instance) with
          | None -> []
          | Some (_, lst) -> lst)
      + List.fold_left
          (fun acc2 times_interval ->
            Course.length_overlap cur_interval times_interval)
          0
          (match Course.get_discussion (snd instance) with
          | None -> []
          | Some (_, lst) -> lst)
      + List.fold_left
          (fun acc2 times_interval ->
            Course.length_overlap cur_interval times_interval)
          0
          (match Course.get_lab (snd instance) with
          | None -> []
          | Some (_, lst) -> lst))
    0 bad_times

let priority_schedules priorityf courses =
  let sort_func sched1 sched2 =
    let cost1 =
      List.fold_left (fun acc elem -> priorityf elem + acc) 0 sched1
    in
    let cost2 =
      List.fold_left (fun acc elem -> priorityf elem + acc) 0 sched2
    in
    cost1 - cost2
  in
  List.sort sort_func (all_schedules courses)

let rec to_string sched =
  match sched with
  | [] -> ""
  | (s, c) :: t ->
      let helper no_string func =
        match func c with
        | None -> no_string ^ "\n\n"
        | Some t ->
            fst t ^ ": "
            ^ List.fold_left
                (fun acc elem -> (Course.interval_to_string elem ^ "\n") ^ acc)
                "" (snd t)
            ^ "\n"
      in
      let lec = helper "No Lectures" Course.get_lecture in
      let dis = helper "No Discussions" Course.get_discussion in
      let lab = helper "No Labs" Course.get_lab in

      s ^ ":\n" ^ lec ^ dis ^ lab ^ "\n\n" ^ to_string t
[@@coverage off]

type t = Course.t list

exception Invalid_parameters of string

(* Helper function to convert time string to minutes since midnight *)
let time_to_minutes time_str =
  let hour, minutes, am_pm =
    Scanf.sscanf time_str "%d:%d%s" (fun x y z -> (x, y, z))
  in
  ((if String.equal am_pm "PM" && hour <> 12 then hour + 12 else hour) * 60)
  + minutes

(* Helper function to convert day to base minutes since Monday *)
let day_to_monday_minutes day =
  match day with
  | 'M' -> 0
  | 'T' -> 1440
  | 'W' -> 2880
  | 'R' -> 4320
  | 'F' -> 5760
  | 'S' -> 7200 (* Saturday *)
  | 'U' | 'u' -> 8640 (* Sunday *)
  | _ -> raise (Invalid_argument ("Invalid day: " ^ String.make 1 day))

let parse_intervals time_start time_end days =
  if time_start = "" then []
  else
    let start_minutes = time_to_minutes time_start in
    let end_minutes = time_to_minutes time_end in
    String.fold_left
      (fun acc day ->
        let base_minutes = day_to_monday_minutes day in
        Course.create_interval
          (base_minutes + start_minutes, base_minutes + end_minutes)
        :: acc)
      [] days

(* Safely extract a string from a JSON node. If null or not a string, return
   "". *)
let json_to_string_default json_val =
  match json_val with
  | `Null -> ""
  | `String s -> s
  | _ -> ""

(* Safely extract a list from a JSON node. If null or not a list, return []. *)
let json_to_list_default json_val =
  match json_val with
  | `Null -> []
  | `List l -> l
  | _ -> []

let parse_json (json : Yojson.Basic.t) : Course.t list =
  let data = Yojson.Basic.Util.member "data" json in
  data
  |> Yojson.Basic.Util.member "classes"
  |> json_to_list_default
  |> List.map (fun item ->
         let class_number =
           item
           |> Yojson.Basic.Util.member "catalogNbr"
           |> json_to_string_default
         in
         let subject =
           item |> Yojson.Basic.Util.member "subject" |> json_to_string_default
         in
         let enroll_groups =
           item
           |> Yojson.Basic.Util.member "enrollGroups"
           |> json_to_list_default
         in

         let parse_sections sections =
           List.fold_right
             (fun section
                  (lecture_intervals, discussion_intervals, lab_intervals) ->
               let meeting_type =
                 section
                 |> Yojson.Basic.Util.member "ssrComponent"
                 |> json_to_string_default
               in
               let section_nbr =
                 section
                 |> Yojson.Basic.Util.member "section"
                 |> json_to_string_default
               in
               let section_name = meeting_type ^ " " ^ section_nbr in
               let meetings =
                 section
                 |> Yojson.Basic.Util.member "meetings"
                 |> json_to_list_default
               in
               let intervals =
                 List.flatten
                   (List.map
                      (fun meeting ->
                        let time_start =
                          meeting
                          |> Yojson.Basic.Util.member "timeStart"
                          |> json_to_string_default
                        in
                        let time_end =
                          meeting
                          |> Yojson.Basic.Util.member "timeEnd"
                          |> json_to_string_default
                        in
                        let days =
                          meeting
                          |> Yojson.Basic.Util.member "pattern"
                          |> json_to_string_default
                        in
                        parse_intervals time_start time_end days)
                      meetings)
               in
               match meeting_type with
               | "DIS" ->
                   ( lecture_intervals,
                     (section_name, intervals) :: discussion_intervals,
                     lab_intervals )
               | "LAB" ->
                   ( lecture_intervals,
                     discussion_intervals,
                     (section_name, intervals) :: lab_intervals )
               | "LEC" | _ ->
                   ( (section_name, intervals) :: lecture_intervals,
                     discussion_intervals,
                     lab_intervals ))
             sections ([], [], [])
         in

         let time_lists =
           List.fold_right
             (fun group (lecs, discs, labs) -> parse_sections group)
             (List.map
                (fun item ->
                  item
                  |> Yojson.Basic.Util.member "classSections"
                  |> json_to_list_default)
                enroll_groups)
             ([], [], [])
         in
         let make_option_tuple (a, b, c) =
           let to_option = function
             | [] -> None
             | lst -> Some lst
           in
           (to_option a, to_option b, to_option c)
         in
         Course.create_course (subject ^ class_number)
           (make_option_tuple time_lists))

let check_status json error_message =
  let status =
    Yojson.Basic.Util.member "status" json |> json_to_string_default
  in
  if status <> "success" then raise (Invalid_parameters error_message)

let fetch_roster semester subject =
  let url =
    "https://classes.cornell.edu/api/2.0/search/classes.json?roster=" ^ semester
    ^ "&subject=" ^ subject
  in
  let buffer = Buffer.create 20000 in
  let curl = Curl.init () in
  Curl.set_url curl url;
  Curl.set_writefunction curl (fun data ->
      Buffer.add_string buffer data;
      String.length data);

  (try Curl.perform curl
   with Curl.CurlException (_code, _i, _s) ->
     Curl.cleanup curl;
     raise
       (Invalid_parameters
          ("Failed API call. Semester: " ^ semester ^ ", Subject: " ^ subject)));

  Curl.cleanup curl;
  let response = Buffer.contents buffer in
  let json = Yojson.Basic.from_string response in
  check_status json
    ("Failed API call. Semester: " ^ semester ^ ", Subject: " ^ subject);
  parse_json json

let get_courses roster = roster

let fetch_all_rosters semester =
  let url =
    "https://classes.cornell.edu/api/2.0/config/subjects.json?roster="
    ^ semester
  in
  let buffer = Buffer.create 20000 in
  let curl = Curl.init () in
  Curl.set_url curl url;
  Curl.set_writefunction curl (fun data ->
      Buffer.add_string buffer data;
      String.length data);

  (try Curl.perform curl
   with Curl.CurlException (_code, _i, _s) ->
     Curl.cleanup curl;
     raise (Invalid_parameters ("Failed API call. Semester: " ^ semester)));

  Curl.cleanup curl;
  let response = Buffer.contents buffer in
  let json = Yojson.Basic.from_string response in
  check_status json ("Failed API call. Semester: " ^ semester);

  let data = Yojson.Basic.Util.member "data" json in
  let subject_list =
    data
    |> Yojson.Basic.Util.member "subjects"
    |> json_to_list_default
    |> List.map (fun item ->
           item |> Yojson.Basic.Util.member "value" |> json_to_string_default)
  in
  List.fold_right
    (fun subject lst -> List.append (fetch_roster semester subject) lst)
    subject_list []

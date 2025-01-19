open Yojson.Basic

type t
(** A type representing a schedule. A schedule will consist of [Course.times]
    and corresponding names of courses where each course is assigned specific
    times for lecture, discussion, and lab. A valid schedule is one in which no
    courses have overlapping times, and all selected times are valid options for
    the respective courses. *)

exception InvalidSchedule
(** Raised when an attempt is made to add a course to the schedule with selected
    times that conflict with the existing schedule or that are invalid for that
    course. *)

val empty : t
(** [empty] is an empty schedule. *)

val is_empty : t -> bool
(** [is_empty sched] is [true] if and only if the schedule [sched] is empty. *)

val add_course : t -> string -> Course.times -> t
(** [add_course sched course times] is the schedule [sched] with the course
    [course] added with the specific times for [lecture], [discussion], and
    [lab] as specified in [times].

    - [times]: The assigned times for the course, which may include lecture,
      discussion, and lab.

    Raises: [InvalidSchedule] if any of the following conditions hold:
    - The selected [lecture], [discussion], or [lab] times overlap with times of
      existing courses in [sched].
    - The selected times are not valid options for the course (i.e., not part of
      the course’s possible time slots). *)

val remove_course : t -> string -> t
(** [remove_course sched course] is the schedule [sched] with [course] removed.
    Returns [empty] if [is_empty sched]. *)

val course_titles : t -> string list
(** [courses sched] is the list of all course titles in the schedule [sched]. *)

val total_courses : t -> int
(** [total_courses sched] is the total number of courses in the schedule
    [sched]. *)

val has_no_conflict : t -> Course.times -> bool
(** [has_conflict sched times] is [true] if and only if the given [times] do not
    overlap with those in [sched]. *)

val schedule_times : t -> Course.times list
(** [schedule_times sched] is the list of all times that courses in [sched]
    occupy. *)

val json : t -> Yojson.Basic.t
(** [json sched] returns the JSON representation of the schedule [sched].

    Each course in the schedule is represented as an object with the following
    fields:
    - ["title"]: the course name as a string.
    - ["lecture_name"]: the name of the selected lecture section, or [null] if
      none.
    - ["lecture"]: a list of intervals for the selected lecture times, or [null]
      if none.
    - ["discussion_name"]: the name of the selected discussion section, or
      [null] if none.
    - ["discussion"]: a list of intervals for the selected discussion times, or
      [null] if none.
    - ["lab_name"]: the name of the selected lab section, or [null] if none.
    - ["lab"]: a list of intervals for the selected lab times, or [null] if
      none.

    Each interval in ["lecture"], ["discussion"], and ["lab"] includes:
    - ["start"]: the start time in minutes from midnight on Monday.
    - ["end"]: the end time in minutes from midnight on Monday.

    Example: If the schedule contains two courses:
    - "CS3110" with a lecture section named "Lec01" from 800 to 900 and no
      discussion or lab.
    - "ENTOM2030" with a lecture section named "Lec02" from 1000 to 1100 and no
      discussion or lab.

    The JSON representation will be:
    {[
      [
        {
          "title": "CS3110",
          "lecture_name": "Lec01",
          "lecture": [ { "start": 800, "end": 900 } ],
          "discussion_name": null,
          "discussion": null,
          "lab_name": null,
          "lab": null
        },
        {
          "title": "ENTOM2030",
          "lecture_name": "Lec02",
          "lecture": [ { "start": 1000, "end": 1100 } ],
          "discussion_name": null,
          "discussion": null,
          "lab_name": null,
          "lab": null
        }
      ]
    ]} *)

val all_schedules : Course.t list -> t list
(** [all_schedules courses] returns a list of all possible valid schedules that
    can be created from the given list of [courses].

    Example:
    - If [courses] contains ["CS3110", "ENTOM2030"], the resulting list will
      contain all possible schedules where these courses do not overlap. *)

val priority_schedules :
  (string * Course.times -> int) -> Course.t list -> t list
(**[priority_schedules priority_fun courses] is the list of the best schedules
   that include all courses in [courses] according to [priority_fun ] where a
   lower priority is more preferable. [priority_schedules  priority_fun courses]
   is guarenteed to be an equivalent set to [all_schedules courses] (The same
   order is not guarenteed and not likely). Note: If no schedules are possible
   then [priority_schedules courses num priority_fun] is [[]] Example:
   - If [courses] contains ["CS3110", "ENTOM2030"] and [priority_fun] is
     [priority_func bad_times] the resulting list will contain either all
     possible schedules where these courses do not overlap, sorted from the
     fewest amount of overlap with [bad_times] to the most.*)

val priority_func : Course.interval list -> string * Course.times -> int
(**[priority_func bad_times instance] is the sum of the amount of minutes that
   [instance] overlaps with all elements of [bad_times]. Note:
   [priority_func bad_times] is the recommended priority function for
   [priority_schedules]. *)

val to_string : t -> string
(**[to_string sched] is a representation of the course as a string. No
   restrictions are placed on how it is implemented except that every instance
   of a course must have its name represented, and every course in the schedule
   must be separated by at least 1 empty line *)

type interval
(** An arbitrary datatype representing an interval of time. Should be used to
    keep track of course times. *)

type t
(** A datatype representing a course. A course must have a name/title and all
    possible times for its lectures/labs/discussion sections.

    Required: There must be a way to represent a course without a
    lab/discussion/lecture. *)

type times
(** A type representing the assigned name and times for a course, including
    specific intervals for lecture, discussion, and lab components if they
    exist. *)

exception PreconditionViolated
(** An exception that is allowed but not guarenteed to be raised if the
    precondition of a function is violated. *)

val create_course :
  string ->
  (string * interval list) list option
  * (string * interval list) list option
  * (string * interval list) list option ->
  t
(** [create_course title (lectures, discussions, labs)] is a course with the
    title [title] and the meeting times for all parts of the course. For every
    element [time] in [lectures], [discussions] or [labs], [fst time] is the
    name of the discussion/lab and [snd time] are the meeting times for that
    discussion/lab. [lectures], [discussions], and [labs] being [None] means
    that the course does not have any of those respectively*)

val create_course2 : Csv.t -> t
(** [create_course2 csv_data] is a course made from the data in [csv_data] in
    the format given by "data/example.csv". A valid example is contained in
    "data/cs_3110.csv". Note: Army time and Leading Zeros are expected in the
    times in [csv_data] *)

val create_interval : int * int -> interval
(** [create_interval (st, en)] represents the interval of time between [st]
    (inclusive) and [en] (exclusive). [st] and [en] represent minutes since
    midnight on Monday morning. Ex: 12:00 PM Monday is [720]. 11:59 PM Monday is
    [1439]. 12:00 AM Tuesday is [1400]. 11:59 PM Sunday is [10079]. Ex:
    [(10020,60)] is the time interval between 11:00 PM Sunday and 1:00 AM
    Monday, [(60,10020)] is the time interval between 1:00 AM Monday and 11:00
    PM Sunday. Requires [0] <= [st] and [en] <= [10079]**)

val get_title : t -> string
(** [get_title course] is the title of the course [course]. *)

val get_lectures : t -> (string * interval list) list option
(** [get_lectures course] are the possible lecture time combinations of the
    course [course]. If [course] has no lectures then [get_lectures course] is
    [None]. *)

val get_discussions : t -> (string * interval list) list option
(** [get_discussions course] is the discussion time combinations of the course
    [course]. If [course] has no discussion sections then
    [get_discussions course] is [None]. *)

val get_labs : t -> (string * interval list) list option
(** [get_labs course] are the possible lab time combinations of the course
    [course]. If [course] has no labs then [get_labs course] is [None]. *)

val create_times :
  (string * interval list) option
  * (string * interval list) option
  * (string * interval list) option ->
  times
(** [create_times (lecture, discussion, lab)] is a [times] record with the
    specified lecture, discussion, and lab times.

    - [lecture]: An optional list of intervals representing possible lecture
      times.
    - [discussion]: An optional list of intervals representing possible
      discussion times.
    - [lab]: An optional list of intervals representing possible lab times.

    Example: [create_times (Some lec_times, None, Some lab_times)] creates a
    [times] record with specified lecture times of [lec_times], no discussion
    times, and lab times of [lab_times]. *)

val get_lecture : times -> (string * interval list) option
(** [get_lecture times] is the list and name of lecture intervals in [times], or
    [None] if there are no lecture intervals. *)

val get_discussion : times -> (string * interval list) option
(** [get_discussion times] is the list and name of discussion intervals in
    [times], or [None] if there are no discussion intervals. *)

val get_lab : times -> (string * interval list) option
(** [get_lab times] is the list and name of lab intervals in [times], or [None]
    if there are no lab intervals. *)

val get_interval : interval -> int * int
(** [get_interval time] is the representation of an interval as a tuple of the
    start time and the end time respectively such that
    [get_interval (create_interval (start_t,end_t)) = (start_t,end_t)] for all
    valid [(start_t, end_t)]. *)

val interval_overlap : interval -> interval -> bool
(** [interval_overlap time1 time2] is [true] if the times [time1] and [time2]
    overlap at all and [false] if they do not. [interval_overlap int1 int2] must
    be equivalent to [interval_overlap int1 int2] for all valid [int1] and
    [int2].

    Note: Because the end boundary of time intervals are exclusive, if one
    interval begins at the same minute as another, there is not overlap. *)

val length_overlap : interval -> interval -> int
(**[length_overlap time1 time2] is the amount of minutes that [time1] and
   [time2] overlap. If [interval_overlap time1 time2] is equal to [false], then
   [length_overlap time1 time2] must be equal to [0]*)

val times_overlap : times -> times -> bool
(** [times_overlap t1 t2] is [true] if any of the intervals in [t1] overlap with
    any of the intervals in [t2] across lecture, discussion, or lab times. It
    checks for overlap between any of the respective components of [t1] and
    [t2].

    Example:
    - If [t1] has a lecture from 10:00 to 11:00 and [t2] has a discussion from
      10:30 to 11:30, [times_overlap t1 t2] is [true].
    - If [t1] has a lab from 14:00 to 15:00 and [t2] has a lecture from 15:00 to
      16:00, [times_overlap t1 t2] is [false].

    This function returns [true] if any overlap occurs across the times of [t1]
    and [t2], and [false] otherwise. *)

val configs : t -> string * times list
(** [configs course] returns a tuple where the first element is the title of
    [course] and the second element is a list of all valid time configurations
    for [course]. Each configuration is a combination of possible lecture,
    discussion, and lab times (if applicable). For courses with multiple time
    options, [configs] produces all valid combinations.

    Example:
    - If [course] has the title "CS1110" with two lecture times and one
      discussion time, [configs course] will return ("CS1101",
      [times1; times2]), where [times1] and [times2] have the same discussion
      time but different lecture times.

    Requires: There must be at least one valid time combination, or the course
    must have no meeting times. *)

val course_to_string : t -> string
(**[course_to_string course] is a representation of [course]. It must contain
   title of the course, All possible lectures times for the course (labeled with
   their lecture name), All possible discussion times for the course (labeled
   with their discussion name), All possible lab times for the course (labeled
   with their lab name). If a course does not have any of a
   lecture/discussion/lab, then that must be conveyed to the client *)

val interval_to_string : interval -> string
(**[interval_to_string inte] is a representation of [inte] in the format of
   ["<day> <time_of_day> <AM/PM> - <day> <time_of_day> <AM/PM>"]. Example: the
   interval created by [create_interval (720,1440)] must be represented as
   ["Monday 12:00 PM - Tuesday 12:00 AM"]*)

val times_to_string : times -> string
(**[times_to_string times] is a representation of [times]. It must contain the
   name of the course, the chosen lecture time/name, the chosen discussion
   time/name, amd the chosen lab time/name. If a course does not have any of a
   lecture/discussion/lab, then that must be conveyed to the client*)

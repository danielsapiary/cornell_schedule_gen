type t
(** A datatype representing a roster of courses. **)

exception Invalid_parameters of string
(** Raised when invalid parameters are passed to fetch functions. *)

val fetch_roster : string -> string -> t
(** [fetch_roster semester subject] returns a roster containing all classes from
    [semester] e.g. "SP24" in [subject] e.g. "MATH".
    https://classes.cornell.edu/api/2.0/search/classes.json API. **)

val get_courses : t -> Course.t list
(** [get_courses roster] returns a list of all courses from [roster].**)

val fetch_all_rosters : string -> t
(** [fetch_all_rosters semester] returns a roster containing all courses
    offerred by Cornell in [semester]. **)

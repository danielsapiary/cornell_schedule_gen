open OUnit2
open Reverse_course_registration

(* Helper function to convert a course list to a readable string for test
   output *)
let course_list_to_string courses =
  List.map Course.course_to_string courses |> String.concat "\n"

let tests =
  "test suite for Roster"
  >::: [
         (* Test: fetch_roster with valid semester and subject.*)
         ( "fetch_roster_valid" >:: fun _ ->
           let semester = "SP24" in
           let dept = "CS" in
           let roster = Roster.fetch_roster semester dept in
           let courses = Roster.get_courses roster in
           assert_bool "Expected non-empty CS courses for SP24"
             (List.length courses > 0) );
         (* Test: fetch_all_rosters with a valid semester. Note: this will take
            approximately 1.5 minutes *)
         ( "fetch_all_roster_valid" >:: fun _ ->
           let semester = "SP24" in
           let roster = Roster.fetch_all_rosters semester in
           let courses = Roster.get_courses roster in
           assert_bool "Expected non-empty courses for SP24"
             (List.length courses > 0) );
         (* Test: fetch_all_rosters with an invalid semester. *)
         ( "fetch_roster_bad_sem" >:: fun _ ->
           let semester = "FA35" in
           assert_raises
             (Roster.Invalid_parameters
                ("Failed API call. Semester: " ^ semester))
             (fun () -> ignore (Roster.fetch_all_rosters semester)) );
         (* Test: fetch_roster with a non-existent subject. *)
         ( "get_courses_non_existent_subject" >:: fun _ ->
           let semester = "SP24" in
           let subject = "Fake Subject" in
           assert_raises
             (Roster.Invalid_parameters
                ("Failed API call. Semester: " ^ semester ^ ", Subject: "
               ^ subject))
             (fun () -> ignore (Roster.fetch_roster semester subject)) );
       ]

let _ = run_test_tt_main tests

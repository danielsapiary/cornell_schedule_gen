open OUnit2
open Yojson.Basic
open Reverse_course_registration.Course
open Reverse_course_registration.Schedule

(* To save space and make the code easier to read *)

(** Functor to create a test suite for any module that matches the Schedule
    interface defined in Schedule.mli *)
module MakeScheduleTester
    (S : module type of Reverse_course_registration.Schedule)
    (C : module type of Reverse_course_registration.Course) : sig
  val all_tests : OUnit2.test list
end = struct
  (* Helper function to create a test case with a name and function *)
  let make_test name f = name >:: f

  (* Maps a list of test cases to named OUnit2 test cases *)
  let map_to_test lead_string func lst =
    List.map
      (fun (expected, input) ->
        lead_string >:: fun _ -> assert_equal expected (func input))
      lst

  (* Representation of specific courses for test cases *)
  let cs_3110 = C.create_course2 (Csv.load "../data/cs_3110.csv")
  let phys_2213 = C.create_course2 (Csv.load "../data/phys_2213.csv")
  let empty_course = C.create_course2 (Csv.load "../data/empty.csv")
  let impossible_course = C.create_course2 (Csv.load "../data/impossible.csv")

  (* Test cases for verifying that schedules do not conflict based on time
     intervals *)
  let make_has_no_conflict_tests =
    [
      make_test "test_has_no_conflict_true" (fun _ ->
          (* Non-overlapping time intervals should not conflict *)
          let times1 =
            C.create_times
              (Some ("", [ C.create_interval (800, 900) ]), None, None)
          in
          let times2 =
            C.create_times
              (Some ("", [ C.create_interval (1000, 1100) ]), None, None)
          in
          let sched = S.add_course S.empty "CS_3110" times1 in
          assert_bool "No conflict expected" (S.has_no_conflict sched times2));
      make_test "test_has_no_conflict_false" (fun _ ->
          (* Overlapping time intervals should conflict *)
          let times1 =
            C.create_times
              (Some ("", [ C.create_interval (800, 900) ]), None, None)
          in
          let times2 =
            C.create_times
              (Some ("", [ C.create_interval (850, 950) ]), None, None)
          in
          let sched = S.add_course S.empty "Name" times1 in
          assert_bool "Conflict expected" (not (S.has_no_conflict sched times2)));
    ]

  (* Test cases for adding courses to a schedule *)
  let make_add_course_tests =
    [
      make_test "test_add_course_valid" (fun _ ->
          (* Adding a valid course to an empty schedule *)
          let times =
            C.create_times
              (Some ("", [ C.create_interval (800, 900) ]), None, None)
          in
          let sched = S.add_course S.empty "CS3110" times in
          assert_equal 1 (S.total_courses sched);
          assert_equal [ "CS3110" ] (S.course_titles sched));
      make_test "test_add_course_conflict" (fun _ ->
          (* Adding a course with a conflicting time interval should raise an
             exception *)
          let times1 =
            C.create_times
              (Some ("", [ C.create_interval (800, 900) ]), None, None)
          in
          let times2 =
            C.create_times
              (Some ("", [ C.create_interval (800, 900) ]), None, None)
          in
          let sched = S.add_course S.empty "course1" times1 in
          assert_raises S.InvalidSchedule (fun () ->
              S.add_course sched "course2" times2));
    ]

  (* Test cases for removing courses from a schedule *)
  let make_remove_course_tests =
    [
      make_test "test_remove_course_existing" (fun _ ->
          (* Removing an existing course from the schedule *)
          let course =
            C.create_course "CS3110"
              (Some [ ("", [ C.create_interval (800, 900) ]) ], None, None)
          in
          let times =
            C.create_times
              (Some ("", [ C.create_interval (800, 900) ]), None, None)
          in
          let sched = S.add_course S.empty "CS3110" times in
          let sched = S.remove_course sched (C.get_title course) in
          assert_equal 0 (S.total_courses sched);
          assert_equal [] (S.course_titles sched));
      make_test "test_remove_course_non_existing" (fun _ ->
          (* Attempting to remove a non-existing course should leave the
             schedule unchanged *)
          let course1 =
            C.create_course "CS3110"
              (Some [ ("", [ C.create_interval (800, 900) ]) ], None, None)
          in
          let course2 =
            C.create_course "ENTOM2030"
              (Some [ ("", [ C.create_interval (1000, 1100) ]) ], None, None)
          in
          let times =
            C.create_times
              (Some ("", [ C.create_interval (800, 900) ]), None, None)
          in
          let sched = S.add_course S.empty "CS3110" times in
          let sched = S.remove_course sched (C.get_title course2) in
          assert_equal 1 (S.total_courses sched);
          assert_equal [ C.get_title course1 ] (S.course_titles sched));
    ]

  (* Test cases for counting the total number of courses in a schedule *)
  let make_total_courses_tests =
    map_to_test "Test total_courses function" S.total_courses
      [
        (0, S.empty);
        ( 1,
          S.add_course S.empty "CS 3110"
            (C.create_times
               (Some ("", [ C.create_interval (800, 900) ]), None, None)) );
        ( 2,
          let sched =
            S.add_course S.empty "CS 3110"
              (C.create_times
                 (Some ("", [ C.create_interval (800, 900) ]), None, None))
          in
          S.add_course sched "ENTOM 2030"
            (C.create_times
               (Some ("", [ C.create_interval (1000, 1100) ]), None, None)) );
      ]

  (* Test cases for checking if a schedule is empty *)
  let make_is_empty_tests =
    map_to_test "Test is_empty function" S.is_empty
      [
        (true, S.empty);
        ( false,
          S.add_course S.empty "CS 3110"
            (C.create_times
               (Some ("", [ C.create_interval (800, 900) ]), None, None)) );
      ]

  (* Test cases for generating all valid schedules from a list of courses *)
  let make_all_schedules_tests =
    map_to_test "Test all_schedules function"
      (fun courses ->
        let schedules = S.all_schedules courses in
        List.length schedules)
      [
        ( 1,
          [
            C.create_course "CS3110"
              (Some [ ("", [ C.create_interval (800, 900) ]) ], None, None);
            C.create_course "ENTOM2030"
              (Some [ ("", [ C.create_interval (1000, 1100) ]) ], None, None);
          ] );
        ( 0,
          [
            C.create_course "CS3110"
              (Some [ ("", [ C.create_interval (800, 900) ]) ], None, None);
            C.create_course "ENTOM2030"
              (Some [ ("", [ C.create_interval (800, 900) ]) ], None, None);
          ] );
        ( 1,
          [
            C.create_course "CS3110"
              (Some [ ("", [ C.create_interval (800, 900) ]) ], None, None);
          ] );
        (1, []);
      ]

  (* Test cases for verifying JSON serialization of schedules *)
  let make_json_tests =
    let pp_json json = Yojson.Basic.pretty_to_string json in
    [
      ( "test_json_empty_schedule" >:: fun _ ->
        (* Test JSON serialization for an empty schedule *)
        let sched = S.empty in
        let expected_json = `List [] in
        assert_equal ~printer:pp_json expected_json (S.json sched) );
      ( "test_json_single_course_all_sections" >:: fun _ ->
        (* Test JSON serialization for a single course with all sections *)
        let times =
          C.create_times
            ( Some ("1", [ C.create_interval (800, 900) ]),
              Some ("2", [ C.create_interval (1000, 1100) ]),
              Some ("3", [ C.create_interval (1200, 1300) ]) )
        in
        let sched = S.add_course S.empty "CS 3110" times in
        let expected_json =
          Yojson.Basic.from_file "../data/test_json_all_sections.json"
        in
        assert_equal ~printer:pp_json expected_json (S.json sched) );
      ( "test_json_single_course_missing_sections" >:: fun _ ->
        (* Test JSON serialization for a single course with some sections
           missing *)
        let times =
          C.create_times
            (Some ("A", [ C.create_interval (800, 900) ]), None, None)
        in
        let sched = S.add_course S.empty "CS 3110" times in
        let expected_json =
          Yojson.Basic.from_file "../data/test_json_missing_sections.json"
        in
        assert_equal ~printer:pp_json expected_json (S.json sched) );
      ( "test_json_multiple_courses" >:: fun _ ->
        (* Test JSON serialization for multiple courses *)
        let times1 =
          C.create_times
            (Some ("B", [ C.create_interval (800, 900) ]), None, None)
        in
        let times2 =
          C.create_times
            (Some ("C", [ C.create_interval (1000, 1100) ]), None, None)
        in
        let sched =
          S.add_course
            (S.add_course S.empty "CS 3110" times1)
            "ENTOM 2030" times2
        in
        let expected_json =
          Yojson.Basic.from_file "../data/test_json_multiple_courses.json"
        in
        assert_equal ~printer:pp_json expected_json (S.json sched) );
    ]

  (**[priority_sched_tests] are test cases that test the behavior of
     [S.priority_schedules] using [S.priority_func] as the priority function. *)
  let priority_sched_tests =
    List.flatten
      [
        (* Test that when no valid schedules are possible,
           [S.priority_schedules] returns [[]]. *)
        map_to_test
          "Testing that the priority_schedules function behaves\n\
          \           according to its specification when using priority_func"
          (S.priority_schedules (S.priority_func []))
          [
            ([], [ impossible_course ]);
            ([], [ empty_course; impossible_course ]);
            ([], [ cs_3110; phys_2213; impossible_course ]);
            ([], [ cs_3110; cs_3110 ]);
          ];
        (* Test that when valid schedules are possible, [S.priority_schedules]
           returns schedules with all courses. *)
        map_to_test
          "Testing that the priority_schedules function behaves\n\
          \           according to its specification when using priority_func"
          (fun (intervals, courses) ->
            List.hd (S.priority_schedules (S.priority_func intervals) courses))
          [
            ( ( Some
                  ( "LEC 001",
                    List.map C.create_interval
                      [ (805, 855); (3685, 3735); (6565, 6615) ] ),
                Some ("DIS 001", List.map C.create_interval [ (610, 685) ]),
                None )
              |> C.create_times
              |> S.add_course S.empty "CS 3110",
              ( List.map C.create_interval [ (0, 600); (690, 10080) ],
                [ cs_3110 ] ) );
            ( (C.create_times
                 ( Some
                     ( "LEC 001",
                       List.rev
                         (List.map C.create_interval
                            [ (6565, 6615); (3685, 3735); (805, 855) ]) ),
                   Some
                     ( "DIS 001",
                       List.rev (List.map C.create_interval [ (610, 685) ]) ),
                   None )
              |> S.add_course S.empty "CS 3110"
              |> S.add_course)
                "PHYS 2213"
                (C.create_times
                   ( Some
                       ( "LEC 002",
                         List.rev
                           (List.map C.create_interval
                              [ (4995, 5045); (2115, 2165) ]) ),
                     Some
                       ( "DIS 202",
                         List.map C.create_interval
                           [ (3360, 3410); (6240, 6290) ] ),
                     None )),
              ( [
                  (0, 610);
                  (686, 2115);
                  (2166, 3360);
                  (3411, 4995);
                  (5046, 6240);
                  (6291, 10080);
                ]
                |> List.map C.create_interval |> List.rev,
                [ cs_3110; phys_2213 ] ) );
          ];
      ]

  (* Combine all test cases into a single list *)
  let all_tests =
    make_has_no_conflict_tests @ make_add_course_tests
    @ make_remove_course_tests @ make_total_courses_tests
    @ make_all_schedules_tests @ make_is_empty_tests @ priority_sched_tests
    @ make_json_tests
end

module ListScheduleTester =
  MakeScheduleTester
    (Reverse_course_registration.Schedule)
    (Reverse_course_registration.Course)

(* Run the test suite *)
let suite = "schedule_tests" >::: ListScheduleTester.all_tests
let () = run_test_tt_main suite

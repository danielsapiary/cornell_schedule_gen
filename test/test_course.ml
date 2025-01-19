open OUnit2
open Yojson.Basic

(**[map_to_test lead_string func list] is a mapping to test (with the preceding
   string [lead_string]) whether, for each element [(expected,input)] of [lst],
   [expected] is equal to [func input]*)
let map_to_test lead_string func lst =
  List.map
    (fun (expected, input) ->
      lead_string >:: fun _ -> assert_equal expected (func input))
    lst

(**[TestInterval (I)] is a functorized test suite for testing
   [Reverse_course_registration.Course.interval]*)
module TestInterval
    (I : module type of Reverse_course_registration.Course) : sig
  val all_tests : OUnit2.test list
end = struct
  (**[get_nth lst n] is the [n]th index of the list [lst]. Raises [DoesNotExist]
     if [List.length lst < n] *)

  exception DoesNotExist

  let rec get_nth lst n =
    match (n, lst) with
    | 0, h :: t -> h
    | n, h :: t -> get_nth t (n - 1)
    | _ -> raise DoesNotExist

  (**[normal_intervals] is a list of intervals where the int representing the
     start time is smaller than the int representing the second time*)
  let normal_intervals =
    List.map I.create_interval
      [ (720, 1440); (1440, 2160); (1440, 2160); (4000, 5000); (0, 100) ]

  (**[intervals_over_eow] is a list of intervals where the int representing the
     start time is smaller than the int representing the second time. Note: this
     means that the time Sunday 11:59 PM is included in all intervals in this
     list*)
  let intervals_over_eow =
    List.map I.create_interval
      [ (10000, 100); (10020, 60); (10079, 0); (1, 0); (2, 1) ]

  (**[test_conflicts_with_opposite_interval num] is a list of test cases to
     check the property that any time interval from (time1, time2) does not
     conflict with (time2, time 1) for randomized time1 and time2. Requires:
     [num] >= 0. Note:
     [List.length (test_conflicts_with_opposite_interval num) = num]*)
  let test_conflicts_with_opposite_interval =
    let rec helper acc = function
      | 0 -> acc
      | num_tests ->
          let rand1 = Random.int 10080 in
          let rand2 = Random.int 10080 in
          helper
            (( "A randomized property-based test to show the property that any \
                time interval from [time1, time2) does not conflict with \
                [time2, time 1) for any time1 and time2"
             >:: fun _ ->
               assert_equal false
                 (I.interval_overlap
                    (I.create_interval (rand1, rand2))
                    (I.create_interval (rand2, rand1))) )
            :: acc)
            (num_tests - 1)
    in
    helper []

  (**[test_conflicts_with_same_interval num] is a list of test cases to check
     the property that any time interval from (time1, time2) does not conflict
     with (time2, time 1) for randomized time1 and time2. Requires: [num] >= 0.
     Note: [List.length (test_conflicts_with_same_interval num) = num]*)
  let test_conflicts_with_same_interval =
    let rec helper acc = function
      | 0 -> acc
      | num_tests ->
          let rand1 = Random.int 10080 in
          let rand2 = Random.int 10080 in
          helper
            (( "A randomized test to show the property that any time interval \
                conflicts with itself"
             >:: fun _ ->
               assert_equal true
                 (I.interval_overlap
                    (I.create_interval (rand1, rand2))
                    (I.create_interval (rand1, rand2))) )
            :: acc)
            (num_tests - 1)
    in
    helper []

  (**[manual_test (lead_string, expected, actual)] asserts that [expected] is
     equivalent to [actual] with [lead_string] as an explanation of the test*)
  let manual_test (lead_string, expected, actual) =
    lead_string >:: fun _ -> assert_equal expected actual

  (**[test_overlaps_over_eow lst] checks whether every interval in [lst]
     conflicts with every other interval in [lst]*)
  let rec test_overlaps_over_eow lst =
    let rec assume_conflict elem lead_string = function
      | [] -> []
      | h :: t ->
          manual_test (lead_string, true, I.interval_overlap h elem)
          :: assume_conflict elem lead_string t
    in
    match lst with
    | [] -> []
    | h :: t ->
        assume_conflict h
          "Any two intervals that go through the wraparound time (Monday 12:00 \
           AM) should conflict with each other"
          lst
        @ test_overlaps_over_eow t

  (**[test_symmetry num_tests] is a list of randomized test cases to check that
     [interval_overlap] is a symmetric function. Requires: [num] >= 0. Note:
     [List.length (test_conflicts_with_opposite_interval num) = num]*)
  let test_overlap_symmetry =
    let rec helper acc = function
      | 0 -> acc
      | num ->
          let rand1 = Random.int 10080 in
          let rand2 = Random.int 10080 in
          let rand3 = Random.int 10080 in
          let rand4 = Random.int 10080 in
          helper
            (( "A randomized property-based test to show that \
                [interval_overlap int1 int2] is equivalent to \
                [interval_overlap int2 int1] for all [int1] and [int2]"
             >:: fun _ ->
               assert_equal
                 (I.interval_overlap
                    (I.create_interval (rand1, rand2))
                    (I.create_interval (rand3, rand4)))
                 (I.interval_overlap
                    (I.create_interval (rand3, rand4))
                    (I.create_interval (rand1, rand2))) )
            :: acc)
            (num - 1)
    in
    helper []

  (**[test_get num_tests] is a list of randomized test cases to check the
     required property that
     [get_interval (create_interval (int1,int2)) = (int1,int2)] for any
     [(int1,int2)]*)
  let test_get =
    let rec helper acc = function
      | 0 -> acc
      | num ->
          let rand1 = Random.int 10080 in
          let rand2 = Random.int 10080 in
          helper
            (( "A randomized property-based test to show that [get_interval \
                (create_interval (elem1, elem2))] is equivalent to \
                [(elem1,elem2)] for all [elem1] and [elem2]"
             >:: fun _ ->
               assert_equal (rand1, rand2)
                 (I.get_interval (I.create_interval (rand1, rand2))) )
            :: acc)
            (num - 1)
    in
    helper []

  let test_length_overlap =
    List.map
      (fun (expected, input1, input2) ->
        "Testing that the length of an overlap follows the specification of \
         length_overlap"
        >:: fun _ -> assert_equal expected (I.length_overlap input1 input2))
      [
        (0, List.hd normal_intervals, List.hd (List.tl normal_intervals));
        (2160 - 1440, get_nth normal_intervals 1, get_nth normal_intervals 2);
        (100, get_nth normal_intervals 4, get_nth intervals_over_eow 0);
        (120, get_nth intervals_over_eow 0, get_nth intervals_over_eow 1);
        (1, I.create_interval (10000, 100), I.create_interval (10001, 10002));
        (1, I.create_interval (10000, 100), I.create_interval (9000, 10001));
        (1, I.create_interval (9000, 10001), I.create_interval (10000, 100));
      ]

  (**[chosen_tests] is a collation of all non-property-based manually chosen
     test cases by the implementor of this test functor*)
  let chosen_tests =
    List.map manual_test
      [
        ( "Two intervals that have no overlap and neither has a greater start \
           time than end time should not have overlap",
          false,
          I.interval_overlap
            (get_nth normal_intervals 0)
            (get_nth normal_intervals 3) );
        ( "Two intervals that have no overlap and neither has a greater start \
           time than end time should not have overlap",
          false,
          I.interval_overlap
            (get_nth normal_intervals 2)
            (get_nth normal_intervals 3) );
        ( "Two intervals that represent the same time window should have overlap",
          true,
          I.interval_overlap
            (get_nth normal_intervals 1)
            (get_nth normal_intervals 2) );
        ( "Two intervals that have no overlap and neither has a greater start \
           time than end time should not have overlap",
          false,
          I.interval_overlap
            (get_nth normal_intervals 3)
            (get_nth normal_intervals 4) );
      ]

  (**[all_tests] is a collation of all created tests to make them more
     convenient to access outside the module*)
  let all_tests =
    chosen_tests @ test_overlap_symmetry 50 @ test_get 50
    @ test_overlaps_over_eow intervals_over_eow
    @ test_conflicts_with_opposite_interval 50
    @ test_conflicts_with_same_interval 50
    @ test_length_overlap
end

(**[TestCourse (C)] is a functorized test suite for testing
   [Reverse_course_registration.Course.t]*)
module TestCourse (C : module type of Reverse_course_registration.Course) : sig
  val all_tests : OUnit2.test list
end = struct
  (**[cs_3110_lectures] are the times of the lectures for CS 3110*)
  let cs_3110_lectures =
    Some
      (List.map
         (fun (name, inte) -> (name, List.map C.create_interval inte))
         [ ("LEC 001", [ (805, 855); (3685, 3735); (6565, 6615) ]) ])

  (**[cs_3110_discussion] are the times of the discussions for CS 3110*)
  let cs_3110_discussion =
    Some
      (List.map
         (fun (name, inte) -> (name, List.map C.create_interval inte))
         [
           ("DIS 001", [ (610, 685) ]);
           ("DIS 002", [ (700, 775) ]);
           ("DIS 003", [ (700, 775) ]);
           ("DIS 004", [ (895, 970) ]);
           ("DIS 005", [ (895, 970) ]);
           ("DIS 006", [ (1170, 1245) ]);
           ("DIS 007", [ (2050, 2125) ]);
           ("DIS 008", [ (2140, 2215) ]);
           ("DIS 009", [ (2245, 2320) ]);
           ("DIS 010", [ (2335, 2410) ]);
           ("DIS 011", [ (1960, 2035) ]);
           ("DIS 012", [ (2245, 2320) ]);
         ])

  (**[entom_4550_lectures] are the times of the lectures for ENTOM 4550*)
  let entom_4550_lectures =
    Some
      (List.map
         (fun (name, inte) -> (name, List.map C.create_interval inte))
         [ ("LEC 001", [ (545, 595); (3425, 3475) ]) ])

  (**[entom_4550_labs] are the lab times for ENTOM 4550*)
  let entom_4550_labs =
    Some
      (List.map
         (fun (name, inte) -> (name, List.map C.create_interval inte))
         [ ("LAB 401", [ (3685, 3865) ]) ])

  (**[entom_4550] is the representation of the course ENTOM 4550*)
  let entom_4550 =
    C.create_course "ENTOM 4550" (entom_4550_lectures, None, entom_4550_labs)

  (**[cs_3110] is the representation of the course CS 3110*)
  let cs_3110 =
    C.create_course "CS 3110" (cs_3110_lectures, cs_3110_discussion, None)

  (**[empty_course] is a course with no lectures, discussions, or labs*)
  let empty_course = C.create_course "EMPTY 0000" (None, None, None)

  (**[empty_course_poor_formatting] is also a course with no lectures,
     discussions, or labs. Used to confirm that [None] and [[]] both result in
     course data [None]*)
  let empty_course_poor_formatting =
    C.create_course "EMPTY 0000" (Some [], Some [], Some [])

  let test_course =
    let times =
      Some
        [
          ("001", [ C.create_interval (0, 1) ]);
          ("002", [ C.create_interval (1440, 1441) ]);
          ("003", [ C.create_interval (2880, 2881) ]);
        ]
    in
    C.create_course "TEST 123" (times, times, times)

  (**[test_create_course2] test that creating a test from a properly formatted
     Csv file should yield the appropriate course*)
  let test_create_course2 =
    map_to_test
      "Creating a test from a properly formatted Csv file should yield the \
       appropriate course"
      C.create_course2
      [
        (cs_3110, Csv.load "../data/cs_3110.csv");
        (empty_course, Csv.load "../data/empty.csv");
        (empty_course_poor_formatting, Csv.load "../data/empty.csv");
        (entom_4550, Csv.load "../data/entom_4550.csv");
        (test_course, Csv.load "../data/test_course.csv");
      ]

  (**[test_title] are test cases to test [C.get_title]*)
  let test_title =
    map_to_test "Test the get_title function" C.get_title
      [
        ("CS 3110", cs_3110);
        ("EMPTY 0000", empty_course);
        ("ENTOM 4550", entom_4550);
        ("EMPTY 0000", empty_course_poor_formatting);
      ]

  (**[test_lectures] are test cases to test [C.get_lectures]*)
  let test_lectures =
    map_to_test "Test the get_lectures function" C.get_lectures
      [
        (cs_3110_lectures, cs_3110);
        (None, empty_course);
        (entom_4550_lectures, entom_4550);
        (None, empty_course_poor_formatting);
      ]

  (**[test_discussions] are test cases to test [C.get_discussions]*)
  let test_discussions =
    map_to_test "Test the get_discussions function" C.get_discussions
      [
        (cs_3110_discussion, cs_3110);
        (None, empty_course);
        (None, entom_4550);
        (None, empty_course_poor_formatting);
      ]

  (**[test_labs] are test cases to test [C.get_labs]*)
  let test_labs =
    map_to_test "Test the get_labs function" C.get_labs
      [
        (None, cs_3110);
        (None, empty_course);
        (entom_4550_labs, entom_4550);
        (None, empty_course_poor_formatting);
      ]

  (**[test_times_overlap] are test cases to test [C.times_overlap]*)
  let test_times_overlap =
    map_to_test "Test the times_overlap function"
      (fun (t1, t2) -> C.times_overlap t1 t2)
      [
        ( true,
          ( C.create_times
              (Some ("", [ C.create_interval (600, 660) ]), None, None),
            C.create_times
              (None, Some ("", [ C.create_interval (630, 690) ]), None) ) );
        ( false,
          ( C.create_times
              (Some ("", [ C.create_interval (600, 660) ]), None, None),
            C.create_times
              (None, None, Some ("", [ C.create_interval (700, 760) ])) ) );
        ( true,
          ( C.create_times
              (None, Some ("", [ C.create_interval (800, 850) ]), None),
            C.create_times
              (None, None, Some ("", [ C.create_interval (825, 875) ])) ) );
      ]

  (**[test_configs] are test cases to test [C.configs]*)
  let test_configs_length =
    map_to_test "Test the configs function length"
      (fun course ->
        let _, configs = C.configs course in
        List.length configs)
      [
        (12, cs_3110);
        (* CS 3110 has 1 lecture and 12 discussion options, resulting in 12
           configs *)
        (1, empty_course);
        (* Empty course has only valid meeting times with one possibility for
           each, resulting in 1 configs *)
        (1, entom_4550);
        (* ENTOM 4550 has 1 lecture and 1 lab, resulting in 1 config *)
        (1, empty_course_poor_formatting);
        (* Poorly formatted empty course has only valid meeting times with one
           possibility for each, resulting in 1 configs *)
        (0, C.create_course2 (Csv.load "../data/impossible.csv"));
        (6, C.create_course2 (Csv.load "../data/test_course.csv"));
      ]

  (**[my_entom_4550] is the only possible configuration of the course ENTOM 4550*)
  let my_entom_4550 =
    ( "ENTOM 4550",
      [
        C.create_times
          ( Some
              ( "LEC 001",
                List.map C.create_interval [ (545, 595); (3425, 3475) ] ),
            None,
            Some ("LAB 401", List.map C.create_interval [ (3685, 3865) ]) );
      ] )

  (**[test_configs_val] is whether configs returns all possible schedules of a
     course*)
  let test_configs_val =
    map_to_test
      "Testing that the configs function returns all possible configurations \
       of a course"
      C.configs
      [
        (my_entom_4550, entom_4550);
        ( ("IMPOS 0000", []),
          C.create_course2 (Csv.load "../data/impossible.csv") );
      ]

  (**[all_tests] is a collation of all created tests to make them more
     convenient to access outside the module*)
  let all_tests =
    test_title @ test_lectures @ test_discussions @ test_labs
    @ test_times_overlap @ test_configs_length @ test_create_course2
    @ test_configs_val
end

(**[TestTimes (T)] is a functorized test suite for testing
   [Reverse_course_registration.Course.times]*)
module TestTimes (T : module type of Reverse_course_registration.Course) : sig
  val all_tests : OUnit2.test list
end = struct
  (**[empty_times] is a representation of the choice of no lectures,
     discussions, or labs*)
  let empty_times = T.create_times (None, None, None)

  (**[empty_times_poor_formatting] is also a representation of the choice of no
     lectures, discussions, or labs*)
  let empty_times_poor_formatting =
    T.create_times (Some ("", []), Some ("", []), Some ("", []))

  (**[my_cs_3110] is a specific instance/combination of lecture, discussion,
     labs of the class CS 3110*)
  let my_cs_3110 =
    T.create_times
      ( Some
          ( "LEC 001",
            List.map T.create_interval
              [ (805, 855); (3685, 3735); (6565, 6615) ] ),
        Some ("DIS 001", List.map T.create_interval [ (895, 970) ]),
        None )

  (**[my_entom_4550] is a specific instance/combination of lecture, discussion,
     labs of the class ENTOM 4550*)
  let my_entom_4550 =
    T.create_times
      ( Some ("LEC 001", List.map T.create_interval [ (545, 595); (3425, 3475) ]),
        None,
        Some ("LAB 001", List.map T.create_interval [ (3685, 3865) ]) )

  (**[test_lectures] are test cases to test [T.get_lecture]*)
  let test_lecture =
    map_to_test "Test the get_lecture function" T.get_lecture
      [
        (None, empty_times);
        (None, empty_times_poor_formatting);
        ( Some
            ( "LEC 001",
              List.map T.create_interval
                [ (805, 855); (3685, 3735); (6565, 6615) ] ),
          my_cs_3110 );
        ( Some
            ("LEC 001", List.map T.create_interval [ (545, 595); (3425, 3475) ]),
          my_entom_4550 );
      ]

  (**[test_discussions] are test cases to test [C.get_discussions]*)
  let test_discussions =
    map_to_test "Test the get_discussions function" T.get_discussion
      [
        (None, empty_times);
        (None, empty_times_poor_formatting);
        (Some ("DIS 001", List.map T.create_interval [ (895, 970) ]), my_cs_3110);
        (None, my_entom_4550);
      ]

  (**[test_labs] are test cases to test [C.get_labs]*)
  let test_labs =
    map_to_test "Test the get_labs function" T.get_lab
      [
        (None, empty_times);
        (None, empty_times_poor_formatting);
        (None, my_cs_3110);
        ( Some ("LAB 001", List.map T.create_interval [ (3685, 3865) ]),
          my_entom_4550 );
      ]

  let make_create_times_tests =
    map_to_test "Test create_times function"
      (fun (lec, disc, lab) -> T.create_times (lec, disc, lab))
      [
        ( T.create_times
            ( Some ("", [ T.create_interval (800, 900) ]),
              None,
              Some ("", [ T.create_interval (1000, 1100) ]) ),
          ( Some ("", [ T.create_interval (800, 900) ]),
            None,
            Some ("", [ T.create_interval (1000, 1100) ]) ) );
        (T.create_times (None, None, None), (None, None, None));
      ]

  (**[all_tests] is a collation of all created tests to make them more
     convenient to access outside the module*)
  let all_tests =
    test_lecture @ test_discussions @ test_labs @ make_create_times_tests
end

module TestsForInterval = TestInterval (Reverse_course_registration.Course)
module TestsForCourses = TestCourse (Reverse_course_registration.Course)
module TestsForTimes = TestTimes (Reverse_course_registration.Course)

let test_suite =
  "Tests for A4"
  >::: TestsForInterval.all_tests @ TestsForCourses.all_tests
       @ TestsForTimes.all_tests

let () = run_test_tt_main test_suite

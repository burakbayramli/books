use integ_test_example;
#[test]
fn files_test1() {
    assert_ne!(integ_test_example::get_process_id(), 0, "Error in code");
}
/* Hello this is a multiline
 * comment
 */

// Hello this is a single line comment

#[test]
fn files_test2() {
    assert_eq!(1 + 1, 2);
}

#[test]
#[ignore]
fn process_test1() {
    assert!(true);
}

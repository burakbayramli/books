fn main() {
    let mut a_number = 5;
    // Create an immutable pointer to the value 5
    let raw_ptr1 = &a_number as *const i32;
    // Create a mutable pointer to the value 5
    let raw_ptr2 = &mut a_number as *mut i32;
    unsafe {
        println!("raw_ptr1 is: {}", *raw_ptr1);
        println!("raw_ptr2 is: {}", *raw_ptr2);
    }
}
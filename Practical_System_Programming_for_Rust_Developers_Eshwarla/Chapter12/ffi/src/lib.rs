#[no_mangle]
pub extern "C" fn see_ffi_in_action() {
    println!("Congrats! You have successfully invoked Rust shared library from a C program");
}

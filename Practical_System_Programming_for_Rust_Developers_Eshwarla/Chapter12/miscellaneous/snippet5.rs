use std::ffi::{CStr, CString};
use std::os::raw::c_char;
extern "C" {
    fn getenv(s: *const c_char) -> *mut c_char;
}
fn main() {
    let c1 = CString::new("MY_VAR").expect("Error");
    unsafe {
        println!("env got is {:?}", CStr::from_ptr(getenv(c1.as_ptr())));
    }
}
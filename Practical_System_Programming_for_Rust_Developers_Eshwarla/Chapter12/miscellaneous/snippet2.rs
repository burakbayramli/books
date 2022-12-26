fn main() {
    let num = 23;
    let borrowed_num = &num; // immutable reference to num
    let raw_ptr = borrowed_num as *const i32; // cast
                                              // reference borrowed_num to raw pointer
    unsafe {
        assert!(*raw_ptr == 23);
    }
}

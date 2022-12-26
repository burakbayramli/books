fn main() {
    let num = 23;
    let borrowed_num = &num; // immutable reference to num
    let raw_ptr = borrowed_num as *const i32; // cast the
                                              // reference borrowed_num to raw pointer
                                              // Note you will get a compiler error in following line
    assert!(*raw_ptr == 23);
}
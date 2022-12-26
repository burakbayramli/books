#[repr(C)]
union MyUnion {
    f1: u32,
    f2: f32,
}
fn main() {
    let float_num = MyUnion { f2: 2.0 };
    let f = unsafe { float_num.f2 };
    println!("f is {:.3}", f);
}
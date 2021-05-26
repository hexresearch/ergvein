#[no_mangle]
pub extern "C" fn test_ffi_func(
  x: i32
) -> i32 {
    println!("Rust: Your number is {}", x);
    x + 1
}

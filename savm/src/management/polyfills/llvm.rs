#[cfg_attr(not(target_env = "msvc"), link(name = "c"))]
unsafe extern "C" {
  pub unsafe fn fma(x: f64, y: f64, z: f64) -> f64;
  pub unsafe fn fmaf(x: f32, y: f32, z: f32) -> f32;
}

use std::default::Default;
use std::ops::{Div, Mul, Rem};

pub fn gcd<T: Copy + Default + Eq + Rem<Output = T>>(x: T, y: T) -> T {
    let (mut a, mut b) = (x, y);
    while b != T::default() {
        (b, a) = (a % b, b);
    }
    a
}

pub fn lcm<T: Copy + Default + Div<Output = T> + Eq + Mul<Output = T> + Rem<Output = T>>(
    x: T,
    y: T,
) -> T {
    x / gcd(x, y) * y
}

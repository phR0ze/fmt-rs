use std::ops::{Add, AddAssign, Neg, Sub, SubAssign};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq)]
pub struct Delta {
    // Internal value we are working with
    pub(crate) value: isize,
}

impl Delta {
    pub fn set(&mut self, value: isize) {
        self.value = value;
    }
    pub fn isize(&self) -> isize {
        self.value
    }
}

impl Default for Delta {
    fn default() -> Self {
        Self { value: 0 }
    }
}

impl Add for Delta {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            value: self.value + other.value,
        }
    }
}

impl AddAssign for Delta {
    fn add_assign(&mut self, other: Self) {
        *self = Self {
            value: self.value + other.value,
        };
    }
}

impl Neg for Delta {
    type Output = isize;

    fn neg(self) -> Self::Output {
        -self.value
    }
}
impl Sub for Delta {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self {
            value: self.value - other.value,
        }
    }
}

impl SubAssign for Delta {
    fn sub_assign(&mut self, other: Self) {
        *self = Self {
            value: self.value - other.value,
        };
    }
}

impl From<isize> for Delta {
    fn from(value: isize) -> Self {
        Self { value }
    }
}

impl From<Delta> for isize {
    fn from(delta: Delta) -> isize {
        delta.value
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn engine() {
        //
    }
}

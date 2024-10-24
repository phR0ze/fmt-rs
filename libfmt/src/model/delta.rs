use std::ops::{Add, AddAssign, Neg, Sub, SubAssign};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq)]
pub struct Delta {
    // Track the delta
    pub(crate) track: bool,
    pub(crate) accum: isize,

    // Internal value we are working with
    pub(crate) value: isize,
}

impl Default for Delta {
    fn default() -> Self {
        Self {
            track: false,
            accum: 0,
            value: 0,
        }
    }
}

impl Add for Delta {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            track: self.track,
            accum: if self.track {
                self.accum + other.value
            } else {
                0
            },
            value: self.value + other.value,
        }
    }
}

impl AddAssign for Delta {
    fn add_assign(&mut self, other: Self) {
        *self = Self {
            track: self.track,
            accum: if self.track {
                self.accum + other.value
            } else {
                0
            },
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
            ..Default::default()
        }
    }
}

impl SubAssign for Delta {
    fn sub_assign(&mut self, other: Self) {
        *self = Self {
            value: self.value - other.value,
            ..Default::default()
        };
    }
}

impl From<isize> for Delta {
    fn from(value: isize) -> Self {
        Self {
            value: value,
            ..Default::default()
        }
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
    fn add() {
        let mut d1 = Delta::from(1);
        d1 += 1.into();
        assert_eq!(d1.value, 2);
        assert_eq!(d1.accum, 0);
        assert_eq!(d1.track, false);

        d1.track = true;
        assert_eq!(d1.value, 2);
        assert_eq!(d1.accum, 0);
        assert_eq!(d1.track, true);
        d1 += 1.into();
        assert_eq!(d1.value, 3);
        assert_eq!(d1.accum, 1);
        assert_eq!(d1.track, true);

        let d2 = d1 + 1.into();
        assert_eq!(d2.value, 4);
        assert_eq!(d2.accum, 2);
        assert_eq!(d2.track, true);
    }
}

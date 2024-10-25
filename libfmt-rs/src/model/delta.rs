use std::ops::{Add, AddAssign, Neg, Sub, SubAssign};

#[derive(Debug, Clone, Copy)]
pub struct Delta {
    // Track the delta
    track_adds: bool,
    track_subs: bool,
    delta: isize,

    // Internal value we are working with
    value: isize,
}

impl Delta {
    pub fn set(&mut self, value: isize) {
        self.value = value;
    }

    /// Start tracking the addition delta between when we started and when we stopped
    pub fn track_adds(&mut self) {
        self.delta = 0;
        self.track_adds = true;
    }

    /// Start tracking the subtration delta between when we started and when we stopped
    pub fn track_subs(&mut self) {
        self.delta = 0;
        self.track_subs = true;
    }

    /// Expose the add tracking state
    pub fn tracking_adds(&self) -> bool {
        self.track_adds
    }

    /// Stop tracking the delta and reset it to zero
    pub fn stop_tracking(&mut self) {
        self.track_adds = false;
        self.track_subs = false;
    }

    /// Add the given value to the internal value
    pub fn add(&mut self, value: usize) {
        self.add_isize(value as isize);
    }

    /// Add the given value to the internal value
    pub fn add_isize(&mut self, value: isize) {
        self.value += value;
        if self.track_adds {
            self.delta += value;
        }
    }

    /// Subtract the given value from the internal value
    pub fn sub(&mut self, value: usize) {
        self.value -= value as isize;
        if self.track_subs {
            self.delta -= value as isize;
        }
    }

    /// Reset the delta value. This allows for decoupling the stop tracking from the reset
    /// so that we can read the value later.
    pub fn reset_delta(&mut self) {
        self.delta = 0;
    }

    /// Get the value
    pub fn value(&self) -> isize {
        self.value
    }

    /// Get the delta value between when we started and when we stopped
    pub fn delta(&self) -> isize {
        self.delta
    }
}

impl Default for Delta {
    fn default() -> Self {
        Self {
            track_adds: false,
            track_subs: false,
            delta: 0,
            value: 0,
        }
    }
}

impl Neg for Delta {
    type Output = isize;

    fn neg(self) -> Self::Output {
        -self.value
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn neg() {
        let d1 = Delta::from(1);
        assert_eq!(-d1, -1);
    }

    #[test]
    fn add() {
        let mut d1 = Delta::from(1);
        d1.add(1);
        assert_eq!(d1.value, 2);
        assert_eq!(d1.delta, 0);
        assert_eq!(d1.track_adds, false);

        d1.track_adds();
        assert_eq!(d1.value, 2);
        assert_eq!(d1.delta, 0);
        assert_eq!(d1.track_adds, true);
        d1.add(1);
        assert_eq!(d1.value, 3);
        assert_eq!(d1.delta, 1);
        assert_eq!(d1.track_adds, true);

        let mut d2 = d1;
        d2.add(1);
        assert_eq!(d2.value, 4);
        assert_eq!(d2.delta, 2);
        assert_eq!(d2.track_adds, true);

        d1.stop_tracking();
        assert_eq!(d1.value, 3);
        assert_eq!(d1.delta, 1);
        assert_eq!(d1.track_adds, false);

        d1.reset_delta();
        assert_eq!(d1.value, 3);
        assert_eq!(d1.delta, 0);
        assert_eq!(d1.track_adds, false);

        d1.track_adds();
        assert_eq!(d1.value, 3);
        assert_eq!(d1.delta, 0);
        assert_eq!(d1.track_adds, true);
    }

    #[test]
    fn sub() {
        let mut d1 = Delta::from(1);
        d1.track_subs();
        d1.sub(1);
        assert_eq!(d1.value, 0);
        assert_eq!(d1.delta, -1);
        assert_eq!(d1.track_adds, false);
        assert_eq!(d1.track_subs, true);
    }
}

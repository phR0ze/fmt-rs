use std::collections::VecDeque;
use std::fmt;
use std::ops::{Index, IndexMut};

use super::BufEntry;

/// RingBuffer provides a circular buffer.
pub struct RingBuffer<T> {
    data: VecDeque<T>,

    // Allows for maintaining fixed index locations despite popping elements from
    // the front of the buffer. When an element is popped the offset is updated
    // to keep external references to buffer index locations valid.
    offset: usize,
}

impl<T> RingBuffer<T> {
    pub fn new() -> Self {
        RingBuffer {
            data: VecDeque::new(),
            offset: 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Push the value onto the end of the buffer and return the offset controlled
    /// index of the value.
    pub fn push(&mut self, value: T) -> usize {
        let index = self.offset + self.data.len();
        self.data.push_back(value);
        index
    }

    pub fn clear(&mut self) {
        self.data.clear();
    }

    /// Get the index of the first element in the buffer as controlled by the current offset.
    pub fn index_of_first(&self) -> usize {
        self.offset
    }

    /// Get a reference to the first element in the buffer.
    pub fn first(&self) -> &T {
        &self.data[0]
    }

    pub fn first_mut(&mut self) -> &mut T {
        &mut self.data[0]
    }

    // Return the first value and update the offset to maintain external index locations.
    pub fn pop_first(&mut self) -> T {
        self.offset += 1;
        self.data.pop_front().unwrap()
    }

    pub fn last(&self) -> &T {
        self.data.back().unwrap()
    }

    pub fn last_mut(&mut self) -> &mut T {
        self.data.back_mut().unwrap()
    }

    pub fn second_last(&self) -> &T {
        &self.data[self.data.len() - 2]
    }

    pub fn pop_last(&mut self) {
        self.data.pop_back().unwrap();
    }
}

impl<T> Index<usize> for RingBuffer<T> {
    type Output = T;
    fn index(&self, index: usize) -> &Self::Output {
        &self.data[index.checked_sub(self.offset).unwrap()]
    }
}

impl<T> IndexMut<usize> for RingBuffer<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.data[index.checked_sub(self.offset).unwrap()]
    }
}

impl fmt::Display for RingBuffer<BufEntry> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "RingBuffer {{")?;
        for i in 0..self.data.len() {
            writeln!(f, "  [{}] = {:?}", i, &self.data[i])?;
        }
        writeln!(f, "}}")?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::Scan;
    use std::borrow::Cow;

    #[test]
    fn test_ring_buffer_debug() {
        let mut buf: RingBuffer<BufEntry> = RingBuffer::new();
        buf.push(BufEntry {
            token: Scan::String(Cow::Borrowed("hello")),
            size: 5,
        });
        assert_eq!(
            format!("{}", buf),
            "RingBuffer {\n  [0] = BufEntry { token: String(\"hello\"), size: 5 }\n}\n",
        );
    }
}

use std::fmt;

// Ensure the given closure is executed once the surrounding scope closes.
// Inspired by Golang's `defer`, Java's finally and Ruby's `ensure`
pub fn guard<T: FnOnce() -> fmt::Result>(f: T) -> impl Drop {
    Ensure(Some(f))
}

// Ensure uses Rust's object destructor to execute the given closure once
// the surrounding scope closes.
struct Ensure<T: FnOnce() -> fmt::Result>(Option<T>);

impl<T: FnOnce() -> fmt::Result> Drop for Ensure<T> {
    fn drop(&mut self) {
        self.0.take().map(|f| f());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ensure() {
        let x = std::cell::RefCell::new(0);
        {
            let _guard = guard(|| {
                *x.borrow_mut() += 1;
                Ok(())
            });
            assert_eq!(*x.borrow(), 0);
        }
        assert_eq!(*x.borrow(), 1);
    }
}

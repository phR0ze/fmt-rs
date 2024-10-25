# libfmtrs

### Quick links
* [Features](#features)
  * [F0003: Drop Amerpesand trailing space](#f0003-drop-ampersand-trailing-space)
  * [F0002: Smart wrapping ](#f0002-smart-wrapping)
  * [F0001: Developer comments](#f0001-developer-comments)
  * [F0000: Drop trailing comma](#f0000-drop-trailing-comma)

## Features
These are departures from the default rustfmt or prettyplease formatting behaviors.

### F0003: Drop Amerpsand trailing space
Rustfmt and Prettyplease both leave a trailling comma after parameter lists.

```rust
// prettyplease
println!("{}", & line);

// libfmtrs
println!("{}", &line);
```

### F0002: Smart wrapping
```rust
// rustfmt
fn reset<T, U, V, W, X>(
    &mut newline: T,
    &mut only_space: U,
    &mut comment_line: V,
    &mut prev_char: W,
    &mut next_char: X,
) where
    T: Display,
    U: Display,
    V: Display,
    W: Display,
    X: Display,
{
    println!(
        "{}{}{}{}{}",
        &line, &only_space, &comment_line, &prev_char, &next_char
    );
}

// libfmtrs
fn reset<T, U, V, W, X>(&mut newline: T, &mut only_space: U, &mut comment_line: V,
    &mut prev_char: W, &mut next_char: X)
where T: Display, U: Display, V: Display, W: Display, X: Display
{
    println!("{}{}{}{}{}", &line, &only_space, &comment_line, &prev_char, &next_char);
}
```

### F0001: Developer comments
Initial support for basic developer comments, not supported by PrettyPlease

* Empty line comments
  ```rust
  println!("{}", "1");

  println!("{}", "2");
  ```

* Block comments
  ```rust
  /**
   * This is a block comment
   */
  println!("{}", "1");
  ```

* Line comments
  ```rust
  // This is a line comment
  println!("{}", "1");
  ```

* Trailing line comments
  ```rust
  println!("{}", "1"); // This is a trailing comment
  ```

### F0000: Drop trailing comma
Rustfmt and Prettyplease both leave a trailling comma after parameter lists.

```rust
// rustfmt
println!("{}", "1",);

// libfmtrs
println!("{}", "1");
```

## Current
* [ ] F0003: Remove Ampersand trailing space
* [ ] F0002: Smart wrapping
  * [ ] Max line width not taking indenting into account

## Backlog
* [ ] Format comments to max line length
* [ ] Order create use statements after upstream use statements
* [ ] Align trailing line comments
      ```
      comment_block = false;         // Reset block checking
      line.truncate(line.len() - 2); // Drop control characters
      iter.next_if_eq(&'\n');        // Consume any trailing newline before its read
      ```

## Completed
Each completed task has an associated issue number used for tracking changes to the code.

* [ ] F0002: Smart wrapping
  * [x] Wrap at the maximum line length using standard indent on next line
  * [x] Function declarations should bump open brace to next line if params are wrapped
* [x] **F0001**: Developer comments
* [x] **F0000**: Drop trailing comma

# libfmt

### Quick links
* [Features](#features)
  * [F0001: Developer comments](#f0001-developer-comments)
  * [F0000: Skip trailing comma](#f0000-skip-trailing-comma)

## Features

### F0001: Developer comments
Initial support for basic developer comments

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

### F0000: Skip trailing comma
```rust
// rustfmt leaves a trailing comma after parameter lists
println!("{}", "1",);

// libfmt removes the trailing comma 
println!("{}", "1");
```

## Current
* [ ] F0002: Smart wrapping
  * [ ] Wrap at the maximum line length using standard indent on next line
  * [ ] Function declarations should bump open brace to next line if params are wrapped

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

* [x] **F0001**: Developer comments
* [x] **F0000**: Skip trailing comma

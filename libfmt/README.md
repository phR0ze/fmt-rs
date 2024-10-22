# libfmt

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
* [x] Initial support for comments
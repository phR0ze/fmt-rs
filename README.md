# fmt-rs
WIP: Simple Rust source code formatter.

### Goals
* Reduce scrolling due to extreme vertical alignment prejudice
* Provide something closer to the `gofmt` design pattern
* Able to be built with off the shelf crates and stable Rust

### Usage
WIP: working on a simple cli binary to read and format Rust code

### Existing work
The only works in this space that I could find were non-starters based on my intended goals. David 
Tolnay's excellent `PrettyPlease` was the closest option but is really intended for a different 
purpose and adhere's to Rustfmt's styling as well.

* [PrettyPlease](https://github.com/dtolnay/prettyplease)
  * Excellent work by David Tolnay
  * Pioneered the ability to build a tool like this from crates.io with stable rust
  * Leverages David Tolnay's Syn package to parse the AST
  * Follow's Rustfmt's style quite closely
  * Designed for formatting generated code and as such:
    * Doesn't support comments in code
    * Accepts nested code formatting 
* [GeneMichaels](https://github.com/andrewbaxter/genemichaels)
  * Leverages David Tolnay's `Syn` package directly which is what `prettyplease` is based on
  * Claims to format all code including macros and comments
* [Rustfmt - Community standard](https://github.com/rust-lang/rustfmt)
  * Stable, venerable and community supported
  * Has an extreme preference for vertical alignment
  * Unable to be built without insider support:
    * Requires the toolchain to be setup for nightly
    * Requires insider bits provided by the compiler tool chaain
    * Can't be built from off the shelf published crates
* [rsfmt](https://github.com/zBaitu/rsfmt)
  * Depends on older published AST crates that require an older version of nightly to compile

## Research

### Verbatim
Verbatim in the syn package is a way to deal with new Rust syntax that is added between syn releases.
That is to say that it can be parsed, but cannot be represented in syn's types as syn doesn't yet 
support the new syntax.

**References**
* [syn github](https://github.com/dtolnay/syn/issues/251)
* [syn panicking](https://users.rust-lang.org/t/why-is-syn-panicking-instead-of-returning-expr-verbatim-when-parsing-custom-syntax/97989)

### doc attribute
[Turns out](https://stackoverflow.com/questions/77971478/how-to-insert-doc-comments-using-syn) the 
`///` and `//!` are not really comments according to the AST but rather syntactic sugar for the 
`#[doc]` attribute. While actual comments are discarded at parse time and cannot be maniuplated by 
`syn`.

## Contributing
The Rust AST related crates can only be built on `Nightly`.

### NixOS dev shell
To setup a NixOS dev shell run `nix develop` from the root of this project which will use the 
`flake.nix` to install Rust nightly and make it available in the shell.

## How I got here
My frustration with `rustfmt` has brought me to a crossroads. It seems the only tool usable off the 
shelf, in the community, for on-save code formatting is `rustfmt`. This means I either need to give 
up on Rust, live with some form of manual code formatting or build an alternative that allows for a 
less abrasive approach than `rustfmt`.

Of course this is highly subjective but every time I go to write something in Rust I run into 
issues with `rustfmt` that leave a bad taste in my mouth. I can't stand it. I've spent countless 
hours trying to configure it to be reasonable. `rustfmt` is strongly opinionated with an emphasis on 
highly vertical code which drives me crazy. Unfortunatly for all the configuration options available 
the only levers that seem to exist for alignement are to allow for code to blindly extend to the 
right farther until the specified limit at which point the algorithm trips back into an extreme 
vertical view. Neither option is useful. The key, in my opionion, is to break the line semantically 
when complexity warrants separating an operation at which point the operational thought should remain 
intact while using as much of the screen realestate as possible.

From my research the community seems split in two camps, those that like `rustfmt` and those that 
would prefer something closer to `gofmt`. I also tend to align more closely with the latter. Semantic 
consistency is of more value to me than syntactic consistency.

### Nightly
As called out by others Rustfmt is written using rustc's internal syntax tree, so it can't be built 
by a stable compiler. Additionally it's releases are not regularly published to crates.io. This has 
the unfortunate affect of having to depend on a git dependency directly rather than crates.io and 
precludes the ability to publish anything based on it back to crates.io. Alternatives like 
`prettyplease` are designed to be easily pulled in as a library depency and buildable by the stable 
compiler.

### References
* [Key Rust devs are disgruntled with rustfmt](https://users.rust-lang.org/t/what-do-you-think-about-gofmt-vs-rustfmt/51605)
* [Rust formatter - Gene Michaels](https://github.com/andrewbaxter/genemichaels)

### Code line length
My personal perspective is that code is more easily understood at a higher level if you can see
more of it at once. However you don't necessarily need to see all the detail. By allowing for less
important code to extend to the right in a longer line you get a simple form of visual collapse.
That is to say I've found code on the left hand side of the screen is typically of more importance 
than the code further to the right.

As a developer I usually only follow the code to the right if I want more detail. I find this to be 
more efficient and useful than scrolling down pages of highly veritical code to get past detail 
that I don't need to pay as much attention to. This scrolling exercise is tedious and slow and makes 
it difficult to retain large amounts of code in my head. Additionally I find having code only show 
on my IDE screen on the left with 3/4 of the screen to the right empty to be frustrating and useless.

However there are definetly times when the use of a few sparing newlines can improve readability 
without wasting screen realestate and tiring out my scroll finger.

### Function braces
I've gone back and forth on this. Typically I prefer the `brace_style = SameLineWhere` rust defaults 
for this as I find again reducing vertical space whenever possible to be a better use of screen
realestate. However I find myself tempted to use the `AlwaysNextLine` option in one specific case. 
I'm a heavy comment user and tend to add comments to sections of functions. Often this means that 
I'll add a comment to the first section of code in a function. In this case rustfmt will move the 
commend up to be just under the function definition rather than allow a clean space to give visual 
separation for the comment. To work around this you can use the `AlwaysNextLine` brace option to put 
the brace in between and give a nice visual separation. Unfortunatly the trade off is that you loose 
vertical space in so many other cases like struct definitions.

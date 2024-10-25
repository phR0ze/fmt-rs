# fmt-rs
WIP: Simple Rust source code formatter.

### Goals
* Reduce scrolling and poor use of screen real estate
* Provide something closer to the `gofmt` experience
* Able to be built with off the shelf crates and stable Rust

### Usage
WIP: working on a simple cli binary to read and format Rust code

### Quick links
* [Preamble](#preamble)
* [Research](#research)
* [Legal](#legal)
  * [Disclaimer](#dislaimer)
  * [Contributions](#contributions)
  * [License](#license)

## Preamble
That is to say, how I arrived at the point where I was willing to invest countless hours building my 
own Rust source code formatting tool. Suffice it to say my frustration with `rustfmt` brought me to a 
crossroads. Without an, off the shelf, alternative I was confronted with either living with the 
community's enforced formatting style or going outside the box; as I am unwilling to give up on Rust 
in its entirety. At the same time I was unwilling to simply disable all code formatting and manually
format my code due to the extreme inefficiency. 

Of course this is highly subjective but every time I go to write something in Rust I run into issues 
with `rustfmt` that leaves a bad taste in my mouth. I can't stand it. Having experienced the simple, 
modern, out of your way, approach that `gofmt` offered it felt like a slap in the face every time 
Rustfmt forced a 2 line function declaration into 10 or more lines of vertical stacking that consumed 
only a tiny portion of my screen real estate on the left hand side and forced me to scroll. No matter 
how much I tried to configure Rustfmt to honor my own style, even with Nightly, I always ended up 
wringing my hands as a virtual break point was reached and suddenly all my code snapped to the 
community's extreme vertical preference. I just want something that doesn't constantly give me cuts 
and bruises with its abrasive, in your face, approach; or at least offer an alternative style to the 
lauded vertical sky scraper approach.

From my research the community seems split in two camps, those that like `rustfmt` and those that 
would prefer something closer to `gofmt` and tend towards manual formatting because there isn't any 
other solution available. I align more closely with the latter.

**References**
* [Key Rust devs are disgruntled with rustfmt](https://users.rust-lang.org/t/what-do-you-think-about-gofmt-vs-rustfmt/51605)
* [Rust formatter - Gene Michaels](https://github.com/andrewbaxter/genemichaels)

### Nightly
Originally I thought, well I'll just modify Rustfmt to suit my needs. However I quickly found out 
that Rustfmt is written using rustc's internal syntax tree, so it can't be built by a stable 
compiler. Some effor that been made to publish these internal crates in a way that they could be 
consumed by other projects, but they are hopelessly out of date and required Nightly to even compile. 
This has the unfortunate affect of having to depend on a git dependencies directly rather than 
crates.io and precludes the ability to publish anything based on it back to crates.io. Given the 
programming hole here around the AST the community responded with developing alternatives like the 
fantastic work from David Tolnay in [prod-macro2](https://crates.io/crates/proc-macro2) and
[syn](https://crates.io/crates/syn) that allow for typical ecosystem behavior like other crates using 
the stable compiler. Thank you David Tolnay.

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

That said occastionally a few sparing newlines can improve readability tremendously without wasting 
screen real estate e.g. a single whitespace line after a function declaration provides a nice visual 
break that aids separation between function and body; too bad Rustfmt doesn't allow for that and 
removes it once I hit save :(

### Function braces
I've gone back and forth on this. Typically I prefer the Rustfmt option `brace_style = SameLineWhere` 
as I find again reducing vertical space whenever possible to be a better use of screen real estate. 
However I find myself tempted to use the `AlwaysNextLine` option in one specific case. I'm a heavy 
comment user and tend to add comments to sections of functions. Often this means that I'll add a 
comment to the first section of code in a function. In this case rustfmt will remove the first 
newline thus moving my comment up to be just under the function declaration rather than allow a clean 
space to give visual separation for the comment. To work around this you can use the `AlwaysNextLine` 
brace option to put the brace in between and give a nice visual separation. Unfortunatly the trade 
off is that you loose vertical space in so many other cases like struct declarations where you don't 
actually need the space separation. Simply allowing for honoring a user's whitespace would alleviate 
this concern. Alas not supported in Rustfmt.

## Research

### Existing work
The only works in this space that I could find were non-starters based on my intended goals. David 
Tolnay's excellent `PrettyPlease` was the closest option but is really intended for a different 
purpose and additionally adhere's closely to Rustfmt's styling.

Here's what I found:
* [PrettyPlease](https://github.com/dtolnay/prettyplease)
  * Excellent work by David Tolnay
  * Pioneered the ability to build a tool like this from crates.io with stable rust
  * Leverages David Tolnay's Syn package to parse the AST
  * Follow's Rustfmt's style quite closely
  * Designed for formatting generated code and as such:
    * Doesn't support comments in code
    * Accepts nested code formatting without regard to total line length
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

### Verbatim
From what I can tell Verbatim in the `syn` package's way to deal with new Rust syntax that is added
between syn releases.  That is to say that it can be parsed and represented in an intermediate 
generic set of types, but cannot be represented in syn's standard types as syn doesn't yet support 
the new syntax.

**References**
* [syn github](https://github.com/dtolnay/syn/issues/251)
* [syn panicking](https://users.rust-lang.org/t/why-is-syn-panicking-instead-of-returning-expr-verbatim-when-parsing-custom-syntax/97989)

### doc attribute
[Turns out](https://stackoverflow.com/questions/77971478/how-to-insert-doc-comments-using-syn) the 
`///` and `//!` are not really comments according to the AST but rather syntactic sugar for the 
`#[doc]` attribute. While actual comments are discarded at parse time and cannot be manipulated by 
`proc_macro2` the doc attributes are preserved and even pretty printed e.g. `#doc = r" Testing"]`.

Outter doc attributes are attributes associated with programmatic components inside the application 
while Inner doc attributes are those associated with the module or package. Quite counter intuitive 
really from the way I look at it.

## Legal

### Disclaimer
As with all open source projects this project is offered ***as-is***, without warranty, and disclaims 
all liability for damages resulting from using this project. No support or guarantees of any kind 
implied or otherwise are offered. Any bug fixes, enhancements or guidance are purely at the 
discretion of the author of this project.

### Contributing
Input and PRs are welcome. Know however that they will only be accepted if they have supporting unit 
tests, documentation and fit with the highly subjective goals of this project; which goals are 
subject to change at any moment without notice.

### License
This project is dual licensed per typical Rust code under the 
[MIT](https://github.com/phR0ze/fmt-rs/blob/main/LICENSE-MIT) and
[Apache 2](https://github.com/phR0ze/fmt-rs/blob/main/LICENSE-APACHE) licenses.



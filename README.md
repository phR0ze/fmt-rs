# fmt-rs
WIP: Simple Rust source code formatter.

### Goals
* Less scrolling due to vertical alignment
* Closer to the `gofmt` design pattern
* Make use of stable Rust

### Usage
WIP: working on a simple cli binary to read and format code


## How I got here
My frustration with `rustfmt` has brought me to a crossroads, either I give up on Rust, live with 
only manual code formatting or build my own simple formatting tool.

Of course this is highly subjective but every time I go to write something in Rust I run into 
issues with `rustfmt` that leave a bad taste in my mouth. I can't stand it. I've spent countless 
hours trying to configure it to be reasonable. `rustfmt` isn't smart enough to format code well. It 
is strongly opinionated with an emphasis on highly vertical code which drives me crazy.

From my research the community seems split in two camps, those that like `rustfmt` and those that 
would prefer something closer to `gofmt`. I also tend to align more closely with the latter. Semantic 
consistency is of more value to me than syntactic consistency.

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

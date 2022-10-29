
<!-- README.md is generated from README.Rmd. Please edit that file -->

# r-manuals

<!-- badges: start -->
<!-- badges: end -->

This project translates the [official R
manuals](https://cran.r-project.org/manuals.html) to
[quarto](https://quarto.org/) with the aim of lightly re-styling for
better readability:

- Separate the manual into page for each chapter
- Code highlighting and formatting
- Footnotes in sidebar
- Improved searching

You can view the resulting
[https://rstudio.github.io/r-manuals](https://rstudio.github.io/r-manuals/r-intro/)

## How this works

Background information

- Every hour the R source code (at SVN) gets copied to github, into the
  repo <https://github.com/wch/r-source>.
- The manuals are in the folder
  <https://github.com/wch/r-source/tree/trunk/doc/manual>
- Each manual is in file of format `texinfo`, the “GNU documentation
  format” (<https://www.gnu.org/software/texinfo/>)
- Since it is possible to render a `texinfo` document to HTML
- Once in HTML format, you can use `pandoc` to convert the HTML to
  markdown, manipulate the markdown and then re-convert to HTML

## Building the manuals

The build script is in the file `scripts/build_website.R`.

## In more detail

For each manual:

- Download the relevant `texinfo` manual files and associated images and
  environment variables
- Run `makeinfo` to convert from `.texi` to HTML
- Run `pandoc` with additional [Lua
  filters](https://pandoc.org/lua-filters.html) to convert to markdown
- Perform additional processing and conversion in R
- Include the markdown files in a quarto book template
- Render the manual to quarto

Then combine the various quarto books into a quarto website.

## Note about installing `makeinfo`

On Linux, you can easily install the `makeinfo` utility using
`sudo apt install texinfo`.

On Windows, `makeinfo` is installed as part of
[RTools40](https://cran.r-project.org/bin/windows/Rtools/rtools40.html).

# Licensing

We provide the conversion code, i.e. this collection of R scripts and
pandoc lua filters, under the MIT license.

The original R manuals are not under this MIT license. Instead, these
manuals contain the following license:

> Copyright © 1999–2021 R Core Team
>
> Permission is granted to make and distribute verbatim copies of this
> manual provided the copyright notice and this permission notice are
> preserved on all copies.
>
> Permission is granted to copy and distribute modified versions of this
> manual under the conditions for verbatim copying, provided that the
> entire resulting derived work is distributed under the terms of a
> permission notice identical to this one.
>
> Permission is granted to copy and distribute translations of this
> manual into another language, under the above conditions for modified
> versions, except that this permission notice may be stated in a
> translation approved by the R Core Team.

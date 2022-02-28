# `.Internal` vs `.Primitive` {#internal-vs-.primitive .chapter}

C code compiled into R at build time can be called directly in what are
termed *primitives* or via the `.Internal` interface, which is very
similar to the `.External` interface except in syntax. More precisely, R
maintains a table of R function names and corresponding C functions to
call, which by convention all start with `do_` and return a
`SEXP`. This table (`R_FunTab` in file `src/main/names.c`) also
specifies how many arguments to a function are required or allowed,
whether or not the arguments are to be evaluated before calling, and
whether the function is 'internal' in the sense that it must be accessed
via the `.Internal` interface, or directly accessible in which case it
is printed in R as `.Primitive`.

Functions using `.Internal()` wrapped in a closure are in general
preferred as this ensures standard handling of named and default
arguments. For example, `grep` is defined as

``` r
grep <-
function (pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE,
         fixed = FALSE, useBytes = FALSE, invert = FALSE)
{
    if (!is.character(x)) x <- structure(as.character(x), names = names(x))
    .Internal(grep(as.character(pattern), x, ignore.case, value,
                   perl, fixed, useBytes, invert))
}
```

and the use of `as.character` allows methods to be dispatched (for
example, for factors).

However, for reasons of convenience and also efficiency (as there is
some overhead in using the `.Internal` interface wrapped in a function
closure), the primitive functions are exceptions that can be accessed
directly. And of course, primitive functions are needed for basic
operations---for example `.Internal` is itself a primitive. Note that
primitive functions make no use of R code, and hence are very different
from the usual interpreted functions. In particular, `formals` and
`body` return `NULL` for such objects, and argument matching can be
handled differently. For some primitives (including `call`, `switch`,
`.C` and `.subset`) positional matching is important to avoid partial
matching of the first argument.

The list of primitive functions is subject to change; currently, it
includes the following.

1.  "Special functions" which really are *language* elements, but
    implemented as primitive functions:
    ``` R
    {       (         if     for      while  repeat  break  next
    return  function  quote  switch
    ```

2.  Language elements and basic *operator*s (i.e., functions usually
    *not* called as `foo(a, b, ...)`) for subsetting, assignment,
    arithmetic, comparison and logic:

    ``` r
                   [    [[    $    @
    <-   <<-  =    [<-  [[<-  $<-  @<-

    +    -    *    /     ^    %%   %*%  %/%
    <    <=   ==   !=    >=   >
    |    ||   &    &&    !
    ```

    When the arithmetic, comparison and logical operators are called as
    functions, any argument names are discarded so positional matching
    is used.

3.  "Low level" 0-- and 1--argument functions which belong to one of the
    following groups of functions:
    a.  Basic mathematical functions with a single argument, i.e.,

        ``` R
        abs     sign    sqrt
        floor   ceiling

        exp     expm1
        log2    log10   log1p
        cos     sin     tan
        acos    asin    atan
        cosh    sinh    tanh
        acosh   asinh   atanh
        cospi   sinpi   tanpi

        gamma   lgamma  digamma trigamma

        cumsum  cumprod cummax  cummin

        Im  Re  Arg  Conj  Mod
        ```

        `log` is a primitive function of one or two arguments with named
        argument matching.

        `trunc` is a difficult case: it is a primitive that can have one
        or more arguments: the default method handled in the primitive
        has only one.

    b.  Functions rarely used outside of "programming" (i.e., mostly
        used inside other functions), such as
        ``` c
        nargs          missing        on.exit        interactive
        as.call        as.character   as.complex     as.double
        as.environment as.integer     as.logical     as.raw
        is.array       is.atomic      is.call        is.character
        is.complex     is.double      is.environment is.expression
        is.finite      is.function    is.infinite    is.integer
        is.language    is.list        is.logical     is.matrix
        is.na          is.name        is.nan         is.null
        is.numeric     is.object      is.pairlist    is.raw
        is.real        is.recursive   is.single      is.symbol
        baseenv        emptyenv       globalenv      pos.to.env
        unclass        invisible      seq_along      seq_len
        ```

    c.  The programming and session management utilities
        ``` R
        browser  proc.time  gc.time tracemem retracemem untracemem
        ```

4.  The following basic replacement and extractor functions

    ``` r
    length      length<-
    class       class<-
    oldClass    oldClass<-
    attr        attr<-
    attributes  attributes<-
    names       names<-
    dim         dim<-
    dimnames    dimnames<-
                environment<-
                levels<-
                storage.mode<-
    ```

    Note that optimizing `NAMED = 1` is only effective within a
    primitive (as the closure wrapper of a `.Internal` will set
    `NAMED = NAMEDMAX` when the promise to the argument is evaluated)
    and hence replacement functions should where possible be primitive
    to avoid copying (at least in their default methods). \[The `NAMED`
    mechanism has been replaced by reference counting.\]

5.  The following functions are primitive for efficiency reasons:


    ``` c
    :           ~           c           list
    call        expression  substitute
    UseMethod   standardGeneric
    .C          .Fortran   .Call        .External
    round       signif      rep         seq.int
    ```

    as well as the following internal-use-only functions

    ``` r
    .Primitive      .Internal
    .Call.graphics  .External.graphics
    .subset         .subset2
    .primTrace      .primUntrace
    lazyLoadDBfetch
    ```

The multi-argument primitives

``` r
call       switch
.C         .Fortran   .Call       .External
```

intentionally use positional matching, and need to do so to avoid
partial matching to their first argument. They do check that the first
argument is unnamed or for the first two, partially matches the formal
argument name. On the other hand,

``` c
attr       attr<-     browser     rememtrace substitute  UseMethod
log        round      signif      rep        seq.int
```

manage their own argument matching and do work in the standard way.

All the one-argument primitives check that if they are called with a
named argument that this (partially) matches the name given in the
documentation: this is also done for replacement functions with one
argument plus `value`.

The net effect is that argument matching for primitives intended for
end-user use *as functions* is done in the same way as for interpreted
functions except for the six exceptions where positional matching is
required.



## Special primitives {#special-primitives .section}

A small number of primitives are *specials* rather than *builtins*, that
is they are entered with unevaluated arguments. This is clearly
necessary for the language constructs and the assignment operators, as
well as for `&&` and `||` which conditionally evaluate their second
argument, and `~`, `.Internal`, `call`, `expression`, `missing`,
`on.exit`, `quote` and `substitute` which do not evaluate some of their
arguments.

`rep` and `seq.int` are special as they evaluate some of their arguments
conditional on which are non-missing.

`log`, `round` and `signif` are special to allow default values to be
given to missing arguments.

The subsetting, subassignment and `@` operators are all special. (For
both extraction and replacement forms, `$` and `@` take a symbol
argument, and `[` and `[[` allow missing arguments.)

`UseMethod` is special to avoid the additional contexts added to calls
to builtins.



## Special internals {#special-internals .section}

There are also special `.Internal` functions: `NextMethod`, `Recall`,
`withVisible`, `cbind`, `rbind` (to allow for the `deparse.level`
argument), `eapply`, `lapply` and `vapply`.



## Prototypes for primitives {#prototypes-for-primitives .section}

Prototypes are available for the primitive functions and operators, and
these are used for printing, `args` and package checking (e.g. by
`tools::checkS3methods` and by package
[**codetools**](https://CRAN.R-project.org/package=codetools)). There
are two environments in the **base** package (and namespace),
`.GenericArgsEnv` for those primitives which are internal S3
generics, and `.ArgsEnv` for the rest. Those environments
contain closures with the same names as the primitives, formal arguments
derived (manually) from the help pages, a body which is a suitable call
to `UseMethod` or `NULL` and environment the base namespace.

The C code for `print.default` and `args` uses the closures in these
environments in preference to the definitions in base (as primitives).

The QC function `undoc` checks that all the functions prototyped in
these environments are currently primitive, and that the primitives not
included are better thought of as language elements (at the time of
writing

``` r
$  $<-  &&  (  :  @  @<-  [  [[  [[<-  [<-  {  ||  ~  <-  <<-  =
break  for function  if  next  repeat  return  while
```

). One could argue about `~`, but it is known to the parser and has
semantics quite unlike a normal function. And `:` is documented with
different argument names in its two meanings.

The QC functions `codoc` and `checkS3methods` also make use of these
environments (effectively placing them in front of base in the search
path), and hence the formals of the functions they contain are checked
against the help pages by `codoc`. However, there are two problems with
the generic primitives. The first is that many of the operators are part
of the S3 group generic `Ops` and that defines their arguments to be
`e1` and `e2`: although it would be very unusual, an operator could be
called as e.g. `"+"(e1=a, e2=b)` and if method dispatch occurred to a
closure, there would be an argument name mismatch. So the definitions in
environment `.GenericArgsEnv` have to use argument names `e1` and `e2`
even though the traditional documentation is in terms of `x` and `y`:
`codoc` makes the appropriate adjustment via
`tools:::.make_S3_primitive_generic_env`. The second discrepancy is with
the `Math` group generics, where the group generic is defined with
argument list `(x, ...)`, but most of the members only allow one
argument when used as the default method (and `round` and `signif` allow
two as default methods): again fix-ups are used.

Those primitives which are in `.GenericArgsEnv` are checked (via
`tests/primitives.R`) to be generic *via* defining methods for
them, and a check is made that the remaining primitives are probably not
generic, by setting a method and checking it is not dispatched to (but
this can fail for other reasons). However, there is no certain way to
know that if other `.Internal` or primitive functions are not internally
generic except by reading the source code.



## Adding a primitive {#adding-a-primitive .section}

\[For R-core use: reverse this procedure to remove a primitive. Most
commonly this is done by changing a `.Internal` to a primitive or *vice
versa*.\]

Primitives are listed in the table `R_FunTab` in
`src/main/names.c`: primitives have `Y = 0` in the
`eval` field.

There needs to be an `\alias` entry in a help file in the
**base** package, and the primitive needs to be added to one of the
lists at the start of this section.

Some primitives are regarded as language elements (the current ones are
listed above). These need to be added to two lists of exceptions,
`langElts` in `undoc()` (in file `src/library/tools/R/QC.R`)
and `lang_elements` in `tests/primitives.R`.

All other primitives are regarded as functions and should be listed in
one of the environments defined in `src/library/base/R/zzz.R`,
either `.ArgsEnv` or `.GenericArgsEnv`: internal generics also need to
be listed in the character vector `.S3PrimitiveGenerics`. Note too the
discussion about argument matching above: if you add a primitive
function with more than one argument by converting a `.Internal` you
need to add argument matching to the C code, and for those with a single
argument, add argument-name checking.

Do ensure that `make check-devel` has been run: that tests most of these
requirements.

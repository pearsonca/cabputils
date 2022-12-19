
#' Get Initial Variable |>'d into Call Chain
#'
#' Defined to denoise [checker()] function body when manually inspected
#'
#' @param sc the result of a [sys.call()] within a `check_...` function created
#' by [checker()]
#'
#' @details This converts a [sys.call()] to its character representation
#' ([as.character()]), then takes the last bit of it ([tail()]), then extracts
#' the innermost parenthetical expression ([gsub()])
#'
get_piped_call_var <- function(sc) {
  return(list(
    sc |> as.character() |> tail(1) |>
      gsub(".*\\(([^\\(\\)]+)\\).*", "\\1", x = _)
  ))
}

#' @rdname get_piped_call_var
get_piped_call_var_wref <- function(sc) {
  pipeline <- sc |> as.character() |> tail(2)
  return(list(
    pipeline[1] |> gsub(".*\\(([^\\(\\)]+)\\).*", "\\1", x = _),
    pipeline[2]
  ))
}

#' @rdname get_piped_call_var
formatmsg <- function(ls, msg) {
  ls$fmt <- msg
  do.call(sprintf, ls)
}

#' @title Generator for `check_...`
#'
#' @param reqexpr a logical expression to check e.g. `is.integer(x)`; must
#' *only* be written in terms of a single variable `x`, though if that variable
#' should be e.g. a list, then `x$something` will work.
#'
#' @param msg a [sprintf()] `fmt` argument, with a single `%s`, which will be
#' filled by the variable name piped into the resulting check function
#'
#' @details This function is a lightweight Factory for making argument checking
#' functions (see example for applications of those checking functions).
#'
#' This somewhat gnarly bit of NSE satisfies several demands:
#'  - it consumes the minimal input from developer to specify a check:
#'  the test & the error message
#'  - it creates a function that works
#'  - that function is human-readable when inspected
#'
#' [alist()] creates the (single) argument list, [substitute()] incorporates
#' the materialized values of the variable test (`reqexpr`) and error message
#' (`msg`) into the function body. [as.function()] turns it all into a real
#' function.
#'
#' @export
#' @examples
#' # define some check functions:
#' check_character <- checker(is.character(x), "`%s` is not class 'character'.")
#' check_scalar <- checker(length(x) == 1, "`%s` must be length == 1.")
#' check_nonemptychar <- checker(all(nchar(x) > 0), "`%s` must be non-empty.")
#' # note the human-readable internals:
#' check_character
#' check_scalar
#' check_nonemptychar
#'
#' # define a function that wants argument checking, and use check functions:
#' helloworld <- function(name) {
#'   name |> check_character() |> check_scalar() |> check_nonemptychar()
#'   sprintf("Hello, %s!", name)
#' }
#'
#' # note the self-documenting argument validation
#' helloworld
#'
#' # works:
#' helloworld("Carl")
#'
#' # doesn't:
#' try(helloworld(1)) # error from check_character
#' try(helloworld(c("Alice", "Bob"))) # error from check_scalar
#' try(helloworld("")) # error from check_nonemptychar
checker <- function(reqexpr, msg) {
  c(alist(x=), substitute(
    if (!(reqexpr)) {
      sys.call() |> get_piped_call_var() |>
        formatmsg(msg) |> stop(call. = FALSE)
    } else invisible(x)
  )) |> as.function()
}

#' @title Expanded Generator for `check_...`
#'
#' @description Creates `check_...` functions with a `ref`erence argument.
#'
#' @param reqexpr a logical expression to check e.g. `x %in% ref`; must
#' *only* be written in terms of variables `x` (the target) and `ref` (the
#' reference), though if those variables have internal structure (e.g. are
#' `list`s), then e.g. `x$something` will work.
#'
#' @param msg a [sprintf()] `fmt` argument, with two `%s`, which will be
#' filled by the variable name piped into the resulting check function (first)
#' and then the reference argument (second)
#'
#' @details This function is a lightweight Factory for making argument checking
#' functions (see example for applications of those checking functions).
#'
#' This somewhat gnarly bit of NSE satisfies several demands:
#'  - it consumes the minimal input from developer to specify a check:
#'  the test & the error message
#'  - it creates a function that works
#'  - that function is human-readable when inspected
#'
#' [alist()] creates the argument list, [substitute()] incorporates
#' the materialized values of the variable test (`reqexpr`) and error message
#' (`msg`) into the function body. [as.function()] turns it all into a real
#' function.
#'
#' @examples
#' # define some single check functions:
#' check_character <- checker(is.character(x), "`%s` is not class 'character'.")
#' check_scalar <- checker(length(x) == 1, "`%s` must be length == 1.")
#' check_nonemptychar <- checker(all(nchar(x) > 0), "`%s` must be non-empty.")
#' # and now one using an additional argument
#' check_among <- checker_against(x %in% ref, "`%s` is not among %s")
#' # note the human-readable internals:
#' check_among
#'
#' # define a function that wants argument checking, and use check functions:
#' helloworld <- function(name) {
#'   name |> check_character() |> check_scalar() |> check_nonemptychar() |>
#'   check_among(c("Alice", "Bob", "Carl"))
#'   sprintf("Hello, %s!", name)
#' }
#'
#' # note the self-documenting argument validation
#' helloworld
#'
#' # works:
#' helloworld("Carl")
#' # doesn't:
#' try(helloworld("Robert"))
#'
#' @export
checker_against <- function(reqexpr, msg) {
  c(alist(x=, ref=), substitute({
    if (missing(ref)) stop("Did not provide a `ref` argument.")
    if (!(reqexpr)) {
      sys.call() |> get_piped_call_var_wref() |>
        formatmsg(msg) |> stop(call. = FALSE)
    } else invisible(x)
  })) |> as.function()
}


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
  return(
    sc |> as.character() |> tail(1) |>
      gsub(".*\\(([^\\(\\)]+)\\).*", "\\1", x = _)
  )
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
      sys.call() |> get_piped_call_var() |> sprintf(fmt = msg) |>
        stop(call. = FALSE)
    } else invisible(x)
  )) |> as.function()
}

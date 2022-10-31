#' @title [base::match.call] Extension
#'
#' @description Produces a `match.call` with defaults
#' also included.
#'
#' @inheritParams base::match.call
#'
#' @export
match.call.defaults <- function(
  definition = sys.function(sys.parent()),
  call = sys.call(sys.parent()),
  expand.dots = TRUE,
  envir = parent.frame(2L)
) {
  # get the call
  mc <- match.call(definition, call, expand.dots, envir)
  # get the formals, tossing any ellipsis
  fs <- formals(definition, envir)
  fs$... <- NULL

  # for any arguments set in formals & not in the call
  for(nm in setdiff(names(fs), names(mc)))
    mc[nm] <- fs[nm] # add those to the call

  return(mc)
}

#' Generic Function Wrapper
#'
#' @description provides a convenience function for
#' producing duplicate functions with different
#' defaults
#'
#' @param FUN the function to wrap
#'
#' @param ... the new defaults
#'
#' @param .ENV the environment for the resulting
#' copy-function (i.e. where any variables will be
#' evaluated). NB, the default (`environment(FUN)`) is
#' mostly convenient, but can be dangerous e.g. by
#' replacing an important function
#'
#'
#' @return the new function
#'
#' @examples
#' require(ggplot2); require(cabputils)
#' require(data.table); require(deSolve)
#'
#' # make some reference plotting data
#' dSIR <- function(t, SIR, params) with(params,{
#'   infections <- beta*SIR[1]*SIR[2]
#'   recoveries <- gamma*SIR[2]
#'   list(c(-infections, infections-recoveries, recoveries))
#' })
#'
#' dSEIR <- function(t, SEIR, params) with(params,{
#'   infections <- beta*SEIR[1]*SEIR[3]
#'   incubation <- lambda*SEIR[2]
#'   recoveries <- gamma*SEIR[3]
#'   list(c(-infections, infections-incubation, incubation-recoveries, recoveries))
#' })
#'
#' SIR.dt <- ode(
#'   c(S=100, I=1, R=0),
#'   seq(0,50,by=0.1),
#'   dSIR, parms = list(beta = 0.01, gamma = .2)
#' ) |> as.data.table() |> melt(id.vars = "time")
#'
#' SEIR.dt <- ode(
#'   c(S=100, E=0, I=1, R=0),
#'   seq(0,50,by=0.1),
#'   dSEIR, parms = list(beta = 0.01, lambda = .1, gamma = .2)
#' ) |> as.data.table() |> melt(id.vars = "time")
#'
#' # USE rejig: define some scales to share
#' scale_color_state <- rejig(
#'   scale_color_manual,
#'   name = paste0(breaks, collapse = ""),
#'   values = c(S="dodgerblue", E="yellow", I="firebrick", R="limegreen")
#' )
#' scale_y_count <- rejig(scale_y_continuous, name = "Count")
#'
#' # note difference between these:
#' scale_color_state
#' scale_y_count
#'
#' scale_x_simtime <- rejig(scale_x_continuous, name = NULL)
#'
#' ggplot(SIR.dt) + aes(time, value, color = variable) +
#'   geom_line() +
#'   scale_color_state(breaks = levels(SIR.dt$variable)) +
#'   scale_y_count() +
#'   scale_x_simtime() +
#'   theme_minimal()
#'
#' ggplot(SEIR.dt) + aes(time, value, color = variable) +
#'   geom_line() +
#'   scale_color_state(breaks = levels(SEIR.dt$variable)) +
#'   scale_y_count() +
#'   scale_x_simtime() +
#'   theme_minimal()
#'
#' @export
rejig <- function(FUN, ..., .ENV = environment(FUN)) {
  # initial validation
  stopifnot(
    "FUN isn't a function." = is.function(FUN),
    "FUN is a primitive function." = !is.primitive(FUN)
  )

  dots <- as.list(match.call())[-1] # get some new defaults
  dots$FUN <- dots$.ENV <- NULL # drop all the not-defaults

  if (length(dots) == 0) {
    warning("... is empty. Just returning FUN.")
    return(FUN)
  }

  .FUN <- FUN # make a duplicate of FUN
  forms <- formals(FUN) # get the original defaults

  # potentially more validation: check for ... argument
  # in FUN and try to partial match all arguments in
  # rejig
  hasdots <- "..." %in% names(forms)
  replacements <- names(forms)[pmatch(names(dots), names(forms))]

  if (any(is.na(replacements)) && !hasdots) {
    errmsg <- sprintf("
FUN does not have ... argument, and
rejig ... arguments do not match FUN arguments:
%s
", names(dots)[is.na(replacements)] |> paste(collapse = ", ")
    )
    stop(errmsg)
  }

  # correct any partially matched defaults
  names(dots)[!is.na(replacements)] <- replacements[!is.na(replacements)]
  # set the new defaults
  formals(.FUN)[names(dots)] <- dots
  environment(.FUN) <- .ENV

  if (hasdots && any(is.na(replacements))) {
    # the internals of FUN may pass around the ellipsis, which now
    # excludes newly set default variables, so need to use it
    body(.FUN) <- substitute({
      mc <- cabputils::match.call.defaults()
      mc[[1]] <- FUN
      eval(mc)
    })
  }

  return(.FUN)

}

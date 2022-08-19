utils::globalVariables(c("value", "."))

#' @title Quantile [data.table] Groups
#'
#' @description Applies [quantile()] to [data.table] groups,
#' across sample column(s), for target column(s). Returns results by
#' group and target(s).
#'
#' @param x a [data.table]
#'
#' @param j A "select" statement for `x`; the resulting columns
#' will be the target(s) of the quantiling. See [data.table::data.table()]
#' for details on `j` argument; select will be grouped by `keyby`
#'
#' @param sampleby Optionally, the sample-defining column(s) (i.e., what gets
#' quantile'd over). Specify `keyby` XOR `sampleby`
#'
#' @param keyby Optionally, the grouping definition. Specify `keyby` XOR
#' `sampleby`.
#'
#' @param probs numeric on `[0, 1]`; n.b. the default is identical to
#' [stats::quantile()] default, though named (see [qprobs()]). If `probs` is provided
#' with names, those names will be used for the resulting quantile columns.
#'
#' @param ... Other arguments to [stats::quantile()].
#'
#' @return A [data.table], key from `keyby` (provided or infered) and a
#' factor "measure" column (levels from `j` names), and column(s) for
#' requested quantile(s); if `probs` in `...` is named, those names will
#' be used for the column names
#'
#' @import data.table
#' @importFrom stats quantile setNames
#'
#' @examples
#' require(data.table)
#' data("mtcars")
#' dtcars <- mtcars |> as.data.table()
#' dtcars |> quantile(mpg, keyby=.(cyl, gear))
#' dtcars |> quantile(mpg, keyby=.(cyl, gear), probs=c(lo=0.05, md=0.5, hi=0.95))
#' dtcars |> quantile(.(mpg, wt), keyby=.(cyl, gear), probs=c(lo=0.05, md=0.5, hi=0.95))
#' @export
quantile.data.table <- function(
    x, j = .(value),
    keyby = setdiff(data.table::key(x), sampleby),
    sampleby,
    probs = qprobs(),
    ...
) {

  dots <- list(...)
  dots$probs <- probs
  tailq <- if (!is.null(probs) && !is.null(names(probs))) {
    \(x) stats::setNames(x, names(probs)) |> as.list()
  } else {
    as.list
  }

  return( eval(substitute(x[,j,keyby,TRUE])) |> (
    \(dt) data.table::melt.data.table(
      dt, id.vars = data.table::key(dt), variable.name = "measure"
      ) |> data.table::setkeyv(data.table::key(dt))
  )() |> (\(dt) dt[,
    do.call(stats::quantile, c(.(value), dots)) |> tailq(),
    keyby = c(data.table::key(dt), "measure")
  ])())

}

#' @title Quantile Ranges from Spans
#'
#' @description expands spans into quantile limits
#'
#' @param spans numeric, on (0, 1); what proportion(s) of the data
#' to quantile at? if `!is.null(names(spans))`, those names will
#' appear on the result as `q\%s(l|h)` ("l" for low, "h" for high)
#'
#' @param mid logical; include median? when `TRUE`, if `spans` named
#' this will be named `qmed`
#'
#' @param extent logical; include min and max values?
#'
#' @return numeric values on 0, 1
#'
#' @export
qprobs <- function(spans = c(`50` = 0.5), mid = TRUE, extent = TRUE) {
  stopifnot(
    "spans not numeric." = is.numeric(spans),
    "spans not on (0, 1)" =
      data.table::between(spans, 0, 1, incbounds = FALSE)
  )

  if (anyDuplicated(spans)) {
    warning("Ignoring duplicate values in qprob(...)")
    spans <- unique(spans)
  }

  isnamed <- !is.null(names(spans))

  qs <- sort(c(0, 1) + rep((1-spans)/2, each = 2) * c(1, -1))

  if (isnamed) {
    stopifnot("if named, cannot provide blank names" = all(names(qs) != ""))
    names(qs) <- sprintf("q%s%s", names(qs), rep(c("l","h"), each=length(spans)))
  }

  if (mid) {
    qs <- c(qs, if(isnamed) c(qmed = 0.5) else c(0.5) )
  }

  if (extent) {
    qs <- c(qs, if(isnamed) c(qmin = 0, qmax = 1) else c(0, 1) )
  }

  return(sort(qs))

}

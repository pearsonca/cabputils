
#' @title Extract Relevant Month Boundaries
#'
#' @description
#'
#' @param x A datetime-like vector or `data.table`-like object containing such
#'
#' @param ... Other arguments passed on particular method
#'
#' @return A [data.table::data.table()], minimally with columns `start` and `end`,
#' both some kind of datetime format. `start` will be all YYYY-MM-01, `end` all
#' YYYY-MM-01 - 1 (i.e. last days of months). The earliest `start` and latest `end`
#' will bound the datetime in `x`
#'
#' @export
monthbounds <- function(x, ...) UseMethod("monthbounds")

#' @rdname monthbounds
#'
#' @param ... Arguments passed on [monthbounds.data.table()]
#'
#' @exportS3Method
monthbounds.data.frame <- function(
    x, ...
) {
  monthbounds.data.table(as.data.table(x), ...)
}

#' @param x A `data.table`
#'
#' @param j Optional, a select state that will be applied per month group
#'
#' @param keyby Optional, a grouping to determine month bounds over
#'
#' @rdname monthbounds
#' @exportS3Method
monthbounds.data.table <- function(
  x,
  j,
  keyby = setdiff(key(x), "date"),
  dateby = date
) {
  dt <- setkeyv(
    eval(substitute(copy(x)[, m := trunc(dateby, "months") ])), c(keyby, "m")
  )
  if (missing(j)) {
    eval(substitute(dt[,.(start = m, end = ),keyby=key(dt)]))
  } else {

  }
}

#' @rdname monthbounds
#' @exportS3Method
monthbounds.Date <- function(x) {
  # takes the range of dates, flattens to YYYY-MM-01
  month.range <- trunc(range(x), "months")
  date.min <- date.range[1]
  # goes into one month past end, then ticks back to last of original end month
  date.max <- seq(date.range[2], by="months", length.out = 2)[2] - 1
  starts <- seq(date.min, date.max, by="months")
  ends <- c(starts[-1] - 1, date.max)
  data.table(start = starts, end = ends)
}

#' @rdname monthbounds
#' @exportS3Method
monthbounds.numeric <- function(x, origin = as.Date.POSIXct(0)) {
  monthbounds.Date(x+date0)
}


# for insight about composite geoms: https://github.com/stefanedwards/lemon/blob/master/R/geom-pointline.r
# essentially:
#  - have this prepare data via monthbounds
#  - pass to geom used to draw boxes
#  - pass to geom used to draw text (months)
#  - pass to geom used to draw text (years)
#  - join those up as a grob

# but maybe this needs to be implemented as an axis guide?
# https://github.com/teunbrand/ggh4x/blob/master/R/guide_axis_manual.R

#' @title GGProto object for [geom_monthbin()]
#'
#' @export
GeomMonthbin <- ggplot2::ggproto(
  "GeomMonthbin", ggplot2::Geom,
  required_aes = c("x"),
  compute_panel = function(
    self, data, scales,
    ylog, band.col, text.col
  ) {
    dt <- data |> as.data.table()
    #
  }
)

#' Paint Month Bands
#'
#' `geom_monthbin()` computes the month boundaries for data, then draws and labels
#' those bands.
#'
#' @inheritParams ggplot2::layer
#' @inheritDotParams ggplot2::layer
#'
#' @param TODO
#'
#' @export
#' @examples
#' TODO
stat_spaghetti <- function(
  mapping = NULL, data = NULL, geom = "line",
  position = "identity", na.rm = FALSE, show.legend = c(alpha = FALSE),
  inherit.aes = TRUE,
  max.lines = if (interactive()) 10 else 100,
  central.fun = median,
  ...
) {
  stopifnot(
    "If non-NA, `max.lines` must be non-negative." = is.na(max.lines) || (max.lines >= 0),
    "Either `central.fun` must be specified or `max.lines` must exceed 0 (or be NA)." =
      !is.null(central.fun) || is.na(max.lines) || (max.lines > 0)
  )
  ggplot2::layer(
    stat = StatSpaghetti, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm, max.lines = max.lines,
      central.fun = central.fun,
      ...
    )
  )
}


# reference from other work
geom_month_background <- function(
    data,
    col.cycle = c(off = NA, on = alpha("lightgrey", 0.75)),
    m.labels = m.abb,
    font.size = 8, font.face = "bold",
    ylog = FALSE,
    datafn = tsref.dt,
    ...
) {
  text.col <- alpha(col.cycle, 1)
  names(text.col) <- c(tail(names(col.cycle), -1), names(col.cycle)[1])
  text.col[is.na(text.col)] <- "white"
  dt <- datafn(data, ...)
  dt[, fill := col.cycle[mtype] ]
  dt[, col := text.col[mtype] ]
  # ggplot isn't great on replicating these across facets
  # would be preferrable to use `annotate`, and let backend recycle fills, but
  # it won't, so have to tell this how many facets there will be
  xform <- if (ylog) {
    function(hi, lo, dropto) lo*(hi/lo)^dropto
  } else {
    function(hi, lo, dropto) lo+(hi-lo)*dropto
  }
  list(
    geom_rect(
      mapping = aes(xmin = start - 0.5, xmax = end + 0.5, ymin = if (ylog) 0 else -Inf, ymax = Inf),
      data = dt, inherit.aes = FALSE, show.legend = FALSE, fill = dt$fill
    ),
    geom_text(
      mapping = aes(x = mid, y = xform(ymax, ymin, .87), label = m.abb[mon]),
      data = dt, inherit.aes = FALSE, show.legend = FALSE, color = dt$col,
      size = font.size, vjust = "bottom", fontface = font.face
    ),
    geom_text(
      mapping = aes(x = mid, y = xform(ymax, ymin, .83), label = yr),
      data = dt[yshow == TRUE], angle = 90,
      inherit.aes = FALSE, show.legend = FALSE, color = dt[yshow == TRUE]$col,
      size = font.size, hjust = "right", fontface = font.face
    )
    # ,
    # geom_text(
    #   mapping = aes(x = mid, y = 0.25, label = yr),
    #   data = dt[yshow == TRUE],
    #   inherit.aes = FALSE, show.legend = FALSE, color = rep(dt[yshow == TRUE]$col, facet.mul)
    # )
  )
}

#' @title GGProto object for [stat_spaghetti()]
#'
#' @export
StatSpaghetti <- ggplot2::ggproto(
  "StatSpaghetti", ggplot2::Stat,
  required_aes = c("x", "y", "sample"),
  # default_aes = ggplot2::aes(alpha = ggplot2::stage(
  #   after_stat = spaghetti, after_scale = alpha*(sampleN^-0.5)
  # )),
  default_aes = ggplot2::aes(
    alpha = after_stat(
      c(sample = 1, central = 1)[spaghetti]*(sampleN^-0.5)
    )
  ),
  setup_data = function(data, params) {
    data |> as.data.table() |> DT(order(group, sample)) |> DT(, sample := {
      tmp <- rle(sample)
      tmp$values <- 1:length(tmp$values)
      inverse.rle(tmp)
    }, by=group)
  },
  compute_panel = function(
    self, data, scales,
    max.lines,
    central.fun
  ) {
    cast_spag <- function(spag = c("sample", "central")) {
      factor(spag, levels = c("sample", "central"), ordered = TRUE)
    }
    dt <- data |> as.data.table()
    cdt <- dt[0]
    maxg <- 0
    if (!is.null(central.fun)) {
      cdt <- dt |> DT(,
        copy(.SD)[, y := .(central.fun(y))],
        by=.(x, group)
      ) |> DT(, c("spaghetti", "sampleN") := .(cast_spag("central"), 1) )
      maxg <- max(cdt$group)
    }

    sdt <- dt[0]
    if (is.na(max.lines) || (max.lines > 0)) {
      samps <- dt[, .(sample = unique(sample)), keyby=.(group)]
      if (!is.na(max.lines)) {
        samps <- samps[, .(sample = head(sample, max.lines)), keyby=.(group)]
      }

      sdt <- dt |> DT(
        samps, on=.(sample, group), nomatch=0
      ) |>
        DT(, spaghetti := cast_spag("sample")) |>
        DT(, sampleN := max(sample), by=group) |>
        DT(, group := maxg + .GRP, by=.(group, sample) )
    }

    return(rbind(
      cdt,
      sdt,
      fill = TRUE
    ))
  }
)

#' Show Replicated Observations + Median
#'
#' `stat_spaghetti()` extends observation groups by `sample` aesthetic and computes a
#' central indicator with `central.fun`
#'
#' @inheritParams ggplot2::layer
#' @inheritDotParams ggplot2::layer
#'
#' @param max.lines the maximum number of samples to display
#'
#' @param alpha the alpha value for sample and central lines. N.b. may also be set
#' by `mapping = aes(alpha = ...)`. May be `NULL`, a 1 or 2 valued numeric between
#' 0 and 1.
#'
#' @param central.fun the function for computing the central indicator
#'
#' @param na.rm should `NA` values be removed?
#'
#' @importFrom stats median
#' @importFrom utils tail
#' @export
#' @examples
#' require(data.table)
#' require(ggplot2)
#' require(cabputils)
#'
#' dt <- CJ(scenario = LETTERS[1:3], sample = 1:100, simt = 0:50)
#' dt[, simf := 0.5*simt*runif(.N, 0.9, 1.1) + 10*as.integer(factor(scenario)) ]
#'
#' core.p <- ggplot(dt) +
#'   aes(simt, log10(simf), color = scenario, sample = sample) +
#'   theme_minimal()
#' # basic behavior
#' core.p + stat_spaghetti()
#' # only show samples
#' core.p + stat_spaghetti(central.fun = NULL)
#' # only show central line
#' core.p + stat_spaghetti(max.lines = 0)
#' # show all the samples
#' core.p + stat_spaghetti(max.lines = NA)
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

#' #' @title Alpha Scale for Spaghetti Plots
#' #'
#' #' @inheritDotParams ggplot2::continuous_scale
#' #'
#' #' @importFrom scales rescale_max
#' #' @importFrom scales rescale_pal
#' #' @export
#' scale_alpha_spaghetti <- function(
#'   labels = function(brks) {
#'     if (length(brks) == 1) {
#'       ifelse((1-brks)^2 < 1e-6, "central", "sample")
#'     } else { c("sample", "central") }
#'   },
#'   breaks = function(lims) if (lims[1] == lims[2]) lims[1] else lims,
#'   ...
#' ) {
#'   continuous_scale(
#'     "alpha", "alpha_c",
#'     palette = scales::rescale_pal(range = c(0, 1)),
#'     rescaler = function(x, from, ...) {
#'       to <- c(1/256, 1)
#'       if (to[1] < from[1]) to[1] <- from[1]
#'       if (to[2] > from[2]) to[2] <- from[2]
#'       return(scales::rescale(x, to=to))
#'     },
#'     labels = labels, breaks = breaks,
#'     ...
#'   )
#' }

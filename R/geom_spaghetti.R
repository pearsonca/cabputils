#' @export
StatSpaghetti <- ggplot2::ggproto(
  "StatSpaghetti", ggplot2::Stat,
  required_aes = c("x", "y", "sample"),
  compute_panel = function(
    self, data, scales,
    max.lines,
    alpha.central, alpha.sample,
    central.fun
  ) {
    dt <- data |> as.data.table()
    samps <- dt[, .(sample = head(unique(sample), max.lines)), keyby=.(group)]
    dt |> DT(,
      copy(.SD)[, c("y", "alpha") := .(central.fun(y), alpha = alpha.central)],
      by=.(x, group)
    ) |> rbind(
      dt |> DT(
        samps, on=.(sample, group), nomatch=0
      ) |> DT(, c("group", "alpha") := .(
        max(group) + as.integer(interaction(group, sample, drop=TRUE)), alpha.sample
      ) )
    )
  }
)

#' Show Replicated Observations + Median
#'
#' `stat_spaghetti()` extends observation groups by `sample` aesthetic and computes a
#' central indicator with `central.fun`
#'
#' @importFrom stats median
#' @importFrom utils tail
#' @export
#' @examples
#' require(data.table)
#'
#' dt <- CJ(scenario = 1:3, sample = 1:100, simt = 0:50)
#' dt[, simf := 0.5*simt*runif(.N, 0.9, 1.1) + 10*scenario ]
#'
#' ggplot(dt) + aes(simt, log10(simf), color = factor(scenario)) + stat_spaghetti(
#'  aes(sample = sample)
#' ) + theme_minimal()
stat_spaghetti <- function(
  mapping = NULL, data = NULL, geom = "line",
  position = "identity", na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE,
  max.lines = if (interactive()) 10 else 100,
  alpha.sample = 1/max.lines, alpha.central = 1,
  central.fun = median,
  ...
) {
  stopifnot("max.lines must be > 1." = max.lines > 1)
  ggplot2::layer(
    stat = StatSpaghetti, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm, max.lines = max.lines,
      alpha.sample = alpha.sample, alpha.central = alpha.central,
      central.fun = central.fun,
      ...
    )
  )
}

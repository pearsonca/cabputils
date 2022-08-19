# Overview

This packages up the `R` utility code that

  * I use frequently enough to have trial-by-fired many approaches
  * I use frequently enough to warrant packaging

# Features

## `quantile`

I do a lot of simulation, with the results ending up in a `data.table`, then summarized by quantile. `quantile.data.table` provides a native-`data.table`-like interface to use the `base::quantile` on groups within a `data.table`. E.g.

```
require(data.table)
dtcars <- mtcars |> as.data.table()
dtcars |> quantile(.(mpg), keyby=.(cyl, gear), probs = 0.5)
dtcars |> quantile(.(mpg, wt), keyby=.(cyl, gear), probs = c(med=0.5))
```

## `commandArgs`

I mostly run code from a command prompt, but debug interactively. This version wraps the `base::commandArgs()` to provide a when-used-interactively return value that is sensible. E.g.

```
.args <- commandArgs(args = c(
  file.path("input", "data.csv"),
  file.path("output", "plot.png")
))
# when interactively coding, returns args
# when running, returns e.g. whatever input csv / output png target
```

# Installation

```
remotes::install_github("pearsonca/cabputils")
```

# Contribution

Sure, why not. I am unlikely to consider your convenience functions, but bug fixes, performance improvements, unit tests, &c to whatever is in here will be considered.

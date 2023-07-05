#' @title Sample Pairs of Distinct Indices
#'
#' @param n positive integer: number of pairs to sample
#'
#' @param prob a numeric vector OR an integer: the population weights for
#' sampling each index OR if length(prob) == 1, then the population size
#' (all members equally weighted)
#'
#' @param verbose Print progress messages?
#'
#' @return A list with two elements, `source` and `sink`, each of length `n`,
#' corresponding to the sampled pairs indices
#'
#' @examples
#' draws <- sample_pair(n = 100, prob = 10)
#'
#' @export
sample_pair <- function(n, prob, verbose = FALSE) {
  if (length(prob) == 1) {
    sampler <- function(sn) {
      sample(prob, sn, replace = TRUE)
    }
  } else {
    sampler <- function(sn) {
      sample(length(prob), sn, replace = TRUE, prob = prob)
    }
  }
  source <- sampler(n)
  sink <-   sampler(n)
  redraw <- which(source == sink)
  while (length(redraw)) {
    if (verbose) {
      message(sprintf("sample_pair: redrawing %i ...", length(redraw)))
    }
    sink[redraw] <- sampler(length(redraw))
    redraw <- redraw[which(source[redraw] == sink[redraw])]
  }
  return(list(source = source, sink = sink))
}

#' @title Sample Event Times Until a Maximum
#'
#' @description
#' Draw from a sample process, summing up the draws, until a maximum exceeded
#'
#'
#' @param maxval a positive number: the maximum limit to simulate until
#'
#' @param meanval a positive number: the mean value of a draw
#'
#' @param sampler a function: the sampling distribution for draws
#'
#' @param ... any parameter arguments to `sampler`
#'
#' @param oversample a number: the amount to over sample by when generating
#'
#' @param verbose Print progress messages?
#'
#' @return A numeric vector of draws: the cumulative sum of draws from
#' `sampler` until the last value exceeds `maxval`. That sum is then trimmed to
#' values `<= maxval`. The first value is `0`.
#'
#' @examples
#' draws <- sample_until(maxval = 100, meanval = 1, sampler = rexp, rate = 1)
#' draws
#'
#' @export
sample_until <- function(
    maxval, meanval, sampler, ...,
    oversample = 0.1, verbose = FALSE
) {
  # generate a vector of draws - oversample by e.g. 10%
  expectedevents <- maxval / meanval
  draws <- c(
    0, cumsum(sampler(ceiling(expectedevents * (1 + oversample)), ...))
  )

  # while the last value < maxval ...
  while (draws[length(draws)] < maxval) {

    # notify if asked to do so ...
    if (verbose) {
      message(sprintf(
        "sample_until: current end %d vs maxval %d -> add %i events ...",
        draws[length(draws)], maxval, ceiling(expectedevents * oversample)
      ))
    }

    # extend draws by the over sample amount (rounded up) ...
    draws <- c(
      draws,
      draws[length(draws)] +
        cumsum(sampler(ceiling(expectedevents * oversample), ...))
    )
  }

  # trim off any events that occur after maxval
  draws <- draws[draws <= maxval]

  return(draws)
}

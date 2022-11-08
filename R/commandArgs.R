
#' @title [base::commandArgs()] with Defaults
#'
#' @param trailingOnly see [base::commandArgs()];
#' no effect on `args`. NB: different default
#'
#' @param args character vector, result to return
#' when `debugging == TRUE`
#'
#' @param debugging logical, use `args` or not
#'
#' @export
commandArgs <- function(
  trailingOnly = TRUE,
  args = c(),
  debugging = interactive()
) {
  if (debugging) {
    if (length(args) == 0) warning("`args` is empty.")
    return(args)
  } else return(base::commandArgs(trailingOnly))
}


#' @title Generic Storage Wrapper
#'
#' @description Provides a generic storage method.
#'
#' @param obj the thing to store; CAVEAT EMPTOR: no
#' checking to see if `obj` is reasonable to store in
#' format inferred from `tail(args, 1)`.
#'
#' @param args character; where to store `obj`. Only
#' the last item, i.e. `tail(args, 1)`, is used. The default
#' assumes script starts with `.args <- commandArgs()`, so
#' all of the script arguments are tossed in here and the
#' last one is assumed to be a target file to write.
#'
#' Method of storage inferred from file extension (not
#' case-sensitive):
#'  * png, jpg, tif(f), bmp: [ggplot2::ggsave()]
#'  * rds: [saveRDS()]
#'  * rda, rdata: [save()]
#'  * csv: [data.table::fwrite()]
#'  * txt: [writeLines()]
#'  * other: Error message
#'
#' @param ... Other arguments to the inferred methods
#' (see `args` for relevant methods).
#'
#' @details `store()` provides a regularized interface to storing data.
#' It is intended to conveniently close scripts opened with
#' `.args <- commandArgs()`, hence the default `args = .args`. The default
#' can of course be replaced as necessary, and the resulting error refers
#' users to this documentation.
#'
#' The first argument to `store()` is the object to be stored; this enables
#' piping style code.
#'
#' Future versions should support pruning `...` elements which are
#' irrelevant to the inferred storage method before passing on. That will
#' facilitate using the same script to e.g. generate csv and rds files
#' associated with a dataset. E.g. the following `make`-style dependencies
#' would be supported:
#'
#' ```
#' some.rds: myscript.R input1.csv input2.csv
#'     Rscript $^ $@
#'
#' some.csv: myscript.R input1.csv input2.csv
#'     Rscript $^ $@
#' ```
#'
#' ... or multi-target rules for versions of `make` that support such.
#'
#' @return whatever is returned from the inferred
#' storage method.
#'
#' @examples
#' require(cabputils)
#' .args <- commandArgs(args = "some.csv") # storing result as a csv
#' data.frame(x=1:10, y=10:1) |> store()
#'
#' @export
store <- function(obj, args = .args, ...) {
  tarfile <- tryCatch(
    tail(args, 1),
    error = function(e) stop(
      "Did not find `.args`; default `args` assumes approach described in `?store` details.",
      call. = FALSE
    )
  )
  stopifnot(
    "Must provide a string via args." = class(tarfile) == "character",
    "Path must have length." = nchar(tarfile) > 0
  )
  # lower-case to simply switch matching
  ext <- tarfile |> tools::file_ext() |> tolower()
  stopifnot(
    "Target file must have extension." = nchar(tools::file_ext(tarfile)) > 0
  )
  fun <- switch(ext,
    rds = \(fl, obj, ...) base::saveRDS(obj, file = fl, ...),
    rda =, rdata = \(fl, obj, ...) base::save(obj, file = tarfile, ...),
    png =, jpg =, tiff =, tif =,
    bmp = \(fl, obj, ...) ggplot2::ggsave(filename = fl, plot = obj, ...),
    csv = \(fl, obj, ...) data.table::fwrite(obj, file = fl, ...),
    txt = \(fl, obj, ...) base::writeLines(text = obj, con = fl, ...),
    \(fl, ...) stop("didn't understand format: ", fl)
  )
  return(fun(tarfile, obj, ...))
}


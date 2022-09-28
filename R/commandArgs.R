
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
#' @param args character; where to store `obj`. Only
#' the last item, i.e. `tail(args, 1)`, is used, so e.g.
#' all of the script arguments can be tossed in here if
#' they end in the file to write to for output.
#' Method of storage inferred from file extension (not
#' case-sensitive):
#'  * png, jpg, tif(f), bmp: [ggplot2::ggsave()]
#'  * rds: [saveRDS()]
#'  * rda, rdata: [save()]
#'  * csv: [data.table::fwrite()]
#'  * txt: [writeLines()]
#'  * other: Error message
#'
#' @param obj the thing to store; CAVEAT EMPTOR: no
#' checking to see if `obj` is reasonable to store in
#' format inferred from `tail(args, 1)`.
#'
#' @param ... Other arguments to the inferred methods
#' (see `args` for relevant methods)
#'
#' @return whatever is returned from the inferred
#' storage method.
#'
#' @export
store <- function(args, obj, ...) {
  tarfile <- tail(args, 1)
  fun <- switch(
    tolower(tools::file_ext(tarfile)),
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


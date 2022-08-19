
#' @title [base::commandArgs()] with Defaults
#'
#' @param trailingOnly see [base::commandArgs()];
#' no effect on `args`
#'
#' @param args character, result to return when
#' debugging
#'
#' @param debugging logical, use `args` or not
#'
#' @export
commandArgs <- function(
  trailingOnly,
  args,
  debugging = interactive()
) {
  if (debugging) {
    return(args)
  } else return(base::commandArgs(trailingOnly))
}


#' @title Generic Storage Wrapper
#'
#' @description Provides a generic storage method.
#'
#' @param args character; where to store `obj`. Only
#' the last item, i.e. `tail(args, 1)`, is used, so
#' all of the script arguments can be tossed in here if
#' they end in the file to write to for output.
#' Method of storage inferred from file extension.
#'
#' @param obj the thing to store; CAVEAT EMPTOR: no
#' checking to see if `obj` is reasonable to store in
#' `tail(args, 1)`.
#'
#' @param ... Other arguments to the inferred methods. See:
#' [saveRDS()] for .rds, [save()] for .rda, [ggplot2::ggsave()]
#' for image formats, [data.table::fwrite()] for csv
#'
#' @export
store <- function(args, obj, ...) {
  tarfile <- tail(args, 1)
  fun <- switch(
    tolower(tools::file_ext(tarfile)),
    rds = \(fl, obj, ...) saveRDS(obj, file = fl, ...),
    rda =, rdata = \(fl, obj, ...) save(obj, file = tarfile, ...),
    png =, jpg =, tiff =, tif =,
    bmp = \(fl, obj, ...) ggplot2::ggsave(filename = fl, plot = obj, ...),
    csv = \(fl, obj, ...) data.table::fwrite(obj, file = fl, ...),
    \(fl, ...) stop("didn't understand ", fl)
  )
  return(fun(tarfile, obj, ...))
}


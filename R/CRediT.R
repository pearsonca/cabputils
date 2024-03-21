
#' create a CRediT statement
#'
#' @param dt an object coerceable to data.table, with columns `Term`,
#' optionally `definition`, and then a series of author names.
#'
#' @return a string corresponding to the statement
#'
#' @export
CRediT <- function(dt) {
  grep("term|definition", names(dt), invert = TRUE, value = TRUE, ignore.case = TRUE) |>
    lapply(\(auth) {
      dt[get(auth) != "", sprintf("%s: %s", auth, paste(Term, collapse = ", "))]
    }) |> paste(collapse = "; ")
}

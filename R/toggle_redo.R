#' Toggle Redo Variables
#'
#' Toggle the state (TRUE to FALSE or FALSE to TRUE) of your redo variables, or set all to TRUE or FALSE.
#' @param redo.vars A character vector of the variable names you want to toggle state.
#' @param set.all.to TRUE/FALSE, instead of toggling the state of individual redo variable, set all to TRUE or FALSE.
#' @export
#' @examples
#' chunk.names <- c("chunk1", "chunk2")
#'
#' assign.redo(chunk.names)
#'
#'toggle.redo("chunk1")
#'
#' if (redo$chunk1) {
#'   x <- rnorm(100)
#' } else {
#'   x <- data(example_data1)
#' }

toggle.redo <- function(redo.vars, set.all.to = NULL) {
  if (!is.null(set.all.to)) {
    assign.redo(ls(redo), state = set.all.to)
  } else {
    for (redo.var in redo.vars) {
      curr.state <- redo[[redo.var]]
      assign(redo.var, ifelse(curr.state == TRUE, FALSE, TRUE), envir = redo)
    }
  }
}

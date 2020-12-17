#' Assign Redo Variables
#'
#' When you have various chunks of code you may or may not to rerun during script execution (and rather load from a file, say), you can set certain variables to TRUE/FALSE for use in if() statements to run that chunk or not. This function creates those variables and assigns them their default state.
#' @param redo.vars A character vector of the variable names you want to give TRUE or FALSE to
#' @param state A the default state to set the redo variables to, either TRUE or FALSE
#' @export
#' @examples
#' chunk.names <- c("chunk1", "chunk2")
#'
#' assign.redo(chunk.names)
#'
#' if (redo$chunk1) {
#'   x <- rnorm(100)
#' } else {
#'   x <- data(example_data1)
#' }


assign.redo <- function(redo.vars, state = FALSE) {
  if (class(state) != "logical") stop("Variable `state` must by logical, TRUE/FALSE")
  redo.vars <- as.character(redo.vars)
  for (var in redo.vars) {
    assign(var, state, envir = redo)
  }
}

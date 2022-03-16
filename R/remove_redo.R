#' Remove Redo Variable(s)
#'
#' Remove one or more redo variables from the redo environment.
#' @param redo.vars A character vector of the variable names you want to remove.
#' @export
#' @examples
#' chunk.names <- c("chunk1", "chunk2")
#'
#' assign.redo(chunk.names)
#' list.redos()
#'
#' remove.redo("chunk1")
#' list.redos()

remove.redo <- function(redo.vars) {
  rm(list = redo.vars, envir = redo)
}

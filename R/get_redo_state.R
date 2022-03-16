#' Get state of Redo Variable(s)
#'
#' Get the state (TRUE or FALSE) of one or more redo variables.
#' @param redo.vars A character vector of the variable names you want to get the state of.
#' @export
#' @examples
#' chunk.names <- c("chunk1", "chunk2")
#'
#' assign.redo(chunk.names)
#'
#' get.redo.sate("chunk1")

get.redo.state <- function(redo.vars) {
  unlist(mget(redo.vars, envir = redo))
}

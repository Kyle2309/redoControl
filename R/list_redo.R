#' List Current Redo Variables
#'
#' Lists the current variables in the `redo` environment.
#' @export
#' @examples
#' chunk.names <- c("chunk1", "chunk2")
#'
#' assign.redo(chunk.names)
#'
#' list.redos()

list.redos <- function() {for(var in ls(redo)) cat(paste0(var, ": ", redo[[var]]), sep = "\n")}

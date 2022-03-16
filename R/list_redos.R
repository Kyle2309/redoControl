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

list.redos <- function() {
  knitr::kable(
    data.frame(
      Redo.Var = ls(redo),
      State = unname(unlist(mget(ls(redo), envir = redo)))
    ),
    format = "simple"
  )
}

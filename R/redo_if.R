#' Run code if redo variable is TRUE
#'
#' Basically executes an IF/ELSE statement to either re-run code (if the redo variable is TRUE) or return data saved in an RDS file (if the redo variable is FALSE).
#' @param redo.var The redo variable name you want to reference.
#' @param save.file The RDS file to which you want to save the results or from which you want to read prior results. If `.rds` is not at the end of the file, it will be appended.
#' @param code The code you want to execute within the statement which should produce the object you want to return and save when the redo variables is TRUE.
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
#'
#' x <- redo.if("chunk1", save.file = "test_file.rds", {rnorm(50)})

redo.if <- function(redo.var, save.file, code, force = FALSE) {
  if (!grepl("\\.rds$", save.file)) {save.file <- paste0(save.file, ".rds")}

  if (force) {
    run.anyway <- TRUE
  } else {
    run.anyway <- FALSE
    affirmatives <- c("y", "yes", "Y", "YES", "Yes")
    negatives <- c("n", "no", "N", "NO", "No")

    if (!file.exists(save.file) & !redo[[redo.var]]) {
      answer <- ""
      while (!(answer %in% c(affirmatives, negatives))) {
        answer <- readline(
          paste0(redo.var, " is set to FALSE, but the save file, ", save.file, ", doesn't exist. Run code anyway? (y/n): ")
        )
      }

      if (answer %in% affirmatives) {
        run.anyway <- TRUE
      }
    }
  }

  if (redo[[redo.var]] | run.anyway) {
    result <- local(code)
    saveRDS(result, file = save.file)
  }
  return(readRDS(save.file))
}

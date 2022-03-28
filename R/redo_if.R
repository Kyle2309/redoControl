#' Run code if redo variable is TRUE
#'
#' Basically executes an IF/ELSE statement to either re-run code (if the redo variable is TRUE) or return data saved in an RDS file (if the redo variable is FALSE).
#' @param redo.var character; The redo variable name you want to reference.
#' @param save.file character; The RDS file to which you want to save the results or from which you want to read prior results. If `.rds` is not at the end of the file, it will be appended.
#' @param code The code (in curly brackets) you want to execute within the statement which should produce the object you want to return and save when the redo variables is TRUE.
#' @param max.save.mb integer; the file size (in megabytes) at which a saved file will trigger a prompt to move it to the `lf.dir`
#' @param lf.dir character; the name of the directory to move files larger than `max.save.mb` to. Default is "Large_files"
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

redo.if <- function(
  redo.var,
  save.file = NULL,
  code,
  force = FALSE,
  max.save.mb = 10,
  lf.dir = "Large_files"
) {
  if (!is.null(save.file)) {
    if (!grepl("\\.rds$", save.file)) { save.file <- paste0(save.file, ".rds") }
  }

  if (force) {
    run.anyway <- TRUE
  } else {
    run.anyway <- FALSE
    affirmatives <- c("y", "yes", "Y", "YES", "Yes")
    negatives <- c("n", "no", "N", "NO", "No")

    if (!is.null(save.file)) {
      if (!file.exists(save.file) & !redo[[redo.var]]) {
        file.name <- sapply(str_split(save.file, "/"), tail, 1)
        if (file.exists(file.path(lf.dir, file.name))) {
          cat(
            paste0(
              "The save file given was:\n\t", save.file, "\nwhich does not exist.",
              "\nHowever, this file does exist:\n\t", file.path(lf.dir, file.name),
              "\nand it has been used to read in saved data.",
              "\n\nTo avoid this notification in the future,",
              "\nplease update the the path supplied to `save.file`."
            ),
            sep = "\n"
          )
        } else {
          answer <- ""
          while (!(answer %in% c(affirmatives, negatives))) {
            cat(
              paste0(
                redo.var,
                " is set to FALSE, but the save file\n\t",
                save.file,
                "\ndoesn't exist."
              ),
              sep = "\n"
            )
            answer <- readline(prompt = "Run code anyway? (y/n): ")
          }

          if (answer %in% affirmatives) {
            run.anyway <- TRUE
          }
        }
      }
    }
  }
  if (redo[[redo.var]] | run.anyway) {
    result <- local(code)
    if (!is.null(save.file)) {
      saveRDS(result, file = save.file)
      file.mbs <- file.size(save.file) / 1024^2
      if (file.mbs > max.save.mb) {
        file.name <- sapply(str_split(save.file, "/"), tail, 1)
        file.copy(from = save.file, to = file.path(lf.dir, file.name), overwrite = T)
        file.remove(save.file)
        cat(
          paste0(
            "The size (",  file.mbs, " MB) of the save file:\n\t", save.file,
            "\nis larger than the maximum file size of:", max.save.mb, "MB.",
            "\nIt has been moved to:\n\t", file.path(lf.dir, file.name)
          ),
          sep = "\n"
        )
      }
    }
  }
  if (!is.null(save.file)) { return(readRDS(save.file)) }
}

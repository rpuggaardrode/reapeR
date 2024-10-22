#' Read in REAPER epochs output file as tibble
#'
#' Reads in an ASCII epochs / pitchmarks file created by REAPER as a
#' vector of epochs
#'
#' @param epochsFile String giving the file name of an ASCII epochs / pitchmarks
#' file created by REAPER.
#' @param audioFile String giving the name of the original audio file. If a
#' string is provided, the function will return a named list instead of a
#' vector (practical if creating a large object with many of these files).
#' Default is `NULL`.
#' @param delete Boolean; should the original REAPER output file be deleted?
#' Default is `FALSE`.
#'
#' @return# A numeric vector of epochs, or a named list with this vector.
#' @export
#'
#' @examples
#' path <- file.path(system.file(package = 'reapeR'), 'extdata', 'epochs')
#' epochs <- read_epochs_out(path)
read_epochs_out <- function(epochsFile, audioFile = NULL, delete = FALSE) {
  pm_file <- readr::read_file(epochsFile)
  if (delete) unlink(epochsFile)
  pmest <- unlist(strsplit(pm_file, 'EST_Header_End\\n'))[2]
  pmest <- suppressMessages(readr::read_delim(pmest))
  colnames(pmest) <- c('time', 'voiced', 'x')
  epochs <- pmest$time[which(pmest$voiced == 1)]
  if (!is.null(audioFile)) {
    epochs <- list(epochs)
    names(epochs) = audioFile
  }
  return(epochs)
}

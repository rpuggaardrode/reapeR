#' Read in REAPER pitch output file as tibble
#'
#' Reads in an ASCII pitch file created by REAPER as a well-formatted tibble
#'
#' @param pitchFile String giving the file name of an ASCII pitch file
#' created by REAPER.
#' @param audioFile String giving the name of the original audio file. If a
#' string is provided, a separate column will be made in the tibble repeating
#' this string (practical if creating a large data frame with many of these
#' files). Default is `NULL`, in which case no such column is created.
#' @param delete Boolean; should the original REAPER output file be deleted?
#' Default is `FALSE`.
#'
#' @return A data frame with four columns:
#' * `time`, giving the frame time in seconds
#' * `voiced` Boolean specifying whether that frame is `voiced`
#' * `f0` F0 in Hz
#' * `file` String giving the name of the analyzed file.
#' @export
#'
#' @examples
#' path <- file.path(system.file(package = 'reapeR'), 'extdata', 'pitch')
#' pitch <- read_pitch_out(path)
read_pitch_out <- function(pitchFile, audioFile = NULL, delete = FALSE) {
  f0_file <- readr::read_file(pitchFile)
  if (delete) unlink(pitchFile)
  f0est <- unlist(strsplit(f0_file, 'EST_Header_End\\n'))[2]
  f0est <- suppressMessages(readr::read_delim(f0est))
  colnames(f0est) <- c('time', 'voiced', 'f0')
  f0est[which(f0est$f0 == -1),'f0'] <- NA
  if (!is.null(audioFile)) f0est$file <- rep(audioFile, nrow(f0est))
  return(f0est)
}

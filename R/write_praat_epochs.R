#' Write REAPER output to Praat PointProcess file
#'
#' Convert the estimated epoch locations of REAPER to a `.PointProcess` file
#' that can be read by Praat.
#'
#' @param reaper_out Data frame with the output of REAPER, as loaded with
#' [read_epochs_out] or created with [reaper].
#' @param directory String giving the location to use for storing the resulting
#' `.Pitch` file.
#' @param filename String giving the file name of the resulting file (note that
#' the `.Pitch` extension is added automatically).
#'
#' @return Used for side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' path <- file.path(system.file(package = 'reapeR'), 'extdata', 'epochs')
#' epo <- read_epochs_out(path)
#' write_praat_epochs(pitch, epo, 'test')
#' }
write_praat_epochs <- function(reaper_out, directory, filename) {
  newFile <- paste0(directory, '/', filename, '.PointProcess')

  toWrite <- paste('File type = "ooTextFile"',
                   'Object class = "PointProcess"', '',
                   'xmin = 0',
                   sep = '\n')
  toWrite <- paste(toWrite, '\nxmax =', max(reaper_out[[1]]))
  toWrite <- paste(toWrite, '\nnt =', length(reaper_out[[1]]))
  toWrite <- paste(toWrite, '\nt []:')
  for (i in 1:length(reaper_out[[1]])) {
    toWrite <- paste0(toWrite, '\nt [', i, '] = ', reaper_out[[1]][i])
  }

  writeLines(toWrite, con=newFile)
}

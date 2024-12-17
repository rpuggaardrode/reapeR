#' Write REAPER output to Praat Pitch file
#'
#' Convert the pitch estimates of REAPER to a `.Pitch` file that can be read by
#' Praat.
#'
#' @param reaper_out Data frame with the output of REAPER, as loaded with
#' [read_pitch_out] or created with [reaper].
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
#' path <- file.path(system.file(package = 'reapeR'), 'extdata', 'pitch')
#' pitch <- read_pitch_out(path)
#' write_praat_pitch(pitch, path, 'test')
#' }
write_praat_pitch <- function(reaper_out, directory, filename) {
  newFile <- paste0(directory, '/', filename, '.Pitch')

  toWrite <- paste('File type = "ooTextFile"',
                   'Object class = "Pitch 1"', '',
                   'xmin = 0',
                   sep = '\n')
  toWrite <- paste(toWrite, '\nxmax =', max(reaper_out$time))
  toWrite <- paste(toWrite, '\nnx =', nrow(reaper_out))
  toWrite <- paste(toWrite, '\ndx =', reaper_out$time[2] - reaper_out$time[1])
  toWrite <- paste(toWrite, '\nx1 =', reaper_out$time[1])
  toWrite <- paste(toWrite, '\nceiling =', max(reaper_out$f0, na.rm=T) + 10)
  toWrite <- paste(toWrite, '\nmaxnCandidates = 1\nframes = []:')
  for (i in 1:nrow(reaper_out)) {
    toWrite <- paste0(toWrite, '\nframes [', i, ']:')
    if (is.na(reaper_out$f0[i])) {
      toWrite <- paste(toWrite, 'intensity = 0',
                       'nCandidates = 1', 'candidates []:',
                       'candidates [1]:', 'frequency = 0', 'strength = 0',
                       sep = '\n')
    } else {
      toWrite <- paste(toWrite, 'intensity = 0.99',
                       'nCandidates = 1', 'candidates []:',
                       'candidates [1]:', paste('frequency =', reaper_out$f0[i]),
                       'strength = 1', sep = '\n')
    }
  }

  writeLines(toWrite, con=newFile)
}

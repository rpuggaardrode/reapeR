#' Convert REAPER output to SSFF format
#'
#' Convert REAPER output for a single file to Simple Signal File Format data.
#' Usually used by [reaper2emuDB], but may also be used as a stand-alone for
#' e.g. plotting REAPER pitch data with `praatpicture`.
#'
#' @param reaper_output Output from [reaper] containing pitch measures.
#'
#' @returns An object of class `AsspDataObj`.
#' @export
#'
#' @examples
#' snd <- file.path(system.file('extdata', package = 'reapeR'), '1.wav')
#' vals <- reaper(snd)
#' ssffObj <- reaper2ssff(vals)
reaper2ssff <- function(reaper_output) {
  if (!is.data.frame(reaper_output)) reaper_output <- reaper_output$pitch
  if (length(unique(reaper_output$file)) > 1) stop(
    'Output should come from only file')

  start <- reaper_output$time[1]
  sr <- round(1 / (reaper_output[[2,'time']] - reaper_output[[1,'time']]), 0)

  ado <- list()
  attr(ado, 'sampleRate') <- sr
  attr(ado, 'origFreq') <- 0
  attr(ado, 'startTime') <- start
  attr(ado, 'startRecord') <- as.integer(1)
  attr(ado, 'endRecord') <- nrow(reaper_output)
  class(ado) <- 'AsspDataObj'
  wrassp::AsspFileFormat(ado) <- 'SSFF'
  wrassp::AsspDataFormat(ado) <- as.integer(2)
  ado <- wrassp::addTrack(ado, 'rF0', reaper_output$f0, 'REAL32')
  ado <- wrassp::addTrack(ado, 'vd', reaper_output$voiced, format='REAL32')
  attr(ado, 'trackFormats') <- rep('REAL32', length(ado))
  return(ado)
}

#' Estimate pitch and/or epochs with REAPER
#'
#' Wrapper function to call David Talkin's REAPER software in R
#'
#' @param file String giving the name of an audio file to analyze
#' @param f0min Numeric value specifying pitch floor. Default is `40`.
#' @param f0max Numeric value specifying pitch ceiling. Default is `500`.
#' @param interval Numeric value giving the F0 output interval in seconds.
#' Default is `0.005`.
#' @param hilbert Boolean; should Hilbert transform be applied prior to
#' analysis? Default is `FALSE`.
#' @param suppress_highpass_filter Boolean; should highpass filter be
#' suppressed? Default is `FALSE`.
#' @param unvoiced_cost Numeric; cost for unvoiced segments. Default is `0.9`,
#' set higher value to estimate more F0 in noise.
#' @param output String or vector of strings specifying which estimates to
#' output. Possible values are `pitch` and `epochs`, default is to output
#' both.
#' @param exePath String giving the location of your REAPER executable. Default
#' is `NULL`, which should work if REAPER was successfully installed with the
#' installation of this R package. If it wasn't, but you have REAPER installed
#' somewhere else, you can specify the path of the executable here.
#'
#' @return If `output = 'pitch'`, returns a data frame with four columns:
#' * `time`, giving the frame time in seconds
#' * `voiced` Boolean specifying whether that frame is `voiced`
#' * `f0` F0 in Hz
#' * `file` String giving the name of the analyzed file.
#'
#' If `output = 'epochs'`, returns a numeric vector of epochs, or glottal
#' closure instants.
#'
#' If `output = c('pitch', 'epochs')`, returns a list with the above two
#' outputs.
#' @export
#'
#' @examples
#' snd <- file.path(system.file('extdata', package = 'reapeR'), '1.wav')
#' vals <- reaper(snd)
reaper <- function(file, f0min=40, f0max=500, interval=0.005,
                   hilbert=FALSE, suppress_highpass_filter=FALSE,
                   unvoiced_cost=0.9, output = c('pitch', 'epochs'),
                   verbose = TRUE, exePath = NULL) {

  if (is.null(exePath)) exePath <- list.files(
    file.path(system.file(package = 'reapeR'),
              'bin'), recursive = TRUE, full.names = TRUE)

  exePath <- paste0('"', exePath, '"')
  fileSafe <- paste0('"', file, '"')

  call <- paste(exePath, '-i', fileSafe, '-x', f0max, '-m', f0min,
                '-e', interval, '-u', unvoiced_cost, '-a')
  if (hilbert) call <- paste(call, '-t')
  if (suppress_highpass_filter) call <- paste(call, '-s')
  if ('pitch' %in% output) call <- paste(call, '-f f0out')
  if ('epochs' %in% output) call <- paste(call, '-p pmout')
  system(call, ignore.stdout = TRUE)
  if (verbose) print(paste(file, 'processed!'))

  if ('pitch' %in% output) f0est <- read_pitch_out('f0out', file, TRUE)
  if ('epochs' %in% output) epochs <- read_epochs_out('pmout', file, TRUE)

  if (length(output) == 2) {
    return(list(pitch = f0est, epochs = epochs))
  } else if (output == 'pitch') {
    return(f0est)
  } else {
    return(epochs)
  }
}

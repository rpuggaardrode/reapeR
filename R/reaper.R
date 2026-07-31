#' Estimate pitch and/or epochs with REAPER
#'
#' Track pitch and/or estimate epochs using David Talkin's REAPER library.
#'
#' @param file String giving the name of a WAV file to analyze
#' @param start Numeric value giving the analysis start time in seconds.
#' When possible, the actual analysis will begin 50 ms prior to this time.
#' Default is `0`.
#' @param end Numeric value giving the analysis end time in seconds.
#' When possible, the actual analysis will begin 50 ms after this time.
#' Default is `Inf`.
#' @param channel Numeric value specifying the channel to be analyzed.
#' Default is `1`.
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
#' @param verbose Boolean; should messages be printed to the console?
#' Default is `TRUE`.
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
reaper <- function(file, start = 0, end = Inf, channel=1, f0min=40, f0max=500,
                   interval=0.005, hilbert=FALSE,
                   suppress_highpass_filter=FALSE,
                   unvoiced_cost=0.9, output = c('pitch', 'epochs'),
                   verbose = TRUE) {

  if (!file.exists(file)) stop('File does not exist')

  realStart <- ifelse(start > 0.05, start - 0.05, start)
  realEnd <- end + 0.05

  snd <- tuneR::readWave(file, toWaveMC = TRUE, from = realStart, to = realEnd,
                         units = 'seconds')
  if (channel > dim(snd@.Data)[2]) stop('Channel does not exist in sound file')
  if (snd@bit != 16) {
    flattened_sig <- (snd@.Data[,channel] / 2^snd@bit) + 0.5
    snd@.Data[,channel] <- as.integer((flattened_sig - 0.5) * 2^16)
  }

  out <- reaper_wrap(snd@.Data[,channel], snd@samp.rate, f0min, f0max,
                     suppress_highpass_filter, hilbert, interval, unvoiced_cost,
                     verbose = verbose)

  if ('pitch' %in% output) {
    f0est <- data.frame(
      time = seq(interval, interval * length(out$f0), by = interval) + realStart,
      voiced = ifelse(out$f0 == 0, 0, 1),
      f0 = ifelse(out$f0 == 0, NA, out$f0),
      file = rep(file, length(out$f0))
    )
    f0est <- f0est[which(f0est$time > start & f0est$time < end),]
  }

  if ('epochs' %in% output) {
    epochs <- out$epochs[which(out$voicing == 1)]
    epochs <- epochs[which(epochs > start & epochs < end)]
    epochs <- list(epochs)
    names(epochs) <- file
  }

  if (length(output) == 2) {
    return(list(pitch = f0est, epochs = epochs))
  } else if (output == 'pitch') {
    return(f0est)
  } else {
    return(epochs)
  }
}

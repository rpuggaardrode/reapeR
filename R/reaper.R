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
#' output. Default is to output `pitch` and `epochs`. Possible values are:
#' * `pitch` Returning a data frame with equidistant pitch estimates at
#' intervals determined by `interval`.
#' * `epochs` returning a named list with a numeric vector specifying the times
#' of estimated glottal closure instants.
#' * `gci_cand` Returning a data frame with times that are candidate glottal
#' closure instants, as well as their time, local pitch value if determined
#' to be voiced, and normalized cross-correlation functions (`nccf`).
#' * `probs` Returning a data frame with pseudo-probabilities of voicing,
#' voicing onset, and voicing offset at regular intervals, as well as bandpassed
#' (low-frequency) root-mean-squared amplitude of the signal, which is used to
#' derive the pseudo-probability of voicing.
#' * `resids` Returning a data frame with the original signal, linear prediction
#' residuals, and linear prediction residuals normalized by local
#' amplitude.
#' @param force_list_output Boolean; if `TRUE`, the function returns a named
#' nested list even if only one output is chosen. Default is `FALSE`.
#' @param verbose Boolean; should diagnostic messages be printed to the console?
#' Default is `FALSE`.
#'
#' @return Depending on the value of `output`, either a nested list of outputs,
#' a data frame, or a numeric vector.
#' @export
#'
#' @examples
#' snd <- file.path(system.file('extdata', package = 'reapeR'), '1.wav')
#' vals <- reaper(snd)
reaper <- function(file, start = 0, end = Inf, channel=1, f0min=40, f0max=500,
                   interval=0.005, hilbert=FALSE,
                   suppress_highpass_filter=FALSE,
                   unvoiced_cost=0.9, output = c('pitch', 'epochs'),
                   force_list_output = FALSE, verbose = FALSE) {

  if (!file.exists(file)) stop('File does not exist')

  if (!any(output %in% c('pitch', 'epochs', 'gci_cand', 'resids', 'probs'))) {
    stop('Legal outputs are: pitch, epochs, gci_cand, resids, probs')
  }

  realStart <- ifelse(start > 0.05, start - 0.05, start)
  realEnd <- end + 0.05

  snd <- tuneR::readWave(file, toWaveMC = TRUE, from = realStart, to = realEnd,
                         units = 'seconds')
  if (channel > dim(snd@.Data)[2]) stop('Channel does not exist in sound file')
  if (snd@bit != 16) {
    flattened_sig <- (snd@.Data[,channel] / 2^snd@bit) + 0.5
    snd@.Data[,channel] <- as.integer((flattened_sig - 0.5) * 2^16)
  }

  dat <- reaper_wrap(snd@.Data[,channel], snd@samp.rate, f0min, f0max,
                     suppress_highpass_filter, hilbert, interval, unvoiced_cost,
                     verbose = verbose)

  out <- list()

  if ('pitch' %in% output) {
    f0est <- data.frame(
      time = seq(interval, interval * length(dat$f0), by = interval) + realStart,
      voiced = ifelse(dat$f0 == 0, 0, 1),
      f0 = ifelse(dat$f0 == 0, NA, dat$f0),
      file = rep(file, length(dat$f0))
    )
    f0est <- f0est[which(f0est$time > start & f0est$time < end),]
    out$pitch <- f0est
  }

  if ('epochs' %in% output) {
    epochs <- dat$epochs[which(dat$voicing == 1)]
    epochs <- epochs + realStart
    epochs <- epochs[which(epochs > start & epochs < end)]
    epochs <- list(epochs)
    names(epochs) <- file
    out$epochs <- epochs
  }

  if ('resids' %in% output) {
    resids <- data.frame(
      time = (1:length(dat$signal) / snd@samp.rate) + realStart,
      signal = dat$signal,
      lp_residual = dat$residual,
      norm_residual = dat$norm_residual,
      peaks_debug = dat$peaks_debug
    )
    resids <- resids[which(resids$time > start & resids$time < end),]
    out$resids <- resids
  }

  if ('gci_cand' %in% output) {
    gci <- data.frame(
      time = dat$f0_time + realStart,
      f0 = ifelse(dat$f0_diag == 0, NA, dat$f0_diag),
      nccf = dat$nccf,
      voiced = dat$voiced_diag,
      file = rep(file, length(dat$nccf))
    )
    gci <- gci[which(gci$time > start & gci$time < end),]
    out$gci_cand <- gci
  }

  if ('probs' %in% output) {
    frameShift <- round(length(dat$signal) / snd@samp.rate /
                          length(dat$prob_voiced), 3)
    probs <- data.frame(
      time = seq(frameShift, frameShift * length(dat$prob_voiced),
                 by = frameShift) + realStart,
      bandpassed_rms = dat$bandpassed_rms,
      voice_onset_prob = dat$voice_onset_prob,
      voice_offset_prob = dat$voice_offset_prob,
      prob_voiced = dat$prob_voiced,
      file = rep(file, length(dat$prob_voiced))
    )
    out$probs <- probs
  }

  if (length(output) == 1 & !force_list_output) out <- out[[1]]
  return(out)

}

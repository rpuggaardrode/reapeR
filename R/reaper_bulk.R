#' Bulk estimate pitch and/or epochs with with REAPER
#'
#' Wrapper function to call David Talkin's REAPER software on all WAV files
#' in a directory
#'
#' @param directory String giving the name of a directory where all WAV files
#' should be processed. Alternatively the handle of a loaded EMU database.
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
#' @param f0min Numeric value specifying pitch floor. Default is `40`.
#' @param f0max Numeric value specifying pitch ceiling. Default is `500`.
#' @param hirst2pass Boolean; should pitch floor and ceiling be dynamically
#' estimated using the two-pass procedure proposed by Hirst and de Looze?
#' In this case, REAPER is first run with liberal floor and ceiling values of
#' 60 Hz and 700 Hz respectively, and then rerun with optimal floor and ceiling
#' values estimated from the first and third quantiles of the first pass.
#' (Floor = 0.75 Q1, ceiling = 1.5 Q3). If `directory` refers to an EMU database
#' with multiple sessions, the two-pass procedure is run separately for each
#' session.
#' @param praat_output Boolean; should REAPER output be stored as a Praat
#' `.Pitch` file? Default is `FALSE`.
#' @param praat_output_dir String giving the location of a directory where
#' Praat `.Pitch` files should be stored. Default is `NULL`.
#' @param hirst2pass_f0min Numeric giving the pitch floor in Hz for the first
#' pass in a two-pass procedure. Default is `60`.
#' @param hirst2pass_f0max Numeric giving the pitch ceiling in Hz for the first
#' pass in a two-pass procedure. Default is `700`.
#' @param ... Further arguments passed on to `reaper()`.
#'
#' @return Depending on the value of `output`, either a nested list of outputs,
#' a data frame, or a numeric vector.
#' @export
#'
#' @examples
#' dir <- file.path(system.file('extdata', package = 'reapeR'))
#' vals <- reaper_bulk(dir)
reaper_bulk <- function(directory, output = c('pitch', 'epochs'),
                        f0min = 40, f0max = 500, hirst2pass = FALSE,
                        praat_output = FALSE, praat_output_dir = NULL,
                        hirst2pass_f0min = 60, hirst2pass_f0max = 700, ...) {

  if ('resids' %in% output) stop('reaper_bulk cannot be used with output resids')
  if (praat_output & !'pitch' %in% output) {
    warning('Praat output will not be generated as pitch is not being estimated')
    praat_output <- FALSE
  }
  if (hirst2pass & !'pitch' %in% output) {
    warning('Pitch will be estimated as hirst2pass is set to TRUE')
    output <- c(output, 'pitch')
  }

  if (inherits(directory, 'emuDBhandle')) {
    bndls <- emuR::list_bundles(directory)
    wavs <- paste0(directory$basePath, '/', bndls$session, '_ses/',
                   bndls$name, '_bndl/', bndls$name, '.wav')
    bndls$file <- wavs
    ses <- unique(bndls$session)
    if (praat_output) {
      dirStructure <- list.dirs(directory$basePath, recursive = TRUE,
                                full.names = FALSE)[-1]
      for (d in 1:length(dirStructure)) dir.create(
        file.path(praat_output_dir, dirStructure[d]))
    }
  } else {
    wavs <- paste0(directory, '/', list.files(directory, pattern='*.wav'))
    ses <- 'dummy'
  }

  if ('pitch' %in% output) pitch <-
      data.frame(time = NA, voiced = NA, f0 = NA, file = NA)
  if ('epochs' %in% output) epochs <- c()
  if ('gci_cand' %in% output) gci_cand <-
      data.frame(time = NA, f0 = NA, nccf = NA, voiced = NA, file = NA)
  if ('probs' %in% output) probs <-
      data.frame(time = NA, bandpassed_rms = NA, voice_onset_prob = NA,
                 voice_offset_prob = NA, prob_voiced = NA, file = NA)

  if (hirst2pass) {
    f0min <- hirst2pass_f0min
    f0max <- hirst2pass_f0max
  }

  for (s in ses) {
    if (inherits(directory, 'emuDBhandle')) {
      fls <- bndls[which(bndls$session == s),]$file
    } else {
      fls <- wavs
    }

    for (f in fls) {
      dat <- reaper(f, output = output, f0min = f0min, f0max = f0max,
                    force_list_output = TRUE, ...)
      if (!hirst2pass) {
        if ('pitch' %in% output) pitch <- rbind(pitch, dat$pitch)
        if ('epochs' %in% output) epochs <- c(epochs, dat$epochs)
        if ('gci_cand' %in% output) gci_cand <- rbind(gci_cand, dat$gci_cand)
        if ('probs' %in% output) probs <- rbind(probs, dat$probs)
        if (praat_output) {
          if (inherits(directory, 'emuDBhandle')) {
            write_praat_pitch(dat$pitch, praat_output_dir,
                              gsub('.*emuDB/', praat_output_dir,
                                   gsub('.wav', '', f)))
          } else {
            write_praat_pitch(dat$pitch, praat_output_dir,
                              gsub('.*/', '', gsub('.wav', '', f)))
          }
        }
      }
    }

    if (hirst2pass) {
      pass1 <- dat$pitch$f0
      q <- stats::quantile(pass1, probs=c(0.25, 0.75), na.rm=T, names=F)
      f0min <- 0.75*q[1]
      f0max <- 1.5*q[2]
      for (f in fls) {
        dat <- reaper(f, output = output, f0min = f0min, f0max = f0max,
                      force_list_output = TRUE, ...)
        if ('pitch' %in% output) pitch <- rbind(pitch, dat$pitch)
        if ('epochs' %in% output) epochs <- c(epochs, dat$epochs)
        if ('gci_cand' %in% output) gci_cand <- rbind(gci_cand, dat$gci_cand)
        if ('probs' %in% output) probs <- rbind(probs, dat$probs)
        if (praat_output) {
          if (inherits(directory, 'emuDBhandle')) {
            write_praat_pitch(dat$pitch, praat_output_dir,
                              gsub('.*emuDB/', praat_output_dir,
                                   gsub('.wav', '', f)))
          } else {
            write_praat_pitch(dat$pitch, praat_output_dir,
                              gsub('.*/', '', gsub('.wav', '', f)))
          }
        }
      }
    }
  }

  out <- list()

  if ('pitch' %in% output) {
    pitch <- pitch[-1,]
    if (inherits(directory, 'emuDBhandle')) pitch <- dplyr::left_join(
      pitch, bndls, by='file')
    out$pitch <- pitch
  }

  if ('epochs' %in% output) out$epochs <- epochs

  if ('gci_cand' %in% output) {
    gci_cand <- gci_cand[-1,]
    if (inherits(directory, 'emuDBhandle')) gci_cand <- dplyr::left_join(
      gci_cand, bndls, by='file')
    out$gci_cand <- gci_cand
  }

  if ('probs' %in% output) {
    probs <- probs[-1,]
    if (inherits(directory, 'emuDBhandle')) probs <- dplyr::left_join(
      probs, bndls, by='file')
    out$probs <- probs
  }

  if (length(output) == 1) out <- out[[1]]
  return(out)

}

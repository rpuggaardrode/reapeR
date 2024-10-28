#' Bulk estimate pitch and/or epochs with with REAPER
#'
#' Wrapper function to call David Talkin's REAPER software on all WAV files
#' in a directory
#'
#' @param directory String giving the name of a directory where all WAV files
#' should be processed. Alternatively the handle of a loaded EMU database.
#' @param output String or vector of strings specifying which estimates to
#' output. Possible values are `pitch` and `epochs`, default is to output
#' both.
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
#' @param ... Further arguments passed on to `reaper()`.
#'
#' @return If `output = 'pitch'`, returns a data frame with four columns:
#' * `time`, giving the frame time in seconds
#' * `voiced` Boolean specifying whether that frame is `voiced`
#' * `f0` F0 in Hz
#' * `file` String giving the name of the analyzed file.
#'
#' If `directory` refers to an EMU database, furthermore contains the columns
#' `session` and `bundle`.
#'
#' If `output = 'epochs'`, returns a numeric vector of epochs, or glottal
#' closure instants.
#'
#' If `output = c('pitch', 'epochs')`, returns a list with the above two
#' outputs.
#' @export
#'
#' @examples
#' dir <- file.path(system.file('extdata', package = 'reapeR'))
#' vals <- reaper(dir)
reaper_bulk <- function(directory, output = c('pitch', 'epochs'),
                        f0min = 40, f0max = 500, hirst2pass = FALSE, ...) {

  if (class(directory) == 'emuDBhandle') {
    bndls <- emuR::list_bundles(directory)
    wavs <- paste0(db$basePath, '/', bndls$session, '_ses/',
                   bndls$name, '_bndl/', bndls$name, '.wav')
    bndls$file <- wavs
    ses <- unique(bndls$session)
  } else {
    wavs <- paste0(directory, '/', list.files(directory, pattern='*.wav'))
    ses <- 'dummy'
  }

  if ('pitch' %in% output) pitch <-
      data.frame(time = NA, voiced = NA, f0 = NA, file = NA)
  if ('epochs' %in% output) epochs <- c()

  if (hirst2pass) {
    f0min <- 60
    f0max <- 700
  }

  for (s in ses) {
    if (class(directory) == 'emuDBhandle') {
      fls <- bndls[which(bndls$session == s),]$file
    } else {
      fls <- wavs
    }

    for (f in fls) {
      out <- reaper(f, output = output, f0min = f0min, f0max = f0max, ...)
      if (!hirst2pass) {
        if (length(output) == 2) {
          pitch <- rbind(pitch, out$pitch)
          epochs <- c(epochs, out$epochs)
        } else if (output == 'pitch') {
          pitch <- rbind(pitch, out)
        } else {
          epochs <- c(epochs, out)
        }
      }
    }

    if (hirst2pass) {
      if (length(output) == 2) {
        pass1 <- out$pitch$f0
      } else {
        pass1 <- out$f0
      }
      q <- quantile(pass1, probs=c(0.25, 0.75), na.rm=T, names=F)
      f0min <- 0.75*q[1]
      f0max <- 1.5*q[2]
      for (f in fls) {
        out <- reaper(f, output = output, f0min = f0min, f0max = f0max, ...)
        if (length(output) == 2) {
          pitch <- rbind(pitch, out$pitch)
          epochs <- c(epochs, out$epochs)
        } else if (output == 'pitch') {
          pitch <- rbind(pitch, out)
        } else {
          epochs <- c(epochs, out)
        }
      }
    }
  }

  if ('pitch' %in% output) pitch <- pitch[-1,]
  if (class(directory) == 'emuDBhandle') pitch <- dplyr::left_join(
    pitch, bndls, by='file')

  if (length(output) == 2) {
    return(list(pitch = pitch, epochs = epochs))
  } else if (output == 'pitch') {
    return(pitch)
  } else {
    return(epochs)
  }

}

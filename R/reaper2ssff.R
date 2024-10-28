#' Convert REAPER output to SSFF files and import to EMU database
#'
#'
#' @param reaper_output Data frame or list object created with the
#' `reaper_bulk()` function.
#' @param db_handle Handle of a loaded EMU database
#'
#' @return Nothing; run for side effects.
#' @export
#'
#' @examples
#' # not now
reaper2ssff <- function(reaper_output, db_handle) {

  if (!is.data.frame(reaper_output)) reaper_output <- reaper_output$pitch
  sr <- round(1 / (reaper_output[[2,'time']] - reaper_output[[1,'time']]), 0)

  dir.create(paste0(getwd(), '/ssff/'))
  sessions <- unique(reaper_output$session)

  for (s in sessions) {
    dir.create(paste0(getwd(), '/ssff/', s))
    ssff_path <- paste0(getwd(), '/ssff/', s)
    tmp_s <- reaper_output[which(reaper_output$session==s),]
    fls <- unique(tmp_s$name)

    for (f in fls) {
      tmp <- tmp_s[which(tmp_s$name==f),]
      start <- tmp$time[1]
      ado <- list()
      attr(ado, 'sampleRate') <- sr
      attr(ado, 'origFreq') <- 0
      attr(ado, 'startTime') <- start
      attr(ado, 'endRecord') <- nrow(tmp)
      class(ado) <- 'AsspDataObj'
      wrassp::AsspFileFormat(ado) <- 'SSFF'
      wrassp::AsspDataFormat(ado) <- as.integer(2)
      ado <- wrassp::addTrack(ado, 'rF0', tmp$f0, 'REAL32')
      ado <- wrassp::addTrack(ado, 'vd', tmp$voiced, format='REAL32')
      attr(ado, 'trackFormats') <- rep('REAL32', length(ado))

      new_path <- paste0(ssff_path, '/', f, '.reaper')
      wrassp::write.AsspDataObj(ado, file=new_path)
    }

    emuR::add_files(db, paste0(getwd(), '/ssff/', s), 'reaper', s)

  }

  emuR::add_ssffTrackDefinition(db, 'rF0', 'rF0', 'reaper')
  emuR::add_ssffTrackDefinition(db, 'vd', 'vd', 'reaper')
  unlink('ssff', recursive=T)

}

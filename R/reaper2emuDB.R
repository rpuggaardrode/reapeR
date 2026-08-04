#' Import REAPER output to an EMU database as SSFF files
#'
#' When pitch has been tracked over an EMU database, this function is used to
#' import the measures to the database in the Simple Signal File Format.
#'
#' @param reaper_output Data frame or list object created with the
#' `reaper_bulk()` function run over a loaded EMU database.
#' @param db_handle Handle of a loaded EMU database
#' @param fileExtension String giving the desired file extension for the new
#' SSFF files. Default is `reaper`.
#'
#' @return Nothing; run for side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' emuR::create_emuRdemoData(tempdir())
#' db <- emuR::load_emuDB(file.path(tempdir(), 'emuR_demoData', 'ae_emuDB'))
#' emuR::list_ssffTrackDefinitions(db)
#' out <- reaper_bulk(db)
#' reaper2emuDB(out, db)
#' emuR::list_ssffTrackDefinitions(db)
#' }
reaper2emuDB <- function(reaper_output, db_handle, fileExtension = 'reaper') {

  if (dir.exists(file.path(tempdir(), 'ssff'))) {
    unlink(file.path(tempdir(), 'ssff'), recursive = TRUE)
  }

  if ('reaper' %in% emuR::list_ssffTrackDefinitions(db_handle)$fileExtension) {
    stop('reaper ssff tracks already exist for the database')
  }

  if (!is.data.frame(reaper_output)) reaper_output <- reaper_output$pitch
  sr <- round(1 / (reaper_output[[2,'time']] - reaper_output[[1,'time']]), 0)

  dir.create(paste0(tempdir(), '/ssff/'))
  sessions <- unique(reaper_output$session)

  for (s in sessions) {
    dir.create(paste0(tempdir(), '/ssff/', s))
    ssff_path <- paste0(tempdir(), '/ssff/', s)
    tmp_s <- reaper_output[which(reaper_output$session==s),]
    fls <- unique(tmp_s$name)

    for (f in fls) {
      tmp <- tmp_s[which(tmp_s$name==f),]
      ado <- reaper2ssff(tmp)
      new_path <- paste0(ssff_path, '/', f, '.', fileExtension)
      wrassp::write.AsspDataObj(ado, file=new_path)
    }

    emuR::add_files(db_handle, paste0(tempdir(), '/ssff/', s), fileExtension, s)

  }

  emuR::add_ssffTrackDefinition(db_handle, 'rF0', 'rF0', fileExtension)
  emuR::add_ssffTrackDefinition(db_handle, 'vd', 'vd', fileExtension)

}

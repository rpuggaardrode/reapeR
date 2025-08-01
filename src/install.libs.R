curwd <- getwd()
on.exit(setwd(curwd))
dest <- file.path(R_PACKAGE_DIR, paste0('bin', R_ARCH))
dir.create(dest, recursive = TRUE, showWarnings = FALSE)
setwd(dest)
system('git clone https://github.com/rpuggaardrode/rpr.git')
setwd('rpr')
dir.create('build')
setwd('build')
system('cmake ..')
system('make')
exec <- 'reaper'
if (WINDOWS) exec <- paste0(exec, '.exe')
file.rename(exec, paste0(dest, '/dummy'))
setwd(dest)
unlink('rpr', recursive = TRUE, force = TRUE)
file.rename('dummy', exec)
setwd(curwd)

# .onLoad <- function(libname, pkgname) {
#   exePath <- system.file(package='reapeR')
#   if (!file.exists(file.path(exePath, 'bin/reaper.exe'))) {
#     curwd <- getwd()
#     on.exit(setwd(curwd))
#     setwd(exePath)
#     if (!dir.exists('bin')) dir.create('bin')
#     setwd('bin')
#     system('git clone https://github.com/google/REAPER.git')
#     setwd('REAPER')
#     dir.create('build')
#     setwd('build')
#     system('cmake ..')
#     system('make')
#     file.rename('reaper.exe', paste0(exePath, '/bin/reaper.exe'))
#     setwd('..')
#     unlink('REAPER', recursive = TRUE, force = TRUE)
#     setwd(curwd)
#   }
# }

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release which wraps the existing C++ library REAPER
(https://github.com/google/REAPER). The original source code is shipped in `src/` 
without modifications in three subdirectories. `R CMD CHECK` shoots a warning
related to the source code which I think is innocuous, so I've opted to leave
the code as is.

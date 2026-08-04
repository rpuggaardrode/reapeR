## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release which wraps the existing C++ library REAPER
(https://github.com/google/REAPER). The original source code is shipped in 
`src/`. I've left out source code relating only to file IO, and made some
minimal patches to the source. This pertains to writing diagnostic messages
and warnings with `REprintf()` and `Rprintf()` instead of using `stdout` and
`stderr`. I have also made patches to capture various internally used measures
in R instead of writing them to disk. There will not be a major maintenance 
burden associated with these patches, as the REAPER library is not being
actively developed.
* The Windows pre-check reports a NOTE during "checking compiled code" due to cc 
not being found on PATH. The package builds successfully though and I cannot
reproduce this note locally.

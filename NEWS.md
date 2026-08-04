## Version 0.2.0
* Removed the subdirectories `wave` and `core` from the REAPER build, as they're
only used for file IO.
* Made REAPER patches to ensure that all diagnostic messages are CRAN-compliant,
and added a `GetDiagnostics()` procedure to capture various internally used 
measures with `reaper_wrap()` instead of writing them to disk.
* Added several `output` options to `reaper()` and `reaper_bulk()`: `gci_cand`
for information about glottal closure instants candidates, `probs` for
various pseudo-probabilities relating to voicing, and `resids` for residual 
waveforms for debugging.
* Removed the `delete` parameters from `read_epochs_out()` and 
`read_pitch_out()`. 
* Refactored `reaper_bulk()` to allow for new outputs smoothly. Also added
parameter `force_list_output` to `reaper()` as part of this.
* Added `hirst2pass_f0min` and `hirst2pass_f0max` arguments to `reaper_bulk()`
for user controlling the pitch floor and ceiling values for first pass.
* Added `fileExtension` parameter to `reaper2emuDB()` for selecting
the extension of SSFF files.
* Various other minor fixes.

## Version 0.1.1
* Fixed time domain bug when estimating epochs with `start` times other than `0`.
* Made minimal REAPER patches to fix CRAN check warnings, as well as a fix to
the `StdoutSilencer` in the C++ wrapper.
* Suppress diagnostic messages from REAPER by default.

## Version 0.1.0
* Previous versions of the library attempted to install the `reaper` command
line tool with CMake, and functions would call REAPER from the command line
and read in output files. This version wraps the REAPER C++ source code 
directly with `Rcpp`, which should be much less fragile. The behavior is 
otherwise very similar, with a few exceptions and breaking changes, including
some improved checks. There's also some added documentation.
* `reaper()` functions largely the same as before, with the addition of the
arguments `start` and `end` for controlling which parts of a sound file to 
analyze, and the `channel` argument for separating out a channel to analyze. 
This isn't possible with the command line tool. The `exePath` argument has
been removed, since the function no longer calls an executable. It is now
possible to pass files that aren't 16-bit (they're converted to 16-bit under
the hood).
* `reaper2ssff()` has been split into two functions: `reaper2ssff()`, which
now *only* converts single file outputs to the SSFF format and stores it in R
memory, and `reaper2emuDB()` for bulk importing to an EMU database. This allows
users to retain the SSFF objects if they e.g. need it for plotting with 
`praatpicture`. A bug was spotted and fixed in the SSFF conversion process.
With `reaper2emuDB()`, SSFF files are now temporarily stored in a temporary 
directory instead of the working directory. 
* The low level `reaper_wrap()` function has been added which just calls a
C++ wrapper function and returns a list that isn't in the most useful format.
A possible advantage of this function is that it operates directly on a numeric
vector of signal values and doesn't require a sound file.
* The `reaper_install()` function has been removed.

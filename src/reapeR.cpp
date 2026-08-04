//' Wrap REAPER C++ library
//'
//' Low-level wrapper for calling REAPER's `epoch_tracker` procedure
//'
//' @param samples Numeric vector giving audio samples in 16-bit integer format.
//' @param sample_rate Numeric giving the sampling rate of `samples`.
//' @param f0min Numeric value specifying pitch floor. Default is `40`.
//' @param f0max Numeric value specifying pitch ceiling. Default is `500`.
//' @param suppress_highpass_filter Boolean; should highpass filter be
//' suppressed? Default is `FALSE`.
//' @param hilbert Boolean; should Hilbert transform be applied prior to
//' analysis? Default is `FALSE`.
//' @param interval Numeric value giving the F0 output interval in seconds.
//' Default is `0.005`.
//' @param unvoiced_cost Numeric; cost for unvoiced segments. Default is `0.9`,
//' set higher value to estimate more F0 in noise.
//' @param unvoiced_pulse_interval Numeric; what should be the interval in
//' seconds of epoch pulses outside of voiced intervals? Default is `0.01`.
//' @param verbose Boolean; should messages be printed to the console?
//'
//' @returns A list object with five elements: `epochs` gives the location
//' of (voiced and unvoiced) epochs; `voicing` (same length as epochs) gives
//' information about whether epochs are voiced; `f0` gives estimated pitch;
//' `correlation` gives correlation between pitch estimates; `f0_interval`
//' returns the value of `interval`.
//' @export
//'
//' @examples
//' file <- file.path(system.file('extdata', package = 'reapeR'), '1.wav')
//' snd <- tuneR::readWave(file)
//' results <- reaper_wrap(snd@left, snd@samp.rate)
//'

#include <Rcpp.h>
#include "epoch_tracker/epoch_tracker.h"

using namespace Rcpp;

#include <cstdio>

#ifdef _WIN32
#include <io.h>
#else
#include <unistd.h>
#endif

bool r_verbose = false;

// [[Rcpp::export]]
List reaper_wrap(IntegerVector samples,
                 double sample_rate,
                 double f0min = 40.0,
                 double f0max = 500.0,
                 bool suppress_highpass_filter = false,
                 bool hilbert = false,
                 double interval = 0.005,
                 double unvoiced_cost = 0.9,
                 double unvoiced_pulse_interval = 0.01,
                 bool verbose = false) {

  r_verbose = verbose;

  EpochTracker tracker;

  std::vector<int16_t> input(samples.size());

  for (R_xlen_t i = 0; i < samples.size(); i++) {
    input[i] = static_cast<int16_t>(samples[i]);
  }

  // Initialise with input waveform
  bool ok = tracker.Init(
    input.data(),
    input.size(),
    sample_rate,
    f0min,
    f0max,
    !suppress_highpass_filter,
    hilbert
  );

  if (!ok) {
    stop("EpochTracker::Init failed");
  }

  // Configure parameters
  tracker.set_min_f0_search(f0min);
  tracker.set_max_f0_search(f0max);
  tracker.set_do_highpass(!suppress_highpass_filter);
  tracker.set_do_hilbert_transform(hilbert);
  tracker.set_unvoiced_cost(unvoiced_cost);
  tracker.set_unvoiced_pulse_interval(unvoiced_pulse_interval);

  // Feature extraction
  if (!tracker.ComputeFeatures()) {
    stop("EpochTracker::ComputeFeatures failed");
  }

  // Build lattice and track
  tracker.CreatePeriodLattice();

  if (!tracker.TrackEpochs()) {
    stop("EpochTracker::TrackEpochs failed");
  }

  // Epoch locations and voicing decisions
  std::vector<float> epoch_times;
  std::vector<int16_t> voicing;

  tracker.GetFilledEpochs(
    unvoiced_pulse_interval,
    &epoch_times,
    &voicing
  );

  // F0 and correlation output
  std::vector<float> f0;
  std::vector<float> correlations;

  if (!tracker.ResampleAndReturnResults(
      interval,
      &f0,
      &correlations)) {
      stop("EpochTracker::ResampleAndReturnResults failed");
  }

  Diagnostics d = tracker.GetDiagnostics();

  return List::create(
    _["epochs"] = epoch_times,
    _["voicing"] = voicing,
    _["f0"] = f0,
    _["correlation"] = correlations,
    _["f0_interval"] = interval,
    _["signal"] = d.signal,
    _["residual"] = d.residual,
    _["norm_residual"] = d.norm_residual,
    _["bandpassed_rms"] = d.bandpassed_rms,
    _["voice_onset_prob"] = d.voice_onset_prob,
    _["voice_offset_prob"] = d.voice_offset_prob,
    _["peaks_debug"] = d.peaks_debug,
    _["prob_voiced"] = d.prob_voiced,
    _["best_corr"] = d.best_corr,
    _["f0_time"] = d.f0_time,
    _["f0_diag"] = d.f0_diag,
    _["nccf"] = d.nccf,
    _["voiced_diag"] = d.voiced_diag
  );
}

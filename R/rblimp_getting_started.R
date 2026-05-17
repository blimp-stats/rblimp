#' Getting Started with rblimp
#'
#' @description
#' A guide to installing and configuring Blimp software for use with rblimp.
#'
#' @section Installing Blimp Software:
#'
#' rblimp requires the Blimp engine. The simplest way to install it is from R:
#'
#' ```r
#' install_blimp()
#' ```
#'
#' This downloads the latest Blimp engine into a user-writable directory:
#'
#' - macOS: `~/.blimp/`
#' - Windows: `%LOCALAPPDATA%/Blimp/`
#' - Linux: `~/.blimp/`
#'
#' Override the location with the `R_BLIMP_HOME` environment variable.
#' Use [`uninstall_blimp`] to remove it.
#'
#' If you'd rather use Blimp's standalone system installer (downloaded from
#' <https://www.appliedmissingdata.com/blimp>), rblimp will auto-detect it and
#' use it instead.
#'
#' If autodetection fails, set the path manually with [`set_blimp`] or the
#' `R_BLIMP` environment variable.
#'
#' **Privacy:** Downloads are recorded for usage statistics.
#'
#' See privacy policy: <https://www.blimpstats.com/privacy>.
#'
#' @section Automatic Update Checks:
#'
#' By default, rblimp automatically checks for Blimp updates when you run models.
#' This ensures you're using the latest version with bug fixes and improvements.
#'
#' To disable automatic update checks:
#'
#' ```r
#' # Disable for current session
#' options(check_blimp_update = FALSE)
#'
#' # Or add to .Rprofile for permanent setting:
#' # options(check_blimp_update = FALSE)
#'
#' # Or set environment variable (useful for CI / sysadmin contexts):
#' # R_BLIMP_NO_UPDATE_CHECK=true
#' ```
#'
#' You can manually update Blimp at any time:
#'
#' ```r
#' update_blimp()
#' ```
#'
#' For managed installs, this re-downloads the latest Blimp engine.
#' For system installs, this launches the Blimp Updater application.
#'
#' @section Function Reference:
#'
#' **Model Fitting:**
#'
#' - [`rblimp`] - Fit Bayesian models
#' - [`rblimp_fcs`] - Fully Conditional Specification imputation
#' - [`rblimp_syntax`] - Generate Blimp syntax
#' - [`rblimp_source`] - Run existing Blimp syntax files
#'
#' **Simulation:**
#'
#' - [`rblimp_sim`] - Generate simulated datasets
#' - [`SIMULATE`] - Create simulation specifications
#'
#' **Output & Analysis:**
#'
#' - [summary][summary,blimp_obj-method] - Model summary
#' - [`estimates`] - Extract parameter estimates
#' - [`describe`] - Descriptive statistics
#' - [`psr`] - Potential Scale Reduction values
#' - [`output`] - Raw Blimp output
#' - [`standardized`] - Extract standardized parameters only
#' - [`compare`] - Compare models
#'
#' **Visualization:**
#'
#' - [`trace_plot`] - MCMC trace plots
#' - [`posterior_plot`] - Posterior density plots
#' - [`residual_plot`] - Residual diagnostics
#' - [`jn_plot`] - Johnson-Neyman plots
#' - [`simple_plot`] - Simple slopes plots
#' - [`model_table`] - Publication tables
#'
#' **Utilities:**
#'
#' - [`by_group`] - Grouped analysis
#' - [`as.mitml`] - Convert to mitml format
#' - [names][names,blimp_obj-method] - Obtain imputation variable names
#' - [with][with,blimp_obj-method] - Evaluate across imputations
#' - [`write.blimp`] - Write results to files
#'
#' **Setup & Configuration:**
#'
#' - [`install_blimp`] - Install Blimp into a managed directory
#' - [`uninstall_blimp`] - Remove the managed install
#' - [`detect_blimp`] - Detect Blimp installation
#' - [`set_blimp`] - Set path to Blimp executable
#' - [`has_blimp`] - Check if Blimp is available
#' - [`update_blimp`] - Update Blimp installation
#'
#' @section Additional Resources:
#'
#' - GitHub repository: <https://github.com/blimp-stats/rblimp>
#' - Example files: <https://github.com/blimp-stats/rblimp-examples>
#' - Blimp User Guide: <https://docs.google.com/document/d/1D3MS79CakuX9mVVvGH13B5nRd9XLttp69oGsvrIRK64>
#' - Report issues: <https://github.com/blimp-stats/rblimp/issues>
#'
#' @name rblimp_getting_started
#' @keywords documentation
NULL

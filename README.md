# rblimp

R interface to Blimp for Bayesian latent variable modeling, missing data analysis, and multiple imputation.

[![R-CMD-check](https://github.com/blimp-stats/rblimp/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/blimp-stats/rblimp/actions/workflows/check-standard.yaml) [![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/rblimp)](https://cran.r-project.org/package=rblimp) [![](https://cranlogs.r-pkg.org/badges/rblimp)](https://cran.r-project.org/package=rblimp)


## Overview

`rblimp` provides a seamless interface to integrate [Blimp software](https://www.appliedmissingdata.com/blimp) into R workflows. Blimp offers general-purpose Bayesian estimation for a wide range of single-level and multilevel structural equation models with two or three levels, with or without missing data.

### Key Features

- **Flexible Model Specification**: Use R formula syntax or raw Blimp syntax
- **Multiple Imputation**: Built-in support for Fully Conditional Specification (FCS)
- **Convergence Diagnostics**: PSR values, trace plots, and posterior distributions
- **Visualization Tools**: Residual plots, simple slopes, Johnson-Neyman regions
- **Integration**: Convert results to `mitml` format for pooling analyses
- **Data Simulation**: Generate simulated datasets for power analysis and methods research
- **Special Variable Types**: Support for binary, ordinal, nominal, and count outcomes
- **Interactive Effects**: Estimation routines for interactions and polynomial effects

## Installation

### Step 1: Install rblimp

Install from CRAN:

```r
install.packages("rblimp")
```

Or install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("blimp-stats/rblimp")
```

### Step 2: Install Blimp Software

`rblimp` requires the Blimp engine. The simplest path is to let rblimp install it for you:

```r
library(rblimp)
install_blimp()
```

This downloads the latest Blimp engine into a user-writable directory:

- macOS: `~/.blimp/`
- Windows: `%LOCALAPPDATA%/Blimp/`
- Linux: `~/.blimp/`

Override the location with the `R_BLIMP_HOME` environment variable. Remove with `uninstall_blimp()`.

If you'd rather use the system installer, visit <https://www.appliedmissingdata.com/blimp> and follow the install instructions there.

### Step 3 (optional): Configure Blimp manually

If you've installed Blimp to a non-standard location:

```r
# Automatic detection (also offered the first time you run a model)
detect_blimp()

# Or set manually
set_blimp("/path/to/blimp")

# Verify
has_blimp()
```

### Privacy

Downloads are recorded for usage statistics. See privacy policy: <https://www.blimpstats.com/privacy>

## Getting Started

View the getting started guide:

```r
?rblimp_getting_started
```

Explore function documentation:

```r
?rblimp          # Fit Bayesian models
?rblimp_fcs      # Multiple imputation
?rblimp_sim      # Data simulation
help(package = "rblimp")
```

## Quick Example

```r
library(rblimp)

# Generate data with latent factor
mydata <- rblimp_sim(
  c(
    'f ~ normal(0, 1)',
    'x1:x5 ~ normal(f, 1)',
    'y ~ normal(10 + 0.3*f, 1 - .3^2)'
  ),
  n = 500,
  seed = 19723,
  variables = c('y', 'x1:x5')
)

# Fit SEM model
model <- rblimp(
  list(
    structure = 'y ~ f',
    measurement = 'f -> x1:x5'
  ),
  mydata,
  seed = 3927,
  latent = ~ f
)

# View results
summary(model)

# Check convergence
trace_plot(model)
```

## Resources

- **Examples**: <https://github.com/blimp-stats/rblimp-examples>
- **Blimp User Guide**: <https://docs.google.com/document/d/1D3MS79CakuX9mVVvGH13B5nRd9XLttp69oGsvrIRK64>
- **Issue Tracker**: <https://github.com/blimp-stats/rblimp/issues>

## Citation

If you use `rblimp` in your research, please cite both the package and Blimp software. Use `citation("rblimp")` for citation information.

## License

GPL-3

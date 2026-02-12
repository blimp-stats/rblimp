## R CMD check results

0 errors | 0 warnings | 0 note

## Resubmission

This is a resubmission. The following changes were made in response to
feedback from the CRAN review:

* Quoted software names in the DESCRIPTION Title and Description fields
  using single quotes (e.g., 'Blimp') as requested.

* Added `\value` tags to all exported method documentation (.Rd files)
  that were missing them, as requested. Each entry describes the structure
  and meaning of the return value, or notes that the function is called
  for side effects. The following files were updated:
  - `as.character.blimp_syntax.Rd`
  - `as.data.frame-blimp_obj-method.Rd`
  - `as.matrix-blimp_obj-method.Rd`
  - `as.mitml.Rd`
  - `names-blimp_obj-method.Rd`
  - `predict-blimp_obj-method.Rd`
  - `print.blimp_syntax.Rd`
  - `resid-blimp_obj-method.Rd`
  - `residuals-blimp_obj-method.Rd`
  - `show-blimp_cp-method.Rd`
  - `show-blimp_obj-method.Rd`
  - `with-blimp_bygroup-method.Rd`
  - `with-blimp_obj-method.Rd`
  - `write.blimp.Rd`

## Initial Submission

* This is a new release.
* This package provides an R interface to a third party software ('Blimp').
* The design is similar to the MplusAutomation packages already on CRAN.

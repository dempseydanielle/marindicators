## Test environments
* checked on all recommended platforms using rhub::check_for_cran()
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results (from R-hub)
There were 0 errors, 0 warnings, and 2 notes returned from the R-hub R CMD check.
The notes are both related to possibly mis-spelled words in the DESCRIPTION. We have verified that these words are spelled correctly.

This is a new submission.

## R CMD check results
> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Danielle Dempsey <Danielle.Dempsey@dfo-mpo.gc.ca>'
  New submission
  
  
  Possibly mis-spelled words in DESCRIPTION:
    Bundy (11:478)
    marindicators (11:18)
  
  Unknown, possibly mis-spelled, fields in DESCRIPTION:
    'Authors'

> On fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ...NB: need Internet access to use CRAN incoming checks
   NOTE
  Maintainer: ‘Danielle Dempsey <Danielle.Dempsey@dfo-mpo.gc.ca>’
  
  Possibly mis-spelled words in DESCRIPTION:
    Bundy (11:478)
    marindicators (11:18)
  
  Unknown, possibly mis-spelled, fields in DESCRIPTION:
    ‘Authors’

0 errors √ | 0 warnings √ | 2 notes x
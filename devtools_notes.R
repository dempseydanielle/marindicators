# July 3, 2019
# devtools notes

# Load the package!
devtools::load_all()
# To find a file in inst/ from code use system.file().
# For example, to find inst/extdata/mydata.csv, you’d call
# system.file("extdata", "mydata.csv", package = "mypackage").
# Note that you omit the inst/ directory from the path.

# Compiles roxygen2 comments into .Rd documentation
devtools::document()

# Automatically checks your code for common problems
# ERRORs: Severe problems that you should fix regardless
# of whether or not you’re submitting to CRAN.
# WARNINGs: Likely problems that you must fix if you’re planning to submit to CRAN
# (and a good idea to look into even if you’re not
# NOTEs: Mild problems If you’re not submitting to CRAN, carefully read each NOTE,
# but don’t go out of your way to fix things that you don’t think are problems.
devtools::check()

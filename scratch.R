### NOTES

## On some systems, RStudio cannot find RTools while building a package even
## when it is installed - use the following to correct this error.
# Set path of Rtools
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "C:/Rtools/bin/",
                        "C:/Rtools/mingw_64/bin", sep = ";")) #for 64 bit version
Sys.setenv(BINPREF = "C:/Rtools/mingw_64/bin")
library(devtools)

#Manually "force" version to be accepted
assignInNamespace("version_info", c(devtools:::version_info, list("3.5" = list(version_min = "3.3.0", version_max = "99.99.99", path = "bin"))), "devtools")
find_rtools() # is TRUE now


## The following line creates a PDF of all function documentation for the package
## Requires RTools and LaTex - current working directory must be dir package is

system("R CMD Rd2pdf . --title=loadcell --output=./loadcell.pdf --force --no-clean --internals")


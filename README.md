# gaiah

[![Travis-CI Build Status](https://travis-ci.org/eriqande/gaiah.svg?branch=master)](https://travis-ci.org/eriqande/gaiah)

This is an R-package for performing **G**enetic **A**nd **I**sotopic **A**ssignment using **H**abitat features.

This is currently under development, a collaboration between Eric C. Anderson, Kristen Ruegg, Kristina Paxton, and Ryan Harrigan.
Most of the package has been written by Eric C. Anderson.  The methodology for assigning individuals using stable isotope was first written by Hannah Vander Zanden and colleagues.  It was reimplemented here and is now efficient enough to do leave one out cross validation, etc.  These parts of the code are in the file `R/vander-zanden-appendix.R`, which, for completeness, also includes the original functions obtained from Vander Zanden (though they are not exported in the package).  

## Preliminaries

This package makes use of functions from the `rgeos` package which is an R-package that 
relies on the Interface to Geometry Engine - Open Source (GEOS).  It appears that the
binary versions of `rgeos` on CRAN now include the binaries for GEOS on Mac Intel and on
Windows systems.  

If you have to install `rgeos` from source, or are on a *nix system, then you can find
information on getting and installing GEOS [here](https://trac.osgeo.org/geos/).

## Installing

To install this from GitHub with the `devtools` package, you can:
```r
devtools::install_github("eriqande/gaiah")
```


## Reproducing the results from Ruegg et al. "Identifying migrant origins using genetics, isotopes and habitat suitability"

If you want to reproduce all the results from the paper, you will have to not only install the package using devtools, but you 
will need to clone the whole repository to get the scripts we used.  Namely:
```sh
git clone https://github.com/eriqande/gaiah
```
Then build the package if you want (no need to if you installed with devtools) and then, to reproduce all our results,
run the following scripts in order:

- `R-main/01-wiwa-analysis.R`  (Does all the computations)
- `R-main/02-move-figures.R`    (Moves figures to the latex directory if you have it)
- `R-main/03-compile-supp-1.R`  (Compiles the supplement maps.)

Note that the steps 2 and 3 only make sense if you have LaTeX installed on your system.

## Note:
The directories `development` and `extra-stuff` are not intended to be part of the final
R-package and will not be installed if you install this from GitHub using `devtools`.


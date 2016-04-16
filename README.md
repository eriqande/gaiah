# gaiah

This is an R-package for performing **G**enetic **A**nd **I**sotopic **A**ssignment using **H**abitat features.

This is currently under development, a collaboration between Eric C. Anderson, Kristen Ruegg, Kristina Paxton, and Ryan Harrigan.

## Preliminaries

This package makes use of functions from the `rgeos` package which is an R-package that 
relies on the Interface to Geometry Engine - Open Source (GEOS).  It appears that if the
binary versions of `rgeos` on CRAN include the binaries for GEOS on Mac Intel and on
Windows systems.  

If you have to install `rgeos` from source, or are on a *nix system, then you can find
information on getting and installing GEOS [here](https://trac.osgeo.org/geos/).


## Development Material

The directories `development` and `extra-stuff` are not intended to be part of the final
R-package and will not be installed if you install this from GitHub using `devtools` (see below).
So, if you want access to those (i.e. Kristina or Ryan...) you should either fork or clone the
repo

## Installing

To install this from GitHub with the `devtools` package, you can:
```r
devtools::install_github("eriqande/gaiah")
```

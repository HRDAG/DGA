## Test environments
* MacOS 11.1, local R installation, R 4.0.3
* Pop!_OS 20.04, local R installation, R 3.6.3
* Debian Linux, R-devel, GCC ASAN/UBSAN (on R-hub)
* Fedora Linux, R-devel, clang, gfortran (on R-hub)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (on R-hub)
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (on R-hub)


## R CMD check results

There were no ERRORs, no WARNINGs, and two NOTEs:

*NOTE
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Olivier Binette <olivier.binette@gmail.com>'
  Days since last update: 0

* NOTE
  unable to verify current time
  
## Explanation

* Attempting to fix an installation error with CRAN checks on r-patched-solaris-x86

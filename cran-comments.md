# worcs 0.1.9

* Update templates from rticles and prereg packages
* Add check_worcs_installation() function to determine whether all non-R-package
  dependencies are installed correctly
* Update references to WORCS paper
* Replace GitHub version of papaja with CRAN version
* Fix URLs
* Update rticles and prereg templates
* Handle literal function names from rticles and prereg, such that users can use
  all available options from those packages without worcs explicitly referencing
  those functions
* Add arguments 'save_expression' and 'load_expression' to closed_data() and
  open_data(). This allows users to save data in different formats. 
  The 'load_expression' is stored in the .worcs file, and referenced by
  load_data().

## Test environments

* local Windows 10 install, R 4.1.2
* win-builder: release
* win-builder: development
* win-builder: oldversion
* rhub check: Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* rhub check: Ubuntu Linux 16.04 LTS, R-release, GCC
* rhub check: Fedora Linux, R-devel, clang, gfortran

## R CMD check results

0 errors | 0 warnings | 1 note

* New maintainer
    + I changed jobs and updated my email address accordingly

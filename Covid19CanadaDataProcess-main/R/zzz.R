.onLoad <- function(libname, pkgname)
{
  # prevent R CMD check from complaining about "."
  utils::globalVariables(".")
}

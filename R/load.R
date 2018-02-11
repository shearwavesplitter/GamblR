.onLoad <- function(libname, pkgname) {
  op <- options()
  op.GamblR <- list(GamblR.odds = "decimal")
  toset <- !(names(op.GamblR) %in% names(op))
  if(any(toset)) options(op.GamblR[toset])
  invisible()
}
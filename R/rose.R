#' @title S Criterion
#' @description Similar to the Kelly Criterion but for known standard deviation
#' @param b Vector of event odds
#' @param p Vector of event probabilities
#' @param sd Vector of event probability standard deviations
#' @param odds Format of input odds (e.g. decimal, see Convert odds)
#' @param fraction Bet fraction (for fractional Kelly)
#' @return A list contaning the S optimal bet fraction, the Kelly optimal bet fraction, and the ratio of S to Kelly (for use with Simultaneous Kelly).
#' @export
rose <- function(b,p,sd,odds="decimal"){
	b <- convertodds(b,from=odds,to="decimal")
	b <- b-1
	s <- (((b+1)*p-1)^3)/(b*(((b+1)*p-1)^2+(b+1)^2*sd^2))
	k <- ((b+1)*p-1)/b

	r <- list()
	r$s <- as.numeric(s)
	r$k <- as.numeric(k)
	r$m <- as.numeric(s/k)

return(r)
}
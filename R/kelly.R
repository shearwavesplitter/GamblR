#' @title Kelly Criterion
#' @description Kelly Criterion calculation
#' @param p A vector of event probabilites
#' @param odds A vector of event odds
#' @param oddstype Format of input odds (e.g. decimal, see Convert odds)
#' @param fraction Bet fraction (for fractional Kelly)
#' @return A vector of bankroll fractions (for each individual bet)
#' @export
kelly <- function(p,odds,oddstype=default.GamblR.odds(),fraction=1){
	odds <- convertodds(odds,from=oddstype,to="decimal",round=FALSE)
	odds <- odds-1
	
	f <- (p*(odds+1)-1)/odds
	f <- f*fraction
return(f)
}
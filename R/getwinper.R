#' @title Determine win percentages
#' @description Calculates the probablities each time will win along with the book's margin
#' @param odds A vector of odds for a single event (e.g. win/loss, win/loss/tie)
#' @param oddstype Format of input odds (e.g. decimal, see Convert odds)
#' @return A list containing the implied probability for each event along with the book's margin
#' @export
winper <- function(odds,oddstype=default.GamblR.odds()){
	ip <- convertodds(odds,from=oddstype,to="improb",round=FALSE)
	total <- sum(ip)
	if(total < 1){stop("Outcome(s) missing: Total probability is less than 1")}
	margin <- (total-1)/length(ip)
	res <- list()
	res$percentages <- ip-margin
	res$margin <- margin
return(res)
}
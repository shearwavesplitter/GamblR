#' @title Reverse Kelly Criterion
#' @description Reverse Kelly Criterion calculation
#' @param oldodds Original odds 
#' @param newodds New odds
#' @param betpercent Bet percentage "tipped" for the original odds
#' @param oddstype1 Format of original odds (e.g. decimal, see Convert odds)
#' @param oddstype2 Format of new odds (e.g. decimal, see Convert odds)
#' @return A list containing the Kelly optimal bet for the new odds, the implied probability of the 'tip', the old implied probability, the old edge, the new implied probability, and the new edge. 
#' @details Calculates the implied propability from a "tip" assuming it is Kelly optimal. Useful for tailing a tip when the odds at your book are different. (e.g. If 5\% at $1.75 is Kelly optimal then 2\% at $1.70 may also be so.
#' @export
reversekelly <- function(oldodds,newodds,betpercent,oddstype1=default.GamblR.odds(),oddstype2=default.GamblR.odds()){
	betpercent <- betpercent/100
	oldimprob <- convertodds(oldodds,from=oddstype1,to="improb",round=FALSE)
	newimprob <- convertodds(newodds,from=oddstype2,to="improb",round=FALSE)

	fn <- function (x) kelly(x,oldodds,oddstype=oddstype1)-betpercent
	improb <- uniroot(fn,c(0,1))$root
	print(paste0("Implied edge of ",round((improb-oldimprob)*100,2),"%"))
	nk <- kelly(improb,newodds,oddstype=oddstype2)
		print(paste0("New edge of ",round((improb-newimprob)*100,2),"%"))
		print(paste0("Bet ",round(nk*100,2),"% on the new odds"))

	ret <- list()
	ret$betfraction <- nk
	ret$improb <- improb
	ret$oldimprob <- oldimprob
	ret$oldedge <- improb-oldimprob
	ret$newimprob <- newimprob
	ret$newedge <- improb-newimprob
return(ret)

}
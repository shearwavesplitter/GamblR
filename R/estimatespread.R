#' @title Estimate spread
#' @description Estimate the effective odds for a different points spread
#' @param mlodds Moneyline odds (vector of length two)
#' @param spreadodds Odds for the original spread (vector of length two)
#' @param oldspread The original points spread
#' @param newspread The 'new' spread to convert to
#' @param oddstype1 Format of original odds (e.g. decimal, see Convert odds)
#' @param oddstype2 Format of output odds (e.g. decimal, see Convert odds)
#' @return A list containing containing the new effective odds and new points spread. 
#' @details Extrapolates what the effective odds for a new point spread would be based on the original spread and the money line odds. Useful for deciding if a tip is worth tailing when your book has a different spread. 
#' @export
espread <- function(mlodds,spreadodds,oldspread=c(-3,3),newspread=c(-3.5,3.5),oddstype1=default.GamblR.odds(),oddstype2=default.GamblR.odds()){
	ml <- winper(mlodds,oddstype=oddstype1)
	spread <- winper(spreadodds, oddstype=oddstype1)
	
	dif <- ml$percentages-spread$percentages
		
	pp <- dif/oldspread
	dif2 <- pp*newspread

	spreadper <- (ml$percentages-dif2)+spread$margin
	newodds <- convertodds(spreadper,from="improb",to=oddstype2)

	print(paste0("With a new spread of ",newspread," the effective odds are ",newodds))
	
	res <- list()
	res$odds <- newodds
	res$spread <- newspread
	
	return(res)

}
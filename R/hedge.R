#' @title Hedge Calculator
#' @description Determine the best hedge bet to make
#' @param initialbet Initial bet amount
#' @param initialodds Odds that initial bet was made at
#' @param hedgeodds Odds of the prospective hedge (this may be able to take any length of input?)
#' @param oddstype Format of input odds (e.g. decimal, see Convert odds)
#' @return A list containing the hedge amount, the amount won if the initial bet wins, and the amount won if the hedge bet wins
#' @export
hedge <- function(initialbet,initialodds,hedgeodds,oddstype=default.GamblR.odds()){
		require(rootSolve)
		initialodds <- convertodds(initialodds,from=oddstype,to="decimal",round=FALSE)
		hedgeodds <- convertodds(hedgeodds,from=oddstype,to="decimal",round=FALSE)
		potentialwinnings <- initialbet*initialodds-initialbet
	
		line <- "max(c(0"
			for (i in 1:length(hedgeodds)){
				line <- paste0(line,",abs(x[",i,"]*(hedgeodds[",i,"]-1)-(initialbet")
				for(j in ((1:length(hedgeodds))[(1:length(hedgeodds)) != i])){
					line <- paste0(line,"+x[",j,"]")
				}
				line <-paste0(line,"))")
			}

			line <- paste0(line,",abs(potentialwinnings-(x[")
			ad <- ""
			for (i in 1:length(hedgeodds)){
				line <- paste0(line,ad,i,"]")
				ad <- "+x["
			}
		
		fline <- paste0(line,"))))")

		pev <- parse(text=fline)
		mod <- function(x) eval(pev)

			init <- rep(10,length(hedgeodds))
			ss <- optim(init,mod,control=list(fnscale=10,maxit=1000),lower=0,upper=1000000,method="L-BFGS-B")
		if(opt$convergence != 0){warning("DOES NOT CONVERGE")}
		hedgeamount <- round(ss$par,0)
		hedgetotal <- sum(hedgeamount)


	r <- list()
	r$hedge <- hedgeamount
	r$initialwin <- potentialwinnings-hedgetotal
	r$hedgewin <- round((hedgeamount*hedgeodds-hedgetotal)-initialbet,2)
return(r)
}
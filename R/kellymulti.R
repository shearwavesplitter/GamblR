#' @title Kelly-multi
#' @description The workhorse for kellymulti2
#' @param pmatrix A matrix of event probabilities
#' @param oddsmatrix A matrix of event odds
#' @param oddstype Format of input odds (e.g. decimal, see Convert odds)
#' @param fraction Bet fraction (for fractional Kelly)
#' @return A matrix containing bet fractions for each event
#' @export
kellymulti <- function(pmatrix,oddsmatrix,oddstype=default.GamblR.odds(),fraction=1){
	ipmatrix <- oddsmatrix
	n <- nrow(oddsmatrix)
	r <- ncol(oddsmatrix)
	require(gtools)
	for (i in 1:n){
		ipmatrix[i,] <- convertodds(ipmatrix[i,],from=oddstype,to="improb",round=FALSE)
		oddsmatrix[i,] <- convertodds(oddsmatrix[i,],from=oddstype,to="decimal",round=FALSE)
	}

	difmatrix <- pmatrix - ipmatrix

	option <- rep(NA, r)
	optiongame <- rep(NA, r)
	for (i in 1:r){
		wh <- which(difmatrix[,i] == max(difmatrix[,i]))
		if(max(difmatrix[,i]) < 0){wh <- NA}
		option[i] <- wh
		optiongame[i] <- i
	}
	odds <- as.numeric()
	p <- as.numeric()
	for(i in 1:length(option)){
		if(!is.na(option[i])){
			odd <- oddsmatrix[option[i],optiongame[i]]
			per <- pmatrix[option[i],optiongame[i]]
			odds <- c(odds,odd)
			p <- c(p,per)
		}
	}
	odds <- odds-1
	require(gtools)
	fir <- c(rep(TRUE,length(p)),rep(FALSE,length(p)))
	perm <- permutations(length(fir),length(fir)/2,v=fir,set=FALSE)

	perm <- unique(perm)

	perm <- as.data.frame(perm)

	
	poutcome <- rep(NA,length(perm$V1))

	for(i in 1:length(perm$V1)){
			res <- perm[i,]
			ctab <- p
			if(length(p) == 1){if(!res[1]){ctab[1] <- 1-ctab[1]}}else{
			for (j in 1:length(p)){
				if(!res[1,j]){ctab[j] <- 1-ctab[j]}
			}}
		prob <- prod(ctab)
		poutcome[i] <- prob
	}
	eqn <- rep(NA,length(perm$V1))
	for(i in 1:length(perm$V1)){
		eqnf <-  paste0("log10(1")
			for (j in 1:length(p)){
				if(perm[i,j]){eqnf <- paste0(eqnf,"+",odds[j],"*x[",j,"]")}else{eqnf <- paste0(eqnf,"-x[",j,"]")}
			}
		eqnf <- paste0(eqnf,")")
		eqn[i] <- eqnf
	}
		EV <- paste0(poutcome[1],"*",eqn[1])
		for(i in 2:length(perm$V1)){
			EV <- paste0(EV,"+",poutcome[i],"*",eqn[i])
		}

		
		pev <- parse(text=EV)
		mod <- function(x) eval(pev)
		init <- rep(0.01,length(p))
		if(length(p) == 1){opt <- optim(init,mod,control=list(fnscale=-10,maxit=1000),lower=0,upper=1,method="L-BFGS-B")}else{
		opt <- optim(init,mod,control=list(fnscale=-10,maxit=9999))}
	if(opt$convergence != 0){warning("DOES NOT CONVERGE")}

	it <- 0
	outmatrix <- matrix(NA,ncol=r,nrow=n)
	for(i in 1:length(option)){
		if(!is.na(option[i])){
			it <- it+1
			outmatrix[option[i],optiongame[i]] <- opt$par[it]
		}
	}
	outmatrix <- outmatrix*fraction
return(outmatrix)
}
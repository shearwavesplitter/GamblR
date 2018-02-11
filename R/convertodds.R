#' @title Convert odds
#' @description Convert between odd types
#' @param odds A vector of odds in the "from" format
#' @param from Odds to convert from
#' @param to Odds to convert to
#' @param round Round the result? (TRUE/FALSE)
#' @return A vector of odds in the "to" format
#' @details Can convert between implied probability (improb), decimal, american, fractional, hongkong, indonesian, and malay odd types
#' @export
convertodds <- function(odds,from=default.GamblR.odds(),to="decimal",round=TRUE){
	opts <- c("decimal","american","fractional","hongkong","indonesian","malay","improb")
	
	if(!(from %in% opts)){print(opts);stop("from not valid option")}
	if(!(to %in% opts)){print(opts);stop("to not valid option")}

require(MASS)
#####From	

	if(from == "improb"){ip <- odds}

	if(from == "decimal"){ip <- 1/odds}

	if(from == "fractional"){
		fodds <- as.fractions(odds)
		split <- strsplit(attr(fodds,"fracs"),"/")
		for(i in 1:length(split)){
			if(length(split[[i]]) == 1){split[[i]] <- cbind(split[[i]][1],1)}
		}
		split <- do.call(rbind,split)
		denom <- as.numeric(split[,2])
		numer <- as.numeric(split[,1])
		ip <- denom/(denom+numer)
	}

	if(from == "american"){
		odds[odds < 0] <- 10000/abs(odds[odds < 0])
		ip <- 100/(odds+100)
	}

	if(from == "indonesian"){
		odds[odds >= 0] <- odds[odds >= 0]+1
		odds[odds < 0] <- 1/abs(odds[odds < 0])+1
		ip <- 1/odds
	}

	if(from == "malay"){
		odds[odds >= 0] <- odds[odds >= 0]+1
		odds[odds < 0] <- 1+1/abs(odds[odds < 0])
		ip <- 1/odds
	}

	if(from == "hongkong"){
		ip <- 1/(odds+1)
	}

	####To

	if(to == "decimal"){
		res <- 1/ip
		if(round){res <- round(res,2)}
		}

	if(to == "improb"){
		res <- ip
		if(round){res <- round(res,2)}
		}
	
	if(to == "fractional"){
		if(round){ip <- round(ip,2)}
		res <- as.fractions(1/ip-1)
	}

	if(to == "american"){
		res <- ((100-(ip*100))/(ip*100))*100
		res[res < 100] <- (10000/res[res < 100])*-1
		if(round){res <- round(res,0)}
	}

	if(to == "malay"){
		res <- 1/ip
		res[ip >= 0.5] <- res[ip >= 0.5]-1
		res[ip < 0.5] <- (1/(res[ip < 0.5]-1))*-1
		if(round){res <- round(res,4)}
	}

	if(to == "indonesian"){
		res <- 1/ip
		res[ip >= 0.5] <- -1/(res[ip >= 0.5]-1)
		res[ip < 0.5] <- res[ip < 0.5]-1
		if(round){res <- round(res,3)}
	}

	if(to == "hongkong"){
		res <- 1/ip-1
		if(round){res <- round(res,3)}
	}

return(res)
}



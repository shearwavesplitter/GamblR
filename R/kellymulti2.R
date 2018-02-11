#' @title Kelly Criterion for multiple simultaneous events/games
#' @description Determine the bet fractions for multiple (uncorrelated) games/event occuring at the same time
#' @param probhome Probability for the home teams to win
#' @param oddhome Odds for the home teams
#' @param oddaway Odds for the away teams
#' @param probtie Probability of ties (optional)
#' @param oddtie Odds for ties (optional)
#' @param homenames Names of the home teams
#' @param awaynames Names of the away teams
#' @param oddstype Format of input odds (e.g. decimal, see Convert odds)
#' @param fraction Bet fraction (for fractional Kelly)
#' @param ou Over/under bet? (TRUE/FALSE)
#' @return A dataframe containing the matchup names along with the fractonal bet amount for the simultaneous Kelly Criterion
#' @export
kellymulti2 <- function(probhome,oddhome,oddaway,probtie=NULL,oddtie=NULL,homenames=NULL,awaynames=NULL,oddstype=default.GamblR.odds(),fraction=1,ou=FALSE){
		
		if(is.null(probtie)){
				probaway <- 1-probhome
				pmatrix <- rbind(probhome,probaway)
				oddmatrix <- rbind(oddhome,oddaway)

		}else{				
				probaway <- 1-(probhome+probtie)
				pmatrix <- rbind(probhome,probaway,probtie)
				oddmatrix <- rbind(oddhome,oddaway,oddtie)
		}			

		r <- kellymulti(pmatrix,oddmatrix,oddstype=oddstype,fraction=fraction)

	
		if(ou){
		if(is.null(probtie)){
			rownames(r) <- c("Over","Under")
		}

		}else{		
		if(is.null(probtie)){
			rownames(r) <- c("Home","Away")
		}else{
			rownames(r) <- c("Home","Away","Tie")
		}	
		}
		
		if(!is.null(homenames) & !is.null(awaynames)){
			colnames(r) <- paste0(homenames," v ",awaynames)
		}
return(r)
}
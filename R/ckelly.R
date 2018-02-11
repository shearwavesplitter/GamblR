#' @title Arbitage-like Kelly Criterion
#' @description Compare odds from multiple books with the Kelly Criterion
#' @param homeodds A vector of odds for the first team for book A
#' @param awayodds A vector of odds for the second team for book A
#' @param chomeodds A vector of odds for the first team for book B
#' @param cawayodds A vector of odds for the second team for book B
#' @param oddstype Format of input odds (e.g. decimal, see Convert odds)
#' @return A vector of bankroll fractions (for each individual bet)
#' @details Uses the odds from book A to determine the win probabilites of team A or B and then gives the Kelly optimal bet for those same teams on book B. Useful for betting when trusted book updates quickly leaving other books with favourable odds.
#' @export
ckelly <- function(homeodds,awayodds,chomeodds,cawayodds,oddstype=default.GamblR.odds()){
	homeodds <- convertodds(homeodds,from=oddstype,to="decimal",round=FALSE)
	awayodds <- convertodds(awayodds,from=oddstype,to="decimal",round=FALSE)

	chomep <- winper(cbind(chomeodds,cawayodds),oddstype=oddstype)$percentages[1]
	cawayp <- 1-chomep

	kh <- kelly(chomep,odds=homeodds,oddstype="decimal")
	kv <- kelly(cawayp,odds=awayodds,oddstype="decimal")

	kh[kh < 0] <- 0
	kv[kv < 0] <- 0

	kf <- kh-kv
return(kf)
}
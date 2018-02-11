#' @export
multipick <- function(odds,betpercent,oddstype=default.GamblR.odds()){
		rk <- odds
		sink("/dev/null")
		for(i in 1:length(odds)){
			rk[i] <- reversekelly(odds[i],odds[i],betpercent[i],oddstype1=oddstype,oddstype2=oddstype)$oldedge
		}
		sink()
		improb <- convertodds(odds,from=oddstype,to="improb",round=FALSE)
	km <- kellymulti(rbind(improb,1-improb),rbind(improb-rk,1-(improb-rk)),oddstype="improb")

return(km[1,])
}
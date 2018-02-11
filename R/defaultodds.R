default.GamblR.odds <- function () 
{
    val <- getOption("GamblR.odds")
    if (is.null(val)) 
        val <- "decimal"
    if (!(val %in% c("decimal","american","fractional","hongkong","indonesian","malay","improb"))) 
        stop("options(\"GamblR.odds\") not set correctly")
    val
}

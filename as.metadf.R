# function to prepare a dataframe of critically reviewed
# articles, such as from an abstractreview, and returns
# a dataframe to conduct inverse-variance weighted
# meta-analysis

as.metadf <- function(criticalreview) {
  metadf <- criticalreview[criticalreview$RRCI,c(1:4,6:9)]
  
  metadf$SE <- log(metadf$UB/metadf$LB)/(2*1.96)
  metadf$VAR <- (metadf$SE)^2
  metadf$WEIGHT<- 1/metadf$VAR
  
  metadf$QI <- NA
  metadf$WEIGHTRE <- NA
  
  return(metadf)
}
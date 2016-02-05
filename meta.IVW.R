# functioned designed to take in a dataframe with the 
# columns defined in as.metadf and return the inverse
# variance weighted estimate and confidence interval.
# For the DerSimonian-Laird random-effects model,
# use RandomEffects = TRUE

meta.IVW <- function(metadf,RandomEffects = FALSE) {
  meta.srr.fe.ln <- sum(log(metadf$RR)*metadf$WEIGHT)/sum(metadf$WEIGHT)
  meta.slb.fe.ln <- meta.srr.fe.ln - 1.96*sqrt(1/sum(metadf$WEIGHT))
  meta.sub.fe.ln <- meta.srr.fe.ln + 1.96*sqrt(1/sum(metadf$WEIGHT))
  
  metadf$QI <- metadf$WEIGHT*(log(metadf$RR) - meta.srr.fe.ln)^2
  Q <- sum(metadf$QI)
  DF <- length(metadf$QI) - 1
  QP <- pchisq(Q,DF,lower.tail=FALSE)
  I2 <- (Q - DF) / Q
  if (I2 < 0) {
    I2 <- 0
  }
  
  if (RandomEffects) {
    TAU2 <- (Q - DF) / (sum(metadf$WEIGHT) - sum((metadf$WEIGHT)^2)/sum(metadf$WEIGHT))
    metadf$WEIGHTRE <- 1/(metadf$VAR + TAU2)
    
    meta.srr.re.ln <- sum(log(metadf$RR)*metadf$WEIGHTRE)/sum(metadf$WEIGHTRE)
    meta.slb.re.ln <- meta.srr.re.ln - 1.96*sqrt(1/sum(metadf$WEIGHTRE))
    meta.sub.re.ln <- meta.srr.re.ln + 1.96*sqrt(1/sum(metadf$WEIGHTRE))
    
    meta.IVW.list <- list(exp(meta.srr.fe.ln),c(exp(meta.slb.fe.ln),exp(meta.sub.fe.ln)),
                          c(Q,DF,QP),I2,
                          exp(meta.srr.re.ln),c(exp(meta.slb.re.ln),exp(meta.sub.re.ln)),TAU2)
    names(meta.IVW.list) <- c("Summary Estimate (Fixed-Effects)",
                              "95% Confidence Interval (Fixed-Effects)",
                              "Q-statistic, Degrees of Freedom, P-value","I-Square",
                              "Summary Estimate (Random-Effects)",
                              "95% Confidence Interval (Random-Effects)","Tau-Square")
  }
  else {
    meta.IVW.list <- list(exp(meta.srr.fe.ln),c(exp(meta.slb.fe.ln),exp(meta.sub.fe.ln)),
                          c(Q,DF,QP),I2)
    names(meta.IVW.list) <- c("Summary Estimate (Fixed-Effects)",
                        "95% Confidence Interval (Fixed-Effects)",
                        "Q-statistic, Degrees of Freedom, P-value","I-Square")
  }
  return(meta.IVW.list)
}
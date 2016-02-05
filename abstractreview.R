# Creates UI within R to conduct abstract reviews and 
# returns a dataframe of key citation information
# and recorded values, such as whether to include
# the abstract in a critical review.
# Function abstractreview requires the R packages
# RISmed and an internet connection. The function
# takes on a single arguement: a pubmed search query


require(RISmed)

abstractreview <- function(query,db="pubmed",mindate=NULL,maxdate=NULL,retmax=1000) {
  res <- EUtilsSummary(query)
  summary(res)
  N.1 <- QueryCount(res)
  ar <- readline("Proceed with Abstract Review? (y/n) ")
  if (ar != "y") {
    return(res)
  }
  if (ar == "y") {
    medline <- EUtilsGet(res)
    keep.index <- rep(FALSE,N.1)
    reason <- rep("",N.1)
    notes <- rep("",N.1)
    studydesign <- rep("",N.1)
    qa.b <- FALSE
    vci.b <- FALSE
    adv.b <- FALSE
    sp.b <- FALSE
    eo.b <- FALSE
    FirstAuthor <- rep("",length(Author(medline)))
    for (i in 1:N.1) {
      FirstAuthor[i] <- Author(medline)[[i]][1,1]
    }
    name <- readline("Investigator name: ")
    date <- Sys.Date()
    topic <- readline("Topic/Question: ")
    qa <- readline("Does the review have a potential for quantitative analysis? (y/n) ")
    if (qa == "y") {
      qa.b <- TRUE
      quant.index <- rep(FALSE,N.1)
      vci <- readline("Record the principal value and 95% CI from abstract, if available? (y/n) ")
      if (vci == "y") {
        vci.b <- TRUE
        value <- rep(NA,N.1)
        lb <- rep(NA,N.1)
        ub <- rep(NA,N.1)
      }
      adv <- readline("Record additional values? (y/n) ")
      if (adv == "y") {
        adv.b <- TRUE
        adv.n <- as.numeric(readline("How many additional values? (integer values) "))
        additionalvalues <- matrix(NA,nrow=N.1,ncol=adv.n)
        adv.name <- rep(NA,adv.n)
        for (j in 1:adv.n) {
          adv.name[j] <- readline(paste("Value",j,"name: "))
        }
      }
    }
    sp <- readline("Record populations and/or subpopulations from abstracts? (y/n) ")
    if (sp == "y") {
      sp.b <- TRUE
      subpopulations <- rep("",N.1)
    }
    eo <- readline("Record exposure(s) and outcomes(s) from abstracts? (y/n) ")
    if (eo == "y") {
      eo.b <- TRUE
      exposures <- rep("",N.1)
      outcomes <- rep("",N.1)
    }
    #print(c(name,date,topic,qa.b,vci.b,adv.b,sp.b,eo.b))
    #if (adv.b) {
    #  print(adv.name)
    #}
    for (i in 1:N.1) {
      cat("***PUBMED LINK***")
      cat("\n")
      cat(paste("http://www.ncbi.nlm.nih.gov/pubmed/",medline@PMID[i],sep=""))
      cat("\n")
      cat("***TITLE***")
      cat("\n")
      cat(ArticleTitle(medline)[[i]])
      cat("\n")
      cat("***FIRST AUTHOR, YEAR***")
      cat("\n")
      cat(paste0(FirstAuthor[i],", ",YearPubmed(medline)[i]))
      cat("\n")
      cat("***ABSTRACT***")
      cat("\n")
      cat(AbstractText(medline)[[i]])
      keep <- readline("Include article? (y/n) ")
      reason[i] <- readline("Reason: ")
      if (keep == "y") {
        keep.index[i] <- TRUE
        studydesign[i] <- readline("Study Design: ")
        if (qa.b) {
          qa.q <- readline("Potential for quantitative analysis? (y/n) ")
          if (qa.q == "y") {
            quant.index[i] <- TRUE
            if (vci.b) {
              value[i] <- as.numeric(readline("Principal value: "))
              lb[i] <- as.numeric(readline("95% Lower Bound: "))
              ub[i] <- as.numeric(readline("95% Upper Bound: "))
            }
            if (adv.b) {
              for (j in 1:adv.n) {
                additionalvalues[i,j] <- as.numeric(readline(paste(adv.name[j],": ",sep="")))
              }
            }
          }
        }
        if (sp.b) {
          subpopulations[i] <- readline("Population and/or subpopulations: ")
        }
        if (eo.b) {
          exposures[i] <- readline("Exposure(s): ")
          outcomes[i] <- readline("Outcome(s): ")
        }
      }
      notes[i] <- readline("Note: ")
    }
    abreview.df <- data.frame(PMID = PMID(medline),
                              FIRSTAUTHOR = FirstAuthor,
                              YEAR = YearPubmed(medline),
                              TITLE = ArticleTitle(medline),
                              ABSTRACT = AbstractText(medline),
                              INCLUDE = keep.index,
                              REASON = reason,
                              NOTES = notes,
                              #nrows=N.1,ncols=9,
                              stringsAsFactors=FALSE)
    if (qa.b) {
      abreview.df$QUANTITATIVE <- quant.index
      if (vci.b) {
        abreview.df$VALUE <- value
        abreview.df$LB <- lb
        abreview.df$UB <- ub
      }
      if (adv.b) {
        for (j in 1:adv.n) {
          abreview.df[[adv.name[j]]] <- additionalvalues[,j]
        }
      }
    }
    if (sp.b) {
      abreview.df$POPULATION_SUBPOPULATIONS <- subpopulations
    }
    if (eo.b) {
      abreview.df$EXPOSURES <- exposures
      abreview.df$OUTCOMES <- outcomes
    }
    cat("Investigator: ")
    cat(name)
    cat("\n")
    cat("Date: ")
    cat(format(date, "%d %b %Y"))
    cat("\n")
    cat("Topic/Question: ")
    cat(topic)
    cat("\n")
    cat("Records identified: ")
    cat(N.1)
    cat("\n")
    cat("Records included: ")
    cat(sum(keep.index))
    cat("\n")
    if (qa.b) {
      cat("Potential records for quantitative analysis: ")
      cat(sum(quant.index))
      cat("\n")
    }
    return(abreview.df)
    
  }
}




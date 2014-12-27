rankall <- function(outcome, num = "best") {
  ## Read outcome data
  
  ## Read outcome data.It is important to add colClasses = "character"
  ## Otherwise, rise.csv loads the data.frame with columns of type factor
  Hdata<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  ## Subset the Data Frame: Select the columns we are interested in: name,state, 
  ## heart attacks, heart failures, pneumonia
  sHdata<-Hdata[,c(2,7,11,17,23)]
  ##change names of columns:
  newNames<-c("HN","ST","HA","HF","PN")
  ##Change the names with the new ones
  names(sHdata)<-newNames
  ## split secondary data by State
 
  if(outcome == "heart attack")
  {
    suppressWarnings(sHdata$HA<-as.numeric(sHdata$HA))
    sHdata<-sHdata[order(sHdata$ST,sHdata$HA,sHdata$HN,na.last=NA),]
  }
  else if(outcome == "heart failure")
  {
    suppressWarnings(sHdata$HF<-as.numeric(sHdata$HF))
    sHdata<-sHdata[order(sHdata$ST,sHdata$HF,sHdata$HN,na.last=NA),]
  }
  else if(outcome == "pneumonia")
  {
    suppressWarnings(sHdata$PN<-as.numeric(sHdata$PN))
    sHdata<-sHdata[order(sHdata$ST,sHdata$PN,sHdata$HN,na.last=NA),]
  }
  else{
    stop("invalid outcome")
  }
  ssHdata<-split(sHdata,sHdata$ST)
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}
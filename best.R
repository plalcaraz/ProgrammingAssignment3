best <- function(state, outcome) {
  ##Define de var for the result
  result<-c("")
  ## Read outcome data.It is important to add colClasses = "character"
  ## Otherwise, rise.csv loads the data.frame with columns of type factor
  Hdata<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  ## Check that state and outcome are valid
  ## Check if a Subset data by state exist
  sHdata<-Hdata[Hdata$State==state,c(2,11,17,23)]
  if(!nrow(sHdata))
    stop("invalid state")
  
  ##Define a char vector with more usable names
  newNames<-c("HN","HA","HF","PN")
  ##Change the names with the new ones
  names(sHdata)<-newNames
  if(outcome == "heart attack")
  {
    ## turn Heart Attack column as numeric
    ##sHdata$HA<-as.numeric(sHdata$HA)
    suppressWarnings(sHdata$HA<-as.numeric(sHdata$HA))
    result<-sHdata[sHdata$HA==min(sHdata$HA,na.rm=TRUE)  & (!is.na(sHdata$HA)),]
  }
  else if(outcome == "heart failure")
  {
    suppressWarnings(sHdata$HF<-as.numeric(sHdata$HF))
    result<-sHdata[sHdata$HF==min(sHdata$HF,na.rm=TRUE)  & (!is.na(sHdata$HF)),]
  }
  else if(outcome == "pneumonia")
  {
    suppressWarnings(sHdata$PN<-as.numeric(sHdata$PN))
    result<-sHdata[sHdata$PN==min(sHdata$PN,na.rm=TRUE)  & (!is.na(sHdata$PN)),]
  }
  else{
      stop("invalid outcome")
  }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  ##min(as.character(result[,1]))
  as.character(result[,1])
}
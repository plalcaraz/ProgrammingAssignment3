rankhospital <- function(state, outcome, num = "best") {
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
  newNames<-c("HN","HA","HF","PN")
  ##Change the names with the new ones
  names(sHdata)<-newNames
  if(outcome == "heart attack")
  {
    suppressWarnings(sHdata$HA<-as.numeric(sHdata$HA))
    sHdata<-sHdata[order(sHdata$HA,sHdata$HN,na.last=NA),]
  }
  else if(outcome == "heart failure")
  {
    suppressWarnings(sHdata$HF<-as.numeric(sHdata$HF))
    sHdata<-sHdata[order(sHdata$HF,sHdata$HN,na.last=NA),]
  }
  else if(outcome == "pneumonia")
  {
    suppressWarnings(sHdata$PN<-as.numeric(sHdata$PN))
    sHdata<-sHdata[order(sHdata$PN,sHdata$HN,na.last=NA),]
  }
  else{
    stop("invalid outcome")
  }
  ## Check for num validity
  index<-integer()
  if(num =="best")
    index = 1
  else if(num == "worst")
    index = nrow(sHdata)
  else if(!is.na(as.numeric(num)))
    index = num
    
  if(index>nrow(sHdata)){
    NA
  }
  else{
    sHdata[[index,1]]
  }
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
}
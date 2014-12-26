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
    sHdata<-sHdata[order(sHdata$HA,sHdata$HN,na.last=NA),]
  }
  else if(outcome == "heart failure")
  {
    sHdata<-sHdata[order(sHdata$HF,sHdata$HN,na.last=NA),]
  }
  else if(outcome == "pneumonia")
  {
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
  else if(as.numeric(num)!=NA)
    index = num
    
  if(index>nrow(sHata))
    return NA
    
  sHdata[[index,1]]
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
}
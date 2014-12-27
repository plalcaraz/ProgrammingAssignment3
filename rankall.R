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
  
  ## reorder data frame according to outcome
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
  ## Split data frame by states
  ssHdata<-split(sHdata,sHdata$ST)
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  index<-integer()
  if(num =="best" || !is.na(suppressWarnings(as.numeric(num))))
  {
    if(num =="best" )
      index = 1
    else 
      index = num
    
    message("inside 1st")
    result<-lapply(ssHdata,function(x) x[index,c(1)])
  }
  else if(num == "worst")
    result<-lapply(ssHdata,function(x) tail(x[,c(1)],1))
  ##Transform the result of type list into a data frame made by the list of hospitals and the states  
  df <- data.frame(matrix(unlist(result), nrow=length(result), byrow=T),names(result))
  names(df)<-c("hospital","state")
  df
}


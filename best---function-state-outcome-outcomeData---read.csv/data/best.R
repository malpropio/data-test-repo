best <- function(state, outcome) {
  outcomeData <- read.csv("~/data/outcome-of-care-measures.csv")
  outcomes<-c("heart attack", "heart failure", "pneumonia")  
  
  if(is.element(state, outcomeData[,7])==FALSE) {
    stop("invalid state")  
  }
  if(is.element(outcome, outcomes)==FALSE) {
    stop("invalid outcome")
  }
  
  outcomeIndex<-switch(outcome,
         'heart attack'=11,
         'heart failure'=17,
         'pneumonia'=23)
  
  colIndexes <- c(2,7, outcomeIndex)
  outcomeData[,outcomeIndex] <- as.numeric(as.character(outcomeData[,outcomeIndex]))
  subset_outcome <- outcomeData[complete.cases(outcomeValues[,outcomeIndex])&outcomeData$State==state,colIndexes]
  subset_outcome[,1]<-as.character(subset_outcome[,1])
  sorting_index<-c(3,1)
  ordered_subset_outcome<-subset_outcome[order(subset_outcome[,sorting_index]),]
  View(ordered_subset_outcome)
  split_sorted<-split(ordered_subset_outcome,ordered_subset_outcome[,3])
  ordered_split<-split_sorted[[1]][order(split_sorted[[1]]$Hospital.Name),]
  message(ordered_split[1])
  val <- ordered_split[[1,1]]
}

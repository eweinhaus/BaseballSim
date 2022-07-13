#Create data frame to measure plate appearances, pitches, walks, and strikeouts
dfTotals <- data.frame(matrix(ncol=4,nrow=0, dimnames=list(NULL, c("PAs", "Pitches", "Walks", "Strikeouts"))))

#Read in stats file
dfStats <- read.csv("countData2.csv")
dfStats <- dfStats[order(Count),]

#NEW INNING FUNCTION   
newInning <- function(dfTotals, dfStats) {
  PAs = 0
  pitches = 0
  walks = 0
  strikeouts = 0
  outs = 0
  actualBalls = 0
  actualStrikes = 0
  hbp = 0

  
  while(outs < 3) {
    #run new batter function
    #count outs, pitches, strikeouts, walks, PAs
    dfBatter = newBatter(outs, pitches)
    outs = dfBatter[c(1)]
    pitches = pitches + dfBatter[c(2)]
    walks = walks + dfBatter[c(3)]
    strikeouts = strikeouts + dfBatter[c(4)]
    hbp = hbp + dfBatter[c(5)]
    
    PAs = PAs + 1
  }
  
  #creates data frame row and adds it to bottom of final dataframe 
  dfNew <- data.frame(PAs, pitches, walks, strikeouts, hbp)
  colnames(dfNew) <- c("PAs","Pitches","Walks","Strikeouts", "HBP")
  dfTotals <- rbind(dfTotals,dfNew)
  return(dfTotals)
}

#NEW BATTER FUNCTION
newBatter <- function(outs, pitches) {
  strikeouts = 0
  walks = 0
  pitches = 0
  actualBalls = 0
  actualStrikes = 0
  endBatter = 0
  hbp = 0
  outs = outs
  
  while(endBatter == 0){
    #Runs newPitch function
    outcome = newPitch(actualBalls, actualStrikes, dfStats)
    pitches = pitches + 1
    if(outcome == "Strike") {
      actualStrikes = actualStrikes + 1
      if(actualStrikes == 3) {
        strikeouts = strikeouts + 1
        outs = outs + 1
        endBatter = 1
      }
      
    } else if(outcome == "Ball") {
      actualBalls = actualBalls + 1
      if(actualBalls == 4) {
        walks = walks + 1
        endBatter = 1
      }
    
    } else if(outcome == "Foul") {
      if(actualStrikes < 2) {
        actualStrikes = actualStrikes + 1
      }
      
    } else if(outcome == "Out"){
      outs = outs + 1
      endBatter = 1
      
      
    } else if(outcome == "Single") {
      endBatter = 1
      
    } else if(outcome == "Double") {
      endBatter = 1
      
    } else if(outcome == "HomeRun") {
      #runners = 3
      endBatter = 1

    }  else if(outcome == "DoublePlay"){
      outs = outs + 2
      endBatter = 1
      
    } else if(outcome == "HBP"){
      hbp = hbp + 1
      endBatter = 1
      
    } else if(outcome == "Triple") {
      #runners = 3
      endBatter = 1
      
    } else if(outcome == "TriplePlay"){
      outs = 3
      endBatter = 1
    }
  }
  #creates and returns dataframe row
  dfCounts <- data.frame(outs, pitches, walks, strikeouts, hbp)
  return(dfCounts)
  

}

#NEW PITCH FUNCTION
newPitch <- function(actualBalls, actualStrikes, dfStats) {

  #Find Count and take that row from data frame
  if(actualBalls == 0 & actualStrikes == 0){
    myRow = dfStats[9,]
  } else if(actualBalls == 0 & actualStrikes == 1){
    myRow = dfStats[1,] 
  } else if(actualBalls == 0 & actualStrikes == 2){
    myRow = dfStats[1,]
  } else if(actualBalls == 1 & actualStrikes == 0){
    myRow = dfStats[9,]
  } else if(actualBalls == 1 & actualStrikes == 1){
    myRow = dfStats[2,]
  } else if(actualBalls == 1 & actualStrikes == 2){
    myRow = dfStats[3,]
  } else if(actualBalls == 2 & actualStrikes == 0){
    myRow = dfStats[4,]
  } else if(actualBalls == 2 & actualStrikes == 1){
    myRow = dfStats[5,]
  } else if(actualBalls == 2 & actualStrikes == 2){
    myRow = dfStats[6,]
  } else if(actualBalls == 3 & actualStrikes == 0){
    myRow = dfStats[7,]
  } else if(actualBalls == 3 & actualStrikes == 1){
    myRow = dfStats[8,]
  } else {
    myRow = dfStats[9,]
  } 
  
  #random number
  myRand <- runif(1, 0, 1)

  #variable to iterate through the row
  myNum = 0
  
  #function to go through row and identify which random outcome we have
  for(x in 2:12){
    myNum = myNum + myRow[x]
    if(myRand < myNum){
      #returns column name as outcome
      return(colnames(dfStats[x]))
    }
  }
}

#RUNNING FUNCTIONS
for(x in 1:43740){
  dfTotals <- newInning(dfTotals, dfStats)
  if(x %% 100 == 0) {
    cat(x, 'out of 43740 half innings complete')
  }
}
print('Full season simulation complete')
cat('New action index =', (sum(dfTotals[,1]-dfTotals[,3:5])/2430))
write.csv(dfTotals,"simSeason.csv", row.names = FALSE)



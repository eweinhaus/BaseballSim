#Create data frame to measure plate appearances, pitches, walks, and strikeouts
dfTotals <- data.frame(matrix(ncol=4,nrow=0, dimnames=list(NULL, c("PAs", "Pitches", "Walks", "Strikeouts"))))

#Read in stats file
dfStats <- read.csv("Count_Percentages - Sheet1.csv")

#NEW INNING FUNCTION   
newInning <- function(dfTotals, dfStats) {
  PAs = 0
  pitches = 0
  walks = 0
  strikeouts = 0
  outs = 0
  actualBalls = 0
  actualStrikes = 0

  
  while(outs < 3) {
    #run new batter function
    #count outs, pitches, strikeouts, walks, PAs
    dfBatter = newBatter()
    print(newBatter())
    outs = newBatter(dfBatter[c(1)])
    pitches = pitches + dfBatter[c(2)]
    walks = walks + dfBatter[c(3)]
    strikeouts = strikeouts + dfBatter[c(4)]
    PAs = PAs + 1
  }
  
  #creates data frame row and adds it to bottom of final dataframe 
  dfNew <- data.frame(PAs, pitches, walks, strikeouts)
  print(dfNew)
  colnames(dfNew) <- c("PAs","Pitches","Walks","Strikeouts")
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
    } else if(outcome == "Foul") {
      if(actualStrikes < 2) {
        actualStrikes = actualStrikes + 1
      }
      
    } else if(outcome == "Ball") {
      actualBalls = actualBalls + 1
      if(actualBalls == 4) {
        walks = walks + 1
        endBatter = 1
      }
      
    } else if(outcome == "Single") {
      endBatter = 1
      
    } else if(outcome == "Double") {
      endBatter = 1
      
    } else if(outcome == "Triple") {
      #runners = 3
      endBatter = 1
      
    } else if(outcome == "HomeRun") {
      #runners = 3
      endBatter = 1
    } else if(outcome == "Out"){
      outs = outs + 1
      endBatter = 1
      
    }  else if(outcome == "DoublePlay"){
      outs = outs + 2
      endBatter = 1
      
    }  else if(outcome == "TriplePlay"){
      outs = 3
      endBatter = 1
      
    } else if(outcome == "Foul"){
      endBatter = 1
      
    } else if(outcome == "HBP"){
      endBatter = 1
    }
  }
  #creates and returns dataframe row
  dfCounts <- data.frame(outs, pitches, walks, strikeouts)
  print(dfCounts)
  return(dfCounts)
  

}

#NEW PITCH FUNCTION
newPitch <- function(actualBalls, actualStrikes, dfStats) {
  print("Start New Pitch")
  #Find Count and take that row from data frame
  print("Count:")
  print(actualBalls)
  print(actualStrikes)
  if(actualBalls == 0 & actualStrikes == 2){
    myRow = dfStats[1,]
  } else if(actualBalls == 1 & actualStrikes == 1){
    myRow = dfStats[2,] 
  } else if(actualBalls == 1 & actualStrikes == 2){
    myRow = dfStats[3,]
  } else if(actualBalls == 2 & actualStrikes == 1){
    myRow = dfStats[4,]
  } else if(actualBalls == 2 & actualStrikes == 2){
    myRow = dfStats[5,]
  } else{
    myRow = dfStats[6,]
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
      print(colnames(dfStats[x]))
      return(colnames(dfStats[x]))
    }
  }
}

#RUNNING FUNCTIONS
dfTotals <- newInning(dfTotals, dfStats)

PROMPT: 
How can action index* be increased in baseball without increasing time of game?

--------------------

SOLUTION:
Credit players for "double points" for each hit in the first two pitches of an at-bat. Examples: after hitting a single on the first or second pitch of an at bat, each runner would automatically advance an extra base after the play. A double would count as a home run. A triple would count as a home run, and the batter would advance to second base. A home run would count as two home runs.

We assume that batters and pitchers will begin treating all 0-0 and 1-0 counts with an 3-2 count approach, and 0-1 counts with an 0-2 count approach. We used data from the outcomes all 2021 MLB pitches based on count "Count_Percentages - Sheet1 (1).csv" to simulate how those approaches would change the outcomes of plate appearances. The simulation is run in "baseballSim.R" The final results are written to "simSeason.csv". 

In conclusion, the action index was raised from 50 to about 207


*Action Index: Batted balls in play per game. (PAs - BBs - HBPs - Ks)

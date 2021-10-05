library(tidyverse)

#I'm not sure there's a need to set the working directory anymore now that we're
#using github. I tried to use the setwd() function, and it wouldn't let me.

#Reading timepoints 1-6
R1<-read.csv("round1_updated.csv")
R2<-read.csv("round2_updated.csv")
R3<-read.csv("round3_updated.csv")
R4<-read.csv("round4_updated.csv")
R5<-read.csv("round5_updated.csv")
R6<-read.csv("round6_updated.csv")

#I believe this was the file saved by accident (do not read in)
#CT1N<-read.csv("CT1Ncorrect.csv")

#Deleting duplicate in R5

#Was at 1049 (16 duplicates)
nrow(R5)

#Can filter out using unique timestamp from csv file
R5<- R5%>%
  filter(level_start_timestamp != "2021-03-25T18:55:58.767Z")

#Now at 1033  
nrow(R5)


#This next step has multiple purposes. First, we are adding "semester" and "timepoint"
#variables to the dataset. Since "user1", "user2"... "usern" was used for both
#semesters, despite these representing different people, I created a new ID variable.
#For semester 1, it is the old ID +100; for semester 2, it is +200.
R1<-mutate(R1, semester=1, timepoint=1, ID = as.factor(as.numeric(gsub("user", "", userid))+100))
R2<-mutate(R2, semester=1, timepoint=2, ID = as.factor(as.numeric(gsub("user", "", userid))+100))
R3<-mutate(R3, semester=1, timepoint=3, ID = as.factor(as.numeric(gsub("user", "", userid))+100))
R4<-mutate(R4, semester=2, timepoint=1, ID = as.factor(as.numeric(gsub("user", "", userid))+200))
R5<-mutate(R5, semester=2, timepoint=2, ID = as.factor(as.numeric(gsub("user", "", userid))+200))
R6<-mutate(R6, semester=2, timepoint=3, ID = as.factor(as.numeric(gsub("user", "", userid))+200))

#Combining all six datasets
Full<-rbind(R1, R2, R3, R4, R5, R6)

#Ensuring the data looks correct and everything merged as expected
str(Full)

#The scorecard variable gives us information about each specific trial. I created
#a new dataset with only the scorecards for each user on each game
Scores<-filter(Full, event_type == "scorecard")

#We can get an idea of what is included (and what is not relevant) for this new dataset
summary(Scores)
str(Scores)

#I then mutated this to only retain variables we are interested in. The others were
#specific to individual items on each trial
Scores2<-select(Scores, c(ID, semester, timepoint,
                          game_name, correct_count, incorrect_count,
                          total_trials, fastest_reaction_time, median_reaction_time,
                          average_reaction_time, level_total_time
                          ))

#We can view our new dataset to ensure it looks correct
str(Scores2)
view(Scores2)

#We see that correct count is missing for all quick tap level 2 games.
#After confirming it matches what is in the original csv file, we see that correct
#can be accurately calculated by subtracting incorrect from total trials

#Create second correct_count variable
Scores2$correct_count2 = Scores2$total_trials-Scores2$incorrect_count

#Create "check" variable that is the difference between two counts
Scores2$countcheck = Scores2$correct_count - Scores2$correct_count2

#View summary by game type
Scores2%>%
  group_by(game_name)%>%
  summarize(min_corr = min(correct_count), min_corr2 = min(correct_count2),
            max_corr = max(correct_count), max_corr2 = max(correct_count2),
            min_check = min(countcheck), max_check = max(countcheck))

#We see that the correct count matches for all color trick games
#For hand swype, however, it is systematically off by one (confirmed in csv).
#A quick check shows that every participant had a final hand swype trial that
#timed out with no response. This adds to the trial total but not to the correct
#or incorrect total. To correct this flaw, we can subtract one for hand swype only

Scores2 <- Scores2%>%
  mutate(correct_count2 = ifelse(game_name == "hand swype",
                                 correct_count2 -1,
                                 correct_count2))

#Create "check" variable that is the difference between two counts
Scores2$countcheck = Scores2$correct_count - Scores2$correct_count2

#View summary by game type
Scores2%>%
  group_by(game_name)%>%
  summarize(min_corr = min(correct_count), min_corr2 = min(correct_count2),
            max_corr = max(correct_count), max_corr2 = max(correct_count2),
            min_check = min(countcheck), max_check = max(countcheck))

#Correct count 2 now accurately summarizes the correct count for each variable

#I then added a value for the proportion of correct responses, as a possible outcome
#for later
Scores2<-mutate(Scores2, prop_cor = (correct_count2/(correct_count2+incorrect_count)))

#Next, I want to summarize each timepoint for each individual.
#Ideally, we should have five values, one for each game, in each of these rows.
countScores<- Scores2%>%
              group_by(ID, semester, timepoint)%>%
              summarize(count = n())
countScores

#Unfortunately, we see that is not true
df<-data.frame(countScores)
df%>%
  summarize(min=min(count), max=max(count))

#We see that ID 119 only has a single timepoint when we sort ascending
head(df[order(df$count),])

#FIXED ON 10/4/21
#We see that ID 206 has an extra timepoint when we sort descending. Returning to
#the original CSV for round5, we confirm that user6 (ID206) did quick tap level 2
#twice. We should likely remove one of these two trials.
#head(df[order(-df$count),])

#Counting number of trials for each participant and each game
Scores2%>%
  group_by(game_name)%>%
  summarize(min_trials = min(total_trials),
            max_trials = max(total_trials),
            avg_trials = mean(total_trials))

#Everything is equal for every game except hand swype, which makes sense because
#of the nature of the game

#We can look at the number of trials for each participant using the code below
Scores2%>%
  filter(game_name == "hand swype")%>%
  ggplot(aes(x = timepoint, y = total_trials))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 34.3, color = "red", linetype = "dashed")+
  facet_wrap(.~ID)

#The solution is in the fact that getting answers correct allows you to attempt
#more trials (the "checkpoint in racing games" format)

#See how correct_count is correlated with total trials
#Similarly, average reaction time is correlated with both
Scores2%>%
  filter(game_name == "hand swype")%>%
  ggplot(aes(y=total_trials, x = correct_count2))+
  geom_point()

#Adding function line to show possible relationship
Scores2%>%
  filter(game_name == "hand swype")%>%
  ggplot(aes(y=total_trials, x = average_reaction_time))+
  geom_point()+
  geom_function(fun = function(x) 5/((x/5000)^1.5)+10)

Scores2%>%
  filter(game_name == "hand swype")%>%
  ggplot(aes(y=correct_count2, x = average_reaction_time))+
  geom_point()

#Our next step is to create datasets for each of the game conditions. We first make
#a "long" version of the dataset with one row per timepoint
#This dataset will be necessary for analysis
ColorTrick1<-filter(Scores2, game_name == "color trick 1")

#Then we create a "wide" dataset with one row per participant
#This dataset will be helpful for descriptive statistics
CT1W<-pivot_wider(data = ColorTrick1,
                  names_from = timepoint,
                  values_from = c(correct_count2, incorrect_count,
                                  total_trials,
                                  fastest_reaction_time, median_reaction_time,
                                  average_reaction_time, level_total_time))

#We can do the same for CT2
ColorTrick2<-filter(Scores2, game_name == "color trick 2")

CT2W<-pivot_wider(data = ColorTrick2,
                  names_from = timepoint,
                  values_from = c(correct_count2, incorrect_count,
                                  total_trials,
                                  fastest_reaction_time, median_reaction_time,
                                  average_reaction_time, level_total_time))

#Next for CT3
ColorTrick3<-filter(Scores2, game_name == "color trick 3")

CT3W<-pivot_wider(data = ColorTrick3,
                  names_from = timepoint,
                  values_from = c(correct_count2, incorrect_count,
                                  total_trials,
                                  fastest_reaction_time, median_reaction_time,
                                  average_reaction_time, level_total_time))
#Then for HS
HandSwipe<-filter(Scores2, game_name == "hand swype")

HSW<-pivot_wider(data = HandSwipe,
                  names_from = timepoint,
                  values_from = c(correct_count2, incorrect_count,
                                  total_trials,
                                  fastest_reaction_time, median_reaction_time,
                                  average_reaction_time, level_total_time))

#Finally for QT2
QuickTap<-filter(Scores2, game_name == "quick tap level 2")

QT2<-pivot_wider(data = QuickTap,
                 names_from = timepoint,
                 values_from = c(correct_count2, incorrect_count,
                                 total_trials,
                                 fastest_reaction_time, median_reaction_time,
                                 average_reaction_time, level_total_time))

#Next, we can start to visualize trajectories
#We first load the lattice package in R
library(lattice)

#We can make a "spaghetti" plot showing each trajectory across semesters using ggplot
#We first do this for the proportion correct
ColorTrick1%>%
  ggplot(aes(x=timepoint, y=prop_cor, color=ID))+
  geom_point()+
  geom_line()+
  facet_wrap(.~semester)+
  ylab("Proportion Correct")

#We can also break the visualizations up into panels, one for each participant
xyplot(prop_cor ~ timepoint | ID, data = ColorTrick1, as.table = T,
       xlab = "Timepoint", ylab = "Proportion Correct", grid = T, pch = 19,
       type = c("p", "r"), col.line = "darkblue", lwd = 3, lty = 4 )

#We can create the same plots for average reaction time
ColorTrick1%>%
  ggplot(aes(x=timepoint, y=average_reaction_time, color=ID))+
  geom_point()+
  geom_line()+
  facet_wrap(.~semester)+
  ylab("Reaction Time")

xyplot(average_reaction_time ~ timepoint | ID, data = ColorTrick1, as.table = T,
       xlab = "Timepoint", ylab = "Reaction Time", grid = T, pch = 19,
       type = c("p", "r"), col.line = "darkblue", lwd = 3, lty = 4 )

#We can also create these plots for level total time
ColorTrick1%>%
  ggplot(aes(x=timepoint, y=level_total_time, color=ID))+
  geom_point()+
  geom_line()+
  facet_wrap(.~semester)+
  ylab("Total Time")

xyplot(level_total_time ~ timepoint | ID, data = ColorTrick1, as.table = T,
       xlab = "Timepoint", ylab = "Total Time", grid = T, pch = 19,
       type = c("p", "r"), col.line = "darkblue", lwd = 3, lty = 4 )

#We can use this same code to create these visualizations for the other game types
#or any other information we're interested in.
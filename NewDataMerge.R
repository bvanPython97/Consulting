#I really want to see how the PHQ was calculated! Something is not making sense

library(tidyverse)

#Reading timepoints 1-6
R1<-read.csv("round1_updated.csv")
R2<-read.csv("round2_updated.csv")
R3<-read.csv("round3_updated.csv")
R4<-read.csv("round4_updated.csv")
R5<-read.csv("round5_updated.csv")
R6<-read.csv("round6_updated.csv")

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

#We know that user40 was a test case, so this person needs to be removed
Full <- Full%>%
  filter(ID != 140)

#NeurUX data needs to be reformatted a bit before we can merge

IntKey <- read.csv("NeurUX.Intervention.Group.csv")

#One problem is that users get the same name across semesters. Need to manually fix this
sem1 <- c(22585580,
          22585581,
          22585582,
          22585583,
          22585586,
          22585588,
          22585591,
          22585592,
          22585593,
          22585594,
          22585596,
          22585597,
          22585599,
          22585600,
          22585601,
          22585604,
          22585606,
          22585609,
          22585610)
sem2 <- c(22585612,
          22585613,
          22585614,
          22585616,
          22585619,
          22585620,
          22585621,
          22585623,
          22585624,
          22585625,
          22585626,
          22585627,
          22585629,
          22585630)

IntKey <- mutate(IntKey, semester = case_when(Study.ID %in% sem1 ~ 1,
                                               Study.ID %in% sem2 ~ 2))
IntKey1 <-filter(IntKey, semester == 1)
IntKey1 <- mutate(IntKey1, ID = as.factor(as.numeric(gsub("user", "", NeurUX.ID))+100))

IntKey2 <-filter(IntKey, semester == 2)
IntKey2 <- mutate(IntKey2, ID = as.factor(as.numeric(gsub("user", "", NeurUX.ID))+200))

IntKey <-rbind(IntKey1, IntKey2)

view(IntKey)

#Can merge with NeurUX sheet to get control and intervention groups
Merge.1 <- merge(
                 x = Full,
                 y = IntKey,
                 by.x = "ID",
                 by.y = "ID"
)

#Trimming SMILE dataset so it only contains the variables we need

smile <- read.csv("SMILE full data 10 23 21_AARThesis_Deidentified_forNL.csv")

smile2 <- select(smile, StudyID_T1, PHQ_T1_total, PHQ_T2_total, PHQ_T3_total)

#Can now merge with SMILE data
Merge.2 <- merge(
                 x = Merge.1,
                 y = smile2,
                 by.x = "Study.ID",
                 by.y = "StudyID_T1"
)

view(Merge.2)

Scores<-filter(Merge.2, event_type == "scorecard")

Scores%>%
  filter(timepoint == 1)%>%
  filter(game_name =="color trick 1")%>%
  ggplot(aes(x = PHQ_T1_total, y = average_reaction_time))+
  geom_jitter()

Scores%>%
  filter(timepoint == 1)%>%
  filter(average_reaction_time < 4000)%>%
  ggplot(aes(x = PHQ_T1_total, y = average_reaction_time))+
  geom_jitter()+
  facet_grid(.~game_name)

Scores%>%
  filter(timepoint == 1)%>%
  ggplot(aes(x = PHQ_T1_total, y = correct_count))+
  geom_jitter()+
  facet_grid(.~game_name)

Scores%>%
  filter(timepoint == 1)%>%
  ggplot(aes(x = average_reaction_time, y = correct_count))+
  geom_jitter()+
  facet_grid(.~game_name)

Scores%>%
  filter(timepoint == 1)%>%
  filter(average_reaction_time < 4000)%>%
  group_by(game_name)%>%
  summarize(corr_RT = cor(PHQ_T1_total, average_reaction_time),
            corr_right = cor(PHQ_T1_total, correct_count))

#Making timepoint into a factor for later use
Full$tp_fac <- ordered(Full$timepoint,
                       levels = c(1, 2, 3),
                       labels = c("Time 1", "Time 2", "Time 3")
)

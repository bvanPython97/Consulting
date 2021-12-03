df <- read.csv("MergedData.csv")

str(df)
#view(df)

#The playlist variables give us information about when the session started and whether it was completed
comp.data <- filter(df, game_name == "playlist")
COMP <- c("ID", "timepoint", "session_complete", "session_start_timestamp", "session_end_timestamp")
comp.data <- comp.data[COMP]
comp.data <- filter(comp.data, session_complete == TRUE)
#view(comp.data)

#We have 87 total timepoints with completed data
sum(comp.data$session_complete, na.rm = TRUE)

#Can save the number of completed timepoints for each person
timepoints <- comp.data%>%
              group_by(ID)%>%
              summarize(n())

#Viewing full dataset (if desired)
#view(timepoints)

df <- mutate(df, ID = factor(ID))

#Removing playlist, since this is more general
df <- filter(df, game_name != "playlist")

#Looking at distributions of reaction times for each game (interested in outliers)
#Color Trick First
CT <- c("color trick 1", "color trick 2", "color trick 3")
df%>%
  filter(game_name %in% CT)%>%
  ggplot(aes(x = ID, y = (response_reaction_time/1000)))+
  geom_violin(alpha = .1, adjust = 1.5)+
  geom_boxplot(alpha = .5)+
  facet_grid(game_name~.)

#A small proportion of trials take over five seconds (not likely due to processing the question)
#Only 30 of the 2610 responses fall into this category. We should likely drop these for calculation
df%>%
  filter(game_name %in% CT)%>%
  summarize(tot.over.5  = sum(response_reaction_time >= 5000, na.rm = TRUE),
            prop.over.5 = sum(response_reaction_time >=5000, na.rm = TRUE)/n(),
            tot.eq.0    = sum(response_reaction_time == 0, na.rm = TRUE),
            prop.eq.0 = sum(response_reaction_time == 0, na.rm = TRUE)/n(),
            total = n())

#Then Hand Swype
df%>%
  filter(game_name == "hand swype")%>%
  ggplot(aes(x = ID, y = (response_reaction_time/1000)))+
  geom_violin(alpha = .1, adjust = 1.5)+
  geom_boxplot(alpha = .5)

#Similarly, it was rare to take longer than 5 seconds to respond to hand swype
#35 out of 3130 were greater than 5s
#87 were equal to 0 (this represents the total number of trials, since all ended with a timeout)
df%>%
  filter(game_name == "hand swype")%>%
  summarize(tot.over.5  = sum(response_reaction_time >= 5000, na.rm = TRUE),
            prop.over.5 = sum(response_reaction_time >= 5000, na.rm = TRUE)/n(),
            tot.eq.0    = sum(response_reaction_time == 0, na.rm = TRUE),
            prop.eq.0 = sum(response_reaction_time == 0, na.rm = TRUE)/n(),
            total = n())

#Finally we want to look at Quick Tap
df%>%
  filter(game_name == "quick tap level 2")%>%
  ggplot(aes(x = ID, y = (response_reaction_time/1000)))+
  geom_violin(alpha = .1, adjust = 1.5)+
  geom_boxplot(alpha = .5)

#For this game, it appears most of the responses should be under one second
#Only 10 responses of 1409 were over 1 second.
#Many responses were 0 (comes from unselected responses and skews averages)
#647 were eaual to zero, which is a flaw but comes from the game structure
df%>%
  filter(game_name == "quick tap level 2")%>%
  summarize(tot.over.5  = sum(response_reaction_time >= 1000, na.rm = TRUE),
            prop.over.5 = sum(response_reaction_time >= 1000, na.rm = TRUE)/n(),
            tot.eq.0    = sum(response_reaction_time == 0, na.rm = TRUE),
            prop.eq.0 = sum(response_reaction_time == 0, na.rm = TRUE)/n(),
            total = n(),
            )

CTdf <- filter(df, game_name %in% CT)
CTdf <- CTdf[CTdf$response_reaction_time < 5000,]
nrow(CTdf)

#Recall handswype is the one that has the "checkpoint style"
#More trials = more successful on the task
#Our RT is less important here (although there is a correlation)
HSdf <- filter(df, game_name =="hand swype")
HSdf <- HSdf[HSdf$response_reaction_time < 5000,]
nrow(HSdf)

#Note that for this one, we only want to count RTs of targets (distractors are 0)
QTdf <- filter(df, game_name =="quick tap level 2")
QTdf <- QTdf[QTdf$response_reaction_time < 1000,]
QTdf <- QTdf[QTdf$response_reaction_time > 0,]
nrow(QTdf)

RTno.out <- rbind(CTdf, QTdf, HSdf)
nrow(df) 
nrow(RTno.out)
nrow(df)-nrow(RTno.out) #Should have removed 75 missing responses and 647 0s from QT

RTno.out%>%
  filter(ID != "NA")%>%
  ggplot(aes(x = ID, y = (response_reaction_time/1000)))+
  geom_violin(alpha = .1, adjust = 1.5)+
  geom_boxplot(alpha = .5)+
  facet_grid(game_name~.)

#We can now calculate reaction times for each game for each participant
RTs <- RTno.out%>%
       group_by(ID, timepoint, game_name)%>%
       summarize(avgRT = mean(response_reaction_time, na.rm = TRUE))

#Next we can move to number correct for each game
#We can skip outliers for this one, since outliers are less likely to be extreme
Correct <- df%>%
           group_by(ID, timepoint, game_name)%>%
           summarize(tot.correct   = sum(is_response_correct == TRUE, na.rm = TRUE),
                     tot.incorrect = sum(is_response_correct == FALSE, na.rm = TRUE),
                     prop.correct = tot.correct/(tot.correct + tot.incorrect))

Game_Data <- merge(
  x = Correct,
  y = RTs,
  by.x = c("ID", "timepoint", "game_name"),
  by.y = c("ID", "timepoint", "game_name")
)

str(Game_Data)

smile <- read.csv("SMILE full data 10 23 21_AARThesis_Deidentified_forNL.csv")

smile2 <- pivot_longer(smile,
                       cols = c(PHQ_T1_total, PHQ_T2_total, PHQ_T3_total),
                       names_to = "Time",
                       values_to = "PHQ.Score")

smile2 <- mutate(smile2, numTime = case_when(Time == "PHQ_T1_total" ~ 1,
                                             Time == "PHQ_T2_total" ~ 2,
                                             Time == "PHQ_T3_total" ~ 3))

smile2 <- select(smile2, StudyID_T1, numTime, PHQ.Score)

str(smile2)

#Need keys from intervention keys
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
                                              Study.ID %in% sem2 ~ 2),
                 Int.Fac = factor(Intervention.Group,
                                  levels = c(0, 1),
                                  labels = c("Control", "Intervention")))
IntKey1 <-filter(IntKey, semester == 1)
IntKey1 <- mutate(IntKey1, ID = as.factor(as.numeric(gsub("user", "", NeurUX.ID))+100))

IntKey2 <-filter(IntKey, semester == 2)
IntKey2 <- mutate(IntKey2, ID = as.factor(as.numeric(gsub("user", "", NeurUX.ID))+200))

IntKey <-rbind(IntKey1, IntKey2)

#Can merge with NeurUX sheet to get control and intervention groups
Merge.1 <- merge(
  x = Game_Data,
  y = IntKey,
  by.x = "ID",
  by.y = "ID"
)

#Can now merge with the smile dataset
PHQ.Corrs <- merge(
  x = Merge.1,
  y = smile2,
  by.x = c("Study.ID", "timepoint"),
  by.y = c("StudyID_T1", "numTime"))

str(PHQ.Corrs)

library(corrplot)

PHQ.Corrs%>%
  ggplot(aes(x = avgRT, y = PHQ.Score))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(game_name ~ timepoint)

PHQ.Corrs%>%
  group_by(timepoint, game_name)%>%
  summarize(cors = cor(PHQ.Score, avgRT))

Time1Corrs <- filter(PHQ.Corrs, timepoint == 1)
Time2Corrs <- filter(PHQ.Corrs, timepoint == 2)
Time3Corrs <- filter(PHQ.Corrs, timepoint == 3)

Time1Corrs.Wide <- pivot_wider(Time1Corrs,
                              id_cols = c(Study.ID, ID, NeurUX.ID, timepoint,
                                          Headspace.ID, Intervention.Group, semester, Int.Fac,
                                          PHQ.Score),
                              names_from = c(game_name),
                              values_from = c(tot.correct, tot.incorrect, prop.correct,
                                              avgRT))

Time2Corrs.Wide <- pivot_wider(Time2Corrs,
                               id_cols = c(Study.ID, ID, NeurUX.ID, timepoint,
                                           Headspace.ID, Intervention.Group, semester, Int.Fac,
                                           PHQ.Score),
                               names_from = c(game_name),
                               values_from = c(tot.correct, tot.incorrect, prop.correct,
                                               avgRT))

Time3Corrs.Wide <- pivot_wider(Time3Corrs,
                               id_cols = c(Study.ID, ID, NeurUX.ID, timepoint,
                                           Headspace.ID, Intervention.Group, semester, Int.Fac,
                                           PHQ.Score),
                               names_from = c(game_name),
                               values_from = c(tot.correct, tot.incorrect, prop.correct,
                                               avgRT))
str(Time1Corrs.Wide)

T1s <- data.frame(Time1Corrs.Wide[9:29])
T1s <- mutate_if(T1CRCT, is.integer, as.numeric)
str(T1s)
T1 <- cor(T1s)
#We can look at the full correlations, but all we care about is PHQ
corrplot(T1)

T1.Corrs <- data.frame(rbind(rownames(T1), T1[1:21]))
T1.Corrs <- data.frame(t(T1.Corrs))
T1.Corrs <- mutate(T1.Corrs, Corr = as.numeric(X2))
str(T1.Corrs)

#Correct answers on quick tap are negatively associated with PHQ
#Incorrect answers on CT3 and CT1 reaction time are also negatively correlated (less)
head(T1.Corrs[order(T1.Corrs$Corr),])

#Incorrect quick tap answers are positively correlated with PHQ
#Correct answers on CT3 are similarly positvely correlated (less)
head(T1.Corrs[order(-T1.Corrs$Corr),])

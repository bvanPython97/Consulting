#read in data
setwd("E:/GQ/LUC/Graduate materials/Lectures/S488-002/Project/team/input/original")
#dataFolder <- dir(setwd("C:/Users/gongqing/Desktop/original"))
#df <- do.call(rbind, lapply(dataFolder, function(x) cbind(read.csv(x), name=strsplit(x,'//.')[[1]][1])))
#df <- do.call(rbind, lapply(dataFolder, read.csv()))
temp = list.files(pattern="*.csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)

#add columns semester 1, 2 and time_point 1, 2, 3
round1_updated$semester <- rep(1, 1525)             # define the semester in round1 as 1
round1_updated$time_point <- rep(1, 1525)         #define the time point in round1 as 1  

round2_updated$semester <- rep(1, 1411)             # define the semester in round1 as 1
round2_updated$time_point <- rep(2, 1411)         #define the time point in round1 as 2 

round3_updated$semester <- rep(1, 1229)             # define the semester in round1 as 1
round3_updated$time_point <- rep(3, 1229)         #define the time point in round1 as 3 

round4_updated$semester <- rep(2, 1205)             # define the semester in round1 as 2
round4_updated$time_point <- rep(1, 1205)         #define the time point in round1 as 1

round5_updated$semester <- rep(2, 1049)             # define the semester in round1 as 1
round5_updated$time_point <- rep(2, 1049)         #define the time point in round1 as 1 

round6_updated$semester <- rep(2, 1010)             # define the semester in round1 as 1
round6_updated$time_point <- rep(3, 1010)         #define the time point in round1 as 1 

#combine all data into one whole dataset
data <- rbind(round1_updated, round2_updated, round3_updated, round4_updated,
              round5_updated, round6_updated)

#rename userid as userID(extract id number of userid)
data$userID <- substr(data$userid, 5, 7)

#subset color trick
colorTrick <- subset(
  data,
  subset = game_name %in% c("color trick 1",
                            "color trick 2",
                            "color trick 3"),
  event_type = trial,
  select = c(
    game_name,
    userID,
    event_type,
    is_response_correct,
    response_reaction_time,
    correct_count,
    incorrect_count,
    user_response,
    average_reaction_time,
    mechanic_name,
    semester,
    time_point
  )
)

sum(colorTrick$is_response_correct == "TRUE") # there are 2213 correct response
sum(colorTrick$is_response_correct == "FALSE") #there are 163 false response

#select variables of interest
colorT <- subset(
  colorTrick,
  event_type == "scorecard",
  select = c(
    game_name,
    userID,
    correct_count,
    incorrect_count,
    average_reaction_time,
    time_point,
    semester
  )
)

#change userID into numeric
colorT$userID <- as.numeric(colorT$userID)
#colorT$game_name <- substr(colorT$game_name, 13, 14)

str(colorT)
# ○	Number correct (ncorrect)
# ○	Number incorrect (nincorrect)
# ○	Average Reaction Time (RT) for ncorrect (RT_ncorrect)
# ■	Plot RT to visualize for each participant? For each sample?

names(colorT)
#load library 
library(tidyverse)
color <- colorT %>% 
          select(userID,
                 game_name,
                 correct_count,
                 incorrect_count,
                 average_reaction_time, 
                 time_point,
                 semester) %>% 
  group_by(semester, time_point, userID) %>% 
  summarise(avg_RT = mean(average_reaction_time),
            ncorrect = mean(correct_count),
            nincorrect = mean(incorrect_count),
              .groups = 'drop') %>% 
  mutate(RT = avg_RT/ncorrect)          #create a new variable--reaction time for each correct

#change time_point, semester and userID into factors

color[, c(1, 2, 3)] <- lapply(color[, c(1, 2, 3)], function(x) as.factor(x))
str(color)

#check sample size
sum(color$semester == '1')                  #sample size for semester1 --- 50
sum(color$semester == '2')                  #sample size for semester2 --- 38
sum(color$time_point == '1')                #sample size for time 1    --- 32
sum(color$time_point == '2')                #sample size for time 2    --- 29
sum(color$time_point == '3')                #sample size for time 3    --- 27

# #plot
# par(mfrow = c(1,3))
# plot(RT ~ as.factor(userID), ,pch = 19, col = "#11dd66")
# plot(RT ~ time_point, pch = 19, col = "#dd1166")
# plot(RT ~ semester, pch = 19, col = "#dd1166")

library(gridExtra)
library(ggplot2)
#RT by subject
p1 <- ggplot(color, aes(x = as.factor(userID), y = RT)) +
  geom_boxplot(col='#12ff11', fill="#666666")+
  labs(title = 'RT by Subject', x='Subject', y = 'RT')
#** RT for each subject different, some outliers

#RT by time point
p2 <- ggplot(color, aes(x = time_point, y = RT)) +
  geom_point(col='#ff1222')
#** RT for 3 time points looks different

#RT by semester
p3 <- ggplot(color, aes(x = semester, y = RT)) +
  geom_point(col='#1211ff')
#** RT for 3 time points looks no big different

grid.arrange(p1, p2, p3, ncol = 3)

#-----------------------  check means for time_point - ----------------------
color %>% select(time_point, RT) %>% 
  group_by(time_point) %>% 
  summarise(mean = mean(RT))
#** RT for time point 1-2, 1-3 different

#ANOVA test to confirm difference 
lm1 <- lm(RT ~ time_point)
anova(lm1)
#** A small p-value indicates mean reaction time for time points significantly diff.

#Check assumptions
shapiro.test(lm1$residuals)    #Normality check    
#** small p-value means normality assumption is violated

#Non-parametric test
kruskal.test(RT, time_point)
#** A small p-value indicates mean reaction time for time points significantly diff.

#Pairwise test to check which groups different
pairwise.wilcox.test(RT, time_point, p.adjust.method = "bonferroni")
#** Pairwise comparison test shows RT for time point 1 and 3 significantly different, other two groups no sig. diff.

#----------------------------- check means for semester ------------------------
color %>% select(semester, RT) %>% 
  group_by(semester) %>% 
  summarise(mean = mean(RT))
#** Mean RT for semester no different

color %>% select(semester, RT) %>% 
  group_by(semester) %>% 
  summarise(median = median(RT))
#** Median RT for semester no big different


#t-test to confirm difference 
s1 <- subset(color, semester == '1', select=c(RT))[['RT']]
s2 <- subset(color, semester == '2', select=c(RT))[['RT']]
t.test(s1, s2)
class(s1)
#** A big p-value indicates mean reaction time for time points no diff.

#Check assumptions
shapiro.test(s1)    #Normality check    
#** small p-value means normality assumption is violated

#Non-parametric test
wilcox.test(s1, s2)
#**  p-value indicates mean reaction time for time points not significantly diff.


#Want to do pair t-test to check if sig. diff. for each subject in two semesters
#
#id1 <- subset(color, semester == '1', select=c(RT, userID))[['userID']]
#id2 <- subset(color, semester == '2', select=c(RT, userID))[['userID']]


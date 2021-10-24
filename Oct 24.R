#-------------- Script for Neuro Game Results-------------------#
# Qing Gong
# 16th OC 2021

#load libraries

library(tidyverse);library(ggplot2);library(gridExtra);library(lattice);

#read in data
setwd("E:/GQ/LUC/Graduate materials/Lectures/S488-002/Project/team/input/original")

#dataFolder <- dir(setwd("C:/Users/gongqing/Desktop/original"))
#df <- do.call(rbind, lapply(dataFolder, function(x) cbind(read.csv(x), name=strsplit(x,'//.')[[1]][1])))
#df <- do.call(rbind, lapply(dataFolder, read.csv()))

#input all original files
temp = list.files(pattern="*.csv")
#creates an environment containing all list components(csv file) as objects and then input them
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)

#Cleaning 
#1. user40 was a test case, which needs to be removed
R1 <- round1_updated %>% filter(userid != 'user40')

#2.Remove extra observation. For each student, they are supposed to do each game once. 
#For each game, every one should have unique start time stamp
#In R5, user6(B6) have two start time stamp for quick tap level 2, 2nd try is deleted here
R5 <- round5_updated %>% filter(level_start_timestamp != "2021-03-25T18:55:58.767Z")

#3. Add treatment information from NeurUX.Intrvention.Group file
#extract id number from NeurUX.ID, add A to Id numbers for 2021 Fall or B for Spring 2021 
Intervention1 <- NeurUX.Intervention.Group %>% 
  slice(2:15) %>% 
  mutate(ID = paste0('B', substr(NeurUX.ID, 5, 7))) %>% 
  select(treatment= Intervention.Group, ID)

Intervention2 <- NeurUX.Intervention.Group %>% slice(18:36) %>% 
  mutate( ID = paste0('A', substr(NeurUX.ID, 5, 7))) %>% 
  select(treatment= Intervention.Group, ID)

#subset intervention data set with only ID and treatment status
Intervention <- rbind(Intervention1, Intervention2)

#add columns time_point 1(round1 and round4), 2(round 2 and round 5), 3(round 3 and round6)
#Since subjects in two semester are different userID could be renamed to unique ID 
#extract id number from ID, add A to Id numbers for subjects in 1 st semester or B for 2nd semester 

R1 <- mutate(R1, time_point = 1, ID = paste0('A', substr(userid, 5, 7)))
R2 <- mutate(round2_updated, time_point = 2, ID = paste0('A', substr(userid, 5, 7)))
R3 <- mutate(round3_updated, time_point = 3, ID = paste0('A', substr(userid, 5, 7)))
R4 <- mutate(round4_updated, time_point = 1, ID = paste0('B', substr(userid, 5, 7)))
R5 <- mutate(R5, time_point = 2, ID = paste0('B', substr(userid, 5, 7)))
R6 <- mutate(round6_updated, time_point = 3, ID = paste0('B', substr(userid, 5, 7)))

#combine all data into one whole dataset
R_all <- rbind(R1, R2, R3, R4, R5, R6)

#Merge Intervention into R_all dataset
data <- merge(R_all, Intervention, by = "ID")

#double check treatment status right
#data$treatment[data$ID == "B9"]

#subset color trick
colorTrick <- subset(
  data,
  subset = game_name %in% c("color trick 1",
                            "color trick 2",
                            "color trick 3"),
  select = c(
    game_name,
    ID,
    event_type,
    is_response_correct,
    response_reaction_time,
    correct_count,
    incorrect_count,
    user_response,
    average_reaction_time,
    level_total_time,
    mechanic_name,
    time_point,
    treatment
  )
)
color <- colorTrick %>% filter(event_type == "scorecard") %>% 
  select(game_name, time_point, treatment,
         ID,
         correct_count,
         incorrect_count,
         average_reaction_time,
         time_point)



color <- subset(
  colorTrick[[event_type != "scorecard"]],
  subset = game_name %in% c("color trick 1",
                            "color trick 2",
                            "color trick 3"),
  select = c(
    game_name,
    ID,
    event_type,
    is_response_correct,
    response_reaction_time,
    correct_count,
    incorrect_count,
    user_response,
    average_reaction_time,
    #level_total_time,
    mechanic_name,
    time_point,
    treatment
  )
)


# ○	Number correct (ncorrect) ---mean # of correct for each subject
# ○	Number incorrect (nincorrect) ---- mean # of incorrect for each subject
# ○	Average Reaction Time (RT) for ncorrect (RT_ncorrect)
# ■	Plot RT to visualize for each participant? For each sample?

# Since average reaction time doesn't include false response reaction time, 
# average reaction time(in scorecard) could be directly used

#select variables of interest
q <- quick %>% filter(event_type == "scorecard") %>% 
  mutate(#RT_ncorrect = round(average_reaction_time/correct_count, 4),
    correct_rate = (correct_count/(correct_count + incorrect_count))) %>% 
  select(game_name, ID, time_point, treatment, 
         level_total_time)
#correct_rate)

#change time_point, and ID into factors
color1[, c(1:4)] <- lapply(color1[, c(1:4)], function(x) as.factor(x))
str(color1)

#check sample size
#sum(color$semester == '1')                  #sample size for semester1 --- 50
#sum(color$semester == '2')                  #sample size for semester2 --- 38
#sum(color$time_point == '1')                #sample size for time 1    --- 32
#sum(color$time_point == '2')                #sample size for time 2    --- 29
#sum(color$time_point == '3')                #sample size for time 3    --- 27

# check subjects for each sample in 1st semester
t1 <- color$ID[color$time_point == '1']; length(unique(t2))
# 1st time point 31
t2 <- color$ID[color$time_point == '2'] 
#2nd time point 29
t3 <- color$ID[color$time_point == '3'] 
# 3rd time point 27
t_com <- intersect(intersect(t1, t2), t3)   #24 same subjects in three time points


#Reduce(intersect, list(s1_com, s2_com)) unique(s2t1)



color1 <- color %>% filter(game_name=='color trick 1') %>% select(game_name, time_point, treatment,
                                                                  ID,
                                                                  correct_count,
                                                                  incorrect_count,
                                                                  average_reaction_time,
                                                                  time_point)

#Change outliers into normal range
for (i in 7:7) {
  for (j in 1:nrow(color1)) {
    if (color1[[j, i]] > quantile(color1[[i]], 0.75) + 1.5*IQR(color1[[i]])) {
      if (i == 7 ) {
        color1[[j, i]] <- round(mean(color1[[i]]), digits = 2)
      } else {
        color1[[j, i]] <- round(mean(color1[[j, i]]), digits = 3)
      }
    }
  }
}


# #plot
attach(color1)
par(mfrow = c(1,2))
plot(average_reaction_time, col = 'blue')
boxplot(average_reaction_time ~ treatment,pch = 19, col = "#ee1311", data = color1)
boxplot(average_reaction_time ~ time_point, pch = 19, col = "#01ff33", data = color1)


#RT by subject
p1 <- ggplot(color, aes(x = game_name, y = average_reaction_time)) +
  geom_boxplot(col='#12ff11', fill="#666666") +
  labs(title = 'ART by Game Name', x='Game', y = 'Average Reaction Time')
#** RT for each subject different, some outliers

#RT by time point
p2 <- ggplot(color, aes(x = game_name, y = average_reaction_time)) +
  geom_point(col='#ff1222')
#** RT for 3 time points looks different



grid.arrange(p1, p2, ncol = 2)

#-----------------------  check population means for time_point - ----------------------
color %>% select(time_point, RT_ncorrect) %>% 
  group_by(time_point) %>% 
  summarise(mean = mean(RT_ncorrect))
#** RT for time point 1-2, 1-3 looks different and then do a test to check

attach(color)
#ANOVA test to confirm difference 
lm1 <- lm(average_reaction_time ~ game_name + treatment, data = color)
summary(lm1)
#** A small p-value indicates mean reaction time for time points significantly diff.

#Check assumptions
shapiro.test(lm1$residuals)    #Normality check    
#** small p-value means normality assumption is violated

#Non-parametric test
wilcox.test(average_reaction_time~treatment, data = color1, conf.level= 0.95)

kruskal.test(average_reaction_time~game_name, data = color)
#** A small p-value indicates mean reaction time for time points significantly diff.

#Pairwise test to check which groups different
pairwise.wilcox.test(average_reaction_time, game_name, data=color)
#p.adjust.method = "bonferroni"
#** Pairwise comparison test shows RT for time point 1 and 3 significantly different, other two groups no sig. diff.



#####----------------------------------- Oct 12 ---------------------------#####
#Spaghetti plots
color %>% ggplot(aes(x= time_point, y=RT_ncorrect, group = ID)) + 
  geom_point() +
  geom_line() +
  facet_wrap(.~treatment) + 
  ylab("Mean RT per Correct Response") +
  xlab("Time Point") 


#lattice plot
xyplot(response_reaction_time ~ time_point | ID, data = color, as.table = T,
       xlab = "Time Point", ylab = "Mean RT per Correct Response", grid = F, 
       pch = 20,  type = c("p", "r"), col.line = "chocolate", lwd = 2, lty = 1)


#++++++++++++++++++++++++++++++++   Oct 16   ++++++++++++++++++++++++++++++++++#
#3 time points should have the same subjects, remove subjects join less than 3 times
colr <- colorT %>% filter(ID %in% t_com)
colr[, c(1, 2, 7)] <- lapply(colr[, c(1, 2, 7)], function(x) as.factor(x))
color3 <- color %>% filter(ID %in% t_com)
color3[, c(1, 2, 6)] <- lapply(color2[, c(1, 2, 6)], function(x) as.factor(x))

lm <- lm(level_total_time ~ treatment + time_point, data = data)

summary(lm)
#Consider reaction time as response, game name, students, treatment, time point could be predictors
shapiro.test(lm$residuals)
#plot the data to see individual slopes to check whether time has fixed or random slope


attach(colr)
xyplot(RT_ncorrect ~ treatment|ID, type = c("p", "r"))

#Fit a model---Null model
library(nlme)
mod1 <- lme(response_reaction_time ~  treatment + game_name, random =~-1|ID, method = 'ML')
summary(mod1)       # Check if time_point significant
intervals(mod1)

#Fit a model---response = average. reaction time, repeated measure=time_point
#predictors:game_name, treatment
#within <- plm(RT_ncorrect ~ treatment + )


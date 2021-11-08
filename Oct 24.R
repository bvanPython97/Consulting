#-------------- Script for Neuro Game Results-------------------#
# Qing Gong
# 16th OC 2021

#load libraries

library(tidyverse);library(ggplot2);library(gridExtra);library(lattice);

#read in data
setwd("/Users/lutznm/Consulting")

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
Intervention1 <- read.csv("NeurUX.Intervention.Group.csv") %>% 
  slice(2:15) %>% 
  mutate(ID = paste0('B', substr(NeurUX.ID, 5, 7))) %>% 
  select(treatment= Intervention.Group, ID)

Intervention2 <- read.csv("NeurUX.Intervention.Group.csv")%>%
  slice(18:36) %>% 
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


#update OCT 24
#subset color trick
colorTrick <- data %>% select (
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
) %>%
  filter(game_name %in% c("color trick 1",
                          "color trick 2",
                          "color trick 3"))


#calculate average reaction time for each subject at each time point for each color trick
colorT <- colorTrick %>% filter(event_type == "trial" &
                          is_response_correct == TRUE) %>%
  group_by(treatment,game_name, ID, time_point) %>%
  summarise('average_RT' = sprintf("%0.2f", mean(response_reaction_time)),
            .groups = 'drop')

#change time_point, and ID into factors
colorT[, c(1:4)] <- lapply(colorT[, c(1:4)], function(x) as.factor(x))
#change average reaction time into numeric
colorT$average_RT <- as.numeric(colorT$average_RT)
str(colorT)
 
#EAD
par(mfrow = c(2,2))
#check average reaction time(RT) distribution
plot(colorT$average_RT, col = 'blue', main = "Mean RT Dist.",
     ylab="Average raction time")    #looks not normal

#check average RT for 3 color tricks
plot(average_RT ~ game_name, data = colorT)

#check average RT for intervention group and control group
plot(average_RT ~ treatment,pch = 19, col = "#ee1311", data = colorT)   

#check average RT for time points
plot(average_RT ~ time_point, pch = 19, col = "#01ff33", data = colorT)
          
#ANOVA test to confirm difference 
lm1 <- lm(average_RT ~ game_name, data = colorT)
anova(lm1)
#** A small p-value indicates mean reaction time for 3 color tricks significantly diff.

#Check assumptions
shapiro.test(lm1$residuals)    #Normality check    
#** small p-value means normality assumption is violated, non-parametric methods required
#These small p values may also be arising because we have violated the assumption of independence
#of observations. Our observations all come from the same individuals and are therefore strongly
#associated with one another. Beyond that, there are some outliers that may be affecting p

colorTresid <- cbind(colorT, lm1$residuals)

str(colorTresid)

colorTresid%>%
  ggplot(aes(x = game_name, y = lm1$residuals, fill = game_name))+
  geom_violin(alpha = .1)+
  geom_boxplot(alpha = .5)

colorTresid%>%
  ggplot(aes(x = game_name, y = lm1$residuals, fill = game_name))+
  geom_violin(alpha = .1)+
  geom_boxplot(alpha = .5)+
  facet_grid(treatment ~ time_point)+
  geom_hline(yintercept = 0)

#This plot shows us the residual differences are based on both timepoint and control vs.
#intervention. Many of the positive residuals are coming from control group in time 1.

#Non-parametric tests
wilcox.test(average_RT ~ treatment, data = colorT, conf.level= 0.95)  #difference for two groups
#**P-value shows sig. different for treatment and control group

kruskal.test(average_RT ~ game_name, data = colorT)                   #difference for three groups
#** A small p-value indicates mean reaction time for 3 color tricks significantly diff.

#Pairwise test to check which groups different
pairwise.wilcox.test(colorT$average_RT, colorT$game_name, 
                     p.adjust.method ="bonf")
#** p-values shows significantly different for any two groups

kruskal.test(average_RT ~ time_point, data = colorT)    #sig. different for 3 time points

pairwise.wilcox.test(colorT$average_RT, colorT$time_point, 
                     p.adjust.method ="bonf")         
#only time point 1 and time point 3 significantly different


#Change outliers into normal range
for (i in 5:5) {
  for (j in 1:nrow(colorT)) {
    if (colorT[[j, i]] > quantile(colorT[[i]], 0.75) + 1.5*IQR(colorT[[i]])) {
      if (i == 5 ) {
        colorT[[j, i]] <- round(mean(colorT[[i]]), digits = 1)
      } else {
        colorT[[j, i]] <- round(mean(colorT[[j, i]]), digits = 2)
      }
    }
  }
}
class(colorT)

#plot to see individual slopes
library(lattice)
xyplot(average_RT~time_point|ID, data = colorT, type = c("p", "r"))




#Fit a model---Null model
library(nlme)
attach(colorT)
mod1 <- lme(average_RT ~ treatment, random =~time_point|ID, method = 'ML')
summary(mod1)       # Check if time_point significant
intervals(mod1)

#The model above did not converge for me. Trying a slightly different method below

#Setting up a no growth model first (separating by terms in the MLM equations)
model.a <- nlme(average_RT ~ beta_0 + d_1i,
                data = colorT,
                fixed = beta_0 ~ 1,
                random = d_1i ~ 1,
                group = ~ ID,
                start = c(beta_0 = 140),
                na.action = "na.omit")
#We don't get a ton of information from this test (it is similar to a repeated measures ANOVA)
#Our beta tells us whether our intercept is significantly different from zero (not surprising
#that it is significant)
summary(model.a)

#We save this value for later when we want to do model comparison
ModelaLL<-2*model.a$logLik

#We can add the fitted values to our original dataset to show how this model fits our data
colorT <- mutate(colorT, ngFixed = data.frame(model.a$fitted)$fixed,
                 tp_cont = as.numeric(time_point),
                 treat_cont = as.numeric(treatment)-1)

#We can then use ggplot to show this model and our (jittered) data across timepoints
colorT%>%
  ggplot()+
  geom_jitter(aes(x = tp_cont, y = average_RT), width = 0.01, alpha = .5)+
  geom_smooth(aes(x = tp_cont, y = ngFixed), method = "lm", show.legend = TRUE)

# #We will now move on to the linear growth model (not converging at this point)
# model.b <- nlme(average_RT ~ (beta_00 + d_0i) + (beta_10 + d_1i)*tp_cont,
#                 data = colorT,
#                 fixed = beta_00 + beta_10 ~ 1,
#                 random = d_0i + d_1i ~ 1,
#                 group = ~ ID,
#                 start = c(beta_00 = 130, beta_10 = -.25),
#                 na.action = "na.omit",
#                 control = nlmeControl(msMaxIter = 1000))
# #Summary of model estimates
# summary(model.b)
# 
# #Getting logLikelihood for comparisons
# ModelbLL<-2*model.b$logLik
# 
# #Comparing to previous model, we see the additional parameter (incorporation of time)
# #significantly improves model fit. Reaction times do decrease on average as a function of time
# anova(model.a, model.b)
# 
# #Again, we can bring the fitted values into the original dataset
# colorT <- mutate(colorT, linFixed = data.frame(model.b$fitted)$fixed)
# 
# #We now add the fitted line for model b to our earlier plot
# colorT%>%
#   ggplot()+
#   geom_jitter(aes(x = tp_cont, y = average_RT), width = 0.01, alpha = .5)+
#   geom_smooth(aes(x = tp_cont, y = ngFixed), method = "lm")+
#   geom_smooth(aes(x = tp_cont, y = linFixed), method = "lm", color = "red")

#Can't get this to converge. Need to work on it later and make sure it is correct
# #We will now move on to the linear growth model and include intervention
# model.c <- nlme(average_RT ~ (beta_00 + beta_01*treat_cont + d_0i) + 
#                   (beta_10 + beta_11*treat_cont + d_1i)*tp_cont,
#                 data = colorT,
#                 fixed = beta_00 + beta_01 + beta_10 + beta_11 ~ 1,
#                 random = d_0i + d_1i ~ 1,
#                 group = ~ ID,
#                 start= c(beta_00 = 1694, beta_01 = -105, beta_10 = -184, beta_11= 16),
#                 na.action = "na.omit",
#                 control = nlmeControl(maxIter = 5000, tolerance = 200, msMaxIter = 1000),
#                 verbose = TRUE)
# #Summary of model estimates
# summary(model.c)

#OCT16
#d1 <- subset(data, game_name == "color trick 1" & ID == "A1" & is_response_correct == TRUE)
#sum(d1$response_reaction_time, na.rm = TRUE)



color <- colorTrick_full %>% filter(event_type == "scorecard") %>% 
  select(game_name, time_point, treatment,
         ID,
         correct_count,
         incorrect_count,
         average_reaction_time,
         time_point)

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



color1 <- colorT %>% filter(game_name=='color trick 1') %>% select(game_name, time_point, treatment,
                                                                  ID,
                                                                  correct_count,
                                                                  incorrect_count,
                                                                  average_reaction_time,
                                                                  time_point)


# #plot
attach(color1)
par(mfrow = c(1,3))
plot(colorT$average_RT, col = 'blue')
plot(average_RT ~ treatment,pch = 19, col = "#ee1311", data = colorT)
plot(average_RT ~ time_point, pch = 19, col = "#01ff33", data = colorT)


#RT by subject
p1 <- ggplot(colorT, aes(x = treatment, y = average_RT)) +
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


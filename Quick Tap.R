library(tidyverse)

df <- read.csv("1204.Merge.csv")

str(df)

QT <- filter(df, game_name == "quick tap level 2")

str(QT)

#This plot shows that people in the control group did worse on this task over time,
#while the intervention group was stable across timepoints
QT%>%
  ggplot(aes(x = timepoint, y = tot.correct)) +
  geom_jitter(alpha = .5, width = .1)+
  geom_smooth(method = "lm", formula = "y ~ x")+
  facet_wrap(.~Int.Fac)

QT$ID.Fac <- factor(QT$ID)

QT%>%
  ggplot(aes(x = timepoint, y = tot.correct, color = ID.Fac))+
  geom_point()+
  geom_smooth(se = FALSE,
              method = "lm",
              formula = "y ~ x",
              size = .5)+
  facet_wrap(.~Int.Fac)+
  theme(legend.position = "none")

ContPlot <-
QT%>%
  filter(Int.Fac == "Control")%>%
  ggplot(aes(x = timepoint, y = tot.correct))+
  geom_point()+
  geom_smooth(se = FALSE,
              method = "lm",
              formula = "y ~ x",
              lty = "dashed",
              size = .5)+
  facet_wrap(.~ID.Fac)

IntPlot <-
QT%>%
  filter(Int.Fac == "Intervention")%>%
  ggplot(aes(x = timepoint, y = tot.correct))+
  geom_point()+
  geom_smooth(se = FALSE,
              method = "lm",
              formula = "y ~ x",
              lty = "dashed",
              size = .5)+
  facet_wrap(.~ID.Fac)

library(ggpubr)

ggarrange(ContPlot, IntPlot,
          labels = c("Control Trajectories", "Intervention Trajectories"),
          nrow = 2)

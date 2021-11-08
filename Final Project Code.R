#loading tidyverse for cleaning/viz
library(tidyverse)
library(naniar)

#loading data
long<-read.csv("/Users/lutznm/OneDrive - Loyola University Chicago/Longitudinal Data Analysis/Final Project/Data/LongDataFinal.csv")
wide<-read.csv("/Users/lutznm/OneDrive - Loyola University Chicago/Longitudinal Data Analysis/Final Project/Data/WideDataFinal.csv")

#Checking formats of variables (focusing on long for now)
str(long)
str(wide)

#reformatting necessary variables
long<-mutate(long,
             ID=as.factor(ID),
             Caregiver=as.factor(Caregiver),
             Gender=factor(Gender, levels=c(0, 1), labels=c("Male", "Female")),
             PostPDI=factor(PostPDI, levels=c(0, 1), labels=c("Pre-PDI", "Post-PDI")),
             ADHD_Missing=factor(ADHD_Missing, levels=c(0, 1), labels=c("Not Missing", "Missing")),
             Date=as.Date(Date, "%m/%d/%Y"),
             )

long<-select(long, ID:DaysSincePDI_C)

#Scanning for missing values as coded in SPSS
miss_scan_count(long, search = list(-99))

#Found missing values in BDs, ECBITotal, ECBIInatt, and ADHDBase, changing to NA for R's sake
long<-mutate(long, BDs=ifelse(BDs == -99, NA, BDs), 
             ECBITotal=ifelse(ECBITotal == -99, NA, ECBITotal),
             ECBIInatt=ifelse(ECBIInatt == -99, NA, ECBIInatt),
             ADHDBase=ifelse(ADHDBase == -99, NA, ADHDBase),
             )

#Missing value information now matches what we know from original data
miss_var_summary(long)

#Viewing data formats
str(long)

#Creating treatment week variable
long<-mutate(long, TXweek = round(DaysSinceCDI/7))
long<-mutate(long, TXfactor=as.factor(TXweek))

#Visualizing average trajectories
long%>%
  ggplot(aes(x=DaysSinceCDI, y=ECBITotal, fill=TXfactor))+
  geom_boxplot(alpha=.5)

#visualizing trajectories as a function of date (not ideal and a bit crowded)
long%>%
  ggplot(aes(x=Date, y=ECBITotal, color = ID))+
  geom_line()

long%>%
  ggplot(aes(x=Date, y=ECBITotal))+
  geom_line()+
  facet_wrap(.~ID)

long%>%
  ggplot(aes(x=Date, y=ECBIInatt, color=ID))+
  geom_line()

long%>%
  ggplot(aes(x=Date, y=ECBIInatt))+
  geom_line()+
  facet_wrap(.~ID)

long%>%
  ggplot(aes(x=Date, y=BDs, color=ID))+
  geom_line()

#Doing this again with days in treatment, which looks a bit more like the 
#lattice plots that come from S&W

long%>%
  ggplot(aes(x = DaysSinceCDI, y = ECBITotal))+
  geom_line()+
  facet_wrap(.~ID)

long%>%
  ggplot(aes(x = DaysSinceCDI, y = ECBITotal))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  facet_wrap(.~ID)

#CAN IGNORE COMMENTED OUT SECTION BELOW. DID NOT USE. PICKS UP ON LINE 464

# #Creating variable for year of study (all become 01-01-YYYY)
# library(lubridate)
# long<-mutate(long, Year=round_date(Date, unit="year"))
# 
# #Filtering by year for visualizations
# 
# #2006 Only
# Y2006<-filter(long, Year=="2006-01-01")
# Y2006%>%
#   ggplot(aes(x=Date, y=BDs, color=ID))+
#   geom_line()
# 
# Y2006%>%
#   ggplot(aes(x=DaysSinceCDI, y=BDs, color=ID))+
#   geom_line()
# 
# #2007 Only
# Y2007<-filter(long, Year=="2007-01-01")
# Y2007%>%
#   ggplot(aes(x=Date, y=BDs, color=ID))+
#   geom_line()
# Y2007%>%
#   ggplot(aes(x=DaysSinceCDI, y=BDs, color=ID))+
#   geom_line()
# 
# #2008 Only
# Y2008<-filter(long, Year=="2008-01-01")
# Y2008%>%
#   ggplot(aes(x=Date, y=BDs, color=ID))+
#   geom_line()
# Y2008%>%
#   ggplot(aes(x=DaysSinceCDI, y=BDs, color=ID))+
#   geom_line()
# 
# #2009 Only
# Y2009<-filter(long, Year=="2009-01-01")
# Y2009%>%
#   ggplot(aes(x=Date, y=BDs, color=ID))+
#   geom_line()
# Y2009%>%
#   ggplot(aes(x=DaysSinceCDI, y=BDs, color=ID))+
#   geom_line()
# 
# #2010 Only
# Y2010<-filter(long, Year=="2010-01-01")
# Y2010%>%
#   ggplot(aes(x=Date, y=BDs, color=ID))+
#   geom_line()
# Y2010%>%
#   ggplot(aes(x=DaysSinceCDI, y=BDs, color=ID))+
#   geom_line()
# 
# #2011 Only
# Y2011<-filter(long, Year=="2011-01-01")
# Y2011%>%
#   ggplot(aes(x=Date, y=BDs, color=ID))+
#   geom_line()
# Y2011%>%
#   ggplot(aes(x=DaysSinceCDI, y=BDs, color=ID))+
#   geom_line()
# 
# #2012 Only
# Y2012<-filter(long, Year=="2012-01-01")
# Y2012%>%
#   ggplot(aes(x=Date, y=BDs, color=ID))+
#   geom_line()
# Y2012%>%
#   ggplot(aes(x=DaysSinceCDI, y=BDs, color=ID))+
#   geom_line()
# 
# #2013 Only
# Y2013<-filter(long, Year=="2013-01-01")
# Y2013%>%
#   ggplot(aes(x=Date, y=BDs, color=ID))+
#   geom_line()
# Y2013%>%
#   ggplot(aes(x=DaysSinceCDI, y=BDs, color=ID))+
#   geom_line()
# 
# #2014 Only
# Y2014<-filter(long, Year=="2014-01-01")
# Y2014%>%
#   ggplot(aes(x=Date, y=BDs, color=ID))+
#   geom_line()
# Y2014%>%
#   ggplot(aes(x=DaysSinceCDI, y=BDs, color=ID))+
#   geom_line()
# 
# #2015 Only
# Y2015<-filter(long, Year=="2015-01-01")
# Y2015%>%
#   ggplot(aes(x=Date, y=BDs, color=ID))+
#   geom_line()
# Y2015%>%
#   ggplot(aes(x=DaysSinceCDI, y=BDs, color=ID))+
#   geom_line()
# 
# #2016 Only
# Y2016<-filter(long, Year=="2016-01-01")
# Y2016%>%
#   ggplot(aes(x=Date, y=BDs, color=ID))+
#   geom_line()
# Y2016%>%
#   ggplot(aes(x=DaysSinceCDI, y=BDs, color=ID))+
#   geom_line()
# 
# #2017 Only
# Y2017<-filter(long, Year=="2017-01-01")
# Y2017%>%
#   ggplot(aes(x=Date, y=BDs, color=ID))+
#   geom_line()
# Y2017%>%
#   ggplot(aes(x=DaysSinceCDI, y=BDs, color=ID))+
#   geom_line()
# 
# #2018 Only
# Y2018<-filter(long, Year=="2018-01-01")
# Y2018%>%
#   ggplot(aes(x=Date, y=BDs, color=ID))+
#   geom_line()
# Y2018%>%
#   ggplot(aes(x=DaysSinceCDI, y=BDs, color=ID))+
#   geom_line()
# 
# #2019 Only
# Y2019<-filter(long, Year=="2019-01-01")
# Y2019%>%
#   ggplot(aes(x=Date, y=BDs, color=ID))+
#   geom_line()
# Y2019%>%
#   ggplot(aes(x=DaysSinceCDI, y=BDs, color=ID))+
#   geom_line()
# 
# #Running again for ECBI Totals
# 
# #2006 Only
# Y2006<-filter(long, Year=="2006-01-01")
# Y2006%>%
#   ggplot(aes(x=Date, y=ECBITotal, color=ID))+
#   geom_line()
# 
# Y2006%>%
#   ggplot(aes(x=DaysSinceCDI, y=ECBITotal, color=ID))+
#   geom_line()
# 
# #2007 Only
# Y2007<-filter(long, Year=="2007-01-01")
# Y2007%>%
#   ggplot(aes(x=Date, y=ECBITotal, color=ID))+
#   geom_line()
# Y2007%>%
#   ggplot(aes(x=DaysSinceCDI, y=ECBITotal, color=ID))+
#   geom_line()
# 
# #2008 Only
# Y2008<-filter(long, Year=="2008-01-01")
# Y2008%>%
#   ggplot(aes(x=Date, y=ECBITotal, color=ID))+
#   geom_line()
# Y2008%>%
#   ggplot(aes(x=DaysSinceCDI, y=ECBITotal, color=ID))+
#   geom_line()
# 
# #2009 Only
# Y2009<-filter(long, Year=="2009-01-01")
# Y2009%>%
#   ggplot(aes(x=Date, y=ECBITotal, color=ID))+
#   geom_line()
# Y2009%>%
#   ggplot(aes(x=DaysSinceCDI, y=ECBITotal, color=ID))+
#   geom_line()
# 
# #2010 Only
# Y2010<-filter(long, Year=="2010-01-01")
# Y2010%>%
#   ggplot(aes(x=Date, y=ECBITotal, color=ID))+
#   geom_line()
# Y2010%>%
#   ggplot(aes(x=DaysSinceCDI, y=ECBITotal, color=ID))+
#   geom_line()
# 
# #2011 Only
# Y2011<-filter(long, Year=="2011-01-01")
# Y2011%>%
#   ggplot(aes(x=Date, y=ECBITotal, color=ID))+
#   geom_line()
# Y2011%>%
#   ggplot(aes(x=DaysSinceCDI, y=ECBITotal, color=ID))+
#   geom_line()
# 
# #2012 Only
# Y2012<-filter(long, Year=="2012-01-01")
# Y2012%>%
#   ggplot(aes(x=Date, y=ECBITotal, color=ID))+
#   geom_line()
# Y2012%>%
#   ggplot(aes(x=DaysSinceCDI, y=ECBITotal, color=ID))+
#   geom_line()
# 
# #2013 Only
# Y2013<-filter(long, Year=="2013-01-01")
# Y2013%>%
#   ggplot(aes(x=Date, y=ECBITotal, color=ID))+
#   geom_line()
# Y2013%>%
#   ggplot(aes(x=DaysSinceCDI, y=ECBITotal, color=ID))+
#   geom_line()
# 
# #2014 Only
# Y2014<-filter(long, Year=="2014-01-01")
# Y2014%>%
#   ggplot(aes(x=Date, y=ECBITotal, color=ID))+
#   geom_line()
# Y2014%>%
#   ggplot(aes(x=DaysSinceCDI, y=ECBITotal, color=ID))+
#   geom_line()
# 
# #2015 Only
# Y2015<-filter(long, Year=="2015-01-01")
# Y2015%>%
#   ggplot(aes(x=Date, y=ECBITotal, color=ID))+
#   geom_line()
# Y2015%>%
#   ggplot(aes(x=DaysSinceCDI, y=ECBITotal, color=ID))+
#   geom_line()
# 
# #2016 Only
# Y2016<-filter(long, Year=="2016-01-01")
# Y2016%>%
#   ggplot(aes(x=Date, y=ECBITotal, color=ID))+
#   geom_line()
# Y2016%>%
#   ggplot(aes(x=DaysSinceCDI, y=ECBITotal, color=ID))+
#   geom_line()
# 
# #2017 Only
# Y2017<-filter(long, Year=="2017-01-01")
# Y2017%>%
#   ggplot(aes(x=Date, y=ECBITotal, color=ID))+
#   geom_line()
# Y2017%>%
#   ggplot(aes(x=DaysSinceCDI, y=ECBITotal, color=ID))+
#   geom_line()
# 
# #2018 Only
# Y2018<-filter(long, Year=="2018-01-01")
# Y2018%>%
#   ggplot(aes(x=Date, y=ECBITotal, color=ID))+
#   geom_line()
# Y2018%>%
#   ggplot(aes(x=DaysSinceCDI, y=ECBITotal, color=ID))+
#   geom_line()
# 
# #2019 Only
# Y2019<-filter(long, Year=="2019-01-01")
# Y2019%>%
#   ggplot(aes(x=Date, y=ECBITotal, color=ID))+
#   geom_line()
# Y2019%>%
#   ggplot(aes(x=DaysSinceCDI, y=ECBITotal, color=ID))+
#   geom_line()
# 
# #Running one final time for ECBI Inattention scores
# 
# #2006 Only
# Y2006<-filter(long, Year=="2006-01-01")
# Y2006%>%
#   ggplot(aes(x=Date, y=ECBIInatt, color=ID))+
#   geom_line()
# 
# Y2006%>%
#   ggplot(aes(x=DaysSinceCDI, y=ECBIInatt, color=ID))+
#   geom_line()
# 
# #2007 Only
# Y2007<-filter(long, Year=="2007-01-01")
# Y2007%>%
#   ggplot(aes(x=Date, y=ECBIInatt, color=ID))+
#   geom_line()
# Y2007%>%
#   ggplot(aes(x=DaysSinceCDI, y=ECBIInatt, color=ID))+
#   geom_line()
# 
# #2008 Only
# Y2008<-filter(long, Year=="2008-01-01")
# Y2008%>%
#   ggplot(aes(x=Date, y=ECBIInatt, color=ID))+
#   geom_line()
# Y2008%>%
#   ggplot(aes(x=DaysSinceCDI, y=ECBIInatt, color=ID))+
#   geom_line()
# 
# #2009 Only
# Y2009<-filter(long, Year=="2009-01-01")
# Y2009%>%
#   ggplot(aes(x=Date, y=ECBIInatt, color=ID))+
#   geom_line()
# Y2009%>%
#   ggplot(aes(x=DaysSinceCDI, y=ECBIInatt, color=ID))+
#   geom_line()
# 
# #2010 Only
# Y2010<-filter(long, Year=="2010-01-01")
# Y2010%>%
#   ggplot(aes(x=Date, y=ECBIInatt, color=ID))+
#   geom_line()
# Y2010%>%
#   ggplot(aes(x=DaysSinceCDI, y=ECBIInatt, color=ID))+
#   geom_line()
# 
# #2011 Only
# Y2011<-filter(long, Year=="2011-01-01")
# Y2011%>%
#   ggplot(aes(x=Date, y=ECBIInatt, color=ID))+
#   geom_line()
# Y2011%>%
#   ggplot(aes(x=DaysSinceCDI, y=ECBIInatt, color=ID))+
#   geom_line()
# 
# #2012 Only
# Y2012<-filter(long, Year=="2012-01-01")
# Y2012%>%
#   ggplot(aes(x=Date, y=ECBIInatt, color=ID))+
#   geom_line()
# Y2012%>%
#   ggplot(aes(x=DaysSinceCDI, y=ECBIInatt, color=ID))+
#   geom_line()
# 
# #2013 Only
# Y2013<-filter(long, Year=="2013-01-01")
# Y2013%>%
#   ggplot(aes(x=Date, y=ECBIInatt, color=ID))+
#   geom_line()
# Y2013%>%
#   ggplot(aes(x=DaysSinceCDI, y=ECBIInatt, color=ID))+
#   geom_line()
# 
# #2014 Only
# Y2014<-filter(long, Year=="2014-01-01")
# Y2014%>%
#   ggplot(aes(x=Date, y=ECBIInatt, color=ID))+
#   geom_line()
# Y2014%>%
#   ggplot(aes(x=DaysSinceCDI, y=ECBIInatt, color=ID))+
#   geom_line()
# 
# #2015 Only
# Y2015<-filter(long, Year=="2015-01-01")
# Y2015%>%
#   ggplot(aes(x=Date, y=ECBIInatt, color=ID))+
#   geom_line()
# Y2015%>%
#   ggplot(aes(x=DaysSinceCDI, y=ECBIInatt, color=ID))+
#   geom_line()
# 
# #2016 Only
# Y2016<-filter(long, Year=="2016-01-01")
# Y2016%>%
#   ggplot(aes(x=Date, y=ECBIInatt, color=ID))+
#   geom_line()
# Y2016%>%
#   ggplot(aes(x=DaysSinceCDI, y=ECBIInatt, color=ID))+
#   geom_line()
# 
# #2017 Only
# Y2017<-filter(long, Year=="2017-01-01")
# Y2017%>%
#   ggplot(aes(x=Date, y=ECBIInatt, color=ID))+
#   geom_line()
# Y2017%>%
#   ggplot(aes(x=DaysSinceCDI, y=ECBIInatt, color=ID))+
#   geom_line()
# 
# #2018 Only
# Y2018<-filter(long, Year=="2018-01-01")
# Y2018%>%
#   ggplot(aes(x=Date, y=ECBIInatt, color=ID))+
#   geom_line()
# Y2018%>%
#   ggplot(aes(x=DaysSinceCDI, y=ECBIInatt, color=ID))+
#   geom_line()
# 
# #2019 Only
# Y2019<-filter(long, Year=="2019-01-01")
# Y2019%>%
#   ggplot(aes(x=Date, y=ECBIInatt, color=ID))+
#   geom_line()
# Y2019%>%
#   ggplot(aes(x=DaysSinceCDI, y=ECBIInatt, color=ID))+
#   geom_line()
# 
# long%>%
#   ggplot(aes(x=DaysSinceCDI, y=BDs, color=ID))+
#   geom_line()+
#   facet_wrap(.~Year)

#Next, I will fit lines using S&W's R Code
#For now, they are just linear trajectories

library(lattice)
xyplot(ECBITotal ~ DaysSinceCDI | ID, data = long, as.table = T,
       xlab = "Days in Treatment", ylab = "ECBI Total Score", grid = T, pch = 19,
       type = c("p", "r"), col.line = "darkblue", lwd = 3, lty = 4 )

xyplot(ECBIInatt ~ DaysSinceCDI | ID, data = long, as.table = T,
       xlab = "Days in Treatment", ylab = "ECBI Inattentive Score", grid = T, pch = 19,
       type = c("p", "r"), col.line = "darkblue", lwd = 3, lty = 4 )

xyplot(BDs ~ DaysSinceCDI | ID, data = long, as.table = T,
      xlab = "Days in Treatment", ylab = "Behavior Descriptions", grid = T, pch = 19,
      type = c("p", "r"), col.line = "darkblue", lwd = 3, lty = 4 )

#Can compute correlations within individual to get an idea of general linear trends
#First for ECBITotal
ECBICorrs<-
  long%>%
  group_by(ID)%>%
  summarize(COR=cor(DaysSinceCDI, ECBITotal, use="complete.obs"))

#Making this for later
ECBICorrs<-data.frame(ECBICorrs)
ECBICorrs<-mutate(ECBICorrs, Response="ECBI Total")

#Then for Inattention
InattCorrs<-
  long%>%
  group_by(ID)%>%
  summarize(COR=cor(DaysSinceCDI, ECBIInatt, use="complete.obs"))

#Making this for later
InattCorrs<-data.frame(InattCorrs)
InattCorrs<-mutate(InattCorrs, Response="ECBI Inatt")

#Finally for Behavior Descriptions
BDCorrs<-
  long%>%
  group_by(ID)%>%
  summarize(COR=cor(DaysSinceCDI, BDs, use="complete.obs"))

#Making this for later
BDCorrs<-data.frame(BDCorrs)
BDCorrs<-mutate(BDCorrs, Response="Behavior Description")

#Combining all three data frames
AllCorrs<-rbind(ECBICorrs, InattCorrs, BDCorrs)
AllCorrs<-mutate(AllCorrs, Response=as.factor(Response))

AllCorrs%>%
  group_by(Response)%>%
  summarize(min=min(COR, na.rm=TRUE),
            max=max(COR, na.rm=TRUE),
            mean=mean(COR, na.rm=TRUE), 
            SD=sd(COR, na.rm=TRUE), 
            median=median(COR, na.rm=TRUE),
            numoverzero=sum(COR>0, na.rm=TRUE), 
            propoverzero=sum(COR>0, na.rm=TRUE)/n())

#Visualizations for correlations
#Overlapping density plots
AllCorrs%>%
  ggplot(aes(x=COR, fill=Response))+
  geom_density(alpha=.5)+
  xlim(-1, 1)

#Side-by-side density plots
AllCorrs%>%
  ggplot(aes(x=COR, fill = Response))+
  geom_density(alpha = .5)+
  facet_grid(Response~.)+
  xlim(-1, 1)

##Side-by-side histograms
AllCorrs%>%
  ggplot(aes(x=COR))+
  geom_histogram()+
  facet_grid(.~Response)+
  xlim(-1, 1)

#Violin plots
AllCorrs%>%
  ggplot(aes(x=Response, y=COR, fill=Response))+
  geom_violin(alpha=.5)+
  ylim(-1, 1)

#Bpx plots
AllCorrs%>%
  ggplot(aes(x=Response, y=COR, fill=Response))+
  geom_boxplot(alpha=.5)+
  ylim(-1, 1)

#I will probably use the overlapping density plots and side-by-side boxplots in the writeup
#It is clear there is an overall trend across all three variables

#These plots show there are a few people with correlations above zero (ECBI) or below zero (BDs)
#Four people had Correlations above 0 for ECBI total
filter(ECBICorrs, COR>0)
PosCorrt<-c("1022", "1029", "1047", "1095")
PosECBIt<-filter(long, ID %in% PosCorrt)

#Isolating these individuals in the xyplot shows linear trends
xyplot(ECBITotal ~ DaysSinceCDI | ID, data = PosECBIt, as.table = T,
       xlab = "Days in Treatment", ylab = "ECBI Total Score", grid = T, pch = 19,
       type = c("p", "r"), col.line = "darkblue", lwd = 3, lty = 4 )

#Fitting these using the "spline" function shows curves fitted to the data more closely
#It appears the trend is not linear for these individuals (among others)
xyplot(ECBITotal ~ DaysSinceCDI | ID, data = PosECBIt, as.table = T,
       xlab = "Days in Treatment", ylab = "ECBI Total Score", grid = T, pch = 19,
       type = c("p", "spline"), col.line = "darkblue", lwd = 3, lty = 4 )

#Five people had correlations above 0 for ECBI inatt
filter(InattCorrs, COR>0)
PosCorri<-c("1022", "1029", "1043", "1092", "1095")
PosECBIi<-filter(long, ID %in% PosCorri)

#Isolating these individuals in the xyplot shows linear trends
xyplot(ECBIInatt ~ DaysSinceCDI | ID, data = PosECBIi, as.table = T,
       xlab = "Days in Treatment", ylab = "ECBI Inattentive Score", grid = T, pch = 19,
       type = c("p", "r"), col.line = "darkblue", lwd = 3, lty = 4 )

#Fitting these using the "spline" function shows curves fitted to the data more closely
#It appears the trend is not linear for these individuals (among others)
xyplot(ECBIInatt ~ DaysSinceCDI | ID, data = PosECBIi, as.table = T,
       xlab = "Days in Treatment", ylab = "ECBI Inattentive Score", grid = T, pch = 19,
       type = c("p", "spline"), col.line = "darkblue", lwd = 3, lty = 4 )

#Fifteen people had negative correlations for BDs
filter(BDCorrs, COR<0)
PosCorrB<-c("944", "968", "990", "1022", "1024", "1026", "1028", "1043", "1053",
            "1055", "1056", "1067", "1069", "1087", "1095")
PosBD<-filter(long, ID %in% PosCorrB)

#Isolating these individuals in the xyplot shows linear trends
xyplot(BDs ~ DaysSinceCDI | ID, data = PosBD, as.table = T,
       xlab = "Days in Treatment", ylab = "Behavior Description", grid = T, pch = 19,
       type = c("p", "r"), col.line = "darkblue", lwd = 3, lty = 4 )

#Fitting these using the "spline" function shows curves fitted to the data more closely
#It appears the trend is not linear for these individuals (among others)
#It also appears some of these trends may indeed be downward
xyplot(BDs ~ DaysSinceCDI | ID, data = PosBD, as.table = T,
       xlab = "Days in Treatment", ylab = "Behavior Description", grid = T, pch = 19,
       type = c("p", "spline"), col.line = "darkblue", lwd = 3, lty = 4 )

#Theory suggests there should be a difference between CDI and PDI, so my next step is separating and viewing trends as above
CDIdf<-filter(long, PostPDI=="Pre-PDI")
PDIdf<-filter(long, PostPDI=="Post-PDI")

#Next, I will fit lines using S&W's R Code for each segment of treatment
#For now, they are just linear trajectories

xyplot(ECBITotal ~ DaysSinceCDI | ID, data = CDIdf, as.table = T,
       xlab = "Days in Treatment", ylab = "ECBI Total Score", grid = T, pch = 19,
       type = c("p", "r"), col.line = "darkblue", lwd = 3, lty = 4 )

xyplot(ECBITotal ~ DaysSinceCDI | ID, data = PDIdf, as.table = T,
       xlab = "Days in Treatment", ylab = "ECBI Total Score", grid = T, pch = 19,
       type = c("p", "r"), col.line = "darkblue", lwd = 3, lty = 4 )

xyplot(ECBIInatt ~ DaysSinceCDI | ID, data = CDIdf, as.table = T,
       xlab = "Days in Treatment", ylab = "ECBI Inattentive Score", grid = T, pch = 19,
       type = c("p", "r"), col.line = "darkblue", lwd = 3, lty = 4 )

xyplot(ECBIInatt ~ DaysSinceCDI | ID, data = PDIdf, as.table = T,
       xlab = "Days in Treatment", ylab = "ECBI Inattentive Score", grid = T, pch = 19,
       type = c("p", "r"), col.line = "darkblue", lwd = 3, lty = 4 )

xyplot(BDs ~ DaysSinceCDI | ID, data = CDIdf, as.table = T,
       xlab = "Days in Treatment", ylab = "Behavior Descriptions", grid = T, pch = 19,
       type = c("p", "r"), col.line = "darkblue", lwd = 3, lty = 4 )

xyplot(BDs ~ DaysSinceCDI | ID, data = PDIdf, as.table = T,
       xlab = "Days in Treatment", ylab = "Behavior Descriptions", grid = T, pch = 19,
       type = c("p", "r"), col.line = "darkblue", lwd = 3, lty = 4 )

#Can compute correlations within individual to get an idea of general linear trends
#First for ECBITotal
CDIECBICorrs<-
  CDIdf%>%
  group_by(ID)%>%
  summarize(COR=cor(DaysSinceCDI, ECBITotal, use="complete.obs"))

PDIECBICorrs<-
  PDIdf%>%
  group_by(ID)%>%
  summarize(COR=cor(DaysSinceCDI, ECBITotal, use="complete.obs"))

#Making this for later
CDIECBICorrs<-data.frame(CDIECBICorrs)
CDIECBICorrs<-mutate(CDIECBICorrs, Response="ECBI Total")
PDIECBICorrs<-data.frame(PDIECBICorrs)
PDIECBICorrs<-mutate(PDIECBICorrs, Response="ECBI Total")


#Then for Inattention
CDIInattCorrs<-
  CDIdf%>%
  group_by(ID)%>%
  summarize(COR=cor(DaysSinceCDI, ECBIInatt, use="complete.obs"))
PDIInattCorrs<-
  PDIdf%>%
  group_by(ID)%>%
  summarize(COR=cor(DaysSinceCDI, ECBIInatt, use="complete.obs"))

#Making this for later
CDIInattCorrs<-data.frame(CDIInattCorrs)
CDIInattCorrs<-mutate(CDIInattCorrs, Response="ECBI Inatt")
PDIInattCorrs<-data.frame(PDIInattCorrs)
PDIInattCorrs<-mutate(PDIInattCorrs, Response="ECBI Inatt")

#Finally for Behavior Descriptions
CDIBDCorrs<-
  CDIdf%>%
  group_by(ID)%>%
  summarize(COR=cor(DaysSinceCDI, BDs, use="complete.obs"))

PDIdf2<-subset(PDIdf, ID != 1023)

PDIBDCorrs<-
  PDIdf2%>%
  group_by(ID)%>%
  summarize(COR=cor(DaysSinceCDI, BDs, use="complete.obs"))

#Making this for later
CDIBDCorrs<-data.frame(CDIBDCorrs)
CDIBDCorrs<-mutate(CDIBDCorrs, Response="Behavior Description")
PDIBDCorrs<-data.frame(PDIBDCorrs)
PDIBDCorrs<-mutate(PDIBDCorrs, Response="Behavior Description")

#Combining all three data frames
CDIAllCorrs<-rbind(CDIECBICorrs, CDIInattCorrs, CDIBDCorrs)
CDIAllCorrs<-mutate(CDIAllCorrs, Response=as.factor(Response))
PDIAllCorrs<-rbind(PDIECBICorrs, PDIInattCorrs, PDIBDCorrs)
PDIAllCorrs<-mutate(PDIAllCorrs, Response=as.factor(Response))


CDIAllCorrs%>%
  group_by(Response)%>%
  summarize(min=min(COR, na.rm=TRUE),
            max=max(COR, na.rm=TRUE),
            mean=mean(COR, na.rm=TRUE), 
            SD=sd(COR, na.rm=TRUE), 
            median=median(COR, na.rm=TRUE),
            numoverzero=sum(COR>0, na.rm=TRUE), 
            propoverzero=sum(COR>0, na.rm=TRUE)/n())
PDIAllCorrs%>%
  group_by(Response)%>%
  summarize(min=min(COR, na.rm=TRUE),
            max=max(COR, na.rm=TRUE),
            mean=mean(COR, na.rm=TRUE), 
            SD=sd(COR, na.rm=TRUE), 
            median=median(COR, na.rm=TRUE),
            numoverzero=sum(COR>0, na.rm=TRUE), 
            propoverzero=sum(COR>0, na.rm=TRUE)/n())


#Visualizations for correlations
#Overlapping density plots
CDIAllCorrs%>%
  ggplot(aes(x=COR, fill=Response))+
  geom_density(alpha=.5)+
  xlim(-1, 1)
PDIAllCorrs%>%
  ggplot(aes(x=COR, fill=Response))+
  geom_density(alpha=.5)+
  xlim(-1, 1)

#Bpx plots
CDIAllCorrs%>%
  ggplot(aes(x=Response, y=COR, fill=Response))+
  geom_boxplot(alpha=.5)+
  ylim(-1, 1)
PDIAllCorrs%>%
  ggplot(aes(x=Response, y=COR, fill=Response))+
  geom_boxplot(alpha=.5)+
  ylim(-1, 1)

#I will probably use the overlapping density plots and side-by-side boxplots in the writeup
#It is clear there is an overall trend across all three variables, ALTHOUGH
#BDs do not tend to increase during PDI (makes sense because this is no longer a focus. Good to see there isn't a huge dropoff)

#These plots show there are a few people with correlations above zero (ECBI) or below zero (BDs)
#Six people had Correlations above 0.3 for ECBI total (CDI)
filter(CDIECBICorrs, COR>0.3)
CDIPosCorrt<-c("957", "974", "995", "1018", "1045", "1095")
CDIPosECBIt<-filter(CDIdf, ID %in% CDIPosCorrt)

#Isolating these individuals in the xyplot shows linear trends
xyplot(ECBITotal ~ DaysSinceCDI | ID, data = CDIPosECBIt, as.table = T,
       xlab = "Days in Treatment", ylab = "ECBI Total Score", grid = T, pch = 19,
       type = c("p", "r"), col.line = "darkblue", lwd = 3, lty = 4 )

#Fitting these using the "spline" function shows curves fitted to the data more closely
#It appears the trend is not linear for these individuals (among others)
xyplot(ECBITotal ~ DaysSinceCDI | ID, data = CDIPosECBIt, as.table = T,
       xlab = "Days in Treatment", ylab = "ECBI Total Score", grid = T, pch = 19,
       type = c("p", "spline"), col.line = "darkblue", lwd = 3, lty = 4 )

#For PDI, a different group of six had positive correlations
filter(PDIECBICorrs, COR>0.3)
PDIPosCorrt<-c("963", "1022", "1023", "1029", "1044", "1064")
PDIPosECBIt<-filter(PDIdf, ID %in% PDIPosCorrt)

#Isolating these individuals in the xyplot shows linear trends
xyplot(ECBITotal ~ DaysSinceCDI | ID, data = PDIPosECBIt, as.table = T,
       xlab = "Days in Treatment", ylab = "ECBI Total Score", grid = T, pch = 19,
       type = c("p", "r"), col.line = "darkblue", lwd = 3, lty = 4 )

#Fitting these using the "spline" function shows curves fitted to the data more closely
#It appears the trend is not linear for these individuals (among others)
xyplot(ECBITotal ~ DaysSinceCDI | ID, data = PDIPosECBIt, as.table = T,
       xlab = "Days in Treatment", ylab = "ECBI Total Score", grid = T, pch = 19,
       type = c("p", "spline"), col.line = "darkblue", lwd = 3, lty = 4 )

#Seven people had correlations above 0.3 for ECBI inatt
filter(CDIInattCorrs, COR>0.3)
CDIPosCorri<-c("974", "995", "1018", "1020", "1026", "1092", "1095")
CDIPosECBIi<-filter(CDIdf, ID %in% CDIPosCorri)

#Isolating these individuals in the xyplot shows linear trends
xyplot(ECBIInatt ~ DaysSinceCDI | ID, data = CDIPosECBIi, as.table = T,
       xlab = "Days in Treatment", ylab = "ECBI Inattentive Score", grid = T, pch = 19,
       type = c("p", "r"), col.line = "darkblue", lwd = 3, lty = 4 )

#Fitting these using the "spline" function shows curves fitted to the data more closely
#It appears the trend is not linear for these individuals (among others)
xyplot(ECBIInatt ~ DaysSinceCDI | ID, data = CDIPosECBIi, as.table = T,
       xlab = "Days in Treatment", ylab = "ECBI Inattentive Score", grid = T, pch = 19,
       type = c("p", "spline"), col.line = "darkblue", lwd = 3, lty = 4 )

#For PDI, ten people had correlations above 0.3 for ECBI inatt
filter(PDIInattCorrs, COR>0.3)
PDIPosCorri<-c("944", "959", "963", "1020", "1022", "1029", "1040", "1044", "1059", "1067")
PDIPosECBIi<-filter(PDIdf, ID %in% PDIPosCorri)

#Isolating these individuals in the xyplot shows linear trends
xyplot(ECBIInatt ~ DaysSinceCDI | ID, data = PDIPosECBIi, as.table = T,
       xlab = "Days in Treatment", ylab = "ECBI Inattentive Score", grid = T, pch = 19,
       type = c("p", "r"), col.line = "darkblue", lwd = 3, lty = 4 )

#Fitting these using the "spline" function shows curves fitted to the data more closely
#It appears the trend is not linear for these individuals (among others)
xyplot(ECBIInatt ~ DaysSinceCDI | ID, data = PDIPosECBIi, as.table = T,
       xlab = "Days in Treatment", ylab = "ECBI Inattentive Score", grid = T, pch = 19,
       type = c("p", "spline"), col.line = "darkblue", lwd = 3, lty = 4 )

#Participant 1020 showed up in both of these, but not the total dataset. Their trend is unique
Unique<-filter(long, ID == "1020")
view(Unique)
#This person shifted to the second session at 71 days. Viewing this plot shows their inattentive symptoms may have worsened
#during both phases of treatment
xyplot(ECBIInatt ~ DaysSinceCDI | ID, data = Unique, as.table = T,
       xlab = "Days in Treatment", ylab = "ECBI Inattentive Score", grid = T, pch = 19,
       type = c("p", "spline"), col.line = "darkblue", lwd = 3, lty = 4 )

#One person had negative correlations for BDs
filter(CDIBDCorrs, COR<0)
CDIPosCorrB<-c("991")
CDIPosBD<-filter(CDIdf, ID %in% CDIPosCorrB)

#Isolating these individuals in the xyplot shows linear trends
xyplot(BDs ~ DaysSinceCDI | ID, data = CDIPosBD, as.table = T,
       xlab = "Days in Treatment", ylab = "Behavior Description", grid = T, pch = 19,
       type = c("p", "r"), col.line = "darkblue", lwd = 3, lty = 4 )

#The spline model doesn't add much here
#The big takeaway is that they started high (and may be an outlier in terms of starting BDs)
xyplot(BDs ~ DaysSinceCDI | ID, data = CDIPosBD, as.table = T,
       xlab = "Days in Treatment", ylab = "Behavior Description", grid = T, pch = 19,
       type = c("p", "spline"), col.line = "darkblue", lwd = 3, lty = 4 )

#For PDI, a number of people (33) had negative correlations for BDs
filter(PDIBDCorrs, COR<0)
#Even increasing the correlation to -0.5 left 15 people included
filter(PDIBDCorrs, COR< -0.5)
PDIPosCorrB<-c("959", "974", "993", "996", "1004", "1014", "1022", "1024", "1026", "1044", "1045", "1053", "1063", "1072", "1097")
PDIPosBD<-filter(PDIdf, ID %in% CDIPosCorrB)

#Isolating these individuals in the xyplot shows linear trends
xyplot(BDs ~ DaysSinceCDI | ID, data = PDIPosBD, as.table = T,
       xlab = "Days in Treatment", ylab = "Behavior Description", grid = T, pch = 19,
       type = c("p", "r"), col.line = "darkblue", lwd = 3, lty = 4 )

#The spline model doesn't add much here. There may actually be a downward trend for some people
#The big takeaway is that they started high (and may be an outlier in terms of starting BDs)
xyplot(BDs ~ DaysSinceCDI | ID, data = PDIPosBD, as.table = T,
       xlab = "Days in Treatment", ylab = "Behavior Description", grid = T, pch = 19,
       type = c("p", "spline"), col.line = "darkblue", lwd = 3, lty = 4 )


#No Growth for ECBITotal
  library(nlme)
  model.a <- lme(ECBITotal ~ 1, long, random = ~ 1|ID, method = "ML", na.action=na.omit)
  model.a <- nlme(ECBITotal ~ beta_0 + d_1i,
                  data = long,
                  fixed = beta_0 ~ 1,
                  random = d_1i ~ 1,
                  group = ~ ID,
                  start = c(beta_0 = 140),
                  na.action = "na.omit")
  summary(model.a)

ModelaLL<-2*model.a$logLik

x<-seq(0, 32, 1)
y<-rep(110.08, 33)
plot(x, y)
nogrowth<-data.frame(x, y)
nogrowth<-mutate(nogrowth, model="no growth")

#Linear Growth for ECBI Total
  model.b <- lme(ECBITotal ~ DaysSinceCDI , data = long, random = ~ DaysSinceCDI | ID,
               method = "ML", na.action=na.omit)
  model.b <- nlme(ECBITotal ~ (beta_0 + d_0i) + (beta_1 + d_1i)*DaysSinceCDI,
                  data = long,
                  fixed = beta_0 + beta_1 ~ 1,
                  random = d_0i + d_1i ~ 1,
                  group = ~ ID,
                  start= c(beta_0 = 130, beta_1 = -.25),
                  na.action = "na.omit")
  #Summary of model estimates
    summary(model.b)
  #checking available variables
   str(model.b)
  #Getting logLikelihood for comparisons
    ModelbLL<-2*model.b$logLik

  #Comparing to previous model
  anova(model.a, model.b)

  #Using model b to examine changes
  x<-seq(0, 32, 1)
  y<-130.19886-0.23362*(x*7)
  plot(x, y)
  lin<-data.frame(x, y)
  lin<-mutate(lin, model="linear")
  
#Incorporation of PDI shift in model (slope only)
  long<-mutate(long, PostPDInum = as.numeric(PostPDI))
    
  model.c <- nlme(ECBITotal ~ (beta_0 + d_0i) + (beta_1 + d_1i)*DaysSinceCDI + (beta_3 + d_3i)*DaysSincePDI_C,
                  data = long,
                  fixed = beta_0 + beta_1 + beta_3 ~ 1,
                  random = d_0i + d_1i+ d_3i ~ 1,
                  group = ~ ID,
                  start = c(beta_0 = 117, beta_1 = -.1, beta_3 = -.13),
                  na.action = "na.omit")

    summary(model.c)
    anova(model.b, model.c)
    
#Incorporation of PDI shift in model (intercept only)
    model.d <- nlme(ECBITotal ~ (beta_0 + d_0i) + (beta_1 + d_1i)*DaysSinceCDI + (beta_2+d_2i)*PostPDInum,
                    data = long,
                    fixed = beta_0 + beta_1 + beta_2 ~ 1,
                    random = d_0i + d_1i + d_2i ~ 1,
                    group = ~ ID,
                    start = c(beta_0 = 130, beta_1 = -.23, beta_2 = .45),
                    na.action = "na.omit")
  
    summary(model.d)
    anova(model.b, model.d)

  #Incorporation of PDI shift in model (intercept and slope)
    model.e <- nlme(ECBITotal ~ (beta_0 + d_0i) + (beta_1 + d_1i)*DaysSinceCDI + (beta_2+d_2i)*PostPDInum + (beta_3 + d_3i)*DaysSincePDI_C,
                    data = long,
                    fixed = beta_0 + beta_1 + beta_2 + beta_3 ~ 1,
                    random = d_0i + d_1i + d_2i + d_3i ~ 1,
                    group = ~ ID,
                    start = c(beta_0 = 123, beta_1 = -.167, beta_2 = .50, beta_3 = -.06),
                    na.action = "na.omit")
    summary(model.e)
    anova(model.b, model.e)
    anova(model.c, model.e)
    anova(model.d, model.e)
    
    #Using model e to examine changes across a hypothetical model with shift halfway through 32 week treatment
    x<-seq(0, 16, 1)
    y<-122.71847-0.16686*(x*7)+0.50134*0-0.06377*0
    pre<-data.frame(x, y)
    pre<-mutate(pre, stage="pre")
    x<-seq(16, 32, 1)
    y<-122.71847-0.16686*(x*7)+0.50134*1-0.06377*((x-16)*7)
    post<-data.frame(x, y)
    post<-mutate(post, stage="post")
    view(pre)
    view(post)
    df<-rbind(pre, post)
    df%>%
      ggplot(aes(x, y, color=stage))+
      geom_line()
      
    #Quadratic Growth for ECBI Total
    model.f<-nlme(ECBITotal ~ (beta_0 + d_0i) + (beta_1 + d_1i)*DaysSinceCDI + (beta_2 + d_2i)*DaysSinceCDI^2,
                  data = long,
                  fixed = beta_0 + beta_1 + beta_2 ~1,
                  random = d_0i+ d_1i + d_2i ~ 1,
                  group = ~ ID,
                  start = c(beta_0 = 130, beta_1 = -.25, beta_2 = 0),
                  na.action = "na.omit")
    summary(model.f)

    anova(model.b, model.f)
    
    #Using model f to examine changes
    x<-seq(0, 32, 1)
    y<-132.19304-0.31143*(x*7)+0.00051*(x*7)^2
    plot(x, y)
    quad<-data.frame(x, y)
    quad<-mutate(quad, model="quadratic")
    
#Cubic Growth for ECBI Total
  
    model.g <- nlme(ECBITotal ~ (beta_0 + d_0i) + (beta_1 + d_1i)*DaysSinceCDI + (beta_2 + d_2i) * DaysSinceCDI^2 + (beta_3 + d_3i) * DaysSinceCDI^3,
                    data = long,
                    fixed = beta_0 + beta_1 + beta_2 + beta_3 ~1,
                    random = d_0i + d_1i + d_2i + d_3i ~ 1,
                    group = ~ ID,
                    start = c(beta_0 = 130, beta_1 = -.1, beta_2 = 0.1, beta_3 = 0.1),
                    na.action = "na.omit")
    summary(model.g)
    
    anova(model.f, model.g)
    
    #Using model f to examine changes
    x<-seq(0, 32, 1)
    y<-134.21685-0.44311*(x*7)+0.00214*(x*7)^2-0.00001*(x*7)^3
    plot(x, y)
    cub<-data.frame(x, y)
    cub<-mutate(cub, model = "cubic")
    
#Logistic Model
  #alpha=ceiling-floor
  #y=alpha/(1+pi0*exp(pi1*x))+floor
  model.h<-nlme(ECBITotal ~ 216/(1+(beta_0 + d_0i)*exp((beta_1+d_1i)*DaysSinceCDI))+36,
                data = long,
                fixed = beta_0 + beta_1 ~ 1,
                random = d_0i + d_1i ~ 1,
                group = ~ ID,
                start = c(beta_0 = 1.5, beta_1 = .005),
                na.action = "na.omit")
  summary(model.h)                

  anova(model.a, model.h)
  
  #Using model h to examine changes
  x<-seq(0, 32, 1)
  y<-216/(1+1.476040*exp(0.0064771*(x*7)))+36
  plot(x, y)
  log<-data.frame(x, y)
  log<-mutate(log, model="logistic")

  #Logistic Spline Model (intercept only)
  model.i<-nlme(ECBITotal ~ (216/(1+(beta_0 + d_0i)*exp((beta_1+d_1i)*DaysSinceCDI))+36) +
                  (beta_4+d_4i)*PostPDInum,
                data = long,
                fixed = beta_0 + beta_1 + beta_4 ~ 1,
                random = d_0i + d_1i +d_4i ~ 1,
                group = ~ ID,
                start = c(beta_0 = 1.28, beta_1 = .012, beta_4 = 0.59),
                na.action = "na.omit",
                control = nlmeControl(maxIter = 100, tolerance = 500))
  summary(model.i)
  anova(model.h, model.i)
  #This model is actually worse than the regular logistic model
  
  #Logistic Spline Model (slope only)
  model.j<-nlme(ECBIInatt ~ (216/(1+(beta_0 + d_0i)*exp((beta_1+d_1i)*DaysSinceCDI)+36)) +
                  (216/(1+(beta_2 + d_2i)*exp((beta_3+d_3i)*DaysSincePDI_C)+36)),
                data = long,
                fixed = beta_0 + beta_1 + beta_2 + beta_3 ~ 1,
                random = d_0i + d_1i + d_2i + d_3i ~ 1,
                group = ~ ID,
                start = c(beta_0 = 5, beta_1 = .0000001, beta_2 = 5, beta_3 = 0.000000001),
                na.action = "na.omit",
                control = nlmeControl(maxIter = 500, pnlsMaxIter = 20, pnlsTol = .000000001, tolerance = 100, msMaxIter = 150))
  #EVEN WITH ALL OF THE ABOVE CONTROLS, THIS DID NOT CONVERGE
  #summary(model.j)
  #anova(model.h, model.j)
  
  #Logistic Spline Model (slope and intercept)
  model.k<-nlme(ECBIInatt ~ (216/(1+(beta_0 + d_0i)*exp((beta_1+d_1i)*DaysSinceCDI)+36)) +
                  (216/(1+(beta_2 + d_2i)*exp((beta_3+d_3i)*DaysSincePDI_C)+36)) +
                  (beta_4+d_4i)*PostPDInum,
                data = long,
                fixed = beta_0 + beta_1 + beta_2 + beta_3 + beta_4 ~ 1,
                random = d_0i + d_1i + d_2i + d_3i + d_4i~ 1,
                group = ~ ID,
                start = c(beta_0 = .5, beta_1 = .00005, beta_2 = .5, beta_3 = .00005, beta_4 = .01),
                na.action = "na.omit",
                control = nlmeControl(maxIter = 100, tolerance = 100, pnlsMaxIter = 30, pnlsTol = .00000000001))
  #AGAIN, THIS DID NOT CONVERGE
  #summary(model.k)
  #anova(model.h, model.k)
  #anova(model.i, model.k)
  #anova(model.j, model.k)
  
  #Comparing all model curves
  allmodels<-rbind(nogrowth, lin, quad, cub, log)
  view(allmodels)  
  df<-data.frame(allmodels)
  df<-mutate(df, model=factor(model, levels = c("no growth", "linear", "quadratic", "cubic", "logistic")))
  
  #Separate Lines
  df%>%
    ggplot(aes(x=x, y=y, color=model))+
    geom_point()+
    geom_line()+
    geom_hline(yintercept=252, color="red")+
    geom_hline(yintercept=36, color="red")
  
  #Separate Panels
    df%>%
    ggplot(aes(x=x, y=y))+
    geom_point()+
    geom_line()+
    facet_wrap(.~model)+
    geom_hline(yintercept=252, color="red")+
    geom_hline(yintercept=36, color="red")

#Examining residuals
    #Model A
    resida<-residuals(model.a, type = "normalized", asList = FALSE)
    fitteda<-fitted(model.a)
    df<-data.frame(resida, fitteda)
    #Examining histogram of residuals (note normal distribution and most residuals within 3 SDs)
    df%>%
      ggplot(aes(x=resida))+
      geom_histogram()+
      geom_vline(xintercept=3, linetype="dotted")+
      geom_vline(xintercept=-3, linetype="dotted")
    #Examining residuals vs fitted (note heteroscedasticity)
    df%>%
      ggplot(aes(x= fitteda, y=resida))+
      geom_jitter()+
      geom_hline(yintercept=3, linetype = "dotted")+
      geom_hline(yintercept= -3, linetype = "dotted")
    
    #Model B
    residb<-residuals(model.b, type = "normalized", asList = FALSE)
    fittedb<-fitted(model.b)
    df<-data.frame(residb, fittedb)
    #Examining histogram of residuals (note normal distribution and most residuals within 3 SDs)
    df%>%
      ggplot(aes(x=residb))+
      geom_histogram()+
      geom_vline(xintercept=3, linetype="dotted")+
      geom_vline(xintercept=-3, linetype="dotted")
    #Examining residuals vs fitted (note heteroscedasticity)
    df%>%
      ggplot(aes(x= fittedb, y=residb))+
      geom_jitter()+
      geom_hline(yintercept=3, linetype = "dotted")+
      geom_hline(yintercept= -3, linetype = "dotted")
    
    #Model C
    residc<-residuals(model.c, type = "normalized", asList = FALSE)
    fittedc<-fitted(model.c)
    df<-data.frame(residc, fittedc)
    #Examining histogram of residuals (note normal distribution and most residuals within 3 SDs)
    df%>%
      ggplot(aes(x=residc))+
      geom_histogram()+
      geom_vline(xintercept=3, linetype="dotted")+
      geom_vline(xintercept=-3, linetype="dotted")
    #Examining residuals vs fitted (note heteroscedasticity)
    df%>%
      ggplot(aes(x= fittedc, y=residc))+
      geom_jitter()+
      geom_hline(yintercept=3, linetype = "dotted")+
      geom_hline(yintercept= -3, linetype = "dotted")
    
    #Model D
    residd<-residuals(model.d, type = "normalized", asList = FALSE)
    fittedd<-fitted(model.d)
    df<-data.frame(residd, fittedd)
    #Examining histogram of residuals (note normal distribution and most residuals within 3 SDs)
    df%>%
      ggplot(aes(x=residd))+
      geom_histogram()+
      geom_vline(xintercept=3, linetype="dotted")+
      geom_vline(xintercept=-3, linetype="dotted")
    #Examining residuals vs fitted (note heteroscedasticity)
    df%>%
      ggplot(aes(x= fittedd, y=residd))+
      geom_jitter()+
      geom_hline(yintercept=3, linetype = "dotted")+
      geom_hline(yintercept= -3, linetype = "dotted")      
    
    #Model E
    reside<-residuals(model.e, type = "normalized", asList = FALSE)
    fittede<-fitted(model.e)
    df<-data.frame(reside, fittede)
    #Examining histogram of residuals (note normal distribution and most residuals within 3 SDs)
    df%>%
      ggplot(aes(x=reside))+
      geom_histogram()+
      geom_vline(xintercept=3, linetype="dotted")+
      geom_vline(xintercept=-3, linetype="dotted")
    #Examining residuals vs fitted (note heteroscedasticity)
    df%>%
      ggplot(aes(x= fittede, y=reside))+
      geom_jitter()+
      geom_hline(yintercept=3, linetype = "dotted")+
      geom_hline(yintercept= -3, linetype = "dotted")      
    
    #Model F
    residf<-residuals(model.f, type = "normalized", asList = FALSE)
    fittedf<-fitted(model.f)
    df<-data.frame(residf, fittedf)
    #Examining histogram of residuals (note normal distribution and most residuals within 3 SDs)
    df%>%
      ggplot(aes(x=residf))+
      geom_histogram()+
      geom_vline(xintercept=3, linetype="dotted")+
      geom_vline(xintercept=-3, linetype="dotted")
    #Examining residuals vs fitted (note heteroscedasticity)
    df%>%
      ggplot(aes(x= fittedf, y=residf))+
      geom_jitter()+
      geom_hline(yintercept=3, linetype = "dotted")+
      geom_hline(yintercept= -3, linetype = "dotted")
    
    #Model G
    residg<-residuals(model.g, type = "normalized", asList = FALSE)
    fittedg<-fitted(model.g)
    df<-data.frame(residg, fittedg)
    #Examining histogram of residuals (note normal distribution and most residuals within 3 SDs)
    df%>%
      ggplot(aes(x=residg))+
      geom_histogram()+
      geom_vline(xintercept=3, linetype="dotted")+
      geom_vline(xintercept=-3, linetype="dotted")
    #Examining residuals vs fitted (note heteroscedasticity)
    df%>%
      ggplot(aes(x= fittedg, y=residg))+
      geom_jitter()+
      geom_hline(yintercept=3, linetype = "dotted")+
      geom_hline(yintercept= -3, linetype = "dotted")
    
    #Model H
    residh<-residuals(model.h, type = "normalized", asList = FALSE)
    fittedh<-fitted(model.h)
    df<-data.frame(residh, fittedh)
    #Examining histogram of residuals (note normal distribution and most residuals within 3 SDs)
    df%>%
      ggplot(aes(x=residh))+
      geom_histogram()+
      geom_vline(xintercept=3, linetype="dotted")+
      geom_vline(xintercept=-3, linetype="dotted")
    #Examining residuals vs fitted (note heteroscedasticity)
    df%>%
      ggplot(aes(x= fittedh, y=residh))+
      geom_jitter()+
      geom_hline(yintercept=3, linetype = "dotted")+
      geom_hline(yintercept= -3, linetype = "dotted")   
    
#Next, I want to run the same models for Inattentive scores
    model.a <- lme(ECBIInatt ~ 1, long, random = ~ 1|ID, method = "ML", na.action=na.omit)
    model.a <- nlme(ECBIInatt ~ beta_0 + d_1i,
                    data = long,
                    fixed = beta_0 ~ 1,
                    random = d_1i ~ 1,
                    group = ~ ID,
                    start = c(beta_0 = 140),
                    na.action = "na.omit")
    summary(model.a)
    
    Modela2LL<-2*model.a$logLik
    
    #Using this model to examine changes (modeling the max number of weeks in the dataset (384/7))
    384/7
    x<-seq(0, 55, 1)
    y<-13.46142
    nogrowth<-data.frame(x, y)
    nogrowth<-mutate(nogrowth, model="no growth")
    plot(nogrowth$x, nogrowth$y)
    
    #Using Weeks instead of days
    long<-mutate(long, WeeksSinceCDI = DaysSinceCDI/7)
    
    #Linear Growth for ECBI Total
    model.b <- nlme(ECBIInatt ~ (beta_0 + d_0i) + (beta_1 + d_1i)*WeeksSinceCDI,
                    data = long,
                    fixed = beta_0 + beta_1 ~ 1,
                    random = d_0i + d_1i ~ 1,
                    group = ~ ID,
                    start= c(beta_0 = 130, beta_1 = -.25),
                    na.action = "na.omit")
    #Summary of model estimates
    summary(model.b)
    #checking available variables
    str(model.b)
    #Getting logLikelihood for comparisons
    Modelb2LL<-2*model.b$logLik
    c<-Modelb2LL-Modela2LL
    c
    1-pchisq(c, 3)
    
    #Comparing to previous model
    anova(model.a, model.b)
    
    #Checking residuals
    hist(model.b$residuals)
    
    #Using model b to examine changes
    x<-seq(0, 55, 1)
    y<-16.135753-0.219674*(x)
    plot(x, y)
    lin<-data.frame(x, y)
    lin<-mutate(lin, model="linear")
    55-sum(lin$y<4)
    55-sum(lin$y>28)
    
    #Incorporation of PDI shift in model (slope only)
    long<-mutate(long, WeeksSincePDI = DaysSincePDI/7, WeeksSincePDI_C = DaysSincePDI_C/7)
    long<-mutate(long, PostPDInum = as.numeric(PostPDI)-1)
    
    model.c <- nlme(ECBIInatt ~ (beta_0 + d_0i) + (beta_1 + d_1i)*WeeksSinceCDI + (beta_3 + d_3i)*DaysSincePDI_C,
                    data = long,
                    fixed = beta_0 + beta_1 + beta_3 ~ 1,
                    random = d_0i + d_1i+ d_3i ~ 1,
                    group = ~ ID,
                    start = c(beta_0 = 117, beta_1 = -.1, beta_3 = -.13),
                    na.action = "na.omit")
    
    summary(model.c)
    #Getting logLikelihood for comparisons
    Modelc2LL<-2*model.c$logLik
    c<-Modelc2LL-Modelb2LL
    c
    1-pchisq(c, 4)
    anova(model.b, model.c)
    
    #Incorporation of PDI shift in model (intercept only)
    model.d <- nlme(ECBIInatt ~ (beta_0 + d_0i) + (beta_1 + d_1i)*WeeksSinceCDI + (beta_2+d_2i)*PostPDInum,
                    data = long,
                    fixed = beta_0 + beta_1 + beta_2 ~ 1,
                    random = d_0i + d_1i + d_2i ~ 1,
                    group = ~ ID,
                    start = c(beta_0 = 130, beta_1 = -.23, beta_2 = .45),
                    na.action = "na.omit")
    
    summary(model.d)
    
    #Getting logLikelihood for comparisons
    Modeld2LL<-2*model.d$logLik
    c<-Modeld2LL-Modelb2LL
    c
    1-pchisq(c, 4)
    
    anova(model.b, model.d)
    
    #Incorporation of PDI shift in model (intercept and slope)
    model.e <- nlme(ECBIInatt ~ (beta_0 + d_0i) + (beta_1 + d_1i)*WeeksSinceCDI + (beta_2+d_2i)*PostPDInum + (beta_3 + d_3i)*DaysSincePDI_C,
                    data = long,
                    fixed = beta_0 + beta_1 + beta_2 + beta_3 ~ 1,
                    random = d_0i + d_1i + d_2i + d_3i ~ 1,
                    group = ~ ID,
                    start = c(beta_0 = 14.279214, beta_1 = -0.010145, beta_2 = 0.057294, beta_3 = -0.020995),
                    na.action = "na.omit",
                    control = nlmeControl(maxIter = 100))
    summary(model.e)
    
    Modele2LL<-2*model.e$logLik
    #comparing to linear
    c1<-Modele2LL-Modelb2LL
    c1
    1-pchisq(c1, 9)
    anova(model.b, model.e)
    #comparing to linear with slope disc.
    c2<-Modele2LL-Modelc2LL
    c2
    1-pchisq(c2, 5)
    anova(model.c, model.e)
    #comparing to linear with int. disc.
    c3<-Modele2LL-Modeld2LL
    c3
    1-pchisq(c3, 5)
    anova(model.d, model.e)
    
    #Using model e to examine changes across a hypothetical model with shift halfway through 32 week treatment
    x<-seq(0, 16, 1)
    y<-14.279814-0.010156*(x*7)+0.057298*0-0.020985*0
    pre<-data.frame(x, y)
    pre<-mutate(pre, stage="pre")
    x<-seq(16, 32, 1)
    y<-14.279814-0.010156*(x*7)+0.057298*1-0.020985*((x-16)*7)
    post<-data.frame(x, y)
    post<-mutate(post, stage="post")
    view(pre)
    view(post)
    df<-rbind(pre, post)
    df%>%
      ggplot(aes(x, y, color=stage))+
      geom_line()
    
    #Quadratic Growth for ECBI Total
    model.f<-nlme(ECBIInatt ~ (beta_0 + d_0i) + (beta_1 + d_1i)*WeeksSinceCDI + (beta_2 + d_2i)*WeeksSinceCDI^2,
                  data = long,
                  fixed = beta_0 + beta_1 + beta_2 ~1,
                  random = d_0i+ d_1i + d_2i ~ 1,
                  group = ~ ID,
                  start = c(beta_0 = 130, beta_1 = -.25, beta_2 = 0),
                  na.action = "na.omit")
    summary(model.f)
    #Getting logLikelihood for comparisons
    Modelf2LL<-2*model.f$logLik
    c<-Modelf2LL-Modelb2LL
    c
    1-pchisq(c, 4)
    
    anova(model.b, model.f)
    
    #Using model f to examine changes
    x<-seq(0, 55, 1)
    y<-16.507323-0.324483*(x)+0.004768*(x)^2
    plot(x, y)
    quad<-data.frame(x, y)
    quad<-mutate(quad, model="quadratic")
    55-sum(quad$y<4)
    55-sum(quad$y>28)
    
    #Cubic Growth for ECBI Total
    
    model.g <- nlme(ECBIInatt ~ (beta_0 + d_0i) + (beta_1 + d_1i)*WeeksSinceCDI + (beta_2 + d_2i) * WeeksSinceCDI^2 + (beta_3 + d_3i) * WeeksSinceCDI^3,
                    data = long,
                    fixed = beta_0 + beta_1 + beta_2 + beta_3 ~1,
                    random = d_0i + d_1i + d_2i + d_3i ~ 1,
                    group = ~ ID,
                    start = c(beta_0 = 130, beta_1 = -.1, beta_2 = 0.1, beta_3 = 0.1),
                    na.action = "na.omit")
    summary(model.g)
    #Getting logLikelihood for comparisons
    Modelg2LL<-2*model.g$logLik
    c<-Modelg2LL-Modelf2LL
    c
    1-pchisq(c, 5)
    
    anova(model.f, model.g)
    
    #Using model g to examine changes
    x<-seq(0, 55, 1)
    y<-16.904618-0.503036*(x)+0.019878*(x)^2-0.000330*(x)^3
    plot(x, y)
    cub<-data.frame(x, y)
    cub<-mutate(cub, model = "cubic")
    55-sum(cub$y<4)
    55-sum(cub$y>28)
    
    #Logistic Model
    #alpha=ceiling-floor
    #y=alpha/(1+pi0*exp(pi1*x))+floor
    model.h<-nlme(ECBIInatt ~ 24/(1+(beta_0 + d_0i)*exp((beta_1+d_1i)*WeeksSinceCDI))+4,
                  data = long,
                  fixed = beta_0 + beta_1 ~ 1,
                  random = d_0i + d_1i ~ 1,
                  group = ~ ID,
                  start = c(beta_0 = 2, beta_1 = .00005),
                  na.action = "na.omit",
                  control = nlmeControl(maxIter = 100, tolerance = 100))
    summary(model.h)
    #Getting logLikelihood for comparisons
    Modelh2LL<-2*model.h$logLik
    
    anova(model.a, model.h)
    
    #Using model h to examine changes
    x<-seq(0, 55, 1)
    y<-24/(1+1.3506025*exp(0.0669579*(x)))+4
    plot(x, y)
    log<-data.frame(x, y)
    log<-mutate(log, model="logistic")
    
    #Fixing beta_1 to zero would eliminate the exponentiated piece of the equation, and it would look like this (a nested model)
    model.hzero<-nlme(ECBIInatt ~ 24/(1+(beta_0 + d_0i))+4,
                       data = long,
                       fixed = beta_0 ~ 1,
                       random = d_0i ~ 1,
                       group = ~ ID,
                       start = c(beta_0 = 2),
                       na.action = "na.omit",
                       control = nlmeControl(maxIter = 100, tolerance = 100))
    summary(model.hzero)
    #Getting logLikelihood for comparisons
    Modelh02LL<-2*model.hzero$logLik
    c<-Modelh02LL-Modelh2LL
    c
    1-pchisq(c, 3)
    
    #our full model fits better than the model with beta_1 fixed to zero (p <.0001)
    anova(model.h, model.hzero)
    
    #Using model hzero to examine changes
    x<-seq(0, 55, 1)
    y<-rep(24/(1+2.58486)+4, 56)
    plot(x, y)
    logzero<-data.frame(x, y)
    logzero<-mutate(logzero, model="logzero")
    
    #Setting beta_1 to a ~relatively~ large number (1 in this case, which is much higher than .0096) allows us to test this nested model
    model.hlarge<-nlme(ECBIInatt ~ 24/(1+(beta_0 + d_0i)*exp(1*WeeksSinceCDI))+4,
                       data = long,
                       fixed = beta_0 ~ 1,
                       random = d_0i ~ 1,
                       group = ~ ID,
                       start = c(beta_0 = 2),
                       na.action = "na.omit",
                       control = nlmeControl(maxIter = 100, tolerance = 100, pnlsTol = .0000001, pnlsMaxIter = 20))
    summary(model.hlarge)
    #Getting logLikelihood for comparisons
    Modelhl2LL<-2*model.hlarge$logLik
    c<-Modelhl2LL-Modelh2LL
    c
    1-pchisq(c, 3)
    
    anova(model.h, model.hlarge)
    
    #Using model hlarge to examine changes
    x<-seq(0, 55, 1)
    y<-24/(1+0.009289646*exp(x))+4
    plot(x, y)
    loglarge<-data.frame(x, y)
    loglarge<-mutate(loglarge, model="loglarge")
    
    #Setting beta_1 to a ~relatively~ large NEGATIVE number (-1 in this case, which is much higher than .0096) allows us to test this nested model
    model.hneg<-nlme(ECBIInatt ~ 24/(1+(beta_0 + d_0i)*exp(-1*WeeksSinceCDI))+4,
                       data = long,
                       fixed = beta_0 ~ 1,
                       random = d_0i ~ 1,
                       group = ~ ID,
                       start = c(beta_0 = 2),
                       na.action = "na.omit",
                       control = nlmeControl(maxIter = 100, tolerance = 100, pnlsTol = .0000001, pnlsMaxIter = 20))
    summary(model.hneg)
    #Getting logLikelihood for comparisons
    Modelhn2LL<-2*model.hneg$logLik
    c<-Modelhn2LL-Modelh2LL
    c
    1-pchisq(c, 3)
    
    anova(model.h, model.hneg)
    
    #Using model hneg to examine changes
    x<-seq(0, 55, 1)
    y<-24/(1+7.510834*exp(-x))+4
    plot(x, y)
    logneg<-data.frame(x, y)
    logneg<-mutate(logneg, model="logneg")
    
    #Combing all log models
    alllogs<-rbind(nogrowth, lin, quad, cub, log, logzero, loglarge, logneg)
    view(alllogs)  
    df<-data.frame(alllogs)
    df<-mutate(df, model=factor(model, levels = c("no growth", "linear", "quadratic", "cubic", "logistic", "logneg", "logzero", "loglarge")))
    
    #Separate Lines
    df%>%
      ggplot(aes(x=x, y=y, color=model))+
      geom_point()+
      geom_line()+
      geom_hline(yintercept=28, color="red")+
      geom_hline(yintercept=4, color="red")
    
    #Separate Panels
    df%>%
      ggplot(aes(x=x, y=y))+
      geom_point(size=.2)+
      geom_line()+
      facet_wrap(.~model)+
      geom_hline(yintercept=28, color="red")+
      geom_hline(yintercept=4, color="red")+
      xlab("Week")+
      ylab("ECBI Inattention")
    
    grad<-filter(long, Session=="Graduation")
    mean(grad$ECBIInatt, na.rm=TRUE)
    
    #Logistic Spline Model (intercept only)
    model.i<-nlme(ECBIInatt ~ (24/(1+(beta_0 + d_0i)*exp((beta_1+d_1i)*WeeksSinceCDI))+4) +
                              (beta_4+d_4i)*PostPDInum,
                  data = long,
                  fixed = beta_0 + beta_1 + beta_4 ~ 1,
                  random = d_0i + d_1i +d_4i ~ 1,
                  group = ~ ID,
                  start = c(beta_0 = 1.28, beta_1 = .012, beta_4 = 0.59),
                  na.action = "na.omit",
                  control = nlmeControl(maxIter = 100, tolerance = 500))
    summary(model.i)
    #Getting logLikelihood for comparisons
    Modeli2LL<-2*model.i$logLik
    c<-Modeli2LL-Modelh2LL
    c
    1-pchisq(c, 4)
    
    anova(model.h, model.i)
    
    #Logistic Spline Model (slope only)
    model.j<-nlme(ECBIInatt ~ (24/(1+(beta_0 + d_0i)*exp((beta_1+d_1i)*WeeksSinceCDI)+4)) +
                    (24/(1+(beta_2 + d_2i)*exp((beta_3+d_3i)*DaysSincePDI_C)+4)),
                  data = long,
                  fixed = beta_0 + beta_1 + beta_2 + beta_3 ~ 1,
                  random = d_0i + d_1i + d_2i + d_3i ~ 1,
                  group = ~ ID,
                  start = c(beta_0 = 1, beta_1 = .5, beta_2 = 1, beta_3 = .5),
                  na.action = "na.omit",
                  control = nlmeControl(maxIter = 500, pnlsMaxIter = 20, pnlsTol = .000000001, tolerance = 100, msMaxIter = 150))
    #EVEN WITH ALL OF THE ABOVE CONTROLS, THIS DID NOT CONVERGE
    # summary(model.j)
    # #Getting logLikelihood for comparisons
    # Modelc2LL<-2*model.c$logLik
    # c<-Modelc2LL-Modelb2LL
    # c
    # 1-pchisq(c, 4)
    
     #anova(model.h, model.j)
      
    #Logistic Spline Model (slope and intercept)
    model.k<-nlme(ECBIInatt ~ (24/(1+(beta_0 + d_0i)*exp((beta_1+d_1i)*WeeksSinceCDI)+4)) +
                    (24/(1+(beta_2 + d_2i)*exp((beta_3+d_3i)*DaysSincePDI_C)+4)) +
                    (beta_4+d_4i)*PostPDInum,
                  data = long,
                  fixed = beta_0 + beta_1 + beta_2 + beta_3 + beta_4 ~ 1,
                  random = d_0i + d_1i + d_2i + d_3i + d_4i~ 1,
                  group = ~ ID,
                  start = c(beta_0 = .5, beta_1 = .00005, beta_2 = .5, beta_3 = .00005, beta_4 = .01),
                  na.action = "na.omit",
                  control = nlmeControl(maxIter = 100, tolerance = 100, pnlsMaxIter = 30, pnlsTol = .00000000001))
    #AGAIN, THIS DID NOT CONVERGE
    #summary(model.k)
    #anova(model.h, model.k)
    #anova(model.i, model.k)
    #anova(model.j, model.k)
    
    

#Examining residuals
    #Model A
      resida<-residuals(model.a, type = "normalized", asList = FALSE)
      fitteda<-fitted(model.a)
      df<-data.frame(resida, fitteda)
      #Examining histogram of residuals (note normal distribution and most residuals within 3 SDs)
      df%>%
        ggplot(aes(x=resida))+
        geom_histogram()+
        geom_vline(xintercept=3, linetype="dotted")+
        geom_vline(xintercept=-3, linetype="dotted")
      #Examining residuals vs fitted (note heteroscedasticity)
      df%>%
        ggplot(aes(x= fitteda, y=resida))+
        geom_jitter()+
        geom_hline(yintercept=3, linetype = "dotted")+
        geom_hline(yintercept= -3, linetype = "dotted")

    #Model B
      residb<-residuals(model.b, type = "normalized", asList = FALSE)
      fittedb<-fitted(model.b)
      df<-data.frame(residb, fittedb)
      #Examining histogram of residuals (note normal distribution and most residuals within 3 SDs)
      df%>%
        ggplot(aes(x=residb))+
        geom_histogram()+
        geom_vline(xintercept=3, linetype="dotted")+
        geom_vline(xintercept=-3, linetype="dotted")
      #Examining residuals vs fitted (note heteroscedasticity)
      df%>%
        ggplot(aes(x= fittedb, y=residb))+
        geom_jitter()+
        geom_hline(yintercept=3, linetype = "dotted")+
        geom_hline(yintercept= -3, linetype = "dotted")
    
    #Model C
      residc<-residuals(model.c, type = "normalized", asList = FALSE)
      fittedc<-fitted(model.c)
      df<-data.frame(residc, fittedc)
      #Examining histogram of residuals (note normal distribution and most residuals within 3 SDs)
      df%>%
        ggplot(aes(x=residc))+
        geom_histogram()+
        geom_vline(xintercept=3, linetype="dotted")+
        geom_vline(xintercept=-3, linetype="dotted")
      #Examining residuals vs fitted (note heteroscedasticity)
      df%>%
        ggplot(aes(x= fittedc, y=residc))+
        geom_jitter()+
        geom_hline(yintercept=3, linetype = "dotted")+
        geom_hline(yintercept= -3, linetype = "dotted")

    #Model D
      residd<-residuals(model.d, type = "normalized", asList = FALSE)
      fittedd<-fitted(model.d)
      df<-data.frame(residd, fittedd)
      #Examining histogram of residuals (note normal distribution and most residuals within 3 SDs)
      df%>%
        ggplot(aes(x=residd))+
        geom_histogram()+
        geom_vline(xintercept=3, linetype="dotted")+
        geom_vline(xintercept=-3, linetype="dotted")
      #Examining residuals vs fitted (note heteroscedasticity)
      df%>%
        ggplot(aes(x= fittedd, y=residd))+
        geom_jitter()+
        geom_hline(yintercept=3, linetype = "dotted")+
        geom_hline(yintercept= -3, linetype = "dotted")      

    #Model E
      reside<-residuals(model.e, type = "normalized", asList = FALSE)
      fittede<-fitted(model.e)
      df<-data.frame(reside, fittede)
      #Examining histogram of residuals (note normal distribution and most residuals within 3 SDs)
      df%>%
        ggplot(aes(x=reside))+
        geom_histogram()+
        geom_vline(xintercept=3, linetype="dotted")+
        geom_vline(xintercept=-3, linetype="dotted")
      #Examining residuals vs fitted (note heteroscedasticity)
      df%>%
        ggplot(aes(x= fittede, y=reside))+
        geom_jitter()+
        geom_hline(yintercept=3, linetype = "dotted")+
        geom_hline(yintercept= -3, linetype = "dotted")      
      
    #Model F
      residf<-residuals(model.f, type = "normalized", asList = FALSE)
      fittedf<-fitted(model.f)
      df<-data.frame(residf, fittedf)
      #Examining histogram of residuals (note normal distribution and most residuals within 3 SDs)
      df%>%
        ggplot(aes(x=residf))+
        geom_histogram()+
        geom_vline(xintercept=3, linetype="dotted")+
        geom_vline(xintercept=-3, linetype="dotted")
      #Examining residuals vs fitted (note heteroscedasticity)
      df%>%
        ggplot(aes(x= fittedf, y=residf))+
        geom_jitter()+
        geom_hline(yintercept=3, linetype = "dotted")+
        geom_hline(yintercept= -3, linetype = "dotted")
      
    #Model G
      residg<-residuals(model.g, type = "normalized", asList = FALSE)
      fittedg<-fitted(model.g)
      df<-data.frame(residg, fittedg)
      #Examining histogram of residuals (note normal distribution and most residuals within 3 SDs)
      df%>%
        ggplot(aes(x=residg))+
        geom_histogram()+
        geom_vline(xintercept=3, linetype="dotted")+
        geom_vline(xintercept=-3, linetype="dotted")
      #Examining residuals vs fitted (note heteroscedasticity)
      df%>%
        ggplot(aes(x= fittedg, y=residg))+
        geom_jitter()+
        geom_hline(yintercept=3, linetype = "dotted")+
        geom_hline(yintercept= -3, linetype = "dotted")

    #Model H
      residh<-residuals(model.h, type = "normalized", asList = FALSE)
      fittedh<-fitted(model.h)
      df<-data.frame(residh, fittedh)
      #Examining histogram of residuals (note normal distribution and most residuals within 3 SDs)
      df%>%
        ggplot(aes(x=residh))+
        geom_histogram(binwidth = .1)+
        geom_vline(xintercept=3, linetype="dotted")+
        geom_vline(xintercept=-3, linetype="dotted")+
        xlab("Residuals")
      #Examining residuals vs fitted (note heteroscedasticity)
      df%>%
        ggplot(aes(x= fittedh, y=residh))+
        geom_jitter()+
        geom_hline(yintercept=3, linetype = "dotted")+
        geom_hline(yintercept= -3, linetype = "dotted")+
        xlab("Fitted Value")+
        ylab("Residual")
   
#Addition of covariates to logistic model
      
    #Model X-Adding Age, Gender, and ADHD to the model
      #alpha=ceiling-floor
      #y=alpha/(1+pi0*exp(pi1*x))+floor
      
      long<-mutate(long, GenderNum = as.numeric(Gender)-1)
      
      #model.x<-nlme(ECBIInatt ~ 24/(1+(beta_00 + beta_01*Age + beta_02*GenderNum + d_0i)*
      #                            exp((beta_10 + beta_11*Age + beta_12*GenderNum + d_1i)*WeeksSinceCDI))+4,
      #              data = long,
      #              fixed = beta_00 + beta_01 + beta_02 + beta_10 + beta_11 + beta_12 ~ 1,
      #              random = d_0i + d_1i ~ 1,
      #              group = ~ ID,
      #              start = c(beta_00 = .0005, beta_01 = 1, beta_02 = .000001,
      #                        beta_10 = .0005, beta_11 = 1, beta_12 = .000001),
      #              na.action = "na.omit",
      #              control = nlmeControl(maxIter = 100, msMaxIter = 100, pnlsTol = .00000000000000000000000000000000000000000000001))
      #DOES NOT CONVERGE
      #summary(model.x)
      
      #anova(model.h, model.x)
      
      model.xGen<-nlme(ECBIInatt ~ 24/(1+(beta_00 + beta_02*GenderNum + d_0i)*
                                  exp((beta_10 + beta_12*GenderNum + d_1i)*WeeksSinceCDI))+4,
                    data = long,
                    fixed = beta_00 + beta_02 + beta_10 + beta_12 ~ 1,
                    random = d_0i + d_1i ~ 1,
                    group = ~ ID,
                    start = c(beta_00 = 1.8004035, beta_02 = -0.3574789,
                              beta_10 = 0.0349925, beta_12 = 0.0244636),
                    na.action = "na.omit",
                    control = nlmeControl(maxIter = 500, tolerance = 200, msMaxIter = 100))
      summary(model.xGen)
      
      anova(model.h, model.xGen)
      
      model.hADHD<-nlme(ECBIInatt ~ 24/(1+(beta_00 + 0*ADHDBase + d_0i)*
                                      exp((beta_10 + 0*ADHDBase + d_1i)*WeeksSinceCDI))+4,
                        data = long,
                        fixed = beta_00 + beta_10 ~ 1,
                        random = d_0i + d_1i ~ 1,
                        group = ~ ID,
                        start = c(beta_00 = 1.30,
                                  beta_10 = 0.07),
                        na.action = "na.omit",
                        control = nlmeControl(maxIter = 500, tolerance = 200, msMaxIter = 100))
      summary(model.hADHD)
      
      #Adding ADHD
      model.xADHD<-nlme(ECBIInatt ~ 24/(1+(beta_00 + beta_02*ADHDBase + d_0i)*
                                      exp((beta_10 + beta_12*ADHDBase + d_1i)*WeeksSinceCDI))+4,
                       data = long,
                       fixed = beta_00 + beta_02 + beta_10 + beta_12 ~ 1,
                       random = d_0i + d_1i ~ 1,
                       group = ~ ID,
                       start = c(beta_00 = 5.61, beta_02 = -0.07,
                                 beta_10 = 0.11, beta_12 = -0.001),
                       na.action = "na.omit",
                       control = nlmeControl(maxIter = 500, tolerance = 200, msMaxIter = 100))
      summary(model.xADHD)
      
      anova(model.hADHD, model.xADHD)
      
      #model.xAge<-nlme(ECBIInatt ~ 24/(1+(beta_00 + beta_01*(Age-2) + d_0i)*
      #                               exp((beta_10 + beta_11*(Age-2) + d_1i)*WeeksSinceCDI))+4,
      #              data = long,
      #              fixed = beta_00 + beta_01 + beta_10 + beta_11 ~ 1,
      #               random = d_0i + d_1i ~ 1,
      #              group = ~ ID,
      #              start = c(beta_00 = .5, beta_01 = 0.005,
      #              beta_10 = .000004, beta_11 = .000001),
      #              na.action = "na.omit",
      #              control = nlmeControl(maxIter = 500, msMaxIter = 100, pnlsTol = .000000000000001))
      #summary(model.xAge)
      #DOES NOT CONVERGE
      
      #anova(model.h, model.xAge)
      
      #ADHD and Gender Added
      model.xADHDGen<-nlme(ECBIInatt ~ 24/(1+(beta_00 + beta_01*ADHDBase + beta_02*GenderNum + d_0i)*
                                         exp((beta_10 + beta_11*ADHDBase + beta_12*GenderNum + d_1i)*WeeksSinceCDI))+4,
                        data = long,
                        fixed = beta_00 + beta_01 + beta_02 + beta_10 + beta_11 + beta_12 ~ 1,
                        random = d_0i + d_1i ~ 1,
                        group = ~ ID,
                        start = c(beta_00 = 6.15, beta_01 = -00.07, beta_02 = -0.40,
                                  beta_10 = 0.09, beta_11 = -0.001, beta_12 = 00.02),
                        na.action = "na.omit",
                        control = nlmeControl(maxIter = 500, tolerance = 200, msMaxIter = 100))
      summary(model.xADHDGen)
      anova(model.hADHD, model.xADHDGen)
      anova(model.xADHD, model.xADHDGen)
      
      
      #Model Z-Adding Behavior Descriptions to the model
        long<-mutate(long, BDsXCDIWeek = BDs*WeeksSinceCDI)
        model.zlog<-nlme(ECBIInatt ~ 24/(1+(beta_00 + 0*ADHDBase + 0*GenderNum + d_0i)*
                                       exp((beta_10 + 0*ADHDBase + 0*GenderNum + d_1i)*WeeksSinceCDI+
                                           0*BDs+
                                           0*BDsXCDIWeek
                                      ))+4,
                      data = long,
                      fixed = beta_00 + beta_10~ 1,
                      random = d_0i + d_1i ~ 1,
                      group = ~ ID,
                      start = c(beta_00 = 1.2, beta_10 = .08),
                      na.action = "na.omit",
                      control = nlmeControl(maxIter = 500, tolerance = 200, msMaxIter = 100))
        summary(model.zlog)
        
        model.zGen<-nlme(ECBIInatt ~ 24/(1+(beta_00 + 0*ADHDBase + beta_02*GenderNum + d_0i)*
                                               exp((beta_10 + 0*ADHDBase + beta_12*GenderNum + d_1i)*WeeksSinceCDI+
                                                     0*BDs+
                                                     0*BDsXCDIWeek))+4,
                             data = long,
                             fixed = beta_00 + beta_02 +
                               beta_10 + beta_12
                             ~ 1,
                             random = d_0i + d_1i ~ 1,
                             group = ~ ID,
                             start = c(beta_00 = 1.39, beta_02 = -0.35,
                                       beta_10 = 0.06, beta_12 = 0.02
                             ),
                             na.action = "na.omit",
                             control = nlmeControl(maxIter = 500, tolerance = 200, msMaxIter = 1000))
        
        summary(model.zGen)
        anova(model.zGen, model.zlog)
        
        model.zADHD<-nlme(ECBIInatt ~ 24/(1+(beta_00 + beta_01*ADHDBase + 0*GenderNum + d_0i)*
                                               exp((beta_10 + beta_11*ADHDBase + 0*GenderNum + d_1i)*WeeksSinceCDI+
                                                     0*BDs+
                                                     0*BDsXCDIWeek))+4,
                             data = long,
                             fixed = beta_00 + beta_01 +
                               beta_10 + beta_11 
                             ~ 1,
                             random = d_0i + d_1i ~ 1,
                             group = ~ ID,
                             start = c(beta_00 = 5.82, beta_01 = -0.07,
                                       beta_10 = 0.13, beta_11 = 0
                             ),
                             na.action = "na.omit",
                             control = nlmeControl(maxIter = 500, tolerance = 200, msMaxIter = 100))
        
        summary(model.zADHD)
        anova(model.zADHD, model.zlog)
        
         model.zADHDGen<-nlme(ECBIInatt ~ 24/(1+(beta_00 + beta_01*ADHDBase + beta_02*GenderNum + d_0i)*
                                            exp((beta_10 + beta_11*ADHDBase + beta_12*GenderNum + d_1i)*WeeksSinceCDI+
                                                0*BDs+
                                                0*BDsXCDIWeek))+4,
                      data = long,
                      fixed = beta_00 + beta_01 + beta_02 +
                              beta_10 + beta_11 + beta_12
                              ~ 1,
                      random = d_0i + d_1i ~ 1,
                      group = ~ ID,
                      start = c(beta_00 = 5,     beta_01 = 0, beta_02 = 0,
                                beta_10 = 0, beta_11 = 0, beta_12 = 0
                      ),
                      na.action = "na.omit",
                      control = nlmeControl(maxIter = 500, tolerance = 200, msMaxIter = 100))
         
summary(model.zADHDGen)
anova(model.zADHDGen, model.zGen)
anova(model.zADHDGen, model.zADHD)

#Using model x ADHD Gen to examine changes
view(wide)

ADHDmean<-mean(wide$ADHDBase, na.rm=TRUE)
ADHDsd<-sd(wide$ADHDBase, na.rm=TRUE)
high<-ADHDmean+ADHDsd
avg<-ADHDmean
low<-ADHDmean-ADHDsd

#Visualizing ADHD Trajectories (using modelzADHD)
x<-seq(0, 55, 1)
ADHDlow<-24/(1+(5.168152-0.062706*low)*exp((0.132358-0.000897*low)*x))+4
ADHDavg<-24/(1+(5.168152-0.062706*avg)*exp((0.132358-0.000897*avg)*x))+4
ADHDhigh<-24/(1+(5.168152-0.062706*high)*exp((0.132358-0.000897*high)*x))+4
df<-data.frame(x, ADHDlow, ADHDavg, ADHDhigh)
dflong<-gather(df, group, score, ADHDlow:ADHDhigh)
dflong%>%
  ggplot(aes(x=x, y=score, color=group))+
  geom_point(size=.2)+
  geom_line(alpha=.5)+
  xlab("Week")+
  ylab("ECBI Inattentive Score")
# 
# wide<-mutate(wide, ADHDclin = ifelse(ADHDBase>=65, 1, 0))
# wide<-mutate(wide,  GenderF = factor(Gender, levels=c(0, 1), labels=c("Male", "Female")),
#              ADHDclinF = factor(ADHDclin, levels=c(0, 1), labels=c("No ADHD", "ADHD")))
# view(wide)
# wide%>%
#   group_by(GenderF, ADHDclinF)%>%
#   summarize(InAtt0=mean(ECBIInatt.1, na.rm=TRUE), InAtt10=mean(ECBIInatt.10, na.rm=TRUE), InAtt20=mean(ECBIInatt.20, na.rm=TRUE))

#This table actually suggests that we are seeing similar patterns to what we see in the graph. This model may be close to our data


      model.zBDs<-nlme(ECBIInatt ~ 24/(1+(beta_00 + 0*ADHDBase + 0*GenderNum + d_0i)*
                                     exp((beta_10 + 0*ADHDBase + 0*GenderNum + d_1i)*WeeksSinceCDI+
                                         (beta_20 + 0*ADHDBase + 0*GenderNum + d_2i)*BDs+
                                         (beta_30 + 0*ADHDBase + 0*GenderNum + d_3i)*BDsXCDIWeek
                                         ))+4,
                       data = long,
                       fixed = beta_00 +
                               beta_10 + 
                               beta_20 + 
                               beta_30  
                               ~ 1,
                       random = d_0i + d_1i + d_2i + d_3i ~ 1,
                       group = ~ ID,
                       start = c(beta_00 = 0.90294, 
                                 beta_10 = 0.08619, 
                                 beta_20 = 0.03749, 
                                 beta_30 = -0.00246
                       ),
                       na.action = "na.omit",
                       control = nlmeControl(maxIter = 500, tolerance = 200, msMaxIter = 100))
      summary(model.zBDs)
      anova(model.zlog, model.zBDs)
      
      summarize(long, meanBDs=mean(BDs, na.rm=TRUE), SDBDs=(sd(BDs, na.rm=TRUE)))
      
      #Visualizing with BDs
      mean(long$BDs, na.rm=TRUE)
      sd(long$BDs, na.rm=TRUE)
      
      week<-seq(0, 55, 1)
      lowBD<-rep(9.12449-6.06172, 56)
      avgBD<-rep(9.12449, 56)
      highBD<-rep(9.12449+6.06172, 56)
      lowINT<-week*lowBD
      avgINT<-week*avgBD
      highINT<-week*highBD
      InattLowBD<-24/(1+0.8983074*
                        exp(0.0873534*week+
                            0.0378320*lowBD+
                            -0.0025984*lowINT
                        ))+4
      InattAvgBD<-24/(1+0.8983074*
                        exp(0.0873534*week+
                              0.0378320*avgBD+
                              -0.0025984*avgINT
                        ))+4
      InattHighBD<-24/(1+0.8983074*
                        exp(0.0873534*week+
                              0.0378320*highBD+
                              -0.0025984*highINT
                        ))+4
      
     df<-data.frame(week, InattLowBD, InattAvgBD, InattHighBD)
     dflong<-gather(df, model, ECBIInatt, InattLowBD:InattHighBD)    
     dflong%>%
       ggplot(aes(x=week, y=ECBIInatt, color=model))+
       geom_point(size=.2)+
       geom_line(alpha=.5)+
       xlab("Week")+
       ylab("ECBI Inattentive Scores")
     min(wide$Sessions)
     mean(wide$Sessions)
     max(wide$Sessions)

    #Zooming out to show full curve
     week<-seq(-100, 100, 1)
     lowBD<-rep(9.12449-6.06172, 201)
     avgBD<-rep(9.12449, 201)
     highBD<-rep(9.12449+6.06172, 201)
     lowINT<-week*lowBD
     avgINT<-week*avgBD
     highINT<-week*highBD
     InattLowBD<-24/(1+0.8983074*
                       exp(0.0873534*week+
                             0.0378320*lowBD+
                             -0.0025984*lowINT
                       ))+4
     InattAvgBD<-24/(1+0.8983074*
                       exp(0.0873534*week+
                             0.0378320*avgBD+
                             -0.0025984*avgINT
                       ))+4
     InattHighBD<-24/(1+0.8983074*
                        exp(0.0873534*week+
                              0.0378320*highBD+
                              -0.0025984*highINT
                        ))+4
     
     df<-data.frame(week, InattLowBD, InattAvgBD, InattHighBD)
     dflong<-gather(df, model, ECBIInatt, InattLowBD:InattHighBD)    
     dflong%>%
       ggplot(aes(x=week, y=ECBIInatt, color=model))+
       geom_point()+
       geom_line(alpha=.5)
     
#Model with ADHD and BDs (Gender removed from original hypothesized model)
    #  model.z<-nlme(ECBIInatt ~ 24/(1+(beta_00 + beta_01*ADHDBase + d_0i)*
    #                              exp((beta_10 + beta_11*ADHDBase + d_1i)*WeeksSinceCDI+
    #                                  (beta_20 + beta_21*ADHDBase + d_2i)*BDs+
    #                                  (beta_30 + beta_31*ADHDBase + d_3i)*BDsXCDIWeek
    #                                   ))+4,
    #                     data = long,
    #                     fixed = beta_00 + beta_01 +
    #                             beta_10 + beta_11 +
    #                             beta_20 + beta_21 +
    #                             beta_30 + beta_31
    #                             ~ 1,
    #                     random = d_0i + d_1i +d_2i + d_3i ~ 1,
    #                     group = ~ ID,
    #                     start = c(beta_00 = 1, beta_01 = -.1,
    #                               beta_10 = 1, beta_11 = 0,
    #                               beta_20 = .1, beta_21 = 0,
    #                               beta_30 = 0, beta_31 = .01
    #                               ),
    #                     na.action = "na.omit",
    #                     verbose=TRUE,
    #                     control = nlmeControl(maxIter = 500, tolerance = 100, msMaxIter = 500, pnlsMaxIter = 1000, pnlsTol = .05))
    # summary(model.z)
    #  #MODEL DID NOT CONVERGE
    # Error in nlme.formula(ECBIInatt ~ 24/(1 + (beta_00 + beta_01 * ADHDBase +  : 
    #                                              step halving factor reduced below minimum in PNLS step
    # 
     # model.z<-nlme(ECBIInatt ~ 24/(1+(beta_00 + beta_01*ADHDBase + beta_02*GenderNum + d_0i)*
     #                             exp((beta_10 + beta_11*ADHDBase + beta_12*GenderNum + d_1i)*WeeksSinceCDI+
     #                                 (beta_20 + beta_21*ADHDBase + beta_22*GenderNum + d_2i)*BDs+
     #                                 (beta_30 + beta_31*ADHDBase + beta_32*GenderNum + d_2i)*BDsXCDIWeek
     #                                  ))+4,
     #                    data = long,
     #                    fixed = beta_00 + beta_01 + beta_02 +
     #                            beta_10 + beta_11 + beta_12 +
     #                            beta_20 + beta_21 + beta_22 +
     #                            beta_30 + beta_31 + beta_32
     #                            ~ 1,
     #                    random = d_0i + d_1i +d_2i + d_3i ~ 1,
     #                    group = ~ ID,
     #                    start = c(beta_00 = 0.898, beta_01 = -.164, beta_02 = -.503,
     #                              beta_10 = 0.087, beta_11 = 0.031, beta_12 = 0.075,
     #                              beta_20 = 0.038, beta_21 = .01, beta_22 = -.01,
     #                              beta_30 = -.003, beta_31 = .01, beta_32 = -.01
     #                              ),
     #                    na.action = "na.omit",
     #                    control = nlmeControl(maxIter = 500, tolerance = 200, msMaxIter = 100))
     #Could not get this model to converge (Error message below:
     #Error in chol.default((value + t(value))/2) :
     #the leading minor of order 4 is not positive definite
      
     # model.lme.BD <- lme(ECBIInatt ~ WeeksSinceCDI + BDs + BDsXCDIWeek,
     #                long, random = ~WeeksSinceCDI + BDs | ID, method = "ML", na.action = "na.omit",
     #                control = lmeControl(opt = "optim"))
     # summary(model.lme.BD)
     # 
     # mean(wide$ADHDBase, na.rm = TRUE)
     # 
     # long<-mutate(long, ADHDBaseC = ADHDBase - 63.53333)
     # 
     # model.lme.FULL<-lme(ECBIInatt ~ WeeksSinceCDI + BDs + BDsXCDIWeek + ADHDBaseC,
     #                     long, random = ~ WeeksSinceCDI + BDs + ADHDBase | ID, method = "ML", na.action = "na.omit",
     #                     control = lmeControl(opt = "optim")
     #                     )
     # summary(model.lme.FULL)
   
     # #Making Dataset with 2+BDs only
     # BD2<-c("1023", "996", "1024", "1044", "985", "1028", "1052")
     # "%notin%" <- Negate('%in%')
     # long2<-subset(long, ID %notin% BD2)
     # 
     # #Using linear model instead (Had to center ADHD and drop Gender; used lme to guide starting values)
     # model.z_lin<-nlme(ECBIInatt ~  beta_00 + beta_01*ADHDBaseC + d_0i + #beta_02*GenderNum + d_0i+
     #                               (beta_10 + beta_11*ADHDBaseC + d_1i)*WeeksSinceCDI+ #beta_12*GenderNum + d_1i)*WeeksSinceCDI+
     #                               (beta_20 + beta_21*ADHDBaseC + d_2i)*BDs, #+ #beta_22*GenderNum + d_2i)*BDs+
     #                               #(beta_30 + beta_31*ADHDBaseC + d_3i)*BDsXCDIWeek, #beta_32*GenderNum + d_2i)*BDsXCDIWeek,
     #                   data = long2,
     #                  fixed = beta_00 + beta_01 + #beta_02 +
     #                          beta_10 + beta_11 + #beta_12 +
     #                          beta_20 + beta_21 #+ #beta_22 +
     #                          #beta_30 + beta_31 #+ beta_32
     #                           ~ 1,
     #                  random = d_0i + d_1i +d_2i ~ 1,#+ d_3i ~ 1,
     #                  group = ~ ID,
     #                  start = c(beta_00 =   16.7, beta_01 = .43, #beta_02 =  .5,
     #                            beta_10 = -.22, beta_11 = -.002, #beta_12 = .01,
     #                            beta_20 = -.05, beta_21 = .001#, #beta_22 = -.0001,
     #                           # beta_30 = .006, beta_31 = -.001 #beta_32 = -.0001
     #                            ),
     #                   na.action = "na.omit",
     #                   method = "REML",
     #                   verbose = "TRUE", 
     #                   control = nlmeControl(maxIter = 1000, tolerance = 1000, msMaxIter = 2000), 
     #    #Could not get this to converge either
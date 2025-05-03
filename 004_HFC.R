# Script -----
# Name: HFC.R
# Author: Aarthi Iyer
# Date: 29/11/2021
# Purpose: High frequency checks of main female survey

#########################################################################################
#Loading packages
library(lubridate)
library(stringr)
library(here)
library(conflicted)
library(tidyverse)
conflict_prefer("select","dplyr")
conflict_prefer("filter","dplyr")

#########################################################################################
#Loading data
df_female <- read.csv(paste0(here(),"/data/clean/main_clean/MainFLFSfemale_clean.csv"), stringsAsFactors = TRUE)

#########################################################################################
#Survey time
df_female$duration <- df_female$duration/60

ggplot(df_female,aes(y=duration,x=""))+
  scale_x_discrete() +
  geom_boxplot()+
  stat_summary(fun=mean,geom="point",shape=20,size=5,color="red",fill="red")+
  ggtitle("Duration of female survey in minutes") +
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Minutes")+
  xlab("")

mean(df_female$duration)

#########################################################################################
#Sectional time of TUS
df_female <- df_female %>%
  rename(
    TUS_start_time = moduleTUS_start_stamp,
    TUS_end_time = moduleTUS_end_stamp
  ) 
df_female$TUS_start_time <- as.POSIXct(df_female$TUS_start_time, format = "%Y-%b-%d %H:%M:%S", tz = "Asia/Calcutta")
df_female$TUS_end_time <- as.POSIXct(df_female$TUS_end_time, format = "%Y-%b-%d %H:%M:%S", tz = "Asia/Calcutta")

# Survey Length 
df_female$TUS_duration <- as.numeric(difftime(df_female$TUS_end_time, df_female$TUS_start_time, units = "mins"))
mean(df_female$TUS_duration)

#Boxplot of TUS sectional time
ggplot(df_female,aes(y=TUS_duration,x=""))+
  scale_x_discrete() +
  geom_boxplot()+
  stat_summary(fun=mean,geom="point",shape=20,size=5,color="red",fill="red")+
  ggtitle("Duration of TUS in female survey in minutes") +
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Minutes")+
  xlab("")


########################################################################################
#Boxplot of survey duration by states
ggplot(df_female,aes(y=duration,x=a1_4))+
  geom_boxplot()+
  stat_summary(fun=mean,geom="point",shape=20,size=5,color="red",fill="red")+
  ggtitle("Duration of female survey in minutes") +
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Minutes")+
  xlab("States")

#Boxplot of TUS sectional survey duration by states
ggplot(df_female,aes(y=TUS_duration,x=a1_4))+
  geom_boxplot()+
  stat_summary(fun=mean,geom="point",shape=20,size=5,color="red",fill="red")+
  ggtitle("Duration of TUS in female survey in minutes") +
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Minutes")+
  xlab("States")

#Mean survey duration by states
df_female %>%
    group_by(a1_4) %>%
      summarize(Mean = mean(duration),na.rm=T)

#Mean TUS sectional survey duration by states
df_female %>%
  group_by(a1_4) %>%
  summarize(Mean = mean(TUS_duration),na.rm=T)


#########################################################################################
#TUS section analysis
df_tus1 <- df_female %>% 
  select(a1_4, starts_with("tus1_")) %>% 
  mutate(across(starts_with("tus1_"), ~.*15)) %>% 
  mutate(sum = rowSums(across(starts_with("tus1_") & ends_with("_mainact")))/60,
         sum_w_travel = rowSums(across(starts_with("tus1_") ))/60,
         range = case_when(sum <= 27.5 & sum >=20 ~ "20-27.5 Hours",
                           TRUE~"Outside range"))

df_tus_long = df_tus1 %>% 
  pivot_longer(
    cols = starts_with("tus1_"),
    names_to = "Activity",
    values_to = "Minutes"
  )

#Boxplot of TUS section by states
ggplot(df_tus_long %>% 
         filter(!str_detect(Activity, '_travel$') & !str_detect(Activity, '_mainact$') & !str_detect(Activity, '_simact$'))) +
  geom_boxplot(aes( x =  Activity, y = Minutes, color = Activity)) + 
  coord_flip() +
  theme(legend.position = "none")  + 
  facet_wrap(~a1_4)

#########################################################################################
#Boxplots for survey duration by survyor ID
ggplot(df_female,aes(y=duration,x=as.factor(enumeratorcode)))+
  geom_boxplot()+
  stat_summary(fun=mean,geom="point",shape=15,size=2,color="red",fill="red")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.8))+
  ggtitle("Duration of female survey in minutes by Surveyor ID") +
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Minutes")+
  xlab("Surveyor ID")

#########################################################################################
#Boxplots for TUS section duration by survyor ID
ggplot(df_female,aes(y=TUS_duration,x=as.factor(enumeratorcode)))+
  geom_boxplot()+
  stat_summary(fun=mean,geom="point",shape=15,size=2,color="red",fill="red")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.1))+
  ggtitle("Duration of TUS in female survey in minutes by Surveyor ID") +
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Minutes")+
  xlab("Surveyor ID")







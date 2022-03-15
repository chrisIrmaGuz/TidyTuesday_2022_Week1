# Focus: Wait times for the DINOSAUR ride at WDW's Animal Kingdom in 2021
# Christina Irma Guzman

# Loading Libraries ####
library(readr)
library(tidyverse)
library(lubridate)
library(png)
library(ggpubr)
library(skimr)
library(ggthemes)

# Background about DINOSAUR ####
entities <- read_csv("data/entities.csv") %>% 
  filter(name == 'DINOSAUR')

# Importing DINOSAUR dataset ####
dinosaur0 <- read_csv("data/dinosaur.csv")

# Looking at structure of DINOSAUR dataset ####
str(dinosaur0)
skim(dinosaur0)
# Note: There is a considerable amount of data missingness.

# Clean Data ####
dinosaur1 <- dinosaur0 %>% 
  rename(DateTime = datetime, 
         ActualWaitTimeMin = SACTMIN, 
         StandbyPostedWaitTimeMin = SPOSTMIN, 
         Date = date) %>%
  mutate(Date = as.Date(Date, "%m/%d/%Y")) %>% 
  filter(Date >= "2021-01-01" & Date <= "2021-12-28") %>% # Filter to 2021 ONLY
  mutate(Hour = hour(DateTime), 
         Weekday = weekdays(DateTime))

# Checking 2021 data for missingness ####
skim(dinosaur1)

# How many actual wait times are provided for each hour each day? ####
waitTimeCheck0 <- dinosaur1 %>% 
  mutate(
    actualWaitTimeFound = case_when(is.na(ActualWaitTimeMin) ~ "Missing Time", 
                                    TRUE ~ "Not Missing Time")) %>% 
  group_by(Date, Hour, actualWaitTimeFound) %>% 
  summarise(Count = n()) %>% 
  filter(actualWaitTimeFound == "Not Missing Time") 
# It looks like 2021-03-12 at 9 am has the most reported actual wait times 
# throughout the hour (n = 6).

# How many days have more than 2 actual wait times recorded per hour? ####
waitTimeCheck1 <- waitTimeCheck0 %>% 
  filter(Count > 2)
# 74 days! 

# How many days out of the 74 are in this list more than once? ####
waitTimeCheck2 <- waitTimeCheck1 %>% 
  group_by(Date) %>% 
  filter(n() > 1)
# 2021-05-20 and 2021-05-30 are days that have 3 hours (8 am, 9 am, 10 am) that 
# have more than 2 actual wait times provided.

# Do 2021-05-20 and 2021-05-30 have consecutive days that can illustrate the 
# wait time within a certain time period on those days? ####
waitTimeCheck3 <- waitTimeCheck0 %>% 
  filter(Date %in% c(as.Date('2021-05-20'), as.Date('2021-05-30')))
# It looks like 2021-05-20 has a wait time for each hour between 8 am - 1 pm.
# This is a nice interval to graph!

# Average Actual Wait time per Hour ####
byHourEachDayAverageActual <- dinosaur1 %>%
  filter(Date == "2021-05-20" & Hour %in% c(8,9,10,11,12,13)) %>% 
  group_by(Hour) %>%
  summarise(AverageActual = mean(ActualWaitTimeMin, na.rm = TRUE)) %>% 
  mutate(AverageActual = floor(AverageActual))

# Average Standby Wait time per Hour ####
byHourEachDayAverageStandbyPosted <- dinosaur1 %>% 
  filter(StandbyPostedWaitTimeMin != "-999") %>% 
  filter(Date == "2021-05-20" & Hour %in% c(8,9,10,11,12,13)) %>% 
  group_by(Hour) %>% 
  summarise(
    AverageStandbyPosted = mean(StandbyPostedWaitTimeMin, na.rm = TRUE)) %>% 
  mutate(AverageStandbyPosted = floor(AverageStandbyPosted))

# All Time Data Combined (Actual and Standby) ####
all <- inner_join(byHourEachDayAverageActual, 
                  byHourEachDayAverageStandbyPosted, by = c("Hour")) %>% 
  pivot_longer(c(AverageActual, AverageStandbyPosted)) %>% 
  mutate(HourMin = paste0(Hour, ":", "00"), 
         HourFormatted = parse_date_time(HourMin, 'H:M'), 
         Time = format(strptime(HourFormatted, "%Y-%m-%d %H:%M:%S"),'%I:%M %p')
         ) %>% 
  select(WaitTimeType = name, Count = value, Time)

# Creating Ggplot ####
# Indicating the order of the time stamp
timeOrder <- c("08:00 AM", "09:00 AM", "10:00 AM", 
               "11:00 AM", "12:00 PM", "01:00 PM")

# Background Image
img <- readPNG("images/check.png")

ggplot(data = all) +
  ### Setting up background ###
  background_image(img) +
  theme_fivethirtyeight() +
  theme(legend.title = element_blank(),
        legend.text = element_text(color = "white", size = 12),
        legend.background = element_rect(fill = "black"),
        legend.key = element_rect(fill = 'black'),
        plot.caption = element_text(color = "white", 
                                    hjust = c(0.5,1), 
                                    size = c(12, 10), 
                                    face = c("plain", "bold.italic")),
        plot.background = element_rect(fill = "black"),
        plot.title = element_text(color = c("white","#B7E2FC"), 
                                  size = c(27,10), 
                                  hjust = c(0,1), 
                                  face = c("bold", "bold.italic")),
        plot.subtitle = element_text(color = "white", 
                                     face = "bold", 
                                     size = 12),
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_blank(),
        axis.text.x = element_text(size = 12, color = "white"),
        axis.text.y = element_blank()) + 
  ### Setting up Aesthetics Layer ###
  aes(x = factor(Time, level = timeOrder), y = Count, fill = WaitTimeType) +
  ylim(0, 70) +
  scale_fill_manual(
    values = c("AverageActual" = "#983439", 
               "AverageStandbyPosted" = "#dfa103"), 
    labels = c("Actual Wait Time", "Standby Wait Time")
  ) +
  labs(title = 
         c("D I N O S A U R", "Located at Walt Disney World's Animal Kingdom"),
       subtitle = 
         "Snapshot of the average wait time (in minutes) to ride DINOSAUR on the morning/early afternoon of 5/20/2021.",
       caption = 
         c("(Wait times are in minutes)", "Creator: Christina Irma Guzman")) +
  scale_x_discrete(labels = as.character(all$Time), breaks = all$Time) +
  ### Setting up geometric layer ###
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Count), 
            vjust = 1.2, 
            colour = "white", 
            position = position_dodge(.9), size = 4.2)



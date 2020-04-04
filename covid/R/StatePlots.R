library(tidyverse)
library(tidyquant)
library(dplyr)
library(ggplot2)
library(scales)
library(ggrepel)

usPop <- read_csv("data/pop4.csv") %>%
  mutate(stayHome = mdy(`Stay at home order`), `Stay at home order` = NULL,
         schoolsClosed = mdy(`Educational facilities closed`), `Educational facilities closed` = NULL,
         servicesClosed = mdy(`Non-essential services closed`), `Non-essential services closed` = NULL)

# Population density versus Trump vote share
usPop %>% filter(abbrev != "DC") %>%
  ggplot(aes(x=`Pop density per mi2`, y=`Trump Margin`, label=abbrev #, color=Party
  )) +
  scale_x_sqrt() +
  scale_y_continuous(labels = percent) +
  #scale_color_manual(values = c("blue","red")) +
  geom_text_repel() +
  geom_smooth(method=lm)



usCovidUrl <- url("http://covidtracking.com/api/states/daily.csv")
usCovidOrig <- read.csv(usCovidUrl) %>% mutate(date = ymd(date))

countDeathsFrom <- 5
minDeaths <- 10

usCovid1 <- usCovidOrig %>%
  filter(!state %in% c("AS", "GU", "MP", "PR", "VI")) %>%
  select(c("date","state","death","deathIncrease"))

usCovid2 <- usCovid1 %>% 
  filter(death >= countDeathsFrom) %>%
  arrange(state,date) %>%
  group_by(state) %>%
  mutate(deathsFromDate = min(date), deathsToDate = max(date), totDeaths = max(death), 
         weeklyIncrease = (death - lag(death,7)) / lag(death,7)) %>%
  filter(totDeaths >= minDeaths) %>%
  ungroup() %>%
  mutate(daysSince = date - deathsFromDate, 
         finalDeaths = ifelse(date == deathsToDate, death, NA),
         finalDailyDeaths = ifelse(date == deathsToDate, deathIncrease, NA),
         finalIncrease = ifelse(date == deathsToDate, weeklyIncrease, NA))

# State deaths by days since Nth death
usCovid2 %>% 
  ggplot(aes(x=daysSince,y=death,color=state)) +
  theme(legend.position = "none") +
  scale_x_continuous(paste("Days since", countDeathsFrom, "deaths")) +
  scale_y_log10("Cumulative deaths (log scale)", breaks=c(5,10,25,50,100,250,500,1000,2500,5000,10000)) +
  # scale_y_continuous("Cumulative deaths (linear scale)", breaks=c(5,10,25,50,100,250,500,1000,2500,5000,10000)) +
  stat_smooth(method = "loess", formula = y ~ x, size = 1, se = FALSE) +
  geom_point() +
  geom_label_repel(aes(x=daysSince,y=finalDeaths,label=state),nudge_x=1)

# State cumulative deaths over time
usCovid2 %>% 
  ggplot(aes(x=date,y=death,color=state)) +
  theme(legend.position = "none") +
  scale_y_log10("Cumulative deaths (log scale)", breaks=c(5,10,25,50,100,250,500,1000,2500,5000,10000)) +
  # scale_y_continuous("Cumulative deaths (linear scale)", breaks=c(5,10,25,50,100,250,500,1000,2500,5000,10000)) +
  stat_smooth(method = "loess", formula = y ~ x, size = 1, se = FALSE) +
  geom_label_repel(aes(x=date,y=finalDeaths,label=state),nudge_x=1)

# State daily deaths
usCovid2 %>% 
  ggplot(aes(x=daysSince,y=deathIncrease,color=state)) +
  theme(legend.position = "none") +
  scale_y_log10("Daily deaths (log scale)") +
  stat_smooth(method = "loess", formula = y ~ x, size = 1, span=1, se = FALSE) +
  geom_label_repel(aes(x=daysSince,y=finalDailyDeaths,label=state),nudge_x=1)

# State weekly death rate over time
usCovid2 %>% 
  ggplot(aes(x=daysSince,y=weeklyIncrease,color=state)) +
  theme(legend.position = "none") +
  scale_x_continuous(paste("Days since", countDeathsFrom, "deaths")) +
  scale_y_continuous("Weekly increase in death rate") +
  geom_point() +
  # geom_line() +
  geom_smooth(method = "loess", formula = y ~ x, size = 1, span=2, se = FALSE) +
  geom_label_repel(aes(x=daysSince,y=finalIncrease,label=state),nudge_x=1)


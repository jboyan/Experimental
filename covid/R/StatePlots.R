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

countDeathsFrom <- 10
minDeaths <- 100

usCovid1 <- usCovidOrig %>%
  filter(!state %in% c("AS", "GU", "MP", "PR", "VI")) %>%
  select(c("date","state","death"))

usCovid2 <- usCovid1 %>% 
  filter(death >= countDeathsFrom) %>%
  group_by(state) %>%
  mutate(deathsFromDate = min(date), deathsToDate = max(date), totDeaths = max(death)) %>%
  filter(totDeaths >= minDeaths) %>%
  ungroup() %>%
  mutate(daysSince = date - deathsFromDate, finalDeaths = ifelse(date == deathsToDate, death, NA))

# Deaths by state
usCovid2 %>% 
  ggplot(aes(x=daysSince,y=death,color=state)) +
  theme(legend.position = "none") +
  scale_x_continuous(paste("Days since", countDeathsFrom, "deaths")) +
  scale_y_log10("Cumulative deaths", breaks=c(5,10,25,50,100,250,500,1000,2500,5000,10000)) +
  # scale_y_log10(breaks = logTicks(n = 4), minor_breaks = logTicks(n = 40))
  stat_smooth(method = "loess", formula = y ~ x, size = 1, se = FALSE) +
  # geom_text_repel(aes(x=daysSince,y=finalDeaths,label=state),nudge_x=1,nudge_y=1)
  geom_text(aes(x=daysSince,y=finalDeaths,label=state),hjust="left",vjust="bottom")

  
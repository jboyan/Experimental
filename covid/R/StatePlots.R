library(tidyverse)
library(tidyquant)
library(dplyr)
library(ggplot2)
library(scales)
library(ggrepel)

usCovidUrl <- url("http://covidtracking.com/api/states/daily.csv")
usCovid <- read.csv(usCovidUrl) %>% mutate(date = ymd(date))

usPop <- read_csv("/Users/jab/Downloads/population - states (4).csv") %>%
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

# Deaths by state
usCovid %>% 
  filter(!state %in% c("AS", "GU","MP", "PR", "VI")) %>%
  filter(death >= 10) %>%
  ggplot(aes(x=date,y=death,color=state)) +
  # geom_ma(ma_fun = SMA, n = 50) +
  scale_y_log10() +
  geom_line()

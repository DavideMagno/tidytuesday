library(dplyr)
library(lubridate)
library(ggplot2)
library(bbplot)
library(jcolors)
library(stringr)

tidytuesday_tweets <- readRDS("~/Documents/R/Tidy Tuesday/tidytuesday/data/2019/2019-01-01/tidytuesday_tweets.rds")

TidyTime <- tidytuesday_tweets %>% 
  filter(!str_detect(.$screen_name,c("R4DScommunity","thomas_mock"))) %>% 
  select(created_at) %>%
  mutate_at("created_at",ymd_hms) %>% 
  mutate(Weekday = wday(.$created_at, label = TRUE)) %>% 
  mutate(Hour = hour(.$created_at)) %>% 
  mutate(Minute = round(minute(.$created_at)/10,0)*10) %>% 
  mutate(Time = Hour + Minute/60) %>% 
  select(Weekday, Time) 

tidytuesday_tweets %>% 
  filter(!str_detect(.$screen_name,c("R4DScommunity","thomas_mock"))) %>% 
  select(created_at) %>%
  mutate_at("created_at",ymd_hms) %>% 
  mutate(Weekday = wday(.$created_at)) %>% 
  mutate(Hour = hour(.$created_at)) %>% 
  mutate(Minute = round(minute(.$created_at)/10,0)*10) %>% 
  mutate(Time = Hour + Minute/60) %>%
  mutate(Cont_Time = (Weekday - 1) * 24 + Time) %>% 
  select(Cont_Time) %>% 
  ggplot(aes(x = Cont_Time)) +
  stat_density(aes(fill = stat(count)), geom = "raster", 
               position = "identity") +
  labs(title = "When users posts on #TidyTuesday",
       x = "Weekday",
       fill = "Number of posts") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_jcolors_contin(palette = "pal3")

TidyTime %>% 
  ggplot(aes(x = Time)) +
  geom_density(aes(colour = Weekday)) +
  labs(title = "3:30pm is #TidyTuesday o' clock",
       x = "Hour in the day",
       y = "Density") +
  theme_bw() +
  scale_x_continuous(limits = c(0, 24),
                     breaks = seq(0,24,4)) +
  theme(legend.position = "bottom") +
  guides(col = guide_legend(ncol = 7, byrow = FALSE)) +
  scale_color_jcolors(palette = "rainbow")
  

TidyTime %>% 
  ggplot(aes(x = Time, y = Weekday)) +
  stat_density(aes(fill = stat(count)), geom = "raster", 
               position = "identity") +
  labs(title = "When users posts on #TidyTuesday",
       x = "Hour in the day",
       y = "Weekday",
       fill = "Number of posts") +
  theme_bw() +
  scale_x_continuous(limits = c(0, 24),
                     breaks = seq(0,24,4)) +
  theme(legend.position = "bottom") +
  scale_fill_jcolors_contin(palette = "pal3")

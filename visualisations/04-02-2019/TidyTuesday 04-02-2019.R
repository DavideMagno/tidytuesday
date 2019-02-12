library(tidyr)
library(tidyquant)
library(mapdata)

us <- map_data("state")
state_abbs <- tibble(name = str_to_lower(state.name), state = state.abb)

State_HPI <- state_hpi %>% 
  mutate(Date = ymd(paste(year,month,"01", sep = "-"))) %>% 
  select(Date,everything()) %>%
  select(-year,-month) %>% 
  group_by(state) %>% 
  arrange(Date, .by_group = TRUE) %>% 
  mutate(pct_change_state = (log(price_index/lag(price_index))*100)) %>% 
  mutate(pct_change = (log(us_avg/lag(us_avg))*100)) %>% 
  filter(!is.na(pct_change)) %>% 
  mutate(spread = pct_change_state - pct_change) %>% 
  group_by(state) %>% 
  summarise(Spread = sum(spread)) %>% 
  left_join(state_abbs, by = "state") %>% 
  select(-state)

ggplot() + 
  geom_map(data = us, map = us,
           aes(x = long, y = lat, map_id = region),
           fill = "#ffffff", color = "#ffffff", size = 0.15) +
  geom_map(data = State_HPI, map = us,
           aes(fill = Spread, map_id = name),
           color = "#ffffff", size = 0.15) +
  scale_fill_gradient2(low = '#108dc7', 
                       mid = '#fffbd5',
                       high = '#ef8e38', 
                       guide = 'colorbar') +
  labs(x = NULL, y = NULL) +
  theme(panel.border = element_blank()) + 
  theme(panel.background = element_blank()) + 
  theme(axis.ticks = element_blank()) + 
  theme(axis.text = element_blank()) + 
  labs(title = "Is your state cheaper o more expensive than US?",
       subtitle = "House price exploded on West Coast and plunged in Mississipi",
       fill = "Change vs US (in %)") +
  theme(plot.subtitle = element_text(size = 8)) +
  theme(legend.title = element_text(size = 8)) +
  guides(fill = guide_colourbar(title.vjust = 0.8)) +
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 8))
  
  
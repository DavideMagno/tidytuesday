library(dplyr)
library(readr)
library(scales)

energy_spending <- read_csv("tidytuesday/data/2019/2019-02-12/energy_spending.csv")

total_spending <- energy_spending %>% 
  group_by(year) %>% 
  summarise(total = sum(energy_spending))

energy_spending %>% 
  left_join(total_spending, by = "year") %>% 
  mutate(percentage = energy_spending/total) %>% 
  select(-energy_spending, -total) %>% 
  ggplot(aes(x = year, y = percentage, fill = department)) +
  geom_area(colour = "black", size = .2, alpha = .6) +
  scale_fill_brewer(palette = "Spectral") + 
  labs(title = "Spending mix in the Dept. of Energy",
       subtitle = "Spending mix is almost unchanged since 1997 ",
       fill = "Sub-agencies",
       x = "Year",
       y = "% of total spending") +
  theme(plot.subtitle = element_text(size = 8)) +
  theme(legend.title = element_text(size = 8)) +
  theme(legend.text = element_text(size = 8)) +
  scale_y_continuous(labels = scales::percent)

ggsave("Spending_Energy.pdf")
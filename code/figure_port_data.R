
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse,
               foreach)


# read data ---------------------------------------------------------------

df_port <- read_csv("data/data_fmt/port_fmt.csv")


# port data ---------------------------------------------------------------

df0 <- df_port %>% 
  group_by(occasion, pit_number) %>% 
  summarize(section = max(section)) %>% 
  ungroup()

df0 %>% 
  slice(500:600) %>% 
  ggplot(aes(x = pit_number,
             y = section,
             color = occasion)) +
  geom_point()
  
  
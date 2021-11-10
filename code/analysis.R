
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse,
               foreach)


# read data ---------------------------------------------------------------

df_cmr <- read_csv("data/data_fmt/cmr_fmt.csv")
df_port <- read_csv("data/data_fmt/port_fmt.csv")


# cmr data ----------------------------------------------------------------

df_density <- df_cmr %>% 
  group_by(occasion, section, species) %>% 
  summarize(abundance = n())

df_response <- df_cmr %>% 
  pivot_wider(id_cols = c(pit_number,
                          species),
              names_from = occasion,
              values_from = c(section,
                              fork_length,
                              body_mass)) %>% 
  mutate(distance = section_2 - section_1,
         growth = fork_length_2 - fork_length_1)


# port data ---------------------------------------------------------------

## base data
df_port_base <- df_port %>% 
  group_by(occasion, pit_number) %>% 
  summarize(section = max(section))
  
## extract individuals detected more than twice
ind_id <- df_port_base %>%   
  group_by(pit_number) %>% 
  summarize(frequency = n()) %>% 
  filter(frequency > 2) %>% 
  pull(pit_number)

## esimate SD
df_sd <- df_port_base %>% 
  filter(pit_number %in% ind_id) %>% 
  group_by(pit_number) %>% 
  summarize(sd_move = sd(section))


# combine data ------------------------------------------------------------

df_m <- df_response %>% 
  left_join(df_sd, by = "pit_number")


# plot --------------------------------------------------------------------

df_m %>% 
  filter(species %in% c("masusalmon",
                        "rainbowtrout",
                        "whitespottedchar")) %>% 
  ggplot(aes(x = sd_move,
             y = growth)) +
  geom_point() +
  facet_wrap(facets = ~species)

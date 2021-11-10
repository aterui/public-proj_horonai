
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

df_port %>% 
  pivot_wider(id_cols = pit_number,
              names_from = occasion,
              names_prefix = "occasion_",
              values_from = section,
              values_fn = max) %>% 
  relocate(pit_number,
           paste("occasion", 1:6, sep = "_")) %>% 
  mutate(x = diff(occasion_1, occasion_2))

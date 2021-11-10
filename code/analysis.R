
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse,
               foreach)


# read data ---------------------------------------------------------------

df0 <- read_csv("data/data_fmt/cmr_fmt.csv")


# density data frame ------------------------------------------------------

df_density <- df0 %>% 
  group_by(occasion, section, species) %>% 
  summarize(abundance = n())

df0 %>% 
  pivot_wider(id_cols = c(pit_number,
                          species),
              names_from = occasion,
              values_from = c(section,
                              fork_length,
                              body_mass)) %>% 
  mutate(distance = section_2 - section_1) %>% 
  view()


# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse,
               foreach)


# read data ---------------------------------------------------------------

df0 <- list.files(path = "data_raw") %>% 
  as_tibble() %>% 
  filter(str_detect(value, "cmr")) %>% 
  filter(str_detect(value, "csv")) %>% 
  pull() %>% 
  paste0("data_raw/", .) %>% 
  lapply(read_csv) %>% 
  do.call(bind_rows, .) %>% 
  select(-recap,
         -recapinsurvey,
         -ID) %>% 
  rename(date = Date,
         section = Section,
         species = Spp,
         fork_length = FL,
         body_mass = BM,
         pit_number = PITNumber,
         pit_number2 = PITNumber2)

skimr::skim(df0)


# data manipulation -------------------------------------------------------

## drop rows with no PIT number
## clean injury column

df1 <- df0 %>% 
  drop_na(pit_number) %>% 
  mutate(injury = case_when(injury == "indured" ~ "YES",
                            TRUE ~ as.character(injury)))

## double tagged individuals
## to avoid confusion, 
pit_double <- unique(na.omit(df1$pit_number2))

for(i in seq_len(length(pit_double))) {
  
  ## extract original pit ID for each double tagged individual
  pit_org <- df1 %>% 
    filter(pit_number2 == pit_double[i]) %>% 
    pull(pit_number)
  
  ## replace pit ID
  df1 <- df1 %>% 
    mutate(pit_number = ifelse(pit_number == pit_double[i],
                               yes = pit_org,
                               no = pit_number))
}

# export ------------------------------------------------------------------

write_csv(df1,
          file = "data_fmt/cmr_fmt.csv")


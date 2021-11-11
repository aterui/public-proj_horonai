
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse,
               foreach)


# read data ---------------------------------------------------------------

df0 <- list.files(path = "data/data_raw") %>% 
  as_tibble() %>% 
  filter(str_detect(value, "port")) %>% 
  filter(str_detect(value, "csv")) %>% 
  pull() %>% 
  paste0("data/data_raw/", .) %>% 
  lapply(read_csv) %>% 
  lapply(FUN = function(x) {
    x$Date <- as.Date(x$Date, format = "%Y/%m/%d")
    return(x)
  }) %>% 
  do.call(bind_rows, .) %>% 
  rename(pit_number = PITNumber)

names(df0) <- str_to_lower(names(df0))

skimr::skim(df0)


# data manipulation -------------------------------------------------------

## create `occasion`
## remove dummy pit data
## remove tributary data
## correct error data - 19800 to 1980
## correct date error
df1 <- df0 %>% 
  filter(dummy == "NO",
         !str_detect(section, "t")) %>% 
  mutate(section = ifelse(section == 19800, 1980, section),
         date = case_when(date == as.Date("2020-12-12") ~ as.Date("2020-11-12"),
                          date == as.Date("2020-01-14") ~ as.Date("2021-01-14"),
                          TRUE ~ as.Date(date))) %>% 
  mutate(year_month_code = as.factor(format(date, "%y-%m")),
         occasion = as.numeric(year_month_code))
  
# export ------------------------------------------------------------------

write_csv(df1, file = "data/data_fmt/port_fmt.csv")

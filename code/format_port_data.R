
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
df1 <- df0 %>% 
  mutate(year_month_code = as.factor(format(date, "%y-%m")),
         occasion = as.numeric(year_month_code)) %>% 
  filter(dummy == "NO",
         !str_detect(section, "t"))

# export ------------------------------------------------------------------

write_csv(df1, file = "data/data_fmt/port_fmt.csv")

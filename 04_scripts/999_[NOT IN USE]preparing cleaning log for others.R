rm(list = ls())
library(dplyr)
library(tidyr)
library(illuminate)

other_cleaning_log<- read.csv("01_inputs/03_cleaning_log/informal_sites__HH_SURVEYs_cleaning_log - reach_others_hh.csv",na.strings = "")

all_others <- list()

 all_others[["other_change_response_only"]]<- other_cleaning_log %>% filter(is.na(column_name)) %>% mutate(
  change_type ="change_response"
) %>% mutate(
  change_type = case_when(new_value == "blank_response" ~ "blank_response",
                          T~change_type)
)


others_without_chan <- other_cleaning_log %>% filter(!is.na(column_name))




other_cl_new_name <- others_without_chan %>% select(-questions) %>% rename(
  questions=column_name
) %>% mutate(
  change_type = "change_response",
  new_value = 1 %>% as.character()
)
other_bank_response <- others_without_chan %>% select(-column_name) %>% mutate(
  change_type = "blank_response",
)

all_others[["oth_cls"]] <- other_bank_response %>%  bind_rows(other_cl_new_name)



cleaning_log_others <- do.call("bind_rows",all_others) %>% mutate(
  new_value = case_when(change_type == "blank_response"~ NA_character_,
                        T~new_value)
)


# data --------------------------------------------------------------------

hh <- read.csv("01_inputs/raw_data/01_hh/01_reach/hh.csv")

new_Cols <- oth_cls$questions[!oth_cls$questions %in% names(hh)] %>% unique()

for (i in new_Cols) {
  x <- gsub("\\..*","",i)
  print(x)
  print(i)
  hh[[i]] <- case_when(!is.na(hh[[x]]) ~ 0)
  
}



data <- implement_cleaning_log(df = hh,df_uuid = "X_uuid",cl = oth_cls,cl_change_type_col = "change_type",
                               cl_change_col = "questions",cl_uuid = "X_uuid",cl_new_val = "new_value")

write.csv(data,"t.csv")

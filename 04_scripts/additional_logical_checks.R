rm(list = ls())
library(readxl)
library(dplyr)
library(tidyr)
library(illuminate)
library(stringr)

read_all_sheet_as_csv_format("02_outputs/01_hh/02_clean_dataset/hh_clean_dataset.xlsx")
read_all_sheet_as_csv_format("01_inputs/01_hh/logical_checks.xlsx")

hh_clean_dataset$number_of_children_hh <- hh_clean_dataset$number_of_children_hh  %>% as.numeric()
additional_logical_checks <- illuminate::logical_check(df =hh_clean_dataset,uuid = "X_uuid",
                                                       logic_list_df = Sheet1,add_description = T,
                                                       logic_col_name = "logic",col_name_issue = "issue")

additional_logical_checks <- additional_logical_checks %>% mutate(
  change_type = case_when(question %in% c("own_shelter_issues.none","shelter_priority_concerns.none",
                                          "info_needed_type.none", "timeframe_waste_collection_sufficient",
                                          "number_of_children_hh"
                                          ) ~ "blank_response", T ~ "to_remove" 
                          )
) %>% filter(change_type!= "to_remove")

additional_logical_checks$question %>% table()

write_excel_as_reach_format(list(additional_logical_checks = "logical_check"),"logical_checks.xlsx")
                                                       
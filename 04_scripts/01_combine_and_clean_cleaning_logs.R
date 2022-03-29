# This script is the first step of data cleaning and analysis. This combine all the cleaning logs adn recode others accordenly
# Make sure you run for both HH and site[ You need to run this script twice by use 1 and 2 individually, line no 11]
rm(list = ls())

library(dplyr)
library(illuminate)
library(stringr)
library(readxl)
library(openxlsx)

dataset_list <-c("hh","sites")[1]

for (dataset in  dataset_list ){

if(dataset == "hh"){
#################################################### Start:: preparing others cleaning log ##################################################

# read cleaning log ------------------------------------------------------------------
read_all_sheet_as_csv_format("01_inputs/01_hh/01_cleaning_log_gsheet/informal_sites__HH_SURVEYs_cleaning_log.xlsx")

rm(suvey_tracking)

# combine_others ----------------------------------------------------------
reach_others_hh <- reach_others_hh %>% rename(
  column_name = new_name
)
all_others <- list()

all_others[["other_change_response_only"]]<- reach_others_hh %>% filter(is.na(column_name)) %>% mutate(
  change_type ="change_response"
) %>% mutate(
  change_type = case_when(new_value == "blank_response" ~ "blank_response",
                          column_name == "blank_response" ~ "blank_response",
                          T~change_type)
  
)

others_without_chan <- reach_others_hh %>% filter(!is.na(column_name))


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
) %>% select(X_uuid,repeat_uuid,questions,change_type,old_value,new_value,issue) %>% filter(questions != "subdistrict_other")



################## fixing question name in new_value #####################
cleaning_log_others2 <- cleaning_log_others  %>% mutate(
  interchange = case_when(grepl("_other",cleaning_log_others$questions) &
                            grepl("\\.",cleaning_log_others$new_value) ~ T,T~F)
)  

intercahnge_true <-  cleaning_log_others2 %>% filter(interchange ==T)

intercahnge_true_blank_response <- intercahnge_true %>% mutate(
  change_type = "blank_response",
  new_value = NA_character_
)
intercahnge_true_change_response <- intercahnge_true %>% mutate(
  questions = new_value,
  new_value = "1",
  old_value = NA_character_
)
  
intercahnge_false <-  cleaning_log_others2 %>% filter(interchange ==F)

cleaning_log_others <- bind_rows(intercahnge_false,intercahnge_true_change_response,intercahnge_true_blank_response) %>% select(-interchange)

######################################


rm(list = ls()[!ls() %in% c("cleaning_log_others","dataset","dataset_list")])

###################################################### END:: preparing others ############################################################

read_all_sheet_as_csv_format("01_inputs/01_hh/01_cleaning_log_gsheet/informal_sites__HH_SURVEYs_cleaning_log.xlsx")
rm(suvey_tracking,reach_others_hh)
cols_for_cl <- names(cleaning_log_others)


# deletation log ----------------------------------------------------------

deletation_log <- deletation_log  %>% distinct(X_uuid,.keep_all= TRUE)


# combine_cleaning_log ----------------------------------------------------
all_cl <- list()
list_of_df <- Filter(function(x) is.data.frame(get(x)), ls())

for (i in list_of_df) {
  print(i)
  df <- get(i) %>% as.data.frame()
  df <- df %>% select(cols_for_cl)
  df <- df %>% mutate_all(funs(as.character))
  all_cl[[i]] <- df %>% mutate(data_source = i)
}

all_cleaning_log <- do.call("bind_rows",all_cl)
cleaning_log_hh <- all_cleaning_log %>% filter(!(data_source != "cleaning_log_others" & issue == "Other_response" )) %>% arrange(X_uuid)

write_excel_as_reach_format(list(cleaning_log_hh="cleaning_log_hh"),"02_outputs/01_hh/01_final_cleaning_log/cleaning_log_hh.xlsx")
}


if(dataset == "sites"){
  
  rm(list = ls()[!ls() %in% c("dataset","dataset_list")])
  
  #################################################### Start:: preparing others cleaning log ##################################################
  
  # read cleaning log ------------------------------------------------------------------
  read_all_sheet_as_csv_format("01_inputs/02_sites/01_cleaning_log_gsheet/informal_sites__TL_SURVEYs_cleaning_log.xlsx")

  
  # combine_others ----------------------------------------------------------
  reach_others_tls <- reach_others_tls %>% rename(
    column_name = new_name
  )
  all_others <- list()
  
  all_others[["other_change_response_only"]]<- reach_others_tls %>% filter(is.na(column_name)) %>% mutate(
    change_type ="change_response"
  ) %>% mutate(
    change_type = case_when(new.value == "blank_response" ~ "blank_response",
                            column_name == "blank_response" ~ "blank_response",
                            T~change_type)
    
  )
  
  others_without_chan <- reach_others_tls %>% filter(!is.na(column_name))
  
  
  other_cl_new_name <- others_without_chan %>% select(-question.name) %>% rename(
    question.name=column_name
  ) %>% mutate(
    change_type = "change_response",
    new.value = 1 %>% as.character()
  )
  other_bank_response <- others_without_chan %>% select(-column_name) %>% mutate(
    change_type = "blank_response",
  )
  
  all_others[["oth_cls"]] <- other_bank_response %>%  bind_rows(other_cl_new_name)
  
  
  
  cleaning_log_others_tls <- do.call("bind_rows",all_others) %>% mutate(
    new.value = case_when(change_type == "blank_response"~ NA_character_,
                          T~new.value)
  ) %>% select(X_uuid,question.name,change_type,old.value,new.value,issue) %>% filter(question.name != "subdistrict_other") %>% filter(question.name != "blank_response")
  
  
  
  ################## fixing question name in new_value #####################
  # cleaning_log_others2 <- cleaning_log_others_tls  %>% mutate(
  #   interchange = case_when(grepl("_other",cleaning_log_others_tls$question.name) &
  #                             grepl("\\.",cleaning_log_others_tls$new.value) ~ T,T~F)
  # )  
  # 
  # intercahnge_true <-  cleaning_log_others2 %>% filter(interchange ==T)
  # 
  # intercahnge_true_blank_response <- intercahnge_true %>% mutate(
  #   change_type = "blank_response",
  #   new.value = NA_character_
  # )
  # intercahnge_true_change_response <- intercahnge_true %>% mutate(
  #   question.name = new.value,
  #   new.value = "1",
  #   old.value = NA_character_
  # )
  # 
  # intercahnge_false <-  cleaning_log_others2 %>% filter(interchange ==F)
  # 
  # cleaning_log_others_tls <- bind_rows(intercahnge_false,intercahnge_true_change_response,intercahnge_true_blank_response) %>% select(-interchange)
  # 
  ######################################

  rm(list = ls()[!ls() %in% c("cleaning_log_others_tls","dataset","dataset_list")])
  
  read_all_sheet_as_csv_format("01_inputs/02_sites/01_cleaning_log_gsheet/informal_sites__TL_SURVEYs_cleaning_log.xlsx")
  
  rm(reach_others_tls)

  
  # combine_cleaning_log ----------------------------------------------------
  all_cl <- list()
  list_of_df <- Filter(function(x) is.data.frame(get(x)), ls())
  
  for (i in list_of_df) {
    df <- get(i) %>% as.data.frame()
    df <- df %>% mutate_all(funs(as.character))
    print(i)
    print(duplicated(names(df)) %>% table())
    all_cl[[i]] <- df %>% mutate(data_source = i)
  }
  
  all_cleaning_log <- do.call("bind_rows",all_cl)
  cleaning_log_sites <- all_cleaning_log %>% rename(
    questions= question.name,
    old_value =old.value,
    new_value = new.value
  ) %>% select(X_uuid,questions,change_type,old_value,new_value,issue,data_source) %>% arrange(X_uuid)
 
   write_excel_as_reach_format(list(cleaning_log_sites="cleaning_log_sites"),"02_outputs/02_sites/01_final_cleaning_log/cleaning_log_sites.xlsx")
  
  }

}







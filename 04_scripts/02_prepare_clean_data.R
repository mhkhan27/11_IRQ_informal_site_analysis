### Need to run 01_combine_and_clean_cleaning_logs.R first
### There are some temporary line in this script. There should be no temporary line in the final version. I have used this temporary line to get rid of some error. So you need to fix the error first.
### Download and save the dataset as csv utf8 

# rm(list = ls())

library(dplyr)
library(openxlsx)
library(readxl)
library(stringr)
library(illuminate)
library(tidyr)
library(lubridate)

# data_set ----------------------------------------------------------------

# dataset <- c("hh","sites") [2]

de_sample <- read.csv("01_inputs/03_sample_frame/Master_list.csv") %>% 
  select(ID.,Governorate,District,Sub.District) %>% rename(
    governorate = Governorate,
    district = District,
    subdistrict = Sub.District
  )

 if(dataset == "hh") {
  output_hh <- list() #create output list
  
  ############################################# hh data combine ####################################
  hh_reach <- read.csv("01_inputs/01_hh/01_reach/hh.csv",na.strings = c("","NA"," "),encoding = "UTF-8") %>% mutate(data_source = "REACH")
  hh_partner <- read.csv("01_inputs/01_hh/02_partner/hh.csv",encoding = "UTF-8") %>%  mutate(data_source = "partner")
  names(hh_reach) %in% names(hh_partner) %>% table()
  names(hh_partner) %in% names(hh_reach) %>% table()
  hh_reach$enumerator_name <-hh_reach$enumerator_name %>% as.character()  
  
  raw_hh <- hh_reach %>% bind_rows(hh_partner)
 raw_hh <- raw_hh %>% rename("start"= "X.U.FEFF.start" )

  
  ############################################ indv data combine ##################################
  indv_reach <- read.csv("01_inputs/01_hh/01_reach/indv.csv",na.strings = c("","NA"," "),encoding = "UTF-8") %>% mutate(data_source = "REACH")
  indv_partner <- read.csv("01_inputs/01_hh/02_partner/indv.csv",encoding = "UTF-8") %>%  mutate(data_source = "partner")
  names(indv_reach) %in% names(indv_partner) %>% table()
  names(indv_partner) %in% names(indv_reach) %>% table()

  indv_raw <- indv_reach %>% bind_rows(indv_partner)
  indv_raw <- indv_reach %>% rename( "index"="X.U.FEFF.index")
  
  rm(list=ls()[!ls() %in% c("raw_hh","indv_raw","dataset","output_hh","de_sample")])
  


  ############################# cleaning log fix #####################################################
  
  cleaning_log_hh_level <- read_excel("02_outputs/01_hh/01_final_cleaning_log/cleaning_log_hh_final.xlsx")
  cleaning_log_hh_level$change_type %>% table()
  cleaning_log_hh_level$questions <-  cleaning_log_hh_level$questions %>% trimws()
  cleaning_log_hh_level <- cleaning_log_hh_level %>% filter(!questions %in% "verification_site_other" )
  cleaning_log_hh_level$change_type %>% table()
  # replace "/" to "."
  cleaning_log_hh_level$questions <- cleaning_log_hh_level$questions %>% str_replace_all("/","\\.")
  
  ## make all remove survey as hh 
  rm_svy_uuid <- (cleaning_log_hh_level %>% filter(change_type == "remove_survey"))$X_uuid
  cleaning_log_hh_level <- cleaning_log_hh_level %>% mutate(
    change_type = case_when(X_uuid %in% rm_svy_uuid~ "remove_survey",
                            T~ change_type)
  )
  
  
  
  # check dataset loop for overall cleaning log
  hh_cleaning_log_hh_data <- cleaning_log_hh_level %>% mutate(
    dataset_loop = case_when(questions %in% names(raw_hh) ~ "hh",
                             questions %in% names(indv_raw) ~ "indv",
                            T ~ NA_character_)
  ) %>% filter(change_type != "no_action") %>% mutate(
    dataset_loop =  case_when(change_type  == "remove_survey" ~ "hh",
                              T~ dataset_loop)
  )
  
 hh_cleaning_log_hh_data_with_loop <- hh_cleaning_log_hh_data %>% filter(!is.na(dataset_loop) | change_type == "remove_survey")
 hh_cleaning_log_hh_data_without_loop <- hh_cleaning_log_hh_data %>% filter(is.na(dataset_loop) & change_type != "remove_survey" )

############## TEMPORARY ##########
 # hh_cleaning_log_hh_data_without_loop$questions <- hh_cleaning_log_hh_data_without_loop$questions %>% str_replace_all("eviction_reasons","risk_of_eviction_reasons")
###################################
 
  hh_cleaning_log_hh_data_without_loop <- hh_cleaning_log_hh_data_without_loop %>% mutate(
   parent_name = sub("\\..*", "", questions)
 ) %>% mutate(
   dataset_loop = case_when(parent_name %in% names(raw_hh) ~ "hh",
                            parent_name %in% names(indv_raw) ~ "indv",
                            T ~ NA_character_)
 )
 
hh_cleaning_log_hh_data_without_loop$dataset_loop %>% is.na() %>% table()
hh_cleaning_log_hh_data_with_loop$dataset_loop %>% is.na() %>% table()
  
final_hh_cleaning_log <- hh_cleaning_log_hh_data_with_loop %>% bind_rows(hh_cleaning_log_hh_data_without_loop) %>% select(-parent_name)
final_hh_cleaning_log <- final_hh_cleaning_log %>% mutate(
  change_type = case_when(change_type == "Change_response" ~ "change_response", T ~ change_type)
)


nrow(hh_cleaning_log_hh_data_with_loop)+ nrow(hh_cleaning_log_hh_data_without_loop)
 nrow(hh_cleaning_log_hh_data)
nrow(final_hh_cleaning_log)
final_hh_cleaning_log$dataset_loop %>% is.na() %>% table()

############ make .other to 0 when _other is blank response ###################################
final_hh_cleaning_log_blank  <- final_hh_cleaning_log %>% filter(change_type == "blank_response")
 
 
 cols_name <- c(names(raw_hh),names(indv_raw))
 
 final_hh_cleaning_log_blank <- final_hh_cleaning_log_blank %>% mutate(
   end_with_other = case_when(grepl("_other",final_hh_cleaning_log_blank$questions) ~1, T~0)) %>% filter(end_with_other ==1) %>% mutate(
     questions = questions %>% str_replace_all("_other",".other"),
     change_type = "change_response",
     new_value = 0 %>% as.character(),
     old_value = 1 %>% as.character()
   ) %>% filter(questions %in% cols_name) %>% select(-end_with_other)
 
 final_hh_cleaning_log <- final_hh_cleaning_log %>% bind_rows(final_hh_cleaning_log_blank)
################################ 



########################## hh data cleaning ##########################################################
hh_cleaning_log_for_hh_data <-   final_hh_cleaning_log %>% filter(dataset_loop == "hh")

# nrow(hh_claning_log_for_hh_data)+ nrow(indv_claning_log_for_hh_data)

##### Create newly created choice muyltiple cols #########
new_cols_hh <- hh_cleaning_log_for_hh_data$questions[!hh_cleaning_log_for_hh_data$questions %in% names(raw_hh)] %>% unique()
new_cols_hh<- new_cols_hh[!new_cols_hh %in% c(NA_real_,"why_not_attending_secondary_other","ï..start","why_not_attending_primary",
                                              "why_not_attending_secondary","why_not_attending_primary_other")]

for (i in new_cols_hh) {
  x <- gsub("\\..*","",i)
  print(x)
  print(i)
  raw_hh[[i]] <- case_when(!is.na(raw_hh[[x]]) ~ 0)
  
}


check_cleaning_log(df = raw_hh,df_uuid = "X_uuid",cl = hh_cleaning_log_for_hh_data,cl_change_type_col = "change_type",cl_uuid = "X_uuid", cl_change_col = "questions",cl_new_val = "new_value")
# write.csv(a,"uuid_does_not_exist.csv")
# hh_cleaning_log_for_hh_data <- hh_cleaning_log_for_hh_data %>% filter(!X_uuid %in% a$X_uuid)

hh_clean_dataset <- implement_cleaning_log(df = raw_hh,df_uuid = "X_uuid",cl = hh_cleaning_log_for_hh_data,cl_change_type_col = "change_type",cl_uuid = "X_uuid", cl_change_col = "questions",cl_new_val = "new_value")




hh_clean_dataset <- recalculate_concerted_col_for_select_multiple(df = hh_clean_dataset,uuid = "X_uuid")


################################### Fix admin boundary according to IOM ##########################

hh_clean_dataset <- hh_clean_dataset %>% mutate(
  location_site = case_when(subdistrict == "other87" ~ "430_Balad_station",
                            T~ location_site)
) %>% mutate(
  ID. = readr::parse_number(location_site)
)

hh_clean_dataset$location_site %>% is.na() %>% table()
hh_clean_dataset$ID. %>% is.na() %>% table()                     #### NEED TO FIX this, there is one other


# subdistrict name cleaning -----------------------------------------------

cols_order <- hh_clean_dataset %>% names() %>% dput()
# to make sure all the data has admin data, here i havent considered filtered sampleframe, instead I used the raw sample frame


hh_clean_dataset <- hh_clean_dataset %>% select(-c("governorate","district","subdistrict"))

hh_clean_dataset <- hh_clean_dataset %>% left_join(de_sample) %>% select(cols_order)

hh_clean_dataset$location_site %>% is.na() %>% table()
(hh_clean_dataset$location_site == "other") %>% table() #### There should be no TRUE (no other)


to_remove_hh <- c("district_other","governorate_other","subdistrict_other","sites_other",
                  "enumerator_num","ngo_label",
                  "enumerator_name",
                  "survey_consent_on_behalf",
                  "further_contact_willing",
                  "contact_details_name",
                  "contact_details_telephone",
                  "site_gps_coordinates",
                  "X_site_gps_coordinates_latitude",
                  "X_site_gps_coordinates_longitude",
                  "X_site_gps_coordinates_altitude",
                  "X_site_gps_coordinates_precision")

raw_hh <- raw_hh %>% select(-to_remove_hh)
hh_clean_dataset <- hh_clean_dataset %>% select(-to_remove_hh)



hh_clean_dataset$settlement_duration_more
hh_clean_dataset$settlement_date <- hh_clean_dataset$settlement_date  %>% ymd()


interval(ymd(hh_clean_dataset$settlement_date), ymd("2021-12-01"))

hh_clean_dataset$settlement_duration_more <-  (year(ymd("2021-12-01"))- year(hh_clean_dataset$settlement_date) ) * 12 + 
  month(ymd("2021-12-01")) - month(hh_clean_dataset$settlement_date) 

hh_clean_dataset <- hh_clean_dataset %>% mutate(
  drinking_water_quality = case_when(drinking_water_quality == "none" ~ NA_character_,
                                      T~drinking_water_quality)
)

hh_clean_dataset$drinking_water_quality
output_hh[["raw_hh"]] <- raw_hh 
output_hh[["indv_raw"]] <- indv_raw
output_hh[["final_hh_cleaning_log"]] <- final_hh_cleaning_log
output_hh[["hh_clean_dataset"]] <- hh_clean_dataset 

######################################### indv data cleaining ######################################
indv_cleaning_log_for_hh_data <-   final_hh_cleaning_log %>% filter(dataset_loop == "indv") 
indv_cleaning_log_for_hh_data$repeat_uuid %>% is.na() %>% table()


indv_pre_clean <-  indv_raw %>% filter(X_submission__uuid %in% hh_clean_dataset$X_uuid)


check_cleaning_log(df = indv_pre_clean,df_uuid = "repeat_uuid",
                   cl = indv_cleaning_log_for_hh_data,
                   cl_change_type_col = "change_type",
                   cl_uuid = "repeat_uuid",
                   cl_change_col = "questions",cl_new_val = "new_value")


indv_cleaning_log_for_hh_data$change_type %>% table() # There should be no remove_survey

indv_clean_data <- implement_cleaning_log(df = indv_pre_clean,df_uuid = "repeat_uuid",
                   cl = indv_cleaning_log_for_hh_data,
                   cl_change_type_col = "change_type",
                   cl_uuid = "repeat_uuid",
                   cl_change_col = "questions",cl_new_val = "new_value")
        
indv_clean_data <- recalculate_concerted_col_for_select_multiple(df = indv_clean_data,uuid = "repeat_uuid")

output_hh[["indv_clean_data"]] <- indv_clean_data

write_excel_as_reach_format(output_hh,"02_outputs/01_hh/02_clean_dataset/hh_clean_dataset.xlsx")
}


 if(dataset == "sites"){
  output_site <- list()
  reach_tls <- read.csv("01_inputs/02_sites/01_reach//hh.csv")
  partner_tls <- read.csv("01_inputs/02_sites/02_partner/hh.csv")
  
  names(reach_tls) %in% names(partner_tls) %>% table()
  names(partner_tls) %in% names(reach_tls) %>% table()
  names(reach_tls)[!names(reach_tls) %in% names(partner_tls)]

  raw_sites <- reach_tls %>% bind_rows(partner_tls)
  # rm(partner_tls,reach_tls)
  
  ###### Site Location fix #######################

  raw_sites <- raw_sites %>% mutate(
    location_site = case_when(subdistrict == "other87" ~ "430_Balad_station",
                              T~ location_site)
  ) 
  
  
  #################################################
  site_cleaning_log <- read_excel("02_outputs/02_sites/01_final_cleaning_log/cleaning_log_sites_final.xlsx")
  site_cleaning_log <- site_cleaning_log %>% filter(change_type != "no_action")
  site_cleaning_log$questions <- site_cleaning_log$questions %>% str_replace_all("/","\\.")
  
  
  
  ##### Create newly created choice muyltiple cols #########
  new_cols_site <- site_cleaning_log$questions[!site_cleaning_log$questions %in% names(raw_sites)] %>% unique()
  new_cols_site<- new_cols_site[!new_cols_site %in% c(NA_real_)]
  
  for (i in new_cols_site) {
    x <- gsub("\\..*","",i)
    print(x)
    print(i)
    raw_sites[[i]] <- case_when(!is.na(raw_sites[[x]]) ~ 0)
    
  }
  
  check_cleaning_log(df = raw_sites,df_uuid = "X_uuid",cl = site_cleaning_log,
                     cl_change_type_col = "change_type",cl_change_col = "questions",
                     cl_uuid = "X_uuid",cl_new_val = "new_value")
  
  clean_data_site <- implement_cleaning_log(df = raw_sites,df_uuid = "X_uuid",cl = site_cleaning_log,
                                         cl_change_type_col = "change_type",cl_change_col = "questions",
                                         cl_uuid = "X_uuid",cl_new_val = "new_value")
  
  clean_data_site <- clean_data_site %>% mutate(
    ID. = readr::parse_number(location_site)
  )
  
  cols_order_site <- names(clean_data_site)
  
  clean_data_site <- clean_data_site %>% select(-c("governorate","district","subdistrict"))  
  clean_data_site <- clean_data_site %>% left_join(de_sample) 
  clean_data_site <- clean_data_site%>% select(cols_order_site)
  
  clean_data_site$location_site %>% is.na() %>% table()
  (clean_data_site$location_site == "other") %>% table() 
  
  
  clean_data_site <- recalculate_concerted_col_for_select_multiple(df = clean_data_site,uuid = "X_uuid")
  
  to_remove_site <- c("further_contact_willing","contact_details_name","ngo_label","ngo_name_other",
                        "contact_details_telephone","today","verification_site_no_name","leader_label",
                        "site_gps_coordinates","ngo_label",
                        "X_site_gps_coordinates_latitude",
                        "X_site_gps_coordinates_longitude",
                        "X_site_gps_coordinates_altitude",
                        "X_site_gps_coordinates_precision",
                        "enumerator_num",
                        "enumerator_name","district_other",
                        "governorate_other","subdistrict_other",
                        "sites_other")
    
  raw_sites <- raw_sites %>% select(-to_remove_site)
  clean_data_site<-clean_data_site  %>% select(-to_remove_site)
  
  
  ####### fix _other and .other ##########

  clean_data_site <- clean_data_site %>% mutate(
    verification_site_type.other= case_when(is.na(verification_site_type_other) ~ as.double(0), T~ as.double(verification_site_type.other)),
    fire_safety_equipment_site_types.other= case_when(is.na(fire_safety_equipment_site_types_other) ~ as.double(0), T~ as.double(fire_safety_equipment_site_types.other)),
    shelter_type_in_site.other= case_when(is.na(shelter_type_in_site_other) ~ as.double(0), T~ as.double(shelter_type_in_site.other))
    
      )
  
  #################
   
  
  output_site[["raw_sites"]] <- raw_sites
  output_site[["site_cleaning_log"]] <- site_cleaning_log
  output_site[["clean_data_site"]] <- clean_data_site  
  write_excel_as_reach_format(output_site,"02_outputs/02_sites/02_clean_data/site_clean_dataset.xlsx")
  
  
   }



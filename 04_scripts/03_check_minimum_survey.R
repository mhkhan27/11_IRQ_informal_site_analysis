#RUN script 1 and 2 first.

rm(list = ls())

library(dplyr)
library(stringr)
library(illuminate)
library(openxlsx)
output <- list()
# read_data ---------------------------------------------------------------

read_all_sheet_as_csv_format("02_outputs/01_hh/02_clean_dataset/hh_clean_dataset.xlsx")

hh_cleaned <- hh_clean_dataset %>% filter(consent =="yes")  %>% filter(verification_site == "yes")
hh_cleaned$location_site %>% is.na() %>% table()
hh_cleaned$ID. %>% is.na() %>% table()

########################################### PROGRESS TRACKER ####################################################

# district_sub_governorate ------------------------------------------------

details_sample <- read.csv("01_inputs/03_sample_frame/sample_frame.csv") %>% rename(
  governorate = Governorate,
  district = District,
  subdistrict = Sub.District
) %>% select(ID.,governorate,district,subdistrict,survey_buffer,Masterlist.Informal.Site.Name) %>% mutate(
  location_Site_df =  paste0(ID., "_", Masterlist.Informal.Site.Name) %>% 
    str_replace_all("/","_") %>% str_replace_all(" ","_") %>%  str_replace_all("&","_") 
) %>% select(-Masterlist.Informal.Site.Name)



completed_by_grps <- hh_cleaned %>% group_by(governorate,district,subdistrict,location_site) %>% summarise(
  completed = n()
) %>% ungroup() %>% mutate(
  ID. = readr::parse_number(location_site)
)


progress_df <- details_sample %>%  left_join(completed_by_grps) %>% select(everything(),survey_buffer) 
progress_df$completed <- case_when(is.na(progress_df$completed)~ as.double(0),
                                   T~as.double(progress_df$completed))

progress_df <- progress_df %>% mutate(
  remaining = survey_buffer - completed
)


df_sites <- progress_df %>% select(everything(),survey_buffer,completed,remaining) %>% select(-location_site) %>% rename(
  location_site= location_Site_df
)

df_sites$remaining <- case_when(is.na(df_sites$remaining)~ as.double(0),
                                T~as.double(df_sites$remaining))

############ sub district #############################
frame <- read.csv("01_inputs/03_sample_frame/summary.csv") %>% rename(
  subdistrict = Stratification 
) %>% select(subdistrict,minimum_survey_needed)

details_sf_by_sub <- details_sample %>% group_by(governorate,district,subdistrict) %>% summarise(
  survey_buffer = sum(survey_buffer,na.rm = T) 
) %>% ungroup() %>% left_join(frame)

progress_df_sub_dist <- progress_df %>% group_by(governorate,district,subdistrict) %>% summarise(
  completed = sum(completed,na.rm = T)) %>% ungroup()


df_sub <-details_sf_by_sub %>% left_join(progress_df_sub_dist) %>% mutate(
  remaining = survey_buffer-completed,
  Minmum_remaining  = minimum_survey_needed - completed
)
df_sub$remaining <- case_when(is.na(df_sub$remaining)~ as.double(0),
                              T~as.double(df_sub$remaining))

df_sub$Minmum_remaining <- case_when(is.na(df_sub$Minmum_remaining)~ as.double(0),
                                     T~as.double(df_sub$Minmum_remaining))

output[["prgrs_by_subdis"]] <- df_sub %>% mutate(
  completed_status = case_when(Minmum_remaining < 1 ~ "Completed",
                               T~ "Incompleted")
)


############ district #############################
details_sf_by_dis <- details_sample %>% group_by(governorate,district) %>% summarise(
  survey_buffer = sum(survey_buffer,na.rm = T) 
)

progress_df_dist <- progress_df %>% group_by(governorate,district) %>% summarise(
  completed = sum(completed,na.rm = T))


output[["prgrs_by_dis"]] <-details_sf_by_dis %>% left_join(progress_df_dist) %>% mutate(
  remaining = survey_buffer-completed
)

############ gov #############################


details_sf_by_gov <- details_sample %>% group_by(governorate) %>% summarise(
  survey_buffer = sum(survey_buffer,na.rm = T) 
)

progress_df_govt <- progress_df %>% group_by(governorate) %>% summarise(
  completed = sum(completed,na.rm = T))


output[["prgrs_by_gov"]] <-details_sf_by_gov %>% left_join(progress_df_govt) %>% mutate(
  remaining = survey_buffer-completed
)


# find tls  ---------------------------------------------------------------

read_all_sheet_as_csv_format("02_outputs/02_sites/02_clean_data/site_clean_dataset.xlsx")
tl_clean_df <- clean_data_site
tl_clean_df$ID. %>% is.na() %>% table()

site_count <- tl_clean_df %>% group_by(ID.) %>% summarise(
  count = n(),
  site_verification = paste(verification_site,collapse =  ",")
)


prgrs_by_sites <- df_sites %>% mutate(
  note = case_when(ID. %in% c(351,171)~"NO IDP exists in this site",
                   ID. == 219 ~ "Data collection was not possible due to Covid 19",
                   ID. == 75 ~ "Not exists" ,
                   ID. == 76 ~ "Not exists",
                   ID. == 148~ "No more IDPs to cover, re-distributed to other sites",
                   ID. == 213 ~ "No more IDPs to cover, re-distributed to 212_Mud___block_structures_East_frqa",
                   ID. ==  352 ~ "No more IDPs to cover, re-distributed to other sites" ),
  check_tl_form = case_when(ID. == 122  ~ NA_character_,
                            !(ID. %in% tl_clean_df$ID.) ~ "Team leader form not found for this site"
  )) %>% mutate(
    check_tl_form = case_when(completed == 0 ~ NA_character_,
                              T~check_tl_form)
  ) %>% left_join(site_count)

 tl_found_but_not_in_our_sample_frame <- site_count$ID.[!site_count$ID. %in% prgrs_by_sites$ID.]
 tl_found_but_not_in_hh <- tl_clean_df %>% filter(ID. %in% tl_found_but_not_in_our_sample_frame) 
 

 output[["prgrs_by_sites"]] <- prgrs_by_sites
 output[["tl_found_but_not_in_hh"]] <- tl_found_but_not_in_hh
 
list2env(output,.GlobalEnv)

write_excel_as_reach_format(output,paste0("02_outputs/03_progress_tracker/",str_replace_all(Sys.Date(),"-","_"),"_progress_tracker.xlsx"))



tl_clean_df$location_site[!tl_clean_df$location_site %in% hh_cleaned$location_site ]

rm(list = ls())

library(dplyr)
library(srvyr)
library(illuminate)
library(stringr)
library(openxlsx)
library(readxl)
library(purrr)
options(scipen = 999)

dataset <- c("hh","sites")[1]
source("04_scripts/02_prepare_clean_data.R")

# read data ---------------------------------------------------------------
if(dataset == "hh") {
  
  read_all_sheet_as_csv_format("02_outputs/01_hh/02_clean_dataset/hh_clean_dataset.xlsx")
  read_all_sheet_as_csv_format("03_tools/01_hh/hh_tool.xlsx")
  
  ##### needed to make newly created multiple choices logical
  sm_parent_child <- illuminate::auto_sm_parent_child(hh_clean_dataset)
  cols_to_logical<-  sm_parent_child$sm_child
  hh_clean_dataset <- hh_clean_dataset %>% mutate_at(cols_to_logical,funs(as.numeric))
  hh_clean_dataset <- hh_clean_dataset %>% mutate_at(cols_to_logical,funs(as.logical))
  
  hh <- fix_data_type_frm_kobo(df = hh_clean_dataset,kobo_survey_sheet = survey)
  
  
  indv <- fix_data_type_frm_kobo(indv_clean_data,survey)
  
  indv <- indv %>% left_join((hh %>% select(X_uuid,subdistrict,district,governorate)),by = c("X_submission__uuid" = "X_uuid"))
  
  rm(hh_clean_dataset,indv_clean_data)
  
  sample_frame <- read.csv("01_inputs/03_sample_frame/sample_frame_updated.csv")
  
  hh <- hh %>% filter(consent == "yes")
  indv <- indv %>% filter(X_submission__uuid %in% hh$X_uuid) 
  
  ######################################### Start::Recoding ##################################
  
  no_1_cols <- c("covid_soap_availability",
                 "feel_safe",
                 "primary_access",
                 "middle_access",
                 "covid_vulnerable_advice",
                 "handwashing_facilities",
                 "covid_practicing_preventative_measures",
                 "covid_testing_access")
  
  for (i in no_1_cols) {
    new_name <- paste0("i_",i)
    print(new_name)
    hh[[new_name]] <-case_when(hh[[i]] == "no"~1,
                               T~0)
  }
  
  
  yes_1_cols <- c("no_access_water","risk_of_eviction")
  
  for (i in yes_1_cols) {
    new_name <- paste0("i_",i)
    print(new_name)
    hh[[new_name]] <-case_when(hh[[i]] == "yes"~1,
                               T~0)
  }
  
  
  
  # distance ----------------------------------------------------------------
  
  
  dis_cols <- c("health_facility_distance","hospital_distance",
                "market_distance"
  )
  
  for (i in dis_cols) {
    new_name <- paste0("i_",i)
    print(new_name)
    hh[[new_name]] <-case_when(hh[[i]] == "within_2km"~0,
                               hh[[i]] == "between_2_5"~.5,
                               hh[[i]] == "more_than5kmaway"~.75,
                               hh[[i]] == "none"~1,
                               hh[[i]] == "do_not_know"~1)
  }
  
  
  
  # health facility services ------------------------------------------------
  
  hfs_cols <- hh %>% select(starts_with("health_facility_services.")) %>% select(-contains("do_not_kn")) %>% names()
  hospital_cols <- hh %>% select(starts_with("hospital_services.")) %>% select(-contains("do_not_kn")) %>%  names()
  
  
  # mutate ------------------------------------------------------------------
  
  
  
  hh <- hh %>% mutate(
    i_shelter_damaged = case_when(shelter_damaged == "none"~0,
                                  shelter_damaged == "less_quarter"~0.25,
                                  shelter_damaged == "most"~0.5,
                                  shelter_damaged == "less_half"~0.5,
                                  shelter_damaged == "nearly"~.75,
                                  shelter_damaged == "all"~1),
    i_insufficient_food = case_when(insufficient_food == "yes_all"~0,
                                    insufficient_food == "nearly_all"~0.75,
                                    insufficient_food == "most_time"~0.5,
                                    insufficient_food == "less_half_time"~0.5,
                                    insufficient_food == "less_quarter_time"~.25,
                                    insufficient_food == "never"~1),
    i_waste_disposal_frequency = case_when(waste_disposal_frequency == "never" ~ 1,
                                           waste_disposal_frequency == "after_1_week" ~ .75,
                                           T~0),
    i_latrine_types = case_when(latrine_types.open_defecation == 1 ~1,
                                latrine_types.open_hole == 1 |latrine_types.bucket_toilet ==1 | latrine_types.plastic_bag ==1| latrine_types.hanging_toilet ==1 ~ .75,
                                latrine_types.pit_latrine_without_a_slab_or_platform ==1 ~ .5,
                                latrine_types.pit_latrine_with_a_slab_and_platform ==1~.25,
                                latrine_types.pit_vip_toilet == 1 | latrine_types.flush_or_pour_toilet == 1 |latrine_types.do_not_know == 1~0),
    
    health_care_services_rs = rowSums(hh[hfs_cols],na.rm = T),
    hospital_services_rs = rowSums(hh[hospital_cols],na.rm = T),
    
    i_health_facility_services = case_when(health_care_services_rs == 11 ~ 0,
                                           health_care_services_rs %in% 6:10 ~ 0.25,
                                           health_care_services_rs %in% 1:5 ~ 0.75,
                                           health_care_services_rs == 0 ~ 1),
    i_hospital_services = case_when(hospital_services_rs == 8 ~ 0,
                                    hospital_services_rs %in% 5:7 ~ 0.25,
                                    hospital_services_rs %in% 1:4 ~ 0.75,
                                    hospital_services_rs == 0 ~ 1),
    
    i_shelter_priority_concerns = case_when(shelter_priority_concerns.none == 1|shelter_priority_concerns.no_improvement == 1~0,
                                            T~1),
    
    i_livelihood_opportunities = case_when(livelihood_opportunities.livelihood_none == 1~1,
                                           livelihood_opportunities.unskilled_agricultural_labour == 1 |
                                             livelihood_opportunities.unskilled_wage_labour ==1 | 
                                             livelihood_opportunities.debts == 1 |
                                             livelihood_opportunities.family_people_assistance ==1|
                                             livelihood_opportunities.casual_unskilled_labour ==1 ~.75,
                                           T~0),
    i_area_mines_chemicals_risk = case_when(area_mines_chemicals_risk.none== 1 ~0,
                                            area_mines_chemicals_risk.do_not_know == 1 ~ NA_real_,
                                            T~1),
    i_fire_safety_equipment = case_when(fire_safety_equipment.none == 1~1,
                                        T~0),
    i_fcs = cereals*2 +nuts_seed*3 + milk_dairy*4 + meat_fish_eggs*4 +vegetables+fruits+oil_fats*.5+sweets*.5+spices_condiments
    
    
  ) %>% select(-health_care_services_rs,-hospital_services_rs) %>% mutate(
    fcs_index = case_when(i_fcs > 42.5 ~ "acceptable",
                          i_fcs > 28.5 ~ "borderline",
                          TRUE ~ "poor")
  )
  
  is.na(hh$i_fcs) %>% table()
  
  

# hunger cetagory ---------------------------------------------------------
df <- hh
  
  
  hh <- hh %>% mutate(
    no_food_score= case_when(no_food == "yes" ~1,T~0),
    no_food_day_score= case_when(no_food_day == "yes" ~1,T~0),
    sleep_hungry_score= case_when(sleep_hungry == "yes" ~1,T~0),
  ) %>% mutate(
    no_food_score = case_when(no_food_frequency == "often_no_food" ~ no_food_score*2,
                              no_food_frequency %in%  c("rarely_no_food", "sometimes_no_food") ~ no_food_score*1,
                              T~0),
    sleep_hungry_score = case_when(sleep_hungry_frequency == "often_no_food" ~ sleep_hungry_score*2,
                                   sleep_hungry_frequency %in%  c("rarely_no_food", "sometimes_no_food") ~ sleep_hungry_score*1,
                              T~0),
    
    no_food_day_score = case_when(no_food_day_frequency == "often_no_food" ~ no_food_day_score*2,
                                   no_food_day_frequency %in%  c("rarely_no_food", "sometimes_no_food") ~ no_food_day_score*1,
                                   T~0)) %>% mutate(
                                     hunger_score= no_food_score+sleep_hungry_score+no_food_day_score
                                   ) %>% mutate(
                                     hunger_index = case_when(hunger_score <= 1 ~ "little_0_1",
                                                              hunger_score <= 3 ~ "Moderate_2_3",
                                                              hunger_score <= 6 ~ "Severe_4_6") 
                                   )

  
  
  
  is.na(hh$hunger_score) %>% table()
  
  
  # red flag calculation ----------------------------------------------------
  
  
  red_flag_cols <- hh %>% select(starts_with("i_")) %>% names()
  red_flag_cols <- red_flag_cols[!red_flag_cols%in% "i_fcs"]
  
  hh <- hh %>% mutate(
    red_fag_rs = rowSums(hh[red_flag_cols],na.rm = T),
    red_flag_score = red_fag_rs/23,
    i_red_flag_class = case_when(red_flag_score <.26 ~ "low_vulnerability",
                                 red_flag_score >.25 & red_flag_score < .51 ~ "moderate_vulnerability",
                                 red_flag_score >.50 & red_flag_score < .76 ~ "high_vulnerability",
                                 red_flag_score >.75 ~ "extreme_vulnerability",)
  )
  
  
  ##################################### END::Recoding ###################################
  
  #################################### Start::weighting #################################
  
  sample_frame_summary <- sample_frame %>% rename(subdistrict=Sub.District) %>% group_by(subdistrict) %>% summarise(
    family_population = sum(X..households),
    indv_population  = sum(X..individuals..estimate.)
  )
  
  
  ###################### hh Weights ######################
  
  weights_hh <- hh %>% group_by(subdistrict) %>% summarise(
    survey_count = n()
  ) %>% left_join(sample_frame_summary) %>% 
    mutate(sample_global=sum(survey_count),
           pop_global=sum(family_population),
           survey_weight= (family_population/pop_global)/(survey_count/sample_global)) %>% 
    select(subdistrict,family_population,pop_global,survey_count,sample_global,survey_weight)
  
  weights_only_hh <- weights_hh %>% select(subdistrict,survey_weight)
  
  hh_with_weight <- hh %>% left_join(weights_only_hh)
  
  ###################### indv Weights ######################
  
  weights_indv <- indv %>% group_by(subdistrict) %>% summarise(
    survey_count = n()
  ) %>% left_join(sample_frame_summary) %>% 
    mutate(sample_global=sum(survey_count),
           pop_global=sum(indv_population),
           survey_weight= (indv_population/pop_global)/(survey_count/sample_global)) %>% 
    select(subdistrict,indv_population,pop_global,survey_count,sample_global,survey_weight)
  
  weights_only_indv <- weights_hh %>% select(subdistrict,survey_weight)
  
  indv_with_weight <- indv %>% left_join(weights_only_indv)
  
  
  #### combine weighting framewrod #####
  weights_hh_c <-weights_hh %>% rename(family_total= pop_global,
                                       family_surveyed = survey_count,
                                       family_surveyed_total = sample_global,
                                       hh_weights= survey_weight)
  weights_indv_c <- weights_indv %>% rename(indv_total= pop_global,
                                            indv_surveyed = survey_count,
                                            indv_surveyed_total = sample_global,
                                            indv_weights= survey_weight)
  
  weighting_frame <- weights_hh_c %>% left_join(weights_indv_c)
  ######################################
  
  
  ############################################# Analysis ################################
  
  # hh analysis -------------------------------------------------------------
  
  hh_with_weight <- hh_with_weight %>% rename(
    livelihood_opportunities.lend_from_others=livelihood_opportunities.debts
  ) 
  
  hh_svy <- as_survey(hh_with_weight,strata = "subdistrict", weight = "survey_weight")
  
  hh_cols_remove_cols<- find_qualitative_cols(df = hh_with_weight,kobo_survey_sheet = survey,
                                              additional_cols_to_remove = c("survey_weight","audit_URL","instance_name","formatted_date_assessment","consent","verification_site",
                                                                            "ngo_name","district","governorate","ï..start",
                                                                            "subdistrict","location_site","ID.")) 
  cols_to_ana <- hh_with_weight %>% select(-hh_cols_remove_cols) %>% names()
  
  overall_analysis_hh <- survey_analysis(df = hh_svy,vars_to_analyze = cols_to_ana,kobo_path = "03_tools/01_hh/hh_tool.xlsx",
                                         question_lable = T,sm_sep = "/")  %>% arrange(main_variable) %>% mutate(
                                           `choice_label::English` = case_when(!is.na(choice) & is.na(`choice_label::English`) ~  (choice %>% str_replace_all("_"," ") %>%  stringr::str_to_sentence()), T~ `choice_label::English`))
  
  
  analysis_by_subdistrict_hh <- survey_analysis(df = hh_svy,vars_to_analyze = cols_to_ana,kobo_path = "03_tools/01_hh/hh_tool.xlsx",
                                                question_lable = T,sm_sep = "/",disag = "subdistrict")   %>% arrange(main_variable)%>% mutate(
                                                  `choice_label::English` = case_when(!is.na(choice) & is.na(`choice_label::English`) ~  (choice %>% str_replace_all("_"," ") %>%  stringr::str_to_sentence()), T~ `choice_label::English`))
  
  
  analysis_by_district_hh <- survey_analysis(df = hh_svy,vars_to_analyze = cols_to_ana,kobo_path = "03_tools/01_hh/hh_tool.xlsx",
                                             question_lable = T,sm_sep = "/",disag = "district")   %>% arrange(main_variable)%>% mutate(
                                               `choice_label::English` = case_when(!is.na(choice) & is.na(`choice_label::English`) ~  (choice %>% str_replace_all("_"," ") %>%  stringr::str_to_sentence()), T~ `choice_label::English`))
  
  analysis_by_gov_hh <- survey_analysis(df = hh_svy,vars_to_analyze = cols_to_ana,kobo_path = "03_tools/01_hh/hh_tool.xlsx",
                                        question_lable = T,sm_sep = "/",disag = "governorate")   %>% arrange(main_variable)%>% mutate(
                                          `choice_label::English` = case_when(!is.na(choice) & is.na(`choice_label::English`) ~  (choice %>% str_replace_all("_"," ") %>%  stringr::str_to_sentence()), T~ `choice_label::English`))
  # 
  # analysis_by_s_location_hh <- survey_analysis(df = hh_svy,vars_to_analyze = cols_to_ana,kobo_path = "03_tools/01_hh/hh_tool.xlsx",
  #                                               question_lable = T,sm_sep = "/",disag = "location_site")   %>% arrange(main_variable)%>% mutate(
  # `choice_label::English` = case_when(!is.na(choice) & is.na(`choice_label::English`) ~  (choice %>% str_replace_all("_"," ") %>%  stringr::str_to_sentence()), T~ `choice_label::English`))

  # analysis_by_f_dis_orig_hh <- survey_analysis(df = hh_svy,vars_to_analyze = cols_to_ana,kobo_path = "03_tools/01_hh/hh_tool.xlsx",
  #                                               question_lable = T,sm_sep = "/",disag = "family_district_of_origin")   %>% arrange(main_variable) %>% mutate(
# `choice_label::English` = case_when(!is.na(choice) & is.na(`choice_label::English`) ~  (choice %>% str_replace_all("_"," ") %>%  stringr::str_to_sentence()), T~ `choice_label::English`))

  
  
  
  # indv analysis -----------------------------------------------------------
  
  indv_svy <- as_survey(indv_with_weight,strata = "subdistrict", weight = "survey_weight")
  
  
  indv_cols_remove_cols<- find_qualitative_cols(df = indv_with_weight,kobo_survey_sheet = survey,
                                                additional_cols_to_remove = c("data_source","ï._index","X.U_FEFF_index","repeat_uuid","survey_weight")) 
  
  cols_to_ana_indv <- indv_with_weight %>% select(-indv_cols_remove_cols) %>% names()
  
  overall_analysis_indv <- survey_analysis(df = indv_svy,vars_to_analyze = cols_to_ana_indv,kobo_path = "03_tools/01_hh/hh_tool.xlsx",
                                           question_lable = T,sm_sep = "/")   %>% arrange(main_variable)%>% mutate(
                                             `choice_label::English` = case_when(!is.na(choice) & is.na(`choice_label::English`) ~  (choice %>% str_replace_all("_"," ") %>%  stringr::str_to_sentence()), T~ `choice_label::English`))
  
  analysis_by_subdistrict_indv <- survey_analysis(df = indv_svy,vars_to_analyze = cols_to_ana_indv,kobo_path = "03_tools/01_hh/hh_tool.xlsx",
                                                  question_lable = T,sm_sep = "/",disag = "subdistrict")   %>% arrange(main_variable)%>% mutate(
                                                    `choice_label::English` = case_when(!is.na(choice) & is.na(`choice_label::English`) ~  (choice %>% str_replace_all("_"," ") %>%  stringr::str_to_sentence()), T~ `choice_label::English`))
  
  
  analysis_by_district_indv <- survey_analysis(df = indv_svy,vars_to_analyze = cols_to_ana_indv,kobo_path = "03_tools/01_hh/hh_tool.xlsx",
                                               question_lable = T,sm_sep = "/",disag = "district")   %>% arrange(main_variable) %>% mutate(
                                                 `choice_label::English` = case_when(!is.na(choice) & is.na(`choice_label::English`) ~  (choice %>% str_replace_all("_"," ") %>%  stringr::str_to_sentence()), T~ `choice_label::English`))
  
  analysis_by_governorate_indv <- survey_analysis(df = indv_svy,vars_to_analyze = cols_to_ana_indv,kobo_path = "03_tools/01_hh/hh_tool.xlsx",
                                                  question_lable = T,sm_sep = "/",disag = "governorate")   %>% arrange(main_variable) %>% mutate(
                                                    `choice_label::English` = case_when(!is.na(choice) & is.na(`choice_label::English`) ~  (choice %>% str_replace_all("_"," ") %>%  stringr::str_to_sentence()), T~ `choice_label::English`))
  # 
  # analysis_by_site_location_indv <- survey_analysis(df = indv_svy,vars_to_analyze = cols_to_ana_indv,kobo_path = "03_tools/01_hh/hh_tool.xlsx",
  #                                                 question_lable = T,sm_sep = "/",disag = "location_site")   %>% arrange(main_variable)%>% mutate(
  # `choice_label::English` = case_when(!is.na(choice) & is.na(`choice_label::English`) ~  (choice %>% str_replace_all("_"," ") %>%  stringr::str_to_sentence()), T~ `choice_label::English`))

  # 
  # analysis_by_f_dis_origin_indv <- survey_analysis(df = indv_svy,vars_to_analyze = cols_to_ana_indv,kobo_path = "03_tools/01_hh/hh_tool.xlsx",
  #                                                   question_lable = T,sm_sep = "/",disag = "family_district_of_origin")   %>% arrange(main_variable)%>% mutate(
# `choice_label::English` = case_when(!is.na(choice) & is.na(`choice_label::English`) ~  (choice %>% str_replace_all("_"," ") %>%  stringr::str_to_sentence()), T~ `choice_label::English`))

  
  
  cleaning_log <- final_hh_cleaning_log %>% filter(change_type != "remove_survey")
  deletion_log <-   final_hh_cleaning_log %>% filter(change_type == "remove_survey")
  
  
  write_list <- list(
    raw_hh = "raw_hh",
    indv_raw="indv_raw",
    cleaning_log = "cleaning_log",
    deletion_log="deletion_log",
    hh_with_weight = "hh_clean_data",
    indv_with_weight = "indv_clean_data",
    weighting_frame="weighting_frame",
    overall_analysis_hh= "overall_analysis_hh",
    analysis_by_subdistrict_hh ="analysis_by_subdistrict_hh",
    analysis_by_district_hh="analysis_by_district_hh",
    analysis_by_gov_hh="analysis_by_gov_hh",
   # analysis_by_s_location_hh="analysis_by_s_location_hh",
    #analysis_by_f_dis_orig_hh="analysis_by_f_dis_orig_hh",
    overall_analysis_indv="overall_analysis_indv",
    analysis_by_subdistrict_indv="analysis_by_subdistrict_indv",
    analysis_by_district_indv="analysis_by_district_indv",
    analysis_by_governorate_indv="analysis_by_governorate_indv"
    #,analysis_by_site_location_indv="analysis_by_s_lctn_indv",
    #analysis_by_f_dis_origin_indv="analysis_by_f_dis_origin_indv"
    
  )
  
  write_excel_as_reach_format(write_list,paste0("02_outputs/01_hh/03_analysis/",str_replace_all(Sys.Date(),"-","_"),"_infromal_site_HH_analysis.xlsx"))  
  
}

if(dataset == "sites") {
  rm(list = ls())
  read_all_sheet_as_csv_format("02_outputs/02_sites/02_clean_data/site_clean_dataset.xlsx")
  cleaning_log <- site_cleaning_log %>% filter(change_type != "remove_survey")
  deletion_log <- site_cleaning_log %>% filter(change_type == "remove_survey")
  
  
  clean_data_site <- clean_data_site %>% filter(consent == "yes") %>% filter(verification_site == "yes")
  kobo_path <- "03_tools/02_sites/tool_site.xlsx"
  read_all_sheet_as_csv_format(kobo_path)
  
  
  # fix_data_type -----------------------------------------------------------
  
  sm_parent_child <- illuminate::auto_sm_parent_child(clean_data_site)
  cols_to_logical<-  sm_parent_child$sm_child
  clean_data_site <- clean_data_site %>% mutate_at(cols_to_logical,funs(as.numeric))
  clean_data_site <- clean_data_site %>% mutate_at(cols_to_logical,funs(as.logical))
  
  clean_data_fix_data_type <- fix_data_type_frm_kobo(df = clean_data_site, survey)
  clean_data_fix_data_type <- clean_data_fix_data_type %>% mutate(
    fire_safety_equipment_site_types.other = case_when(fire_safety_equipment_site == "no" ~ NA,
                                                       T~ fire_safety_equipment_site_types.other)
  ) 
  clean_data <- clean_data_fix_data_type
  # analysis ----------------------------------------------------------------
  
  other_unnecessary <- c("formatted_date_assessment","verification_site","start","end","date_assessment","consent","deviceid","audit_URL","instance_name","ngo_name","ID.","location_site","subdistrict","consent","screener_2","governorate","district")
  cols_not_to_ana <- find_qualitative_cols(df = clean_data_fix_data_type,kobo_survey_sheet = survey,additional_cols_to_remove = other_unnecessary)
  col_to_analysis <- clean_data_fix_data_type %>% dplyr::select(-cols_not_to_ana) %>% names()
  
  
  df_svy <- as_survey(clean_data_fix_data_type)
  
  # 
  # overall_analysis <- survey_analysis(df = df_svy,vars_to_analyze = col_to_analysis,question_lable = T,kobo_path =kobo_path) %>% arrange(main_variable) %>% mutate(
  #   `choice_label::English` = case_when(!is.na(choice) & is.na(`choice_label::English`) ~  (choice %>% str_replace_all("_"," ") %>%  stringr::str_to_sentence()), T~ `choice_label::English`))
  # 
  analysis_by_subdistrict <- survey_analysis(df = df_svy,vars_to_analyze = col_to_analysis,question_lable = T,kobo_path =kobo_path,disag = "subdistrict")  %>% arrange(main_variable) %>% mutate(
    `choice_label::English` = case_when(!is.na(choice) & is.na(`choice_label::English`) ~  (choice %>% str_replace_all("_"," ") %>%  stringr::str_to_sentence()), T~ `choice_label::English`))
  
  # analysis_by_district <- survey_analysis(df = df_svy,vars_to_analyze = col_to_analysis,question_lable = T,kobo_path =kobo_path,disag = "district")  %>% arrange(main_variable) %>% mutate(
  #   `choice_label::English` = case_when(!is.na(choice) & is.na(`choice_label::English`) ~  (choice %>% str_replace_all("_"," ") %>%  stringr::str_to_sentence()), T~ `choice_label::English`))
  # 
  # analysis_by_gov <- survey_analysis(df = df_svy,vars_to_analyze = col_to_analysis,question_lable = T,kobo_path =kobo_path,disag = "governorate")  %>% arrange(main_variable) %>% mutate(
  #   `choice_label::English` = case_when(!is.na(choice) & is.na(`choice_label::English`) ~  (choice %>% str_replace_all("_"," ") %>%  stringr::str_to_sentence()), T~ `choice_label::English`))
  # 
  
  
  
  write_list <- list(
    raw_sites = "raw_sites",
    cleaning_log = "cleaning_log",
    deletion_log = "deletion_log",
    clean_data = "clean_data",
    # overall_analysis = "overall_analysis",
    analysis_by_subdistrict = "analysis_by_subdistrict"
    # analysis_by_district="analysis_by_district",
    # analysis_by_gov="analysis_by_gov"
  )
  
  write_excel_as_reach_format(write_list,paste0("02_outputs/02_sites/03_analysis/",str_replace_all(Sys.Date(),"-","_"),"_infromal_site_SITES_analysis.xlsx"))  
  
  
}



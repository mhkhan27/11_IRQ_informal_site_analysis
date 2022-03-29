rm(list = ls())
library(dplyr)

hh <- read.csv("01_inputs/01_hh/hh.csv",na.strings = c("", " ","na","N/A"),stringsAsFactors = F)



# recoding ----------------------------------------------------------------

no_1_cols <- c("covid_soap_availability",
             "feel_safe",
             "primary_access",
             "middle_access",
             "covid_vulnerable_advice",
             "handwashing_facilities",
             "covid_practicing_preventative_measures",
             "covid_testing_access")

for (i in no_1_cols) {
  new_name <- paste0("i.",i)
  print(new_name)
  hh[[new_name]] <-case_when(hh[[i]] == "no"~1,
                             T~0)
}


yes_1_cols <- c("no_access_water","risk_of_eviction")

for (i in yes_1_cols) {
  new_name <- paste0("i.",i)
  print(new_name)
  hh[[new_name]] <-case_when(hh[[i]] == "yes"~1,
                             T~0)
}



# distance ----------------------------------------------------------------


dis_cols <- c("health_facility_distance","hospital_distance",
              "market_distance"
              )

for (i in dis_cols) {
  new_name <- paste0("i.",i)
  print(new_name)
  hh[[new_name]] <-case_when(hh[[i]] == "within_2km"~0,
                             hh[[i]] == "between_2_5"~.5,
                             hh[[i]] == "more_than5kmaway"~.75,
                             hh[[i]] == "none"~1,
                             hh[[i]] == "do_not_know"~1)
}



# health facility services ------------------------------------------------

hfs_cols <- hh %>% select(starts_with("health_facility_services.")) %>% select(-contains("do_not_kn")) %>% names()
hospital_cols <- hh %>% select(starts_with("hospital_services.")) %>% select(-contains("do_not_kn")) %>% names()


# mutate ------------------------------------------------------------------



hh <- hh %>% mutate(
  i.shelter_damaged = case_when(shelter_damaged == "none"~0,
                                shelter_damaged == "less_quarter"~0.25,
                                shelter_damaged == "most"~0.5,
                                shelter_damaged == "less_half"~0.5,
                                shelter_damaged == "nearly"~.75,
                                shelter_damaged == "all"~1),
  i.insufficient_food = case_when(insufficient_food == "yes_all"~0,
                                  insufficient_food == "nearly_all"~0.75,
                                  insufficient_food == "most_time"~0.5,
                                  insufficient_food == "less_half_time"~0.5,
                                  insufficient_food == "less_quarter_time"~.25,
                                  insufficient_food == "never"~1),
  i.waste_disposal_frequency = case_when(waste_disposal_frequency == "never" ~ 1,
                                         waste_disposal_frequency == "after_1_week" ~ .75,
                                         T~0),
  i.latrine_types = case_when(latrine_types.open_defecation == 1 ~1,
                              latrine_types.open_hole == 1 |latrine_types.bucket_toilet ==1 | latrine_types.plastic_bag ==1| latrine_types.hanging_toilet ==1 ~ .75,
                              latrine_types.pit_latrine_without_a_slab_or_platform ==1 ~ .5,
                              latrine_types.pit_latrine_with_a_slab_and_platform ==1~.25,
                              latrine_types.pit_vip_toilet == 1 | latrine_types.flush_or_pour_toilet == 1 |latrine_types.do_not_know == 1~0),
  
  health_care_services_rs = rowSums(hh[hfs_cols],na.rm = T),
  hospital_services_rs = rowSums(hh[hospital_cols],na.rm = T),
  
  i.health_facility_services = case_when(health_care_services_rs == 8 ~ 0,
                                         health_care_services_rs %in% 5:7 ~ 0.25,
                                         health_care_services_rs %in% 1:4 ~ 0.75,
                                         health_care_services_rs == 0 ~ 1),
  i.hospital_services = case_when(hospital_services_rs == 8 ~ 0,
                                  hospital_services_rs %in% 5:7 ~ 0.25,
                                  hospital_services_rs %in% 1:4 ~ 0.75,
                                  hospital_services_rs == 0 ~ 1),
  
  i.shelter_priority_concerns = case_when(shelter_priority_concerns.none == 1|shelter_priority_concerns.no_improvement == 1~0,
                                          T~1),
  
  i.livelihood_opportunities = case_when(livelihood_opportunities.livelihood_none == 1~1,
                                         livelihood_opportunities.unskilled_agricultural_labour == 1 |livelihood_opportunities.unskilled_wage_labour ==1 | livelihood_opportunities.casual_unskilled_labour ==1 ~.75,
                                         T~0),
  i.area_mines_chemicals_risk = case_when(area_mines_chemicals_risk.none== 1 ~0,
                                          T~1),
  i.fire_safety_equipment = case_when(fire_safety_equipment.none == 1~1,
                                      T~0)
  
  
) %>% select(-health_care_services_rs,-hospital_services_rs)


# red flag calculation ----------------------------------------------------


red_flag_cols <- hh %>% select(starts_with("i.")) %>% names()


hh <- hh %>% mutate(
  red_fag_rs = rowSums(hh[red_flag_cols],na.rm = T),
  red_flag_score = red_fag_rs/23,
  i.red_flag_class = case_when(red_flag_score <.26 ~ "low_vulnerability",
                             red_flag_score >.25 & red_flag_score < .51 ~ "moderate_vulnerability",
                             red_flag_score >.50 & red_flag_score < .76 ~ "high_vulnerability",
                             red_flag_score >.75 ~ "extreme_vulnerability",)
)


# hh_red <- hh %>% select(red_flag_cols,red_fag_rs,red_flag_score,i.red_flag_class)




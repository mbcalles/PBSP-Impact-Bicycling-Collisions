
library(dplyr)

ibiccs <- read.csv("Data/Database_recoded_2012-2014_weights_Walkscore_RTA.csv")

#### BICYCLING EXPOSURE #####

# Q27: Now think only about the bicycling you did to travel to and from work, to do errands, or to go from place to place.
# Only include bicycling that you did for at least 10 minutes at a time.
# During the past month, on how many days, on average, per week did youbicycle to go from place to place? Days per week.

# Q28: How much time did you usually spend on one of those days to bicycle from place to place?
# Q28b: Hours per day
# Q28c: minutes per day
# Q28d: Hours per week
# Q28e: minutes per week


ibiccs <- ibiccs %>% mutate(cycling_exposure_weekly_mins1 = Q27_N_days_per_week_travel_bike*q28b*60, #convert to minutes per week
                            cycling_exposure_weekly_mins2 = Q27_N_days_per_week_travel_bike*q28c,#convert to minutes per week
                            cycling_exposure_weekly_mins3 = q28d*60, #convert to minutes per week
                            cycling_exposure_weekly_mins4 = q28e) %>%
  mutate(cycling_exposure_weekly_mins1 = ifelse(is.na(cycling_exposure_weekly_mins1),0,cycling_exposure_weekly_mins1), #convert NAs to 0
         cycling_exposure_weekly_mins2 = ifelse(is.na(cycling_exposure_weekly_mins2),0,cycling_exposure_weekly_mins2), #convert NAs to 0
         cycling_exposure_weekly_mins3 = ifelse(is.na(cycling_exposure_weekly_mins3),0,cycling_exposure_weekly_mins3), #convert NAs to 0
         cycling_exposure_weekly_mins4 = ifelse(is.na(cycling_exposure_weekly_mins4),0,cycling_exposure_weekly_mins4)) %>% #convert NAs to 0
  mutate(total_weekly_cycling_mins = cycling_exposure_weekly_mins1 + cycling_exposure_weekly_mins2 + cycling_exposure_weekly_mins4,           #add up for total average weekly exposure in minutes
         total_weekly_cycling_hours = total_weekly_cycling_mins / 60) ### Convert to hour


### Outcome variables ----------------------------------------------------

#### collisions based on q34 (selected at least one of the given reasons for
#### their crash or fall)

ibiccs <- ibiccs %>%
  mutate(crash_car_door = case_when(q34a=="Oui"~1,
                          TRUE ~ 0),
         crash_motor_vehicle = case_when(q34b=="Oui"~1,
                          TRUE ~ 0),
         crash_another_cyclist = case_when(q34c=="Oui"~1,
                          TRUE ~ 0),
         crash_pedestrian = case_when(q34d=="Oui"~1,
                          TRUE ~ 0),
         crash_road_hazard = case_when(q34e=="Oui"~1,
                          TRUE ~ 0),
         crash_avoid_collision = case_when(q34f=="Oui"~1,
                          TRUE ~ 0),
         crash_distraction = case_when(q34g=="Oui"~1,
                          TRUE ~ 0),
         crash_other = case_when(q34h=="Oui"~1,
                          TRUE ~ 0),
         crashed = ifelse((crash_car_door + crash_motor_vehicle + crash_another_cyclist + crash_pedestrian +
                             crash_road_hazard + crash_avoid_collision + crash_distraction + crash_other)>0,1,0),
         crashed_factor = ifelse(crashed==1,"Crash","No Crash"),
         most_recent_crash_injured =  case_when(q35=="Oui"~1,
                                                TRUE ~ 0),
         most_recent_crash_emergency_room =  case_when(q36=="Oui"~1,
                                                TRUE ~ 0),
         most_recent_crash_pbs_bicycle =  case_when(q38=="VÃ©lo en libre-service"~1,
                                                       TRUE ~ 0),

         )

### Exposure variables ---------------------------------------------------

#### year

ibiccs$year <- ibiccs$COLLECT

ibiccs$year_factor <- factor(ibiccs$year, levels = c("2012","2013","2014"))

#### city type

ibiccs$city_pbsp <- case_when(ibiccs$CITY == "Boston" | ibiccs$CITY == "Toronto" | ibiccs$CITY == "Montreal"~ "Existing",
                              ibiccs$CITY == "New York" | ibiccs$CITY == "Chicago"~"Newly Implemented",
                              TRUE~"None")

ibiccs$city_pbsp <- factor(ibiccs$city_pbsp, levels = c("None", "Newly Implemented", "Existing"))


#### city

ibiccs$city <- as.factor(ifelse(ibiccs$CITY == "DÃ©troit","Detroit",as.character(ibiccs$CITY)))

#### age category

ibiccs$age_category <-  cut(ibiccs$Q42_age, breaks=c(-Inf, 24, 34, 44, 54, 64, Inf),
                   labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"))

#### gender

ibiccs$gender <- factor(case_when(ibiccs$Q54_gender=="male"~"Men",
                           ibiccs$Q54_gender=="Female"~"Women"),levels= c("Women","Men"))

#### education

ibiccs$education_attainment <- factor(ibiccs$Q51_Education_category, levels = c(
  "High school or less", "College or University"))

#### helmet use
ibiccs$helmet_use <- factor(ibiccs$Q31_own_bike_wear_helmet,
                                          levels = c("Never","Seldom","Often","Always"))

### perceived safety of cycling
ibiccs$perceived_city_safety <- recode_factor(ibiccs$Q29_safe_bike_city,
                                           `Somewhat dangerous` = "Dangerous",
                                           `Very dangerous` = "Dangerous",
                                           `Neither safe nor unsafe (neutral)` = "Neither safe nor unsafe",
                                           `Somewhat safe`= "Safe",
                                           `Very safe` = "Safe")

### Walk Score by units of 10

ibiccs$walkscore10 <- ibiccs$WalkScore/10
ibiccs$walk_score <- ibiccs$WalkScore

### survey weights

ibiccs$post_strat_weights <- ibiccs$pond

ibiccs$post_strat_weights_trunc <- case_when(ibiccs$post_strat_weights<0.2~0.2,
                                             ibiccs$post_strat_weights>10~10,
                                             TRUE~ibiccs$post_strat_weights)

ibiccs <- ibiccs %>%
  filter(total_weekly_cycling_hours>0) %>%
  select(starts_with("crash"),starts_with("most_recent"),
         year,year_factor,
         city,city_pbsp,
         age_category, gender,education_attainment,
         total_weekly_cycling_hours,helmet_use,perceived_city_safety,
         walk_score, walkscore10,
         post_strat_weights,post_strat_weights_trunc)


save(ibiccs, file = "Data/ibiccs_did_collisions.rdata")

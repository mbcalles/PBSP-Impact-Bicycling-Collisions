source("R/Custom_Functions.R")

load("Data/ibiccs_did_collisions.rdata")

#load packages

library(dplyr)
library(tidyr)
library(survey)
library(broom)
library(ggeffects)
library(ggplot2)


# remove respondents with missing covariates

ibiccs <- ibiccs %>%
  drop_na()

# create svy object

svy <- svydesign(ids = ~1, data = ibiccs, weights = ~post_strat_weights_trunc)


################# Table 1 #################


table_1_vars <- names(ibiccs %>% select(crashed,starts_with("crash"),starts_with("most_recent"),-crashed_factor))

table1 <- lapply(table_1_vars,sum_weights) %>%
  bind_rows()

total_crashed <- table1 %>%
  filter(variable=="crashed") %>%
  pull(total)

table1 <- table1 %>%
  mutate(`(%)` = total/total_crashed*100) %>%
  select(-SE)

table1 %>% knitr::kable(digits=1)

################# TABLE 2 #################


table_2_vars <- names(ibiccs %>%
                        select(crashed_factor,
                                        city_pbsp,
                                        age_category,
                                        gender,
                                        education_attainment,
                                        helmet_use,
                                        perceived_city_safety))

table_2_cat <- lapply(table_2_vars,sum_cat_var_weights_by_year) %>%
  bind_rows()

table_2_cat %>% knitr::kable(digits=1)


table_2_vars <- names(ibiccs %>% select(total_weekly_cycling_hours,walk_score))

table_2_num <- lapply(table_2_vars,sum_num_var_weights_by_year) %>%
  bind_rows()

table_2_num %>% knitr::kable(digits=1)

################# TABLE 3 #################


simple_did <- svyglm(crashed ~ city_pbsp*year_factor + city_pbsp + year_factor  +
                       total_weekly_cycling_hours , design = svy, family = quasibinomial())


tidy(simple_did, conf.int = T, conf.level = 0.95,
     exponentiate = T) %>%
  filter(stringr::str_detect(term,"year|pbsp")) %>%
  select(term,estimate,conf.low,conf.high) %>%
  mutate(term = factor(term, levels = c("city_pbspNewly Implemented:year_factor2013",
                                        "city_pbspExisting:year_factor2013",
                                        "city_pbspNewly Implemented:year_factor2014",
                                        "city_pbspExisting:year_factor2014",
                                        "year_factor2013",
                                        "year_factor2014",
                                        "city_pbspNewly Implemented" ,
                                        "city_pbspExisting"))) %>%
  arrange(term) %>%
  knitr::kable(digits = 2)

adjusted_did <- svyglm(crashed ~ city_pbsp*year_factor + city_pbsp + year_factor  +
                         total_weekly_cycling_hours +
                         age_category +
                         gender +
                         education_attainment +
                         perceived_city_safety +
                         helmet_use +
                         walk_score, design = svy, family = quasibinomial())


tidy(adjusted_did, conf.int = T, conf.level = 0.95,
     exponentiate = T) %>%
  filter(stringr::str_detect(term,"year|pbsp")) %>%
  select(term,estimate,conf.low,conf.high) %>%
  mutate(term = factor(term, levels = c("city_pbspNewly Implemented:year_factor2013",
                                        "city_pbspExisting:year_factor2013",
                                        "city_pbspNewly Implemented:year_factor2014",
                                        "city_pbspExisting:year_factor2014",
                                        "year_factor2013",
                                        "year_factor2014",
                                        "city_pbspNewly Implemented" ,
                                        "city_pbspExisting"))) %>%
  arrange(term) %>%
  knitr::kable(digits = 2)

################# FIGURE 1 #################


marginal_effects <- ggpredict(adjusted_did, c("year_factor","city_pbsp"),
                condition = c(age_category ="25-34",
                              gender="Men",
                              education_attainment = "College or University",
                              perceived_city_safety = "Safe",
                              helmet_use = "Always"),typical = "median")


marginal_effects_plot <- marginal_effects %>% as_tibble() %>%
  ggplot(aes(x=x,y=predicted,ymax=conf.high,ymin=conf.low,colour=group,group=group)) +
  geom_point(position=position_dodge(width=0.3),size = 3) +
  geom_errorbar(width=0.15,position=position_dodge(width=0.3),size = 1) +
  geom_line(position=position_dodge(width=0.3),linetype = "dashed",size = 1,alpha = 0.75) +
  scale_y_continuous(labels = scales::percent,
                     name = "Predicted probability of a crash"
  ) +
  xlab("Year") +
  ggpubr::theme_pubclean() +
  theme(legend.position = "top") +
  scale_colour_brewer(palette= "Set2",name="City PBSP Status",
                      labels = c("None","Newly Implemented","Existing")
  )

marginal_effects_plot

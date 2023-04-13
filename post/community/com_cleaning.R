library(dplyr)
library(readxl)
library(stringr)

cds <- read_xlsx("post/community/community_data.xlsx", sheet = "responses")

cds_names <-  c("timestamp","consent","collector_id","age","district","collection_loc","sex","mari_stat","edu_stat","job_stat",
                 "res_stat","res_who","heard_camp","participate_camp","on_treatment","treatment_type","when_start_treat",
                 "heard_huru","info_huru","du_know","du_causing_drugs","du_causes","du_risks",
                 "du_prevention","du_effects","du_treatment","adu_strict_measure","adu_load","adu_prob_per",
                 "adu_prob_soc","adu_relations","du_use","du_age_first_use","du_how_long_using","du_inject",
                 "di_first_age","di_how_long","di_freq","di_share", "du_past_six", 
                 "du_use_most","du_route","du_location")

names(cds)[1:43] <- cds_names

cds$age_cat <-cut(cds$age, breaks = c(0,25,35,45,Inf), 
                   labels = c("<25","25-34","35-44","45+"), 
                   right=TRUE, include.lowest=TRUE)


table(participated_campaign,on_treatment)

cds$participate_camp2 <- ifelse(cds$participate_camp == "Ndio", 'Ndio', 'Hapana')
#                               ***INTERIM ANALYSIS***
summary(age)
descriptives(age_cat)
descriptives(district)
descriptives(sex)
descriptives(mari_stat)
descriptives(edu_stat)
descriptives(res_stat)
descriptives(res_who)
descriptives(heard_camp)
descriptives(on_treatment)
descriptives(heard_huru)
descriptives(du_know)
descriptives(du_use)

descriptives(when_start_treat)

cds$start_treat_cat <- ifelse(when_start_treat <= 3, "Past 3 Months",
                              ifelse(when_start_treat <=6, "Past 6 Months",
                                     ifelse(when_start_treat <= 12,"Past 1 Year", "More than 1 Year")))

descriptives(cds$start_treat_cat)

cds$start_treat_cat <- factor(cds$start_treat_cat,
                              levels = c("Past 3 Months","Past 6 Months","Past 1 Year","More than 1 Year"))




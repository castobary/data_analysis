library(dplyr)
library(readxl)
library(stringr)

sds <- read_xlsx("post/school/school_data.xlsx", sheet="responses")

sds_names <-  c("timestamp","consent","collector_id","age","sex","location","school_name","school_type","class","live_with",
                 "father_edu_level","father_employed","father_drugs","father_drugs_name","mother_edu_level","mother_employed",
                 "mother_drugs", "mother_drugs_name","school_campaign","participated_campaign","participated_events","spoke_debate",
                 "campaign_help","heard_huru","huru_features","du_know","du_causing_drugs","du_causes","du_risks",
                 "du_prevention","du_effects","du_treatment","adu_su_unaccepted","adu_decrease_performance","adu_stop",
                 "adu_report_friends","adu_feel_good","adu_take_hospi","adu_test","use_drugs","age_first_use","use_how_many","use_which", "use_route",
                "friends_drugs")

names(sds)[1:45] <- sds_names

sds <- sds %>% mutate(sex = recode(sex,
                                     `Mvulana` = "Boy",
                                     `Msichana` = "Girl"))

sds <- sds %>% mutate(school_type = recode(school_type,
                                   `Bweni` = "Boarding",
                                   `Kutwa` = "Day"))

sds <- sds %>% mutate(class = recode(class,
                                           `Cha Kwanza` = "Form One",
                                           `Cha Pili` = "Form Two",
                                           `Cha Tatu` = "Form Three",
                                           `Cha Nne` = "Form Four",
                                           `Cha Tano` = "Form Five" ))

sds <- sds %>% mutate(live_with = recode(live_with,
                                     `Baba na Mama` = "Both Parents",
                                     `Baba pekee` = "Father Only",
                                     `Mama pekee` = "Mother Only",
                                     `Mlezi/ Mwangalizi` = "Relative/Guardian"))

sds <- sds %>% mutate(father_edu_level = recode(father_edu_level,
                                          `Hana elimu` = "No Education",
                                          `Elimu ya msingi` = "Primary Education",
                                          `Elimu ya sekondari`= "Secondary Education",
                                          `Elimu ya chuo na kuendelea`="College+"))

sds <- sds %>% mutate(father_employed = recode(father_employed,
                                           `Ana kazi` = "Employed",
                                           `Hana kazi` = "Unemployed"))

sds <- sds %>% mutate(father_drugs = recode(father_drugs,
                                          `Ndio` = "Yes",
                                          `Hapana` = "No"))

sds$age_cat <- ifelse(sds$age < 18, "10 - 17 Years", "18 + Years")



#                 *********INTERIM ANALYSIS**********
descriptives <- function(x){
  x <- na.omit(x)
  y <- table(x)
  z <- round(prop.table(table(x)),4)*100
  print(y)
  print(z)
}

attach(sds)

descriptives(sex)
descriptives(location)
descriptives(age_cat)

schools <- sds %>%
  group_by(location, school_name) %>%
  summarize(frequency = n())

descriptives(school_type)

descriptives(school_campaign)

descriptives(participated_campaign)
descriptives(friends_drugs)
descriptives(use_which)
descriptives(use_drugs)
descriptives(du_know)





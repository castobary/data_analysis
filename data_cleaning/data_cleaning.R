library(readr)
library(dplyr)
library(stringr)

huru <- read.csv("data_cleaning/tanga_data.csv", 
                 header = TRUE, 
                 stringsAsFactors = FALSE, 
                 sep = ",")

huru_names <-  c("timestamp","email","id","age","location","sex",
                 "mari_stat","edu_stat","can_read","job_stat",
                  "res_stat","res_who","phone_pos","phone_type",
                 "live_phone","use_phone","who_phone",
                  "keep_phone","keep_time","sell_phone",
                 "ph_sell_times","buy_credit", "buy_credit_times",
                 "mobile_money","ph_learning","ph_alone","du_know",
                 "du_causing_drugs","du_causes","du_risks",
                 "du_prevention","du_effects","du_treatment",
                 "adu_strict_measure","adu_load","adu_prob_per",
                 "adu_prob_soc","adu_relations","adu_get_high",
                 "alc_use_times","alc_how_many","alc_one_day",
                 "du_happy","du_use_which","du_use_most",
                 "du_route","du_age_first_use","du_how_long_using",
                 "du_past_six","du_location", "du_inject",
                 "di_first_age","di_how_long","di_freq","di_share")

names(huru)[1:55] <- huru_names

huru <- huru %>% mutate(sex = recode(sex,
                                      `Mwanaume` = "Male",
                                      `Mwanamke` = "Female"))

huru <- huru %>% mutate(mari_stat = recode(mari_stat,
                                     `Sijaoa` = "Single",
                                     `Nimeoa` = "Married",
                                     `Nimepewa/Nimetoa Talaka`="Divorced",
                                     `Nimefiwa na Mwenza` = "Widowed"))

huru <- huru %>% mutate(edu_stat = recode(edu_stat,
                                     `Hana Elimu` = "No Education",
                                     `Elimu ya Msingi` = "Primary Education",
                                     `Elimu ya Sekondari na Kuendelea`= "Secondary Education+"))

huru <- huru %>% mutate(can_read = recode(can_read,
                                     `Ndio` = "Yes",
                                     `Hapana` = "No"))

huru <- huru %>% mutate(job_stat = recode(job_stat,
                                     `Ndio` = "Has Job",
                                     `Hapana` = "Jobless"))

huru <- huru %>% mutate(res_stat = recode(res_stat,
                                     `Sina Makazi` = "Homeless",
                                     `Nina Makazi` = "Got Home"))


huru <- huru %>% mutate(res_who = recode(res_who,
                                     `Wazazi` = "Parents",
                                     `Familia yangu` = "My Family",
                                     `Watoto wangu` = "My Children",
                                     `Mke wangu` = 'My Wife',
                                     `Wengine (Marafiki)` = "My Friends",
                                     `Mwenyewe` = 'Alone'))

huru <- huru %>% mutate(phone_pos = recode(phone_pos,
                                     `Ndio` = "Yes",
                                     `Hapana` = "No"))

huru <- huru %>% mutate(phone_type = recode(phone_type,
                                     `Simu ya Kawaida` = "Feature Phone",
                                     `Simu janja` = "Smart Phone",
                                     `Zote Mbili` = "Both"))

huru <- huru %>% mutate(live_phone = recode(live_phone,
                                            `Ndio` = "Yes",
                                            `Hapana` = "No"))

huru <- huru %>% mutate(use_phone = recode(use_phone,
                                            `Ndio` = "Yes",
                                            `Hapana` = "No"))

huru <- huru %>% mutate(who_phone = recode(who_phone,
                                            `Yangu` = "Mine",
                                            `Rafiki` = "Friend",
                                           `Familia` = "Family"))

huru <- huru %>% mutate(keep_phone = recode(keep_phone,
                                            `Ndio` = "Yes",
                                            `Hapana` = "No"))

huru <- huru %>% mutate(keep_time = recode(keep_time,
                                            `Siku Moja` = "One Day",
                                            `Wiki Moja` = "One Week",
                                           `Mwezi Mmoja` = "One Month",
                                           `Mwaka Mmoja` = "One Year"))

huru <- huru %>% mutate(sell_phone = recode(sell_phone,
                                            `Ndio` = "Yes",
                                            `Hapana` = "No"))

huru <- huru %>% mutate(ph_sell_times = recode(ph_sell_times,
                                            `Moja` = "Once",
                                            `Mbili` = "Twice",
                                            `Tatu` = "Thrice",
                                            `Zaidi ya Mara Tatu` = "More than Thrice"))

huru <- huru %>% mutate(buy_credit = recode(buy_credit,
                                            `Ndio` = "Yes",
                                            `Hapana` = "No"))


huru <- huru %>% mutate(buy_credit_times = recode(buy_credit_times,
                                               `Mara Moja` = "Once",
                                               `Mara Mbili` = "Twice",
                                               `Mara Tatu` = "Thrice",
                                               `Mara Nyingi` = "Many Times"))

huru <- huru %>% mutate(mobile_money = recode(mobile_money,
                                            `Ndio` = "Yes",
                                            `Hapana` = "No"))

huru <- huru %>% mutate(ph_learning = recode(ph_learning,
                                            `Ndio` = "Yes",
                                            `Hapana` = "No"))

huru <- huru %>% mutate(ph_alone = recode(ph_alone,
                                            `a. Ndio, ninaweza kutumia mwenyewe` = "Yes",
                                            `b. Hapana, ninahitaji msaada` = "No"))

huru <- huru %>% mutate(du_know = recode(du_know,
                                            `Ndio` = "1",
                                            `Hapana` = "0"))


huru <- huru %>% mutate(adu_strict_measure = recode(adu_strict_measure,
                                         `Nakubali Kabisa` = "5",
                                         `Nakubali` = "4",
                                         `Sikubali Wala Sikatai` = "3",
                                         `Nakataa` = "2",
                                         `Nakataa Kabisa` = "1"
                                         ))

huru <- huru %>% mutate(adu_load = recode(adu_load,
                                                    `Nakubali Kabisa` = "1",
                                                    `Nakubali` = "2",
                                                    `Sikubali Wala Sikutai` = "3",
                                                    `Nakataa` = "4",
                                                    `Nakataa Kabisa` = "5"))

huru <- huru %>% mutate(adu_prob_per = recode(adu_prob_per,
                                          `Nakubali Kabisa` = "1",
                                          `Nakubali` = "2",
                                          `Sikubali wala sikatai` = "3",
                                          `Nakataa` = "4",
                                          `Nakataa Kabisa` = "5"))

huru <- huru %>% mutate(adu_prob_soc = recode(adu_prob_soc,
                                          `Nakubali Kabisa` = "5",
                                          `Nakubali` = "4",
                                          `Sikubali wala Sikatai` = "3",
                                          `Nakataa` = "2",
                                          `Nakataa Kabisa` = "1"))

huru <- huru %>% mutate(adu_relations = recode(adu_relations,
                                              `Nakubali Kabisa` = "1",
                                              `Nakubali` = "2",
                                              `Sikubali wala Sikatai` = "3",
                                              `Nakataa` = "4",
                                              `Nakataa Kabisa` = "5"))

huru <- huru %>% mutate(adu_get_high = recode(adu_get_high,
                                          `Nakubali Kabisa` = "1",
                                          `Nakubali` = "2",
                                          `Sikubali wala Sikatai` = "3",
                                          `Nakataa` = "4",
                                          `Nakataa Kabisa` = "5"))

huru <- huru %>% mutate(alc_use_times = recode(alc_use_times,
                                          `Sijanywa hata mara moja` = "0",
                                          `Mwezi mmoja na pungufu` = "1",
                                          `Mara 2 - 4 kwa mwezi` = "2",
                                          `Mara 2 - 3 kwa wiki` = "3",
                                          `Mara 4 na Zaidi kwa wiki` = "4",
                                          `Sijui` = "NA"))


huru <- huru %>% mutate(alc_how_many = recode(alc_how_many,
                                               `1 au 2` = "1",
                                               `3 au 4` = "2",
                                               `5 au 6` = "3",
                                               `7 au 9` = "4",
                                               `10 na zaidi` = "5",
                                               `Sijui` = "NA"))

huru <- huru %>% mutate(alc_one_day = recode( alc_one_day,
                                              `Sijawahi` = "0",
                                              `Chini ya Mwezi mmoja` = "1",
                                              `Kwa mwezi mzima` = "2",
                                              `Kwa wiki nzima` = "3",
                                              `Kila Siku` = "4",
                                              `Sijui` = "NA"))

huru <- huru %>% mutate(du_past_six = recode( du_past_six,
                                              `Mara moja na pungufu` = "1",
                                              `Mara 2 hadi 4 kwa mwezi` = "2",
                                              `Mara 2 hadi 3 kwa wiki` = "3",
                                              `Mara 4 na zaidi kwa wiki` = "4",
                                              `Sijui` = "NA"))

huru <- huru %>% mutate(du_happy = recode(du_happy,
                                         `Ndio` = "1",
                                         `Hapana` = "0"))

huru <- huru %>% mutate(du_inject = recode(du_inject,
                                          `Ndio` = "1",
                                          `Hapana` = "0"))

huru <- huru %>% mutate(di_freq = recode(di_freq,
                                               `Mwezi mmoja na pungufu` = "1",
                                               `Mara 2 hadi 4 kwa mwezi` = "2",
                                               `Mara 2 hadi 3 kwa wiki` = "3",
                                               `Mara 4 na zaidi kwa wiki` = "4",
                                               `Sijui` = "NA"))

huru <- huru %>% mutate(di_share = recode(di_share,
                                           `Ndio` = "1",
                                           `Hapana` = "0"))

huru$age <- as.numeric(huru$age)

huru$age_cat <-cut(huru$age, breaks = c(0,25,35,45,Inf), 
                 labels = c("<25","25-34","35-44","45+"), 
                 right=TRUE, include.lowest=TRUE)




## Pending Columns

# 1. du_causing_drugs
# 2. du_causes
# 3. du_risks
# 4. du_prevention
# 5. du_effects
# 6. du_treatment
# 7. du_use_which
# 8. du_use_most
# 9. du_route
# 10. du_past_six
# 11. du_location

drugs <- huru$du_causing_drugs

drugx <- str_replace(drugs,"(Heroin, Methadone, Codeine, Morphine)","")
drugx <- str_replace(drugx, "(coke, crack etc)","")
drugx <- str_replace(drugx, "((gundi, petrol, rangi))","")
drugx <- str_replace(drugx, "((sigara, kutafuna tumbaku))","")
drugx <- str_replace(drugx, "((Valiam, serepax))","")
drugx <- str_replace(drugx,"((LSD, tindikali, uyoga, PCP))","")
drugx <- str_replace(drugx, "((Amphetamines))", "")
drugx <- str_replace(drugx, "(bia, mvinyo)","")
drugx <- str_replace_all(drugx, "[(\\)]","")
drug <-  str_split(drugx, ",", simplify = TRUE)
drug <- as.data.frame(drug)

write_csv(drug,"data_cleaning/drugs.csv")


du_causes <- huru$du_causes
du_causes <-  str_split(du_causes, ",", simplify = TRUE)
du_causes <- as.data.frame(du_causes)

write_csv(du_causes,"data_cleaning/causes.csv")

du_risks <- huru$du_risks
du_risks <-  str_split(du_risks, ",", simplify = TRUE)
du_risks <- as.data.frame(du_risks)

write_csv(du_risks, "data_cleaning/risks.csv")

du_prevention <- huru$du_prevention
du_prevention<-  str_split(du_prevention, ",", simplify = TRUE)
du_prevention <- as.data.frame(du_prevention)

write_csv(du_prevention, "data_cleaning/prevention.csv")

du_effects <- huru$du_effects
du_effects <-  str_split(du_effects, ",", simplify = TRUE)
du_effects <- as.data.frame(du_effects)

write_csv(du_effects, "data_cleaning/effects.csv")

du_treatment <- huru$du_treatment
du_treatment <-  str_split(du_treatment, ",", simplify = TRUE)
du_treatment <- as.data.frame(du_treatment)

write_csv(du_treatment, "data_cleaning/treatment.csv")

du_use_which <- huru$du_use_which
du_use_which <-  str_split(du_use_which, ",", simplify = TRUE)
du_use_which <- as.data.frame(du_use_which)

write_csv(du_use_which, "data_cleaning/du_use_which.csv")

# du_use_most <- huru$du_use_most
# du_use_most <-  str_split(du_use_most, ",", simplify = TRUE)
# du_use_most <- as.data.frame(du_use_most)

du_route <- huru$du_route
du_route <-  str_split(du_route, ",", simplify = TRUE)
du_route <- as.data.frame(du_route)

write_csv(du_route,"data_cleaning/du_route.csv")

#du_past_six, i have to translate this variable then recode later as there are no multiple answers.


#du location is variable that requires splitting treatment
# du_location<- huru$du_location
# du_location <-  str_split(du_location, ",", simplify = TRUE)
# du_location <- as.data.frame(du_location)

# dropping unneccessary variables

huru <- subset(huru, select = -c(du_causing_drugs,du_causes,du_risks,du_prevention,du_effects,
                                 du_treatment,du_use_which,du_route))

huru$age_cat_fu <-cut(huru$du_age_first_use, breaks = c(0,15,25,35,Inf), 
                   labels = c("<15","15-24","25-34","35+"), 
                   right=TRUE, include.lowest=TRUE)

huru$longu_cat <- cut(huru$du_how_long_using, breaks = c(0,10,Inf),
                      labels = c("< 10 Years","> 10 Years"),
                      right = TRUE, include.lowest = TRUE)


source("functions.R")
attach(ds)

# Calculation of Knowledge, Attitude and Substance Use Practice Scores

ds$attitude_score = adu_strict_measure + adu_load + adu_prob_per + 
  adu_prob_soc + adu_relations + adu_get_high

ds[c(29:30,32:33)] <- sapply(ds[c(29:30,32:33)], attifun)
ds[c(28,31)] <- sapply(ds[c(28,31)], attifun2)

myvars_a <- names(ds[28:35])

table_a <- CreateTableOne(vars = myvars_a, data = ds)
print(table_a)

ds$attitude_cat <- ifelse(ds$attitude_score >= 15, "Good Attitude",
                          "Poor Attitude")

ds$knowledge_score = (du_know + opioid + cocaine +  amphe + inhalants + marijuana + 
                      hallu + sedatives + tobacco + alcohols + feel_good + feel_better +
                      be_better + curiosity + aggressive + lack_parent + poor_social + 
                      stress + experi + drug_availability + poverty + self_control + parental +
                       positive_rel + academic_excel + policies+crimes + poor_status + e_hiv +
                       e_hepatitis+e_tb+e_sti+poor_health+poor_economy+stigma+methadone+rehab+
                       cbt + fa_therapy+al_therapy)

ds$knowledge_cat <- ifelse(ds$knowledge_score <= 13,
                           "Poor Knowledge","Good Knowledge")

ds[51:59] <- sapply(ds[51:59], myfun)

myvars_b <- names(ds[51:59])

table_b <- CreateTableOne(vars = myvars_b, data = ds)
print(table_b)


ds[60:63] <- sapply(ds[60:63], myfun)

myvars_c <- names(ds[60:63])

table_c <- CreateTableOne(vars = myvars_c, data = ds)
print(table_c)

ds[76:89] <- sapply(ds[76:89], myfun)

myvars_d <- names(ds[76:89])

table_d <- CreateTableOne(vars = myvars_d, data = ds)
print(table_d)


ds$practice_score = (du_inject + du_past_six + di_share + du_happy + u_heroin +
                      u_cocaine + u_crack + u_amphe + u_inhalants+u_hero_coca +
                       u_marijuana + u_hallu)

ds$practice_cat <- ifelse(ds$practice_score >= 6, 
                          "Poor Practice","Good Practice")


# ATTIDUDE TOWARDS SUBSTANCE USE

myVars5<-c("attitude_score","attitude_cat","knowledge_score",
           "knowledge_cat", "practice_score","practice_cat")
tab5<-CreateTableOne(vars=myVars5, data=ds)
print(tab5)

# with strata

tab6<-CreateTableOne(vars=myVars5, strata=c("location"), 
                     data=ds, test=TRUE)
print(tab6)

ds[90:97] <- sapply(ds[90:97], myfun)

myvars_e <- names(ds[90:97])

table_e <- CreateTableOne(vars = myvars_e, data = ds)
print(table_e)

ds$du_inject <- myfun(du_inject)
descriptives(ds$du_inject)

ds[,98:101] <- sapply(ds[,98:101],myfun)
descriptives(ds$ro_ingest)

descriptives(ds$ro_smoke)

descriptives(ds$ro_snif)

table(ds$alc_how_many)

du_location <- ds$du_location
du_location <-  str_split(du_location, ",", simplify = TRUE)
du_location <- as.data.frame(du_location)
table(du_location$V1)
table(du_location$V2)
table(du_location$V3)
table(du_location$V4)


# PLOTTING...


(knowledge <- ds %>%
  filter(!is.na(knowledge_cat)) %>%
  group_by(location,knowledge_cat) %>%
  summarise(count = n()) %>%
  summarise(location,knowledge_cat,count,
            prop = (count/sum(count)*100)) %>%
    ungroup())


(p1 <- ggplot(knowledge) +
  geom_col(
    mapping = aes( x = location,
                   y = prop,
                   fill = knowledge_cat),  width = 0.75,
    position = position_dodge2(width = 0.9, preserve = "single")) + 
  theme_bw() +
  common_theme()+
  scale_fill_lancet() +
  ylim(0,100) +
  xlab("District") +
  ylab("Proportion(%)") +
  labs(fill = "Knowledge Level")+
  ggtitle("Knowledge Level"))


ds$practice_cat <- factor(ds$practice_cat,
                          levels = c("Good Practice", "Poor Practice"))

(practice <- ds %>%
  filter(!is.na(practice_cat)) %>%
  group_by(location,practice_cat) %>%
  summarise(count = n()) %>%
  summarise(location,practice_cat,count,
            prop = (count/sum(count)*100)) %>%
  ungroup())


(p2 <- ggplot(practice) +
  geom_col(
    mapping = aes( x = location,
                   y = prop,
                   fill = practice_cat), width = 0.75,
    position = position_dodge2(width = 0.9, preserve = "single")) + 
  theme_bw() +
  common_theme()+
  scale_fill_lancet() +
  ylim(0,100) +
  xlab("District") +
  ylab("Proportion(%)") +
  labs(fill = "Practice Level") +
  ggtitle("Practice Level"))


(attitude <- ds %>%
  filter(!is.na(attitude_cat)) %>%
  group_by(location,attitude_cat) %>%
  summarise(count = n()) %>%
  summarise(location,attitude_cat,count,
            prop = (count/sum(count)*100)) %>%
  ungroup())


(p3 <- ggplot(attitude) +
  geom_col(
    mapping = aes( x = location,
                   y = prop,
                   fill = attitude_cat), width = 0.75,
    position = position_dodge2(width = 0.9, preserve = "single")) + 
  theme_bw() +
  common_theme()+
  scale_fill_lancet() +
  ylim(0,100) +
  xlab("District") +
  ylab("Proportion(%)") +
  labs(fill = "Attitude Level") +
  ggtitle('Attitude Level'))

p1 + p3 + p2+
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect")


ds$phone_pos <- factor(ds$phone_pos,
                          levels = c("Yes", "No"))

(phone_poss <- ds %>%
  filter(!is.na(phone_pos)) %>%
  group_by(location,phone_pos) %>%
  summarise(count = n()) %>%
  summarise(location,phone_pos,count,
            prop = (count/sum(count)*100)) %>%
  ungroup())


(p4 <- ggplot(phone_poss) +
  geom_col(
    mapping = aes( x = location,
                   y = prop,
                   fill = phone_pos), width = 0.75,
    position = position_dodge2(width = 0.9, preserve = "single")) + 
  theme_bw() +
  common_theme() +
  scale_fill_lancet() +
  ylim(0,100) +
  xlab("District") +
  ylab("Proportion(%)") +
  labs(fill = "Own A Mobile Phone"))

ds$ph_learning <- factor(ds$ph_learning,
                       levels = c("Yes", "No"))

(phone_learning <- ds %>%
  filter(!is.na(ph_learning)) %>%
  group_by(location,ph_learning) %>%
  summarise(count = n()) %>%
  summarise(location,ph_learning,count,
            prop = (count/sum(count)*100)) %>%
  ungroup())

(p5 <- ggplot(phone_learning) +
  geom_col(
    mapping = aes( x = location,
                   y = prop,
                   fill = ph_learning), width = 0.75,
    position = position_dodge2(width = 0.9, preserve = "single")) + 
  theme_bw() +
  common_theme()+
  scale_fill_lancet() +
  ylim(0,100) +
  xlab("District") +
  ylab("Proportion(%)") +
  labs(fill = "Ready to Use Phone for Learning"))

ds$ph_alone <- factor(ds$ph_alone,
                         levels = c("Yes", "No"))
(ph_alone <- ds %>%
  filter(!is.na(ph_alone)) %>%
  group_by(location,ph_alone) %>%
  summarise(count = n()) %>%
  summarise(location,ph_alone,count,
            prop = (count/sum(count)*100)) %>%
  ungroup())

(p6 <- ggplot(ph_alone) +
  geom_col(
    mapping = aes( x = location,
                   y = prop,
                   fill = ph_alone), width = 0.75,
    position = position_dodge2(width = 0.9, preserve = "single")) + 
  theme_bw() +
  common_theme() +
  scale_fill_lancet() +
  ylim(0,100) +
  xlab("District") +
  ylab("Proportion(%)") +
  labs(fill = "Can Use Phone Alone to Learn"))

p4 + p5 + p6+
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect")


# Exploring Age Distribution

ggplot(ds,
       aes(x=age)) +
  geom_histogram(aes(y=..density..),binwidth = 3) +
  geom_density(alpha = 0, fill ="#FF6666",col = "steelblue") +
  facet_wrap(~location) +
  theme_minimal() +
  xlab("Participant Age") +
  ylab("Density") +
  ggtitle("Histogram of Participants Age by District with
          Superimposed Density Plot")

#write_csv(ds,"mds.csv")
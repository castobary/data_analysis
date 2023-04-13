source("functions.R")

# Creating Modelling Dataset
ds <- read_csv("mds.csv",
               show_col_types = FALSE)

mds <- ds[,c(5:12,48:50,seq(103,107,2))]

glimpse(mds)

mds$attitude_cat <- factor(mds$attitude_cat,
                         levels = c("Poor Attitude",
                                    "Good Attitude"))

mds$knowledge_cat <- factor(mds$knowledge_cat,
                           levels = c( "Poor Knowledge",
                                       "Good Knowledge"))

mds$practice_cat <- factor(mds$practice_cat,
                            levels = c( "Poor Practice",
                                        "Good Practice"))

mds$mari_stat <- factor(mds$mari_stat,
                        levels=c("Single", "Married",
                                 "Divorced","Widowed"))

know_modal <- glm(knowledge_cat ~ sex + age_cat+mari_stat
                  +edu_stat+res_stat+age_cat_fu+longu_cat+location, 
                  data = mds , 
                  family = binomial(link = "logit"))

summary(know_modal)

(know <- round(exp(cbind(OR=coef(know_modal),
                        confint(know_modal))),2))

 
#(atti <- round(exp(cbind(OR=coef(atti_modal), 
                        #confint(atti_modal))),2))

practice_modal <- glm(practice_cat ~ sex + age_cat+mari_stat
                      +edu_stat+res_stat+age_cat_fu+longu_cat+location, 
                      data = mds , 
                      family = binomial(link = "logit"))

summary(practice_modal)

pract <- round(exp(cbind(OR=coef(practice_modal),
                         confint(practice_modal))), 2)

tanga <- subset(ds, location=="Tanga City")
muheza <- subset(ds, location!="Tanga City")
t.test(tanga$knowledge_score,
       muheza$knowledge_score,
       mu=0)
sd(muheza$knowledge_score,na.rm = TRUE)
sd(tanga$knowledge_score,na.rm = TRUE)

chisq.test(ds$location, ds$knowledge_cat)
mean(ds$attitude_score, na.rm = TRUE)

sd(ds$attitude_score, na.rm = TRUE)
t.test(tanga$attitude_score,
       muheza$attitude_score,
       mu=0)
sd(muheza$attitude_score,na.rm = TRUE)
sd(tanga$attitude_score,na.rm = TRUE)

chisq.test(ds$location, ds$attitude_cat)


mean(ds$practice_score, na.rm = TRUE)
sd(ds$practice_score, na.rm = TRUE)
t.test(tanga$practice_score,
       muheza$practice_score,
       mu=0)
sd(muheza$practice_score,na.rm = TRUE)
sd(tanga$practice_score,na.rm = TRUE)

chisq.test(ds$location, ds$practice_cat)

att <- ds[,c(28:33)]

att <-  gather(att, measure,
                     level, adu_strict_measure:adu_get_high, factor_key=TRUE)


att %<>% 
  filter(!is.na(level))

att$measure <- ifelse(
  att$measure  %in% "adu_strict_measure", "We Need Strict Control of Drugs",
  ifelse(att$measure  %in% "adu_load", "People who Use Drugs are a Burden",
          ifelse(att$measure  %in% "adu_prob_per", 'Nothing is Wrong if \n Drugs make User Feel Good',
                  ifelse(att$measure  %in% "adu_prob_soc", 'Something is wrong with the World \n When DU became Accepted Way of Life',
                          ifelse(att$measure %in% "adu_relations", "Drugs can Improve \n Relations among People",
                                  "Welcomes an Opportunity \n to Get High on Drugs"
                          )))))

att$level <- ifelse(
  att$level %in% c("Strongly Agree","Agree"), "Agree",
  ifelse(
    att$level %in% c("Strongly Disagree","Disagree"), "Disagree", att$level
  )
)

att_data <- att %>%
  group_by(measure,level) %>%
  summarize(freq = n()) %>%
  summarize(measure,level,freq,
            prop = round((freq/sum(freq)*100),1)) %>%
  ungroup()

att_data$level <- factor(att_data$level,
                         levels = c("Disagree", "Neutral","Agree"))

ggplot(att_data, aes(fill=level, y=prop, x=measure,label=prop)) + 
  geom_bar(position="stack", stat="identity", width = 0.5) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  coord_flip() +
  theme_bw() +
  theme_classic() +
  theme(text = element_text(family = "Tahoma", size = 10, face = "bold"))+
  theme(axis.text.x = element_text(face="bold", vjust = 1, hjust = 1)) +
  scale_fill_jama() +
  xlab("Attitude Measure") +
  ylab("Proportion (%)") +
  labs(fill = "Attitude Level")
  



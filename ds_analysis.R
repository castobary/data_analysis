source("functions.R")

attach(ds)


# Table 1 : Demographics
#  Descriptives for du_know, location, sex, age_cat, mari_stat, edu_stat, can_read,
# res_stat, res_who, alc_use_times, du_inject, di_share
summary(na.omit(age))
vars <- c(27,5:12,48,34,43,47)

sapply(ds[vars], descriptives)


myVars<-c("location", "sex","age_cat","mari_stat","edu_stat","can_read","res_stat","res_who")
catVars<-c("age_cat")
tab1<-CreateTableOne(vars=myVars, data=ds, factorVars=catVars, test=TRUE)
print(tab1)

#with strata

myVars2<-c("sex","age_cat","mari_stat","edu_stat","can_read","res_stat","res_who", "age_cat_fu","longu_cat")
tab2<-CreateTableOne(vars=myVars2, strata=c("location"), data=ds, factorVars=catVars, test=TRUE)
print(tab2)


# 2 Table 2 : Mobile Phone Possession and Use

# 11. Do you have a mobile phone?

descriptives(phone_pos)

# 12. What Kind of Mobile Phone Do you have?

descriptives(phone_type)


# 13. Do you live with anyone having a mobile phone?

descriptives (live_phone)


# 14. Do you ever use a mobile phone?

descriptives(use_phone)

# 15. Whose mobile Phone do you use?

descriptives(who_phone)

# 16. Can you stay with a mobile phone?

descriptives(keep_phone)

# 17. For how long can you stay with a mobile phone?

descriptives(keep_time)


# 18. Have you ever sold a mobile phone that you owned

descriptives (sell_phone)

# 19. If ever sold, how many times?

descriptives(ph_sell_times)

# 20. Ever used mobile money service like m-pesa?

descriptives(mobile_money)

# 21. Are you willing to use your mobile phone for learning purposes?

descriptives(ph_learning)

# 22. Can you use it alone or you need assistance?

descriptives(ph_alone)

#In summary

myVars3<-c("phone_pos", "phone_type","live_phone","use_phone","who_phone","keep_phone",
          "keep_time","sell_phone","ph_sell_times", "buy_credit", "buy_credit_times",
          "mobile_money","ph_learning", "ph_alone")
tab3<-CreateTableOne(vars=myVars3, data=ds)
print(tab3)

#with strata

myVars4<-c("phone_pos", "phone_type","live_phone","use_phone","who_phone","keep_phone",
           "keep_time","sell_phone","ph_sell_times", "buy_credit", "buy_credit_times",
           "mobile_money","ph_learning", "ph_alone")
tab4<-CreateTableOne(vars=myVars4, strata=c("location"), data=ds, test=TRUE)
print(tab4)

detach(ds)


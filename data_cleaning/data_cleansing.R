source("data_cleaning/data_cleaning.R")

drugs <- read_csv("data_cleaning/drugs.csv",show_col_types = FALSE)

drugs <- drugs %>%
  mutate(
    opioid = case_when(
      V1 %in% c("Opioids") ~ V1,
      TRUE ~ NA_character_
    )
  )

drugs <- drugs %>%
  mutate(
    cocaine = case_when(
      V1 %in% "Cocaine" ~ V1,
      V2 %in% "Cocaine" ~ V2,
      TRUE ~ NA_character_
    )
  )

drugs <- drugs %>%
  mutate(
    amphe = case_when(
      V1 %in% "Visisimua mwili" ~ V1,
      V2 %in% "Visisimua mwili" ~ V2,
      V3 %in% "Visisimua mwili" ~ V3,
      TRUE ~ NA_character_
    )
  )

drugs <- drugs %>%
  mutate(
    inhalants = case_when(
      V1 %in% "Dawa za kuvuta kupitia puani" ~ V1,
      V2 %in% "Dawa za kuvuta kupitia puani" ~ V2,
      V3 %in% "Dawa za kuvuta kupitia puani" ~ V3,
      V4 %in% "Dawa za kuvuta kupitia puani" ~ V4,
      TRUE ~ NA_character_
    )
  )


drugs <- drugs %>%
  mutate(
    marijuana = case_when(
      V1 %in% "Bangi" ~ V1,
      V2 %in% "Bangi" ~ V2,
      V3 %in% "Bangi" ~ V3,
      V4 %in% "Bangi" ~ V4,
      V5 %in% "Bangi" ~ V5,
      TRUE ~ NA_character_
    )
  )

drugs <- drugs %>%
  mutate(
    hallu = case_when(
      V1 %in% "Viamsha njozi- hallucinogens" ~ V1,
      V2 %in% "Viamsha njozi- hallucinogens" ~ V2,
      V3 %in% "Viamsha njozi- hallucinogens" ~ V3,
      V4 %in% "Viamsha njozi- hallucinogens" ~ V4,
      V5 %in% "Viamsha njozi- hallucinogens" ~ V5,
      V6 %in% "Viamsha njozi- hallucinogens" ~ V6,
      TRUE ~ NA_character_
    )
  )

drugs <- drugs %>%
  mutate(
    sedatives = case_when(
      V1 %in% "Dawa za kuleta usingizi" ~ V1,
      V2 %in% "Dawa za kuleta usingizi" ~ V2,
      V3 %in% "Dawa za kuleta usingizi" ~ V3,
      V4 %in% "Dawa za kuleta usingizi" ~ V4,
      V5 %in% "Dawa za kuleta usingizi" ~ V5,
      V6 %in% "Dawa za kuleta usingizi" ~ V6,
      V7 %in% "Dawa za kuleta usingizi" ~ V7,
      TRUE ~ NA_character_
    )
  )

drugs <- drugs %>%
  mutate(
    tobacco = case_when(
      V1 %in% "Bidhaa za tumbaku" ~ V1,
      V2 %in% "Bidhaa za tumbaku" ~ V2,
      V3 %in% "Bidhaa za tumbaku" ~ V3,
      V4 %in% "Bidhaa za tumbaku" ~ V4,
      V5 %in% "Bidhaa za tumbaku" ~ V5,
      V6 %in% "Bidhaa za tumbaku" ~ V6,
      V7 %in% "Bidhaa za tumbaku" ~ V7,
      TRUE ~ NA_character_
    )
  )

drugs <- drugs %>%
  mutate(
    alcohols = case_when(
      V1 %in% "Vinywaji vya vileo/vilevi" ~ V1,
      V2 %in% "Vinywaji vya vileo/vilevi" ~ V2,
      V3 %in% "Vinywaji vya vileo/vilevi" ~ V3,
      V4 %in% "Vinywaji vya vileo/vilevi" ~ V4,
      V5 %in% "Vinywaji vya vileo/vilevi" ~ V5,
      V6 %in% "Vinywaji vya vileo/vilevi" ~ V6,
      V7 %in% "Vinywaji vya vileo/vilevi" ~ V7,
      TRUE ~ NA_character_
    )
  )

drugs <- subset(drugs, select = -c(1:7))

drugs[is.na(drugs)] = "0"

drugs$opioid[drugs$opioid=="Opioids"]<-1
drugs$cocaine[drugs$cocaine=="Cocaine"]<-1
drugs$amphe[drugs$amphe=="Visisimua mwili"] <- 1
drugs$inhalants[drugs$inhalants=="Dawa za kuvuta kupitia puani"] <- 1
drugs$marijuana[drugs$marijuana=="Bangi"] <- 1
drugs$sedatives[drugs$sedatives=="Dawa za kuleta usingizi"] <- 1
drugs$hallu[drugs$hallu == "Viamsha njozi- hallucinogens"] <- 1
drugs$tobacco[drugs$tobacco == "Bidhaa za tumbaku"] <- 1
drugs$alcohols[drugs$alcohols=="Vinywaji vya vileo/vilevi"] <- 1

drugs$id <- 1:481

causes <- read_csv("data_cleaning/causes.csv",show_col_types = FALSE)

causes <- causes %>%
  mutate(
    feel_good = case_when(
      V1 %in% c("Ili wajisikie vizuri") ~ V1,
      TRUE ~ NA_character_
    )
  )

causes <- causes %>%
  mutate(
    feel_better = case_when(
      V1 %in% c("Ili wajisikie Maridadi") ~ V1,
      V2 %in% c("Ili wajisikie Maridadi") ~ V2,
      TRUE ~ NA_character_
    )
  )

causes <- causes %>%
  mutate(
    be_better = case_when(
      V1 %in% c("Ili wawe maridadi") ~ V1,
      V2 %in% c("Ili wawe maridadi") ~ V2,
      V3 %in% c("Ili wawe maridadi") ~ V3,
      TRUE ~ NA_character_
    )
  )


causes <- causes %>%
  mutate(
    curiosity = case_when(
      V1 %in% c("Kwa ajili ya kupenda kujua kwa kuwa wengine wanafanya") ~ V1,
      V2 %in% c("Kwa ajili ya kupenda kujua kwa kuwa wengine wanafanya") ~ V2,
      V3 %in% c("Kwa ajili ya kupenda kujua kwa kuwa wengine wanafanya") ~ V3,
      TRUE ~ NA_character_
    )
  )

causes <- subset(causes, select = -c(1:3))

causes[is.na(causes)] = "0"

causes$feel_good[causes$feel_good =="Ili wajisikie vizuri"] <- 1
causes$feel_better[causes$feel_better == "Ili wajisikie Maridadi"] <- 1
causes$be_better[causes$be_better == "Ili wawe maridadi"] <- 1
causes$curiosity[causes$curiosity == "Kwa ajili ya kupenda kujua kwa kuwa wengine wanafanya"] <- 1

causes$id <- 1:481

combined <- merge(drugs,causes, by = "id")

unlink("data_cleaning/drugs.csv")
unlink("data_cleaning/causes.csv")

risks <- read_csv("data_cleaning/risks.csv")

risks <- risks %>%
  mutate(
    aggressive = case_when(
      V1 %in% c("Ukorofi Utotoni") ~ V1,
      TRUE ~ NA_character_
    )
  )

risks <- risks %>%
  mutate(
    lack_parent = case_when(
      V1 %in% c("Kukosa Uangalizi wa wazazi") ~ V1,
      V2 %in% c("Kukosa Uangalizi wa wazazi") ~ V2,
      TRUE ~ NA_character_
    )
  )

risks <- risks %>%
  mutate(
    poor_social = case_when(
      V1 %in% c("Poor Social Skills") ~ V1,
      V2 %in% c("Poor Social Skills") ~ V2,
      V3 %in% c("Poor Social Skills") ~ V3,
      TRUE ~ NA_character_
    )
  )

risks <- risks %>%
  mutate(
    poor_social = case_when(
      V1 %in% c("Poor Social Skills") ~ V1,
      V2 %in% c("Poor Social Skills") ~ V2,
      V3 %in% c("Poor Social Skills") ~ V3,
      V4 %in% c("Poor Social Skills") ~ V4,
      TRUE ~ NA_character_
    )
  )

risks <- risks %>%
  mutate(
    stress = case_when(
      V1 %in% c("Msongo wa mawazo") ~ V1,
      V2 %in% c("Msongo wa mawazo") ~ V2,
      V3 %in% c("Msongo wa mawazo") ~ V3,
      V4 %in% c("Msongo wa mawazo") ~ V4,
      V5 %in% c("Msongo wa mawazo") ~ V5,
      TRUE ~ NA_character_
    )
  )

risks <- risks %>%
  mutate(
  experi = case_when(
    V1 %in% c("Kujaribu Matumizi ya dawa") ~ V1,
    V2 %in% c("Kujaribu Matumizi ya dawa") ~ V2,
    V3 %in% c("Kujaribu Matumizi ya dawa") ~ V3,
    V4 %in% c("Kujaribu Matumizi ya dawa") ~ V4,
    V5 %in% c("Kujaribu Matumizi ya dawa") ~ V5,
    V6 %in% c("Kujaribu Matumizi ya dawa") ~ V6,
    TRUE ~ NA_character_
  )
)

risks <- risks %>%
  mutate(
    drug_availability = case_when(
      V1 %in% c("Upatikanaji wa dawa mitaani") ~ V1,
      V2 %in% c("Upatikanaji wa dawa mitaani") ~ V2,
      V3 %in% c("Upatikanaji wa dawa mitaani") ~ V3,
      V4 %in% c("Upatikanaji wa dawa mitaani") ~ V4,
      V5 %in% c("Upatikanaji wa dawa mitaani") ~ V5,
      V6 %in% c("Upatikanaji wa dawa mitaani") ~ V6,
      V7 %in% c("Upatikanaji wa dawa mitaani") ~ V7,
      TRUE ~ NA_character_
    )
  )


risks <- risks %>%
  mutate(
    poverty = case_when(
      V1 %in% c("Umasikini kwenye jamii") ~ V1,
      V2 %in% c("Umasikini kwenye jamii") ~ V2,
      V3 %in% c("Umasikini kwenye jamii") ~ V3,
      V4 %in% c("Umasikini kwenye jamii") ~ V4,
      V5 %in% c("Umasikini kwenye jamii") ~ V5,
      V6 %in% c("Umasikini kwenye jamii") ~ V6,
      V7 %in% c("Umasikini kwenye jamii") ~ V7,
      V8 %in% c("Umasikini kwenye jamii") ~ V8,
      TRUE ~ NA_character_
    )
  )

risks <- subset(risks, select = -c(1:8))

risks[is.na(risks)] = "0"

risks$aggressive[risks$aggressive == "Ukorofi Utotoni"] <- 1
risks$lack_parent[risks$lack_parent == "Kukosa Uangalizi wa wazazi"] <- 1
risks$poor_social[risks$poor_social == "Poor Social Skills"] <- 1
risks$stress[risks$stress == "Msongo wa mawazo"] <- 1
risks$drug_availability[risks$drug_availability == "Upatikanaji wa dawa mitaani"] <- 1
risks$poverty[risks$poverty =="Umasikini kwenye jamii"] <- 1
risks$experi[risks$experi == "Kujaribu Matumizi ya dawa"] <- 1

risks$id <- 1:481

combined <- merge(combined,risks, by = "id")

unlink("data_cleaning/risks.csv")

prevention <- read_csv("data_cleaning/prevention.csv")

prevention <- prevention %>%
  mutate(
    self_control = case_when(
      V1 %in% c("Uwezo mzuri wa kujiongoza") ~ V1,
      TRUE ~ NA_character_
    )
  )

prevention <- prevention %>%
  mutate(
    parental = case_when(
      V1 %in% c("Uangalizi na usaidizi wa wazazi") ~ V1,
      V2 %in% c("Uangalizi na usaidizi wa wazazi") ~ V2,
      TRUE ~ NA_character_
    )
  )

prevention <- prevention %>%
  mutate(
    positive_rel = case_when(
      V1 %in% c("Mahusiano Chanya") ~ V1,
      V2 %in% c("Mahusiano Chanya") ~ V2,
      V3 %in% c("Mahusiano Chanya") ~ V3,
      TRUE ~ NA_character_
    )
  )

prevention <- prevention %>%
  mutate(
    academic_excel = case_when(
      V1 %in% c("Uwezo Mzuri kielimu") ~ V1,
      V2 %in% c("Uwezo Mzuri kielimu") ~ V2,
      V3 %in% c("Uwezo Mzuri kielimu") ~ V3,
      V4 %in% c("Uwezo Mzuri kielimu") ~ V4,
      TRUE ~ NA_character_
    )
  )

prevention <- prevention %>%
  mutate(
    policies = case_when(
      V1 %in% c("Sera za kupambana na matumizi ya dawa za kulevya") ~ V1,
      V2 %in% c("Sera za kupambana na matumizi ya dawa za kulevya") ~ V2,
      V3 %in% c("Sera za kupambana na matumizi ya dawa za kulevya") ~ V3,
      V4 %in% c("Sera za kupambana na matumizi ya dawa za kulevya") ~ V4,
      TRUE ~ NA_character_
    )
  )

prevention <- subset(prevention, select = -c(1:4))

prevention[is.na(prevention)] = "0"

prevention$self_control[prevention$self_control == "Uwezo mzuri wa kujiongoza"] <- 1
prevention$parental[prevention$parental == "Uangalizi na usaidizi wa wazazi"] <- 1
prevention$positive_rel[prevention$positive_rel == "Mahusiano Chanya"] <- 1
prevention$academic_excel[prevention$academic_excel == "Uwezo Mzuri kielimu"] <- 1
prevention$policies[prevention$policies == "Sera za kupambana na matumizi ya dawa za kulevya"] <- 1

prevention$id <- 1:481

combined <- merge(combined,prevention, by = "id")

unlink("data_cleaning/prevention.csv")


effects <- read_csv("data_cleaning/effects.csv")

effects <- effects %>%
  mutate(
    crimes = case_when(
      V1 %in% c("Uhalifu") ~ V1,
      TRUE ~ NA_character_
    )
  )

effects <- effects %>%
  mutate(
    poor_status = case_when(
      V1 %in% c("Nafasi duni Kwenye jamii") ~ V1,
      V2 %in% c("Nafasi duni Kwenye jamii") ~ V2,
      TRUE ~ NA_character_
    )
  )

effects <- effects %>%
  mutate(
    e_hiv = case_when(
      V1 %in% c("Kupata Ukimwi") ~ V1,
      V2 %in% c("Kupata Ukimwi") ~ V2,
      V3 %in% c("Kupata Ukimwi") ~ V3,
      TRUE ~ NA_character_
    )
  )

effects <- effects %>%
  mutate(
    e_hepatitis = case_when(
      V1 %in% c("Kupata Homa ya Ini") ~ V1,
      V2 %in% c("Kupata Homa ya Ini") ~ V2,
      V3 %in% c("Kupata Homa ya Ini") ~ V3,
      V4 %in% c("Kupata Homa ya Ini") ~ V4,
      TRUE ~ NA_character_
    )
  )

effects <- effects %>%
  mutate(
    e_tb = case_when(
      V1 %in% c("Kupata Kifua Kikuu") ~ V1,
      V2 %in% c("Kupata Kifua Kikuu") ~ V2,
      V3 %in% c("Kupata Kifua Kikuu") ~ V3,
      V4 %in% c("Kupata Kifua Kikuu") ~ V4,
      V5 %in% c("Kupata Kifua Kikuu") ~ V5,
      TRUE ~ NA_character_
    )
  )

effects <- effects %>%
  mutate(
    e_sti = case_when(
      V1 %in% c("Kupata Magonjwa ya Ngono") ~ V1,
      V2 %in% c("Kupata Magonjwa ya Ngono") ~ V2,
      V3 %in% c("Kupata Magonjwa ya Ngono") ~ V3,
      V4 %in% c("Kupata Magonjwa ya Ngono") ~ V4,
      V5 %in% c("Kupata Magonjwa ya Ngono") ~ V5,
      V6 %in% c("Kupata Magonjwa ya Ngono") ~ V6,
      TRUE ~ NA_character_
    )
  )

effects <- effects %>%
  mutate(
    poor_health = case_when(
      V1 %in% c("Afya Duni") ~ V1,
      V2 %in% c("Afya Duni") ~ V2,
      V3 %in% c("Afya Duni") ~ V3,
      V4 %in% c("Afya Duni") ~ V4,
      V5 %in% c("Afya Duni") ~ V5,
      V6 %in% c("Afya Duni") ~ V6,
      V7 %in% c("Afya Duni") ~ V7,
      TRUE ~ NA_character_
    )
  )


effects <- effects %>%
  mutate(
    poor_economy = case_when(
      V1 %in% c("Uwezo duni wa kiuchumi") ~ V1,
      V2 %in% c("Uwezo duni wa kiuchumi") ~ V2,
      V3 %in% c("Uwezo duni wa kiuchumi") ~ V3,
      V4 %in% c("Uwezo duni wa kiuchumi") ~ V4,
      V5 %in% c("Uwezo duni wa kiuchumi") ~ V5,
      V6 %in% c("Uwezo duni wa kiuchumi") ~ V6,
      V7 %in% c("Uwezo duni wa kiuchumi") ~ V7,
      V8 %in% c("Uwezo duni wa kiuchumi") ~ V8,
      TRUE ~ NA_character_
    )
  )

effects <- effects %>%
  mutate(
    stigma = case_when(
      V1 %in% c("Kutengwa") ~ V1,
      V2 %in% c("Kutengwa") ~ V2,
      V3 %in% c("Kutengwa") ~ V3,
      V4 %in% c("Kutengwa") ~ V4,
      V5 %in% c("Kutengwa") ~ V5,
      V6 %in% c("Kutengwa") ~ V6,
      V7 %in% c("Kutengwa") ~ V7,
      V8 %in% c("Kutengwa") ~ V8,
      V9 %in% c("Kutengwa") ~ V9,
      TRUE ~ NA_character_
    )
  )

effects <- subset(effects, select = -c(1:10))

effects[is.na(effects)] = "0"

effects$crimes[effects$crimes == "Uhalifu"] <- 1
effects$poor_status[effects$poor_status == "Nafasi duni Kwenye jamii"] <- 1
effects$e_hiv[effects$e_hiv == "Kupata Ukimwi"] <- 1
effects$e_hepatitis[effects$e_hepatitis == "Kupata Homa ya Ini"] <-1
effects$e_tb[effects$e_tb == "Kupata Kifua Kikuu"] <- 1
effects$e_sti[effects$e_sti == "Kupata Magonjwa ya Ngono"] <- 1
effects$poor_health[effects$poor_health == "Afya Duni"] <- 1
effects$poor_economy[effects$poor_economy == "Uwezo duni wa kiuchumi"] <- 1
effects$stigma[effects$stigma == "Kutengwa"] <- 1 

effects$id <- 1:481

combined <- merge(combined,effects, by = "id")

unlink("data_cleaning/effects.csv")

treatment <- read_csv("data_cleaning/treatment.csv")

treatment <- treatment %>%
  mutate(
    methadone = case_when(
      V1 %in% c("Dawa ya Methadoni") ~ V1,
      TRUE ~ NA_character_
    )
  )

treatment <- treatment %>%
  mutate(
    rehab = case_when(
      V1 %in% c("Rehabilitation") ~ V1,
      V2 %in% c("Rehabilitation") ~ V2,
      TRUE ~ NA_character_
    )
  )

treatment <- treatment %>%
  mutate(
    cbt = case_when(
      V1 %in% c("Cognitive Behavioral Therapy") ~ V1,
      V2 %in% c("Cognitive Behavioral Therapy") ~ V2,
      V3 %in% c("Cognitive Behavioral Therapy") ~ V3,
      TRUE ~ NA_character_
    )
  )

treatment <- treatment %>%
  mutate(
    fa_therapy = case_when(
      V1 %in% c("Family Therapy") ~ V1,
      V2 %in% c("Family Therapy") ~ V2,
      V3 %in% c("Family Therapy") ~ V3,
      TRUE ~ NA_character_
    )
  )

treatment <- treatment %>%
  mutate(
    al_therapy = case_when(
      V1 %in% c("Alternative Therapy") ~ V1,
      V2 %in% c("Alternative Therapy") ~ V2,
      V3 %in% c("Alternative Therapy") ~ V3,
      TRUE ~ NA_character_
    )
  )

treatment <- subset(treatment, select = -c(1:3))

treatment[is.na(treatment)] = "0"

treatment$methadone[treatment$methadone == "Dawa ya Methadoni"] <- 1
treatment$rehab[treatment$rehab == "Rehabilitation"] <- 1
treatment$cbt[treatment$cbt == "Cognitive Behavioral Therapy"] <- 1
treatment$fa_therapy[treatment$fa_therapy == "Family Therapy"] <- 1
treatment$al_therapy[treatment$al_therapy == "Alternative Therapy"] <- 1

treatment$id <- 1:481

combined <- merge(combined,treatment, by = "id")

unlink("drugs_cleaning/treatment.csv")


du_use_which <- read_csv("data_cleaning/du_use_which.csv")

du_use_which <- du_use_which %>%
  mutate(
    u_heroin = case_when(
      V1 %in% c("Heroin") ~ V1,
      TRUE ~ NA_character_
    )
  )

du_use_which <- du_use_which %>%
  mutate(
    u_cocaine = case_when(
      V1 %in% c("Cocaine") ~ V1,
      V2 %in% c("Cocaine") ~ V2,
      TRUE ~ NA_character_
    )
  )

du_use_which <- du_use_which %>%
  mutate(
    u_crack = case_when(
      V1 %in% c("Crack") ~ V1,
      V2 %in% c("Crack") ~ V2,
      V3 %in% c("Crack") ~ V3,
      TRUE ~ NA_character_
    )
  )

du_use_which <- du_use_which %>%
  mutate(
    u_amphe = case_when(
      V1 %in% c("Amphetamine") ~ V1,
      V2 %in% c("Amphetamine") ~ V2,
      V3 %in% c("Amphetamine") ~ V3,
      V4 %in% c("Amphetamine") ~ V4,
      TRUE ~ NA_character_
    )
  )

du_use_which <- du_use_which %>%
  mutate(
    u_inhalants = case_when(
      V1 %in% c("Vivutikaji") ~ V1,
      V2 %in% c("Vivutikaji") ~ V2,
      V3 %in% c("Vivutikaji") ~ V3,
      V4 %in% c("Vivutikaji") ~ V4,
      V5 %in% c("Vivutikaji") ~ V5,
      TRUE ~ NA_character_
    )
  )

du_use_which <- du_use_which %>%
  mutate(
    u_hero_coca = case_when(
      V1 %in% c("Heroin ikiwa ana cocaine") ~ V1,
      V2 %in% c("Heroin ikiwa ana cocaine") ~ V2,
      V3 %in% c("Heroin ikiwa ana cocaine") ~ V3,
      V4 %in% c("Heroin ikiwa ana cocaine") ~ V4,
      V5 %in% c("Heroin ikiwa ana cocaine") ~ V5,
      TRUE ~ NA_character_
    )
  )

du_use_which <- du_use_which %>%
  mutate(
    u_marijuana = case_when(
      V1 %in% c("Bangi") ~ V1,
      V2 %in% c("Bangi") ~ V2,
      V3 %in% c("Bangi") ~ V3,
      V4 %in% c("Bangi") ~ V4,
      V5 %in% c("Bangi") ~ V5,
      TRUE ~ NA_character_
    )
  )

du_use_which <- du_use_which %>%
  mutate(
    u_hallu = case_when(
      V1 %in% c("Hallicinogens") ~ V1,
      V2 %in% c("Hallicinogens") ~ V2,
      V3 %in% c("Hallicinogens") ~ V3,
      V4 %in% c("Hallicinogens") ~ V4,
      V5 %in% c("Hallicinogens") ~ V5,
      TRUE ~ NA_character_
    )
  )

du_use_which <- subset(du_use_which, select = -c(1:5))

du_use_which[is.na(du_use_which)] = "0"

du_use_which$u_heroin[du_use_which$u_heroin == "Heroin"] <- 1
du_use_which$u_cocaine[du_use_which$u_cocaine == "Cocaine"] <- 1
du_use_which$u_crack[du_use_which$u_crack == "Crack"] <- 1
du_use_which$u_amphe[du_use_which$u_amphe== "Amphetamine"] <- 1
du_use_which$u_inhalants[du_use_which$u_inhalants == "Vivutikaji"] <- 1
du_use_which$u_hero_coca[du_use_which$u_hero_coca == "Heroin ikiwa ana cocaine"] <- 1
du_use_which$u_marijuana[du_use_which$u_marijuana == "Bangi"] <- 1
du_use_which$u_hallu[du_use_which$u_hallu == "Hallicinogens"] <- 1

du_use_which$id <- 1:481

combined <- merge(combined,du_use_which, by = "id")

unlink("drugs_cleaning/du_use_which.csv")


du_route <- read_csv("data_cleaning/du_route.csv")

du_route <- du_route %>%
  mutate(
    ro_smoke = case_when(
      V1 %in% c("Kuvuta kwa mdomo") ~ V1,
      TRUE ~ NA_character_
    )
  )

du_route <- du_route %>%
  mutate(
    ro_ingest = case_when(
      V1 %in% c("Kumeza") ~ V1,
      V2 %in% c("Kumeza") ~ V2,
      TRUE ~ NA_character_
    )
  )

du_route <- du_route %>%
  mutate(
    ro_snif = case_when(
      V1 %in% c("Kuvuta kwa pua") ~ V1,
      V2 %in% c("Kuvuta kwa pua") ~ V2,
      V3 %in% c("Kuvuta kwa pua") ~ V3,
      TRUE ~ NA_character_
    )
  )

du_route <- du_route %>%
  mutate(
    ro_inject = case_when(
      V1 %in% c("Najidunga") ~ V1,
      V2 %in% c("Najidunga") ~ V2,
      V3 %in% c("Najidunga") ~ V3,
      TRUE ~ NA_character_
    )
  )

du_route <- subset(du_route, select = -c(1:3))

du_route[is.na(du_route)] = "0"

du_route$ro_smoke[du_route$ro_smoke == "Kuvuta kwa mdomo"] <- 1
du_route$ro_snif[du_route$ro_snif == "Kuvuta kwa pua"] <- 1
du_route$ro_ingest[du_route$ro_ingest == "Kumeza"] <- 1
du_route$ro_inject[du_route$ro_inject == "Najidunga"] <- 1

du_route$id <- 1:481

combined <- merge(combined,du_route, by = "id")

huru$id <- 1:481

df <- merge(huru,combined, by = "id")

unlink("data_cleaning/du_route.csv")
unlink("data_cleaning/du_use_which.csv")
unlink("data_cleaning/treatment.csv")

write_csv(df,"df.csv")

df <- read_csv("df.csv",show_col_types = FALSE)

unlink("df.csv")

df$alc_how_many[is.na(df$alc_how_many)] <- 0
df$alc_use_times[is.na(df$alc_use_times)] <- 0
df$alc_one_day[is.na(df$alc_one_day)] <- 0
df$du_inject[is.na(df$du_inject)] <- 0
df$du_happy[is.na(df$du_happy)] <- 0
df$di_share[is.na(df$di_share)] <- 0
df$di_freq[is.na(df$di_freq)] <- 0
df$du_past_six[is.na(df$du_past_six)] <- 0

df <- df %>% mutate(location = recode(location,
                                      `Tanga Mjini` = "Tanga City",
                                      `Muheza` = "Muheza District"))
write_csv(df,"ds.csv")

rm(list = ls())

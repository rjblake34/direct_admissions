rm(list = ls(all.names = TRUE))
library(tidyverse)
library(ipumsr)
library(fixest)
library(miceadds)

#LOAD ACS DATA
setwd("/Users/ryan/DataspellProjects/direct_admissions/")
ddi <- read_ipums_ddi("usa_00019.xml", lower_vars = TRUE)
ipums <- read_ipums_micro(ddi)

#EXPLORE METADATA
#ipums_var_label(ddi, gradeattd)
#ipums_var_desc(ddi, gradeattd)
#ipums_val_labels(ddi, gradeatt) %>% strwrap(60)
#ipums_view(ddi)


#LOAD POLICY SPREADSHEET
policysheet <- read.csv("/Users/ryan/DataspellProjects/direct_admissions/policysheet.csv")
#LOAD NHGIS SPREADHSEET
nhgis <- read.csv("/Users/ryan/DataspellProjects/direct_admissions/nhgis.csv")

#CLEAN NHGIS VARIABLES FOR DFL REWEIGHTING
nhgis <- nhgis %>%
  mutate(
    #percent living in rural area
    rural_pct = rural / total_region,
    #percent w/ high school degree
    hs_complete_pct =
      (malehs+malecollege1+malecollege2+maleassoc+malebach+malemaster+maleprof+maledoc+
        femalehs+femalecollege1+femalecollege2+femaleassoc+femalebach+femalemaster+femaleprof+femaledoc)/
        total_attainment,
    #percent college go on
    college_pct =
      (malecollege1+malecollege2+maleassoc+malebach+malemaster+maleprof+maledoc+
        femalecollege1+femalecollege2+femaleassoc+femalebach+femalemaster+femaleprof+femaledoc)/
        (malehs +femalehs),
    #percent young adults <25 unemployed in labor force
    youth_unemployment =
      (male16_labor_civ_unemployed + male20_labor_civ_unemployed + male22_labor_civ_unemployed +
        female16_labor_civ_unemployed + female20_labor_civ_unemployed + female22_labor_civ_unemployed)/
        (male16_labor_civ + male20_labor_civ + male22_labor_civ +
          female16_labor_civ + female20_labor_civ + female22_labor_civ),
    #percent young adults <25 in labor force who serve in the military
    vet_pct =
      (male16_labor_vet + male20_labor_vet + male22_labor_vet +
        female16_labor_vet + female20_labor_vet + female22_labor_vet)/
        (male16_labor_total + male20_labor_total + male22_labor_total +
          female16_labor_total + female20_labor_total + female22_labor_total),
  )%>%
  select(state, statefip, rural_pct, hs_complete_pct, youth_unemployment, vet_pct, median_income)

#MERGE POLICY AND NHGIS DATA
states <- merge(policysheet, nhgis, by=c("state","statefip"))


#CALCULATE DFL
reg_nhgis <- glm(da2020 ~ rural_pct + hs_complete_pct + youth_unemployment + vet_pct +  median_income,
                 family=binomial(link='probit'), data = states)

summary(reg_nhgis)
with(summary(reg_nhgis), 1 - deviance/null.deviance) #print r squared

predict_da <- fitted(reg_nhgis)
states  <- states %>%
  mutate(pda = predict_da,
         dfl = ifelse(da2020 == 1, 1, (predict_da/(1-predict_da))))

#DFL reweight check
lm(rural_pct ~ da2020, data = states)
lm(rural_pct ~ da2020, data = states, weights = dfl)

lm(hs_complete_pct ~ da2020, data = states)
lm(hs_complete_pct ~ da2020, data = states, weights = dfl)

lm(youth_unemployment ~ da2020, data = states)
lm(youth_unemployment ~ da2020, data = states, weights = dfl)

lm(vet_pct ~ da2020, data = states)
lm(vet_pct ~ da2020, data = states, weights = dfl)

lm(median_income ~ da2020, data = states)
lm(median_income~ da2020, data = states, weights = dfl)


#WIDE DATA TO LONG DATA
states_long <- states %>%
  pivot_longer(
    starts_with("da"),
    names_to = c(".value", "year"),
    names_pattern = "^(.*?)(\\d+)$",
  ) %>%
  mutate(year = as.numeric(year))

#MERGE STATE LEVEL POLICY DATA ONE TO MANY WITH ACS DATA
df <- inner_join(states_long, ipums, by = c("statefip", "year"), multiple = "all")

#FILTER TO INCLUDE ONLY HIGH SCHOOL GRADUATES
#MUTATE TO CREATE BINARY OUTCOME VARIABLE 'ENROLL' & ADD CONTROLS
df <- df %>%
  filter(educd >= 63, age == 19, school == 1 | school == 2, migrate1 == 1 | migrate1 == 2) %>%
  mutate(enroll = ifelse(educd >= 65, 1, 0), #OUTCOME VARIABLE
         female = ifelse(sex == 2, 1,0),
         hispanic = ifelse(hispand > 0,1,0),
         black = ifelse(race == 2 & hispand == 0,1,0),
         othrace = ifelse(race > 2 & hispand == 0,1,0),
         citizen = ifelse(citizen == 3, 0, 1),
         english = ifelse(speakeng == 1, 0, 1),
         insurance = ifelse(hcovany == 1, 0, 1),
         dflrewt = perwt*dfl)

#SUMMARY STATISTICS
df %>%
  summarize(
    n = n(),
    college_go_on = mean(enroll),
    college_go_on_sd = sd(enroll),
    female = mean(female),
    black = mean(black),
    hispanic = mean(hispanic),
    othrace = mean(othrace),
    citizen = mean(citizen),
    english = mean(english),
    insurance = mean(insurance))

df %>%
  group_by(year) %>%
  summarize(
    n = n(),
    college_go_on = mean(enroll),
    college_go_on_sd = sd(enroll),
    female = mean(female),
    black = mean(black),
    hispanic = mean(hispanic),
    othrace = mean(othrace),
    citizen = mean(citizen),
    english = mean(english),
    insurance = mean(insurance))

df %>%
  group_by(da) %>%
  summarize(
    n = n(),
    college_go_on = mean(enroll),
    college_go_on_sd = sd(enroll),
    female = mean(female),
    black = mean(black),
    hispanic = mean(hispanic),
    othrace = mean(othrace),
    citizen = mean(citizen),
    english = mean(english),
    insurance = mean(insurance))



#REGRESSION MODELS
olsmod1 <- miceadds::glm.cluster(data=df,
                                 formula=enroll ~ da + female + hispanic + black + othrace + citizen + insurance + english +
                                   factor(statefip) + factor(year),
                                 cluster="state",
                                 weights= df$perwt,
                                 family="gaussian")
summary(olsmod1$glm_res)
with(summary(olsmod1$glm_res), 1 - deviance/null.deviance) #print r squared

olsmod2 <- miceadds::glm.cluster(data=df,
                                 formula=enroll ~ da + female + hispanic + black + othrace + citizen + insurance + english +
                                   factor(statefip) + factor(year),
                                 cluster="state",
                                 weights= df$dflrewt,
                                 family="gaussian")
summary(olsmod2$glm_res)
with(summary(olsmod2$glm_res), 1 - deviance/null.deviance) #print r squared

logmod1 <- miceadds::glm.cluster(data=df,
                                 formula=enroll ~ da + female + hispanic + black + othrace + citizen + insurance + english +
                                   factor(statefip) + factor(year),
                                 cluster="state",
                                 weights= df$perwt,
                                 family=binomial(link= "logit"))
summary(logmod1$glm_res)
with(summary(logmod1$glm_res), 1 - deviance/null.deviance) #print r squared


logmod2 <- miceadds::glm.cluster(data=df,
                                 formula= enroll ~ da + female + hispanic + black + othrace + citizen + insurance + english +
                                   factor(statefip) + factor(year),
                                 cluster="state",
                                 weights= df$dflrewt,
                                 family=binomial(link= "logit"))
summary(logmod2$glm_res)
with(summary(logmod2$glm_res), 1 - deviance/null.deviance) #print r squared

#### EVENT STUDY
##https://lost-stats.github.io/Model_Estimation/Research_Design/event_study.html
event_study <- df %>%
  mutate(year_treated = case_when(state == "Idaho" ~ 2016,
                                  state == "South Dakota" ~ 2018,
                                  state != "Idaho" | state != "South Dakota" ~ 0))

es_dflrewt = feols(enroll ~ sunab(year_treated, year, ref.p = -1) +
  female + hispanic + black + othrace + citizen + insurance + english |
  state + year,
                   cluster = ~statefip,
                   weights = event_study$dflrewt,
                   data = event_study)
summary(es_dflrewt)

iplot(es_dflrewt,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')
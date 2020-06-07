#########################################################################################
#	MIT License
#
#	Copyright (c) [2020] [Gonzalo Daniel Garcia: https://github.com/gonzalofichero/]
#
#	Permission is hereby granted, free of charge, to any person obtaining a copy
#	of this software and associated documentation files (the "Software"), to deal
#	in the Software without restriction, including without limitation the rights
#	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#	copies of the Software, and to permit persons to whom the Software is
#	furnished to do so, subject to the following conditions:
#
#	The above copyright notice and this permission notice shall be included in all
#	copies or substantial portions of the Software.
#
#	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#	SOFTWARE.
##########################################################################################

# Title of code: "Mujeres al Borde de un ataque de Nervios: Welfare y Salud Mental.R"
# DOI: https://zenodo.org/badge/latestdoi/265623238


# Loading packages
library(tidyverse)
library(ggplot2)
library(foreign)
library(stargazer)
library(survey)


# Loading data
ess_6 <- read.spss("ESS6e02_4.sav", to.data.frame=TRUE)


##################################################
# Keeping the variables needed for analysis


  # Dependent variables
# fltdpr = felt depressed
# flteeff = felt everything as effort
# slprl = sleep badly
# fltlnl = feel lonely
# fltsd = felt sad
# cldgng = could not get going
# enjlf = enjoyed life
# wrhpp = felt happy


  # Working related independent variables
# mnactic = main activity last 7 days
# wrkctra = working contract
# wkdcorga = decides how daily works organizes
# stfjb = satisfied with job


  # Other control variables
# gndr = gender (1 = Male, 2 = Female)
# agea = age of respondent
# chldhm = Children living in Home (1 = YES, 2 = NO)
# eduyrs = Years Completed Education
# brncntr = Born in Country (1 = YES, 2 = NO, 7 = REFUSAL)
# hinctnta = Household Income
# rlgdgr = how religious are you
# cntry = Country


# Selecciono variables a analizar, me quedo con variables a factorear, descarto niveles no usados
ess_6_v2 <- ess_6 %>%
  select(fltdpr,flteeff,slprl,fltlnl,fltsd,cldgng,enjlf,wrhpp,
         mnactic,wrkctra,wkdcorga,stfjb,
         gndr, cntry, agea, chldhm, eduyrs, brncntr, hinctnta, rlgdgr) %>% 
  droplevels()

# Check formatos
glimpse(ess_6_v2)



#################################
# DATA WRANGLING

# Re-formating
ess_6_v2$age_2 <- as.numeric(as.character(ess_6_v2$agea))
ess_6_v2$edu_2 <- as.numeric(as.character(ess_6_v2$eduyrs))


# Reformateo de categorías para Ronda 2
ess_6_v3 <- ess_6_v2 %>% 
  mutate(
    # CES-D eight-item depression scale
    feel_depressed = case_when(fltdpr == "None or almost none of the time" ~ 1,
                          fltdpr == "Some of the time" ~ 2,
                          fltdpr == "Most of the time" ~ 3,
                          fltdpr == "All or almost all of the time" ~ 4,
                          TRUE ~ NA_real_),
    feel_effort = case_when(flteeff == "None or almost none of the time" ~ 1,
                         flteeff == "Some of the time" ~ 2,
                         flteeff == "Most of the time" ~ 3,
                         flteeff == "All or almost all of the time" ~ 4,
                         TRUE ~ NA_real_),
    bad_sleep = case_when(slprl == "None or almost none of the time" ~ 1,
                            slprl == "Some of the time" ~ 2,
                            slprl == "Most of the time" ~ 3,
                            slprl == "All or almost all of the time" ~ 4,
                            TRUE ~ NA_real_),
    feel_lonely = case_when(fltlnl == "None or almost none of the time" ~ 1,
                          fltlnl == "Some of the time" ~ 2,
                          fltlnl == "Most of the time" ~ 3,
                          fltlnl == "All or almost all of the time" ~ 4,
                          TRUE ~ NA_real_),
    feel_sad = case_when(fltsd == "None or almost none of the time" ~ 1,
                            fltsd == "Some of the time" ~ 2,
                            fltsd == "Most of the time" ~ 3,
                            fltsd == "All or almost all of the time" ~ 4,
                            TRUE ~ NA_real_),
    not_going = case_when(cldgng == "None or almost none of the time" ~ 1,
                          cldgng == "Some of the time" ~ 2,
                          cldgng == "Most of the time" ~ 3,
                          cldgng == "All or almost all of the time" ~ 4,
                         TRUE ~ NA_real_),
    feel_joy = case_when(enjlf == "None or almost none of the time" ~ 4,
                          enjlf == "Some of the time" ~ 3,
                          enjlf == "Most of the time" ~ 2,
                          enjlf == "All or almost all of the time" ~ 1,
                          TRUE ~ NA_real_),
    feel_happy = case_when(wrhpp == "None or almost none of the time" ~ 4,
                           wrhpp == "Some of the time" ~ 3,
                           wrhpp == "Most of the time" ~ 2,
                           wrhpp == "All or almost all of the time" ~ 1,
                         TRUE ~ NA_real_)
  )


ess_6_v3 <- ess_6_v3 %>% 
  mutate(
    # Summarise all flags into CES-D deppresive feelings scale (Radloff, 1977)
    CESD_scale = select(., c(feel_depressed, feel_effort, bad_sleep, feel_lonely,
                             feel_sad, not_going, feel_joy, feel_happy)) %>% apply(1, sum, na.rm=TRUE),
    
    # Welfare State typology by Esping-Andersen (1999)
    welfare_andersen = case_when(
      cntry == "Austria" ~ "Continental",
      cntry == "Belgium" ~ "Continental",
      cntry == "Germany" ~ "Continental",
      cntry == "Spain" ~ "South_Europe",
      cntry == "Finland" ~ "Social_Democratic",
      cntry == "France" ~ "Continental",
      cntry == "United Kingdom" ~ "Liberal",
      cntry == "Ireland" ~ "Liberal",
      cntry == "Italy" ~ "South_Europe",
      cntry == "Netherlands" ~ "Social_Democratic",
      cntry == "Portugal" ~ "South_Europe",
      cntry == "Sweden" ~ "Social_Democratic",
      cntry == "Norway" ~ "Social_Democratic",
      cntry == "Denmark" ~ "Social_Democratic",
      TRUE ~ "Other"
    ),
    # Welfare State typology by Lewis (1992)
    welfare_lewis = case_when(
      cntry == "Austria" ~ "Modified_BW",
      cntry == "Belgium" ~ "Modified_BW",
      cntry == "Germany" ~ "Modified_BW",
      cntry == "Spain" ~ "Strong_BW",
      cntry == "Finland" ~ "Weak_BW",
      cntry == "France" ~ "Modified_BW",
      cntry == "United Kingdom" ~ "Strong_BW",
      cntry == "Ireland" ~ "Strong_BW",
      cntry == "Italy" ~ "Strong_BW",
      cntry == "Netherlands" ~ "Weak_BW",
      cntry == "Portugal" ~ "Strong_BW",
      cntry == "Sweden" ~ "Weak_BW",
      cntry == "Norway" ~ "Weak_BW",
      cntry == "Denmark" ~ "Weak_BW",
      TRUE ~ "Other"
    ),
    
    # Create Age groups
    age_group = case_when(age_2 <= 30 & age_2 >= 18 ~ "01 18 to 30 years",
                          age_2 <= 45 & age_2 > 30 ~ "02 30 to 45 years",
                          age_2 <= 60 & age_2 > 45 ~ "03 45 to 60 years",
                          age_2 <= 80 & age_2 > 60 ~ "04 60 to 80 years",
                          age_2 > 80 ~ "05 Higher 80 years",
                          TRUE ~ "06 No Age Reported"),
    
    # How satisfied are you with your job?
    work_satisfaction = case_when(stfjb == "Extremely dissatisfied" ~ 0,
                             stfjb == "Extremely satisfied" ~ 10,
                             TRUE ~ as.numeric(as.character(stfjb))),
    # How religious are you?
    religiosity = case_when(rlgdgr == "Not at all religious" ~ 0,
                                 rlgdgr == "Very religious" ~ 10,
                                 TRUE ~ as.numeric(as.character(rlgdgr))),
    
    # Create Income groups
    income_group = case_when(hinctnta %in% c("J - 1st decile","R - 2nd decile","C - 3rd decile") ~ "01 Lower class",
                             hinctnta %in% c("M - 4th decile","F - 5th decile","S - 6th decile","K - 7th decile","P - 8th decile") ~ "02 Middle class",
                             hinctnta %in% c("D - 9th decile","H - 10th decile") ~ "03 High class",
                             TRUE ~ "04 - No Reported Income"),
    
    # Sex instead of Gender, Man as baseline
    sex = gndr,
    # Children in home: 1 == YES, no children as baseline
    children = case_when(chldhm == "Respondent lives with children at household grid" ~ "02 Living with Children",
                         chldhm == "Does not" ~ "01 No living with children",
                         TRUE ~ "03 No response"),
    # Control over daily work
    job_control = case_when(wkdcorga %in% c("I have/had no influence", "1", "2") ~ "01 Low Control",
                         wkdcorga %in% c("3", "4", "5", "6", "7") ~ "02 Medium Control",
                         wkdcorga %in% c("8", "9", "I have/had complete control") ~ "03 High Control",
                         TRUE ~ "00 No response"),
    # Job Situation
    job_situation = case_when(mnactic %in% c("Unemployed, looking for job", "Unemployed, not looking for job") ~ "Unemployed",
                              mnactic == "Paid work" ~ "Paid work",
                              mnactic == "Education" ~ "Education",
                              mnactic == "Retired" ~ "Retired",
                              mnactic == "Housework, looking after children, others" ~ "Housework",
                         TRUE ~ "Other"),
    # Born national: No == foreign
    National_citizen = brncntr
  )


####################
# FINAL CLEANING
ess_6_v4 <- ess_6_v3 %>% 
              filter(CESD_scale >= 8, welfare_andersen != "Other", 
                     !is.na(sex), children != "03 No response", 
                     income_group != "04 - No Reported Income",
                     age_group != "06 No Age Reported",
                     #job_control != "04 No response",
                     job_situation != "Other") %>% 
              droplevels()



##############################
# ANALYTICS

# Distribution of depression by welfare state
ess_6_v4 %>% 
  ggplot(aes(x=CESD_scale)) + geom_histogram(bins=25) +
  facet_wrap(. ~ welfare_andersen, ncol=2)


ess_6_v4 %>% 
  ggplot(aes(x =welfare_andersen, y=CESD_scale)) + geom_boxplot()

ess_6_v4 %>% 
  ggplot(aes(x =welfare_lewis, y=CESD_scale)) + geom_boxplot()


# Density by Welfare
ess_6_v4 %>% 
  ggplot(aes(x =CESD_scale, color=welfare_andersen)) + geom_density()

ess_6_v4 %>% 
  ggplot(aes(x =CESD_scale, color=welfare_lewis)) + geom_density()


# By Sex
ess_6_v4 %>% 
  ggplot(aes(y = CESD_scale, x = sex)) + geom_boxplot() +
  facet_wrap(. ~ cntry, ncol=5)

ess_6_v4 %>%
    group_by(cntry, sex) %>% 
    summarise(m = mean(CESD_scale, na.rm = T)) %>% 
    pivot_wider(names_from = sex, values_from = m)
  
# By Income
ess_6_v4 %>% 
  ggplot(aes(y = CESD_scale, x = income_group)) + geom_boxplot()


# By Nationality
ess_6_v4 %>% 
  filter(!is.na(National_citizen)) %>% 
  ggplot(aes(y = CESD_scale, x = National_citizen)) + geom_boxplot()


# By Work Condition
ess_6_v4 %>% 
  sample_n(size = 500) %>% 
  ggplot(aes(x = work_satisfaction, y = CESD_scale, color = welfare_lewis)) + geom_jitter(alpha=0.5)

ess_6_v4 %>% 
  #filter(job_control != "04 No response") %>% 
  ggplot(aes(x = job_control, y = CESD_scale)) + geom_boxplot()


# By kids
ess_6_v4 %>% 
  #filter(job_control != "04 No response") %>% 
  ggplot(aes(x = children, y = CESD_scale)) + geom_boxplot() +
  facet_wrap(. ~ job_situation, ncol=3)

ess_6_v4 %>% 
  ggplot(aes(x=CESD_scale)) + geom_histogram(bins=25) +
  facet_wrap(. ~ children, ncol=2)


ess_6_v4 %>%
  group_by(cntry, children) %>% 
  summarise(m = mean(CESD_scale, na.rm = T)) %>% 
  pivot_wider(names_from = children, values_from = m)


# By Religion
ess_6_v4 %>% 
  sample_n(size = 500) %>% 
  ggplot(aes(x = religiosity, y = CESD_scale, color = welfare_lewis)) + geom_jitter(alpha=0.5)




#######################################
# MAP PLOTS
library(grid)
library(rworldmap)
library(mapproj)

# Get the world map
worldMap <- getMap()

# Member States of the European Union
europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Rep.","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","United Kingdom")

# Select only the index of states member of the E.U.
indEU <- which(worldMap$NAME%in%europeanUnion)

europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})

europeCoords <- do.call("rbind", europeCoords)

# Create table with country and mean CES-D scale value for 2012 year
dep_by_cnty <- ess_6_v4 %>% 
  group_by(cntry) %>% 
  summarise(depression = mean(CESD_scale, na.rm = T)) %>% 
  #pivot_wider(names_from = sex, values_from = depression) %>% 
  rename(country = cntry)

# Matching country, depression scale and lat-long coordinates
europeCoords$value <- dep_by_cnty$depression[match(europeCoords$region,dep_by_cnty$country)]


# Plot the map
ggplot() + geom_polygon(data = europeCoords, aes(x = long, y = lat, group = region, fill = value),
                        colour = "black", size = 0.1) +
  coord_map(xlim = c(-13, 35),  ylim = c(32, 71)) +
  scale_fill_gradient(name = "CES-D scale", low = "#FF0000FF", high = "#FFFF00FF", na.value = "grey50") +
  theme(#panel.grid.minor = element_line(colour = NA), panel.grid.minor = element_line(colour = NA),
    #panel.background = element_rect(fill = NA, colour = NA),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(), axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(), axis.title = element_blank(),
    #rect = element_blank(),
    plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))




###############################
# REGRESSION
# Multilevel regression
library(lme4)
library(lmerTest)
library(texreg)

model_andersen <- lmer(CESD_scale ~ age_group +  edu_2 + job_control + job_situation + sex + religiosity + income_group + children + National_citizen + (1|welfare_andersen), data=ess_6_v4)
summary(model_andersen)
confint(model_andersen)
anova(model_andersen)


model_lewis <- lmer(CESD_scale ~ age_group +  edu_2 + job_control + job_situation + sex + religiosity + income_group + children + National_citizen + (1|welfare_lewis), data=ess_6_v4)
summary(model_lewis)
confint(model_lewis)
anova(model_lewis)

model_lewis2 <- lmer(CESD_scale ~ age_group +  edu_2 + job_control + job_situation *  children + sex + religiosity + income_group + National_citizen + (1|welfare_lewis), data=ess_6_v4)
summary(model_lewis2)


# Comparing the 2 models
# https://stackoverflow.com/questions/31319030/r-stargazer-lme4-and-lmertest-incompatibility
# https://rstudio-pubs-static.s3.amazonaws.com/78096_0b53eb1825a44a1d99225993aab213d0.html
anova(model_andersen, model_lewis)
rand(model_andersen)
rand(model_lewis)

screenreg(list(model_andersen, model_lewis))


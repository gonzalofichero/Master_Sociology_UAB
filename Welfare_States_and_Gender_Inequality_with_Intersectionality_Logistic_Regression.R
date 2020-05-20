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

# Title of code: "Welfare_States_and_Gender_Inequality_with_Intersectionality_Logistic_Regression.R"
# DOI: https://zenodo.org/badge/latestdoi/265623238


# Loading packages
library(tidyverse)
library(ggplot2)
library(foreign)
library(stargazer)
library(survey)


# Loading data
ess_2 <- read.spss("ESS2e03_6.sav", to.data.frame=TRUE)
ess_5 <- read.spss("ESS5e03_4.sav", to.data.frame=TRUE)


##################################################
# Keeping the variables needed for analysis
# wmcpwrk = A woman should be prepared to cut down on her paid work for the sake of her family.
# mnrgtjb = When jobs are scarce, men should have more right to a job than women.
# gndr = gender (1 = Male, 2 = Female)
# agea = age of respondent
# chldhm = Children living in Home (1 = YES, 2 = NO)
# eduyrs = Years Completed Education
# brncntr = Born in Country (1 = YES, 2 = NO, 7 = REFUSAL)
# hinctnta = Household Income
# lrscale = placement on Political Scale (1 = Left, 10 = Right)
# cntry = Country



# Selecciono variables a analizar, me quedo con variables a factorear, descarto niveles no usados
ess_2_v2 <- ess_2 %>%
  select(gndr, wmcpwrk, cntry, mnrgtjb, agea, chldhm, eduyrs, brncntr, hinctnt, lrscale) %>% 
  droplevels()

ess_5_v2 <- ess_5 %>%
  select(gndr, wmcpwrk, cntry, mnrgtjb, agea, chldhm, eduyrs, brncntr, hinctnta, lrscale) %>% 
  droplevels()

# Change age from factor to double
ess_2_v2$age_2 <- as.numeric(as.character(ess_2_v2$agea))
ess_5_v2$age_2 <- as.numeric(as.character(ess_5_v2$agea))


# Check datos
glimpse(ess_2_v2)



# Reformateo de categorías para Ronda 2
ess_2_v2 <- ess_2_v2 %>% 
  mutate(
    cat_women = case_when(wmcpwrk == "Agree strongly" ~ 1,
                          wmcpwrk == "Agree" ~ 1,
                          wmcpwrk == "Disagree" ~ 0,
                          wmcpwrk == "Disagree strongly" ~ 0,
                          TRUE ~ NA_real_),
    cat_jobs = case_when(mnrgtjb == "Agree strongly" ~ 1,
                         mnrgtjb == "Agree" ~ 1,
                         mnrgtjb == "Disagree" ~ 0,
                         mnrgtjb == "Disagree strongly" ~ 0,
                          TRUE ~ NA_real_),
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
    # Adjust the political spectrum to numeric
    pol_espectro = case_when(lrscale == "Left" ~ 0,
                             lrscale == "Right" ~ 10,
                             TRUE ~ as.numeric(as.character(lrscale))),
    # Create political groups: Left 0 to 2, Center 3 to 7, Right 8 to 10
    political_group = case_when(pol_espectro <= 2 ~ "01 Left",
                                pol_espectro > 2 & pol_espectro < 8 ~ "02 Center",
                                pol_espectro >= 8 ~ "03 Right",
                                TRUE ~ "04 No political spectrum available"),
    # Create Income groups
    income_group = case_when(hinctnt %in% c("J","R","C") ~ "01 Lower class",
                             hinctnt %in% c("M","F","S","K","P") ~ "02 Middle class",
                             hinctnt %in% c("D","H","U","N") ~ "03 High class",
                             TRUE ~ "04 - No Reported Income"),
    # Sex instead of Gender, Man as baseline
    sex = gndr,
    # Children in home: 1 == YES, no children as baseline
    children = case_when(chldhm == "Respondent lives with children at household grid" ~ "02 Living with Children",
                         chldhm == "Does not" ~ "01 No living with children",
                         TRUE ~ "03 No response"),
    # Born national: No == foreign
    National_citizen = brncntr
  )




# Reformateo de categorías para Ronda 5
ess_5_v2 <- ess_5_v2 %>% 
  mutate(
    cat_women = case_when(wmcpwrk == "Agree strongly" ~ 1,
                          wmcpwrk == "Agree" ~ 1,
                          wmcpwrk == "Disagree" ~ 0,
                          wmcpwrk == "Disagree strongly" ~ 0,
                          TRUE ~ NA_real_),
    cat_jobs = case_when(mnrgtjb == "Agree strongly" ~ 1,
                         mnrgtjb == "Agree" ~ 1,
                         mnrgtjb == "Disagree" ~ 0,
                         mnrgtjb == "Disagree strongly" ~ 0,
                         TRUE ~ NA_real_),
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
    # Adjust the political spectrum to numeric
    pol_espectro = case_when(lrscale == "Left" ~ 0,
                             lrscale == "Right" ~ 10,
                             TRUE ~ as.numeric(as.character(lrscale))),
    # Create political groups: Left 0 to 2, Center 3 to 7, Right 8 to 10
    political_group = case_when(pol_espectro <= 2 ~ "01 Left",
                                pol_espectro > 2 & pol_espectro < 8 ~ "02 Center",
                                pol_espectro >= 8 ~ "03 Right",
                                TRUE ~ "04 No political spectrum available"),
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
    # Born national: No == foreign
    National_citizen = brncntr
  )


###############################################
# Exploratorio

# Tabla doble entrada, por país apoyo a mujeres deben dejar trabajo 
prop.table(table(cuanti$cntry, cuanti$cat_women),1)
# Tabla doble entrada, por país hombres deben mantener trabajo cuando scarce
prop.table(table(cuanti$cntry, cuanti$mnrgtjb),1)



table(ess_2_v2$cntry, ess_2_v2$welfare)

write.table(table(ess_2_v2$cntry, ess_2_v2$welfare), sep=";", file="ronda2.txt")
write.table(table(ess_5_v2$cntry, ess_5_v2$welfare), sep=";", file="ronda5.txt")


##################################
# Base sin "falta de respuestas"
ess_2_v3 <- ess_2_v2 %>% 
                filter(welfare_andersen != "Other") %>% 
                filter(income_group != "04 - No Reported Income") %>% 
                filter(political_group != "04 No political spectrum available") %>% 
                filter(children != "03 No response") %>% 
                filter(age_group != "06 No Age Reported") %>% 
                filter(!is.na(cat_women))


ess_5_v3 <- ess_5_v2 %>% 
                filter(welfare_andersen != "Other") %>% 
                filter(income_group != "04 - No Reported Income") %>% 
                filter(political_group != "04 No political spectrum available") %>% 
                filter(children != "03 No response") %>% 
                filter(age_group != "06 No Age Reported") %>% 
                filter(!is.na(cat_women))
  


##########################################
# REGRESIONES
#
#
# A woman should be prepared to cut down on her paid work for the sake of her family

# Round 2
r_ess2_andersen <- glm(cat_women ~ sex + National_citizen + income_group + political_group + age_group + children + National_citizen + welfare_andersen, data = ess_2_v3, family = "binomial")
summary(r_ess2_andersen)


r_ess2_lewis <- glm(cat_women ~ sex + National_citizen + income_group + political_group + age_group + children + National_citizen + welfare_lewis, data = ess_2_v3, family = "binomial")
summary(r_ess2_lewis)

anova(r_ess2_andersen, r_ess2_lewis, test = "LR")

# Round 5
r_ess5_andersen <- glm(cat_women ~ sex + National_citizen + income_group + political_group + age_group + children + National_citizen + welfare_andersen, data = ess_5_v3, family = "binomial")
summary(r_ess5_andersen)

r_ess5_lewis <- glm(cat_women ~ sex + National_citizen + income_group + political_group + age_group + children + National_citizen + welfare_lewis, data = ess_5_v3, family = "binomial")
summary(r_ess5_lewis)

anova(r_ess5_andersen, r_ess5_lewis)


##################################
# Exporto tablas resultados

stargazer(r_ess2_andersen, title="",
          align=F, dep.var.labels=c("Estimated beta and Standard Error"),
          no.space=F, type="html", out="regression_andersen_r2.html")

stargazer(r_ess2_lewis, title="",
          align=F, dep.var.labels=c("Estimated beta and Standard Error"),
          no.space=F, type="html", out="regression_lewis_r2.html")


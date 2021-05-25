# Here is a resource for grabbing the CES2019 data:
# https://awstringer1.github.io/sta238-book/section-short
# -tutorial-on-pulling-data-for-assignment-1.html#section-
# canadian-election-study                          

library(tidyverse)
library(janitor)

# In this code you can load in and clean the survey data 

# First, if you don't already have it, install the devtools package:
# install.packages("devtools")

# get the dataframe
survey_data <- read_csv("ces2019-phone_clean.csv")


# find the required cols
# q3 -> gender/sex? 1 Male, 2 Female
# q4 -> provience  (1) Newfoundland and Labrador, (2) Prince Edward Island, (3) Nova Scotia, (4) New Brunswick, (5) Quebec, (6) Ontario,
        # (7) Manitoba, (8) Saskatchewan, (9) Alberta, (10) British Columbia, 
                                    # (11) Northwest Territories, (12) Yukon, (13) Nunavut
# age -> q2(2019 -age)? 
# total_children -> NA
# feelings_life -> q6
# pop_center -> NA
#aboriginal -> NA
#vis_minority -> NA
# citizenship_status -> q1 [filter out]
#education -> q61 
#living_arrangement -> NA
#average_hours_worked, -> NA
#regilion_importance -> q63
# income_family -> q69
#occupation -> q52
# political_pref -> q11



# choosing the above selected cols
survey_data.1 <- select(survey_data, 25:29,93,140,84,38, 245, 247)#, 96)
# renaming the cols for ease
survey_data.2 <- survey_data.1 %>% 
  clean_names() %>%
  rename(born = q2,
         feelings_life = q6,
         citizenship_status = q1,
         education = q61, #  regilion_importance = q63, -> 96
         religion_participation = p54,
         income = q69,
         occupation = p52,
         political_pref = q11,
         sex = q3,
         province = q4
         )

# removing all the NA values form the data set
survey_data.3 <- na.omit(survey_data.2)

# calculating the age of the participants
survey_data.4 <- survey_data.3  %>% mutate(age= 2019-born)
  # removing the born col
survey_data.4 <- select(survey_data.4, -2)



## adding the tags to the categorical variables


### EDUCATION

# removing the people who answered Don't know(-9) OR Refused(-8) OR Skipped(-7)
survey_data.5 <- survey_data.4[!grepl(-8, survey_data.4$education), ]
survey_data.5 <- survey_data.5[!grepl(-9, survey_data.5$education), ]

## grouping the categories into lesser categories

# first change the data type from dbl to chr
survey_data.5$education <- as.character(survey_data.5$education)

# Less than high school diploma or its equivalent = (1) No schooling + (2) Some elementary school +
            # (3) Completed elementary school +  (4) Some secondary / high school
survey_data.5$education <- replace(survey_data.5$education, survey_data.5$education == "1", "Less than high school diploma or its equivalent")
survey_data.5$education <- replace(survey_data.5$education, survey_data.5$education == "2", "Less than high school diploma or its equivalent")
survey_data.5$education <- replace(survey_data.5$education, survey_data.5$education == "3", "Less than high school diploma or its equivalent")
survey_data.5$education <- replace(survey_data.5$education, survey_data.5$education == "4", "Less than high school diploma or its equivalent")

# High school diploma or a high school equivalency certificate = (5) Completed secondary / high school +
          # (6) Some technical, community college, CEGEP, College Classi  [as it was not completed]
survey_data.5$education <- replace(survey_data.5$education, survey_data.5$education == "5", "High school diploma or a high school equivalency certificate")
survey_data.5$education <- replace(survey_data.5$education, survey_data.5$education == "6", "High school diploma or a high school equivalency certificate")

# College, CEGEP, trade certificate or other non-university certificate or di... = (7) Completed technical, community college, CEGEP, College C +
  # (6) Some technical, community college, CEGEP, College Classi
survey_data.5$education <- replace(survey_data.5$education, survey_data.5$education == "7", "College, CEGEP, trade certificate or other non-university certificate or di...")
survey_data.5$education <- replace(survey_data.5$education, survey_data.5$education == "6", "College, CEGEP, trade certificate or other non-university certificate or di...")

# University certificate or diploma below the bachelor's level = (8) Some university
survey_data.5$education <- replace(survey_data.5$education, survey_data.5$education == "8", "University certificate or diploma below the bachelor's level")

# Bachelor's degree (e.g. B.A., B.Sc., LL.B.) = (9) Bachelor's degree
survey_data.5$education <- replace(survey_data.5$education, survey_data.5$education == "9", "Bachelor's degree (e.g. B.A., B.Sc., LL.B.)")

# University certificate, diploma or degree above the bach... = (10) Master's degree + (11) Professional degree or doctorate
survey_data.5$education <- replace(survey_data.5$education, survey_data.5$education == "10", "University certificate, diploma or degree above the bach...")
survey_data.5$education <- replace(survey_data.5$education, survey_data.5$education == "11", "University certificate, diploma or degree above the bach...")


## Canadian Citizen
  #because everyone is a Canadian citizen hence we dont do anything and can just remove that col
survey_data.5 <- select(survey_data.5, -1)



## Gender/Sex

# removing the people who answered Don't know(-9) OR Refused(-8) OR Skipped(-7) OR Other(3) <- as not in dataset
survey_data.5 <- survey_data.5[!grepl(-7, survey_data.5$sex), ]
survey_data.5 <- survey_data.5[!grepl(-8, survey_data.5$sex), ]
survey_data.5 <- survey_data.5[!grepl(-9, survey_data.5$sex), ]
survey_data.5 <- survey_data.5[!grepl(3, survey_data.5$sex), ]

## first change the data type from dbl to chr, to assign chr values
survey_data.5$sex <- as.character(survey_data.5$sex)

# Assigning 1 Male, 2 Female
survey_data.5$sex <- replace(survey_data.5$sex, survey_data.5$sex == "1", "Male")
survey_data.5$sex <- replace(survey_data.5$sex, survey_data.5$sex == "2", "Female")



## Province

# removing the people who answered Don't know(-9) OR Refused(-8) OR Skipped(-7)
survey_data.5 <- survey_data.5[!grepl(-7, survey_data.5$province), ]
survey_data.5 <- survey_data.5[!grepl(-8, survey_data.5$province), ]
survey_data.5 <- survey_data.5[!grepl(-9, survey_data.5$province), ]

## first change the data type from dbl to chr, to assign chr values
survey_data.5$province <- as.character(survey_data.5$province)

# (1) Newfoundland and Labrador
survey_data.5$province <- replace(survey_data.5$province, survey_data.5$province == "1", "Newfoundland and Labrador")
#(2) Prince Edward Island
survey_data.5$province <- replace(survey_data.5$province, survey_data.5$province == "2", "Prince Edward Island")
#(3) Nova Scotia
survey_data.5$province <- replace(survey_data.5$province, survey_data.5$province == "3", "Nova Scotia")
#(4) New Brunswick
survey_data.5$province <- replace(survey_data.5$province, survey_data.5$province == "4", "New Brunswick")
#(5) Quebec
survey_data.5$province <- replace(survey_data.5$province, survey_data.5$province == "5", "Quebec")
#(6) Ontario
survey_data.5$province <- replace(survey_data.5$province, survey_data.5$province == "6", "Ontario")
#(7) Manitoba
survey_data.5$province <- replace(survey_data.5$province, survey_data.5$province == "7", "Manitoba")
#(8) Saskatchewan
survey_data.5$province <- replace(survey_data.5$province, survey_data.5$province == "8", "Saskatchewan")
#(9) Alberta
survey_data.5$province <- replace(survey_data.5$province, survey_data.5$province == "9", "Alberta")
#(10) British Columbia
survey_data.5$province <- replace(survey_data.5$province, survey_data.5$province == "10", "British Columbia")



## Religion Participation)     ##------New TAB--------##

# removing the people who answered Don't know(-9) OR Refused(-8) OR Skipped(-7)
survey_data.5 <- survey_data.5[!grepl(-7, survey_data.5$religion_participation), ]
survey_data.5 <- survey_data.5[!grepl(-8, survey_data.5$religion_participation), ]
survey_data.5 <- survey_data.5[!grepl(-9, survey_data.5$religion_participation), ]

## first change the data type from dbl to chr, to assign chr values
survey_data.5$religion_participation <- as.character(survey_data.5$religion_participation)

# Not at all = (1) Never
survey_data.5$religion_participation <- replace(survey_data.5$religion_participation, survey_data.5$religion_participation == "1", "Not at all")
# At least once a year = (2) Once a year + (3) Two to eleven times a year
survey_data.5$religion_participation <- replace(survey_data.5$religion_participation, survey_data.5$religion_participation == "2", "At least once a year")
survey_data.5$religion_participation <- replace(survey_data.5$religion_participation, survey_data.5$religion_participation == "3", "At least once a year")
# At least once a month = (4) Once a month + (5) Two or more times a month
survey_data.5$religion_participation <- replace(survey_data.5$religion_participation, survey_data.5$religion_participation == "4", "At least once a month")
survey_data.5$religion_participation <- replace(survey_data.5$religion_participation, survey_data.5$religion_participation == "5", "At least once a month")
# At least once a week = (6) Once a week + (7) More than once a week
survey_data.5$religion_participation <- replace(survey_data.5$religion_participation, survey_data.5$religion_participation == "6", "At least once a week")
survey_data.5$religion_participation <- replace(survey_data.5$religion_participation, survey_data.5$religion_participation == "7", "At least once a week")



## Remove occupation as can't filter out according to the census data
survey_data.6 <- select(survey_data.5, -6, -8)




## Political Preference 

# removing the people who answered Don't know(-9) OR Refused(-8) OR Skipped(-7) OR (9) None of these OR (10) Will spoil ballet
survey_data.6 <- survey_data.6[!grepl(-7, survey_data.6$political_pref), ]
survey_data.6 <- survey_data.6[!grepl(-8, survey_data.6$political_pref), ]
survey_data.6 <- survey_data.6[!grepl(-9, survey_data.6$political_pref), ]
survey_data.6 <- survey_data.6[!grepl(9, survey_data.6$political_pref), ]

## first change the data type from dbl to chr, to assign chr values
survey_data.6$political_pref <- as.character(survey_data.6$political_pref)

#(1) Liberal (Grits)
survey_data.6$political_pref <- replace(survey_data.6$political_pref, survey_data.6$political_pref == "1", "Liberal (Grits)")
#(2) Conservatives (Tory, PCs, Conservative Party of Canada)
survey_data.6$political_pref <- replace(survey_data.6$political_pref, survey_data.6$political_pref == "2", "Conservatives (Tory, PCs, Conservative Party of Canada)")
#(3) NDP (New Democratic Party, New Democrats, NDPers)
survey_data.6$political_pref <- replace(survey_data.6$political_pref, survey_data.6$political_pref == "3", "NDP (New Democratic Party, New Democrats, NDPers)")
#(4) Bloc Québécois (BQ, PQ, Bloc, Parti Québéc)
survey_data.6$political_pref <- replace(survey_data.6$political_pref, survey_data.6$political_pref == "4", "Bloc Québécois (BQ, PQ, Bloc, Parti Québécois)")
#(5) Green Party (Greens)
survey_data.6$political_pref <- replace(survey_data.6$political_pref, survey_data.6$political_pref == "5", "Green Party (Greens)")
#(6) People's Party
survey_data.6$political_pref <- replace(survey_data.6$political_pref, survey_data.6$political_pref == "6", "People's Party")
#(7) Other
survey_data.6$political_pref <- replace(survey_data.6$political_pref, survey_data.6$political_pref == "7", "Other")
#(8) Will not vote
survey_data.6$political_pref <- replace(survey_data.6$political_pref, survey_data.6$political_pref == "8", "Will not vote")
#(9) None of these
survey_data.6$political_pref <- replace(survey_data.6$political_pref, survey_data.6$political_pref == "9", "None of these")
#(10) Will spoil ballet
survey_data.6$political_pref <- replace(survey_data.6$political_pref, survey_data.6$political_pref == "10", "Will spoil ballet")





## INCOME Family

# removing the people who answered Don't know(-9) OR Refused(-8) OR Skipped(-7) OR (9) None of these OR (10) Will spoil ballet
survey_data.7 <- survey_data.6[!grepl(-7, survey_data.6$income), ]
survey_data.7 <- survey_data.7[!grepl(-8, survey_data.7$income), ]
survey_data.7 <- survey_data.7[!grepl(-9, survey_data.7$income), ]

# making a new col for income level
survey_data.7 <- survey_data.7 %>%
  mutate(income_family = case_when(survey_data.7$income < 25000 ~ "Less than $25,000",
                                   survey_data.7$income <= 49999 ~ "$25,000 to $49,999",
                                   survey_data.7$income <= 74999 ~ "$50,000 to $74,999",
                                   survey_data.7$income <= 99999 ~ "$75,000 to $99,999",
                                   survey_data.7$income <= 124999 ~ "$100,000 to $ 124,999",
                                   124999 <  survey_data.7$income ~ "$125,000 and more"))

#now remove the income col as no longer required
survey_data.7a <- select(survey_data.7, -5) 



## Feeling Life

# removing the people who answered Don't know(-9) OR Refused(-8) OR Skipped(-7)
survey_data.8 <- survey_data.7a[!grepl(-7, survey_data.7a$feelings_life), ]
survey_data.8 <- survey_data.8[!grepl(-8, survey_data.8$feelings_life), ]
survey_data.8 <- survey_data.8[!grepl(-9, survey_data.8$feelings_life), ]

## first change the data type from dbl to chr, to assign chr values
survey_data.8 <- survey_data.8 %>%
  mutate(feelings_life = case_when(survey_data.8$feelings_life == 1 ~ "Very satisfied",
                                   survey_data.8$feelings_life == 2 ~ "Fairly satisfied",
                                   survey_data.8$feelings_life == 3 ~ "Not very satisfied",
                                   survey_data.8$feelings_life == 4 ~ "Not satisfied at all"))






write_csv(survey_data.8, "ces2019_clean.csv")

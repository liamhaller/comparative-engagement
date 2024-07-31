#Data cleaning

library(haven)
library(tidyverse)
library(purrr)
library(stringr)

source('functions.R')

# Load Engagement / preprocessing ---------------------------------------------------------

dta <- read_dta('engagement.dta')

#political and engaged persons motivation questions were coded as two separate questions
#here we combine them into a single question by coalescing them
dta <- dta %>%
  mutate(mot1 = coalesce(mot1, mota1)) %>% 
  mutate(mot2 = coalesce(mot2, mota2)) %>% 
  mutate(mot3 = coalesce(mot3, mota3)) %>% 
  mutate(mot4 = coalesce(mot4, mota4)) %>% 
  mutate(mot5 = coalesce(mot5, mota5)) %>% 
  mutate(mot6 = coalesce(mot6, mota6)) %>% 
  mutate(mot7 = coalesce(mot7, mota7)) %>% 
  mutate(mot8 = coalesce(mot8, mota8)) %>% 
  mutate(mot9 = coalesce(mot9, mota9)) %>% 
  mutate(mot10 = coalesce(mot10, mota10)) %>% 
  mutate(mot11 = coalesce(mot11, mota11)) %>% 
  mutate(mot12 = coalesce(mot12, mota12)) %>% 
  mutate(mot13 = coalesce(mot13, mota13)) %>% 
  mutate(mot14 = coalesce(mot14, mota14)) 
  

#Selecting demographic groups
eng <- dta %>% 
  select(#Demographics
          age_o, gdr, brf, bld, hhg_o ,hek, ppf,#plz_o
          
          #Motivations
          mot1, mot2, mot3, mot4, mot5, mot7, 
          mot11, mot12,
         
         #engagement
         polx, tkt6,
         
         #state engagement 
         eus1r1c1
         
         ) %>% 
  rename (#demographics groups
          dg_age = age_o , dg_gender = gdr, dg_employment = brf, dg_school = bld, dg_hhsize = hhg_o,
          dg_income = hek,  dg_party = ppf, #dg_plz = plz_o,
          
          #motivations
          m_helpsociety = mot1, m_improvecoexistence =  mot2, m_learnsth =  mot3, 
          m_democracy = mot11, m_religious = mot12, m_giveback = mot4, m_helpothers = mot5,
          m_helprefugees = mot7,
          
          #engagement
          political = polx, hosted = tkt6,
      
          #state engagement 
          st_eng_c = eus1r1c1
          ) 


#standardize demographic variables
eng <- eng %>% 
  mutate(dg_age = 2023 - dg_age) %>% 
  mutate(dg_gender = case_when(dg_gender == 1 ~ "F",
                               dg_gender == 2 ~ "M",
                               dg_gender == 3 ~ "D",
                               dg_gender == 4 ~ NA_character_,
                               dg_gender == 5 ~ "A99")) %>% 
  mutate(dg_employment = case_when(dg_employment == 1 ~ "paid employment",
                                   dg_employment == 2 ~ "paid employment",
                                   dg_employment == 3 ~ "retired",
                                   dg_employment == 4 ~ "training",
                                   dg_employment == 5 ~ "paid employment", #maternity leave 
                                   dg_employment == 6 ~ "unemployed",
                                   dg_employment == 7 ~ "unemployed",
                                   dg_employment == 8 ~ "paid employment", #marginally employed
                                   dg_employment == 9 ~ "unemployed", #one euro job
                                   dg_employment == 10 ~ "paid employment", #irregularly employed
                                   dg_employment == 11 ~ "training", 
                                   dg_employment == 12 ~ "military", 
                                   dg_employment == 13 ~ "volunteer", 
                                   TRUE ~ NA_character_)) %>% 
  mutate(dg_school = case_when(dg_school == 1 ~ "no graduation",
                               dg_school == 2 ~ "secondary school",
                               dg_school == 3 ~ "realschul",
                               dg_school == 4 ~ "advanced technical degree",
                               dg_school == 5 ~ "abitur", #maternity leave 
                               dg_school == 6 ~ NA_character_, #foreign school
                               dg_school == 7 ~ NA_character_, #other
                               dg_school == 8 ~ NA_character_, #still a student
                               dg_school == 9 ~ NA_character_, #don't know
                               dg_school == 10 ~ NA_character_, #not specififed
                               TRUE ~ NA_character_)) %>% 
  mutate(dg_income = case_when(dg_income == 7 ~ NA_integer_,
                               dg_income == 8 ~ NA_integer_,
                               TRUE ~ dg_income)) %>% 
  mutate(dg_party = case_when(dg_party == 1 ~ "SPD", 
                              dg_party == 2 ~ "CDU",
                              dg_party == 3 ~ "die grünen",
                              dg_party == 4 ~ "FDP",
                              dg_party == 5 ~ "AfD",
                              dg_party == 6 ~ "die linke",
                              dg_party == 7 ~ "other",
                              TRUE ~ NA_character_)) %>% 
         mutate(st_eng = case_when(st_eng_c <= 20 ~ as.numeric(5), 
                                   st_eng_c > 20 & st_eng_c <= 40 ~ as.numeric(4), 
                                   st_eng_c > 40 & st_eng_c <= 60 ~ as.numeric(3), 
                                   st_eng_c > 60 & st_eng_c <= 80  ~ as.numeric(2), 
                                   st_eng_c > 80 ~ as.numeric(1), 
                                     TRUE ~ NA_integer_))
  
  
#standardize motivation levels
eng <- eng %>% 
  mutate(
    across(
      starts_with('m_'), ~case_when(
      .x == 6 ~ NA_integer_, 
      .x == 7 ~ 99,
      TRUE ~ .x
      )
    )
  )  
    

eng$dataset <- "eng"

# Private accommodation ----------------------------------------------------


#Load dataset from csv file
ukr <- read.csv('private_accommodation.csv')

pa <- ukr %>% 
  select(
    #Demographics
    age, gender, iplberuf, school,hhsize ,einkommen,partei,#plz
    
    #Motivations
    engawhy.SQ001., engawhy.SQ002., engawhy.SQ003., engawhy.SQ005., engawhy.SQ006.,
    engawhy.SQ007., engawhy.SQ008., 
    engawhy.SQ009.,
    
    #role of the state
    stateeval.SQ03.,
    
    #engagement
    untfreq
    
  ) %>% 
  rename (#demographics groups
    dg_age = age , dg_gender = gender, dg_employment = iplberuf, dg_school = school, 
    dg_hhsize = hhsize, dg_income = einkommen,  dg_party = partei, #dg_plz = plz,
    
    
    #motivations
    m_helpsociety = engawhy.SQ001., m_improvecoexistence =  engawhy.SQ002., 
    m_learnsth =  engawhy.SQ003., m_giveback = engawhy.SQ007., m_helpothers = engawhy.SQ008.,
    m_helprefugees = engawhy.SQ009., m_democracy = engawhy.SQ005., m_religious = engawhy.SQ006., 
    
    #engagement
    political = untfreq,
    
    #state engagement
    st_eng = stateeval.SQ03.
    
    
  ) 


pa <- pa %>% 
  mutate(st_eng_c = case_when(st_eng == 5 ~ as.numeric(10), 
                              st_eng == 4 ~ as.numeric(30), 
                              st_eng == 3 ~ as.numeric(50), 
                              st_eng == 2  ~ as.numeric(70), 
                              st_eng == 1  ~ as.numeric(90), 
                              TRUE ~ NA_integer_)) %>% 
  mutate(dg_employment = case_when(dg_employment == "A1" ~ "paid employment",
                                   dg_employment == "A2" ~ "training",
                                   dg_employment == "A3" ~ "unemployed",
                                   dg_employment == "A4" ~ "unemployed",
                                   dg_employment == "A5" ~ "unemployed",
                                   dg_employment == "A6" ~ "retired",
                                   dg_employment == "A7" ~ "military",
                                   dg_employment == "A8" ~ "volunteer",
                                   dg_employment == "A9" ~ "unemployed", #unpaid housework 
                                   TRUE ~ NA_character_ )) %>% 
  mutate(dg_school = case_when(dg_school == "A1" ~ "no graduation",
                               dg_school == "A2" ~ "secondary school",
                               dg_school == "A3" ~ "realschul",
                               dg_school == "A4" ~ "advanced technical degree",
                               dg_school == "A5" ~ "abitur",
                               TRUE ~ NA_character_ )) %>% 
  mutate(dg_income = case_when(dg_income == "A1" ~ as.numeric('1'), #under 1000
                               dg_income == "A2" ~ as.numeric('2'),
                               dg_income == "A3" ~ as.numeric('3'),
                               dg_income == "A4" ~ as.numeric('4'),
                               dg_income == "A5" ~ as.numeric('5'),
                               dg_income == "A6" ~ as.numeric('6'), #over 5000
                               dg_income == "A99" ~ NA_integer_)) %>% 
  mutate(dg_party = case_when(dg_party == "A1" ~ "SPD",
                              dg_party == "A2" ~ "CDU",
                              dg_party == "A3" ~ "die grünen",
                              dg_party == "A4" ~ "FDP",
                              dg_party == "A5" ~ "die linke",
                              dg_party == "A5" ~ "AfD",
                              dg_party == "-oth-" ~ "other",
                              TRUE ~ NA_character_ )) %>% 
 # mutate(dg_plz = as.character(dg_plz)) %>% 
  filter(political >= 1) 

#none of the pa respondents are only political since they all hosted (after filtering for that)
pa$political <- 0
pa$hosted <- 1
pa$dataset <- "pa"



# Merge datasets  --------------------------------------------------------

compeng <- bind_rows(eng, pa)
#remove intermediary datasets
rm(list = c('dta', 'eng', 'pa', 'ukr'))

#Remove unrealistic/human error free inputs
compeng$dg_hhsize <- ifelse(compeng$dg_hhsize > 10, yes = NA_integer_, compeng$dg_hhsize)

#pa dataset is only those who have hosted, 
compeng <- compeng %>% 
                mutate(hosted = case_when(political == 1 ~ "political",
                                          political == 0 & hosted == 0 ~ "Engaged, no host",
                                          political == 0 & hosted == 1 ~ "Engaged, host",
                                           TRUE ~ NA_character_ )) 


# Column-level changes ----------------------------------------------------


#make strongly agree 5, strongly disagree 1 for motivation questions 
compeng <- Zreplace(dataframe = compeng, columns_to_replace = 8:15, old_text = c(1,2,3,4,5), new_text = c(5,4,3,2,1))


#Set engagement variable to be binary
compeng$political <- factor(compeng$political, levels = c(0,1))

#make gender a factor for modeling, and remove D given rarity
compeng$dg_gender <- ifelse(compeng$dg_gender == "D", NA_character_, compeng$dg_gender)
compeng$dg_gender <- ifelse(compeng$dg_gender == "A99", NA_character_, compeng$dg_gender)

#Set factors
compeng$dg_gender <- as.factor(compeng$dg_gender)
compeng$dg_party <- as.factor(compeng$dg_party)

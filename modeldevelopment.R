#model development
library(tidyverse)
library(rethinking) #From Richard McElreath's book Statistical Rethinking
library(pROC)
library(boot) #LOOCV
library(broom)

#Data is imported from dataloading file
source('dataloading.R')



# Data Selection and Standardization  --------------------------------------------------------------

map_dbl(compeng, function(x) sum(is.na(x)))



#standardize Numeric variables
compeng_std <- compeng %>% 
  mutate(dg_age = standardize(dg_age)) %>% 
  mutate(m_democracy = standardize(m_democracy)) %>% 
  mutate(m_religious = standardize(m_religious)) %>% 
  mutate(m_giveback = standardize(m_giveback)) %>% 
  mutate(m_helpothers = standardize(m_helpothers)) %>% 
  mutate(m_learnsth = standardize(m_learnsth)) %>% 
  mutate(m_improvecoexistence = standardize(m_improvecoexistence)) %>% 
  mutate(m_helpsociety = standardize(m_helpsociety)) %>% 
  mutate(m_helprefugees = standardize(m_helprefugees))
  

#Convert characters to factors
compeng_std <- compeng_std %>% 
                  mutate(dg_school = case_when(dg_school == "no graduation" ~ as.numeric('1'),
                             dg_school == "secondary school" ~ as.numeric('2'),
                             dg_school == "realschul" ~ as.numeric('3'),
                             dg_school == "advanced technical degree" ~ as.numeric('4'),
                             dg_school == "abitur" ~ as.numeric('5'))) %>% 
                  mutate(dg_school = standardize(dg_school))

compeng_std$genderno <- ifelse(compeng_std$dg_gender == 'M', 1, 0)


compeng_std <- compeng_std[complete.cases(compeng_std),]


compeng_std$political

compeng_std %>%
  summarise_if(is.numeric, mean)



# Question 1: Model Creation ----------------------------------------------------------

#Question 1: How does motivation influence individuals' choices of engagement type?  

set.seed(514)
#Split data in test and train for evaluation
ids.compeng = sample(nrow(compeng_std), .9*NROW(compeng_std))
data.train = compeng_std[ids.compeng, ]
data.test = compeng_std[-ids.compeng, ]


#Regression with training dataset "data.train", that is used to later validate the model
engagment.motivation_full_test <- glm(political ~ dg_age  + dg_gender + dg_school + 
                              m_democracy + m_religious + 
                              m_giveback + m_helpsociety +
                              m_helpothers + m_helprefugees  +
                              m_learnsth + m_improvecoexistence, 
                             family = 'binomial', 
                             na.action = na.exclude,
                             data = data.train)

#Regression with full, standardized dataset "compeng_std"
engagment.motivation_full <- glm(political ~ dg_age  + dg_gender + dg_school + 
                                        m_democracy + m_religious + 
                                        m_giveback + m_helpsociety +
                                        m_helpothers + m_helprefugees  +
                                        m_learnsth + m_improvecoexistence, 
                                      family = 'binomial', 
                                      na.action = na.exclude,
                                      data = compeng_std)


# Question 1: Model Results -----------------------------------------------




#Model summary
summary(engagment.motivation_full)

#true point estimates

#Demographic
plogis(engagment.motivation_full$coefficients[5])

#Religious
plogis(engagment.motivation_full$coefficients[6])


plogis(-1.3)


##averaged over full distribtuion
b <- coef(engagment.motivation_full)
hi <- 2
lo <- -2


delta.democratic <- plogis(b[1] + b[2]*compeng_std$dg_age + b[3]*compeng_std$genderno + 
                             b[4]*compeng_std$dg_school + b[5]*hi +
                             b[6]*compeng_std$m_religious + b[7]*compeng_std$m_giveback +
                             b[8]*compeng_std$m_helpsociety + b[9]*compeng_std$m_helpothers +
                             b[10]*compeng_std$m_helprefugees + b[11]*compeng_std$m_learnsth +
                             b[12]*compeng_std$m_improvecoexistence) -
  plogis(b[1] + b[2]*compeng_std$dg_age + b[3]*compeng_std$genderno + 
           b[4]*compeng_std$dg_school + b[5]*lo +
           b[6]*compeng_std$m_religious + b[7]*compeng_std$m_giveback +
           b[8]*compeng_std$m_helpsociety + b[9]*compeng_std$m_helpothers +
           b[10]*compeng_std$m_helprefugees + b[11]*compeng_std$m_learnsth +
           b[12]*compeng_std$m_improvecoexistence)

round(mean(delta.democratic), 2)


delta.religion <- plogis(b[1] + b[2]*compeng_std$dg_age + b[3]*compeng_std$genderno + 
                           b[4]*compeng_std$dg_school + b[5]*compeng_std$m_democracy +
                           b[6]*hi + b[7]*compeng_std$m_giveback +
                           b[8]*compeng_std$m_helpsociety + b[9]*compeng_std$m_helpothers +
                           b[10]*compeng_std$m_helprefugees + b[11]*compeng_std$m_learnsth +
                           b[12]*compeng_std$m_improvecoexistence) -
  plogis(b[1] + b[2]*compeng_std$dg_age + b[3]*compeng_std$genderno + 
           b[4]*compeng_std$dg_school + b[5]*compeng_std$m_democracy +
           b[6]*lo + b[7]*compeng_std$m_giveback +
           b[8]*compeng_std$m_helpsociety + b[9]*compeng_std$m_helpothers +
           b[10]*compeng_std$m_helprefugees + b[11]*compeng_std$m_learnsth +
           b[12]*compeng_std$m_improvecoexistence)

round(mean(delta.religion), 2)



  

# Question 1: Model Validation -------------------------------------------------------

test_prob <- predict(engagment.motivation_full_test, newdata = data.test, type = "response")
test_roc <- roc(data.test$political ~ test_prob, plot = TRUE, print.auc = TRUE)


#Brier
pred <- predict(engagment.motivation_full, type = "response")
y <- ifelse(compeng_std$political == 0, 0,1)
brierScore <- mean((pred-y)^2)

#Leave one out cross validation 
null_model <-  glm(political ~ 1, 
                   family = 'binomial', 
                   na.action = na.exclude,
                   data = compeng_std)

cv.null <- cv.glm(compeng_std,null_model)
cv.null$delta

cv.test <- cv.glm(compeng_std, engagment.motivation_full)
cv.test$delta


#Model 1: Robustness checks, removing hosts
no_pa <- compeng_std %>% filter(hosted != "Engaged, host")

engagment.motivation_robust <- glm(political ~ dg_age  + dg_gender + dg_school + 
                                     m_democracy + m_religious + 
                                     m_giveback + m_helpsociety +
                                     m_helpothers + m_helprefugees  +
                                     m_learnsth + m_improvecoexistence, 
                                   family = 'binomial', 
                                   na.action = na.exclude,
                                   data = no_pa)




# Question 2: Model Creation ----------------------------------------------

#Research Question 2:
#To what extent does type of engagement influence volunteers’ perspective on the role of the state? 
  
eng <- compeng  %>% 
  #dplyr::filter(dataset == 'eng') %>% 
  mutate(dg_school = case_when(dg_school == "no graduation" ~ as.numeric('1'),
                               dg_school == "secondary school" ~ as.numeric('2'),
                               dg_school == "realschul" ~ as.numeric('3'),
                               dg_school == "advanced technical degree" ~ as.numeric('4'),
                               dg_school == "abitur" ~ as.numeric('5'))) %>% 
  mutate(dg_school = rethinking::standardize(dg_school)) %>% 
  select(st_eng_c, dg_age, political, dg_gender, dg_school, dg_party) 

lm.eng <- lm(st_eng_c ~ dg_age + political + dg_gender + dg_school + dg_party, data = eng) 





# Question 2: Robustness checks ------------------------------------

nopa_state <- compeng_std %>% filter(hosted != "Engaged, host") %>% 
  select(st_eng_c, dg_age, political, dg_gender, dg_school, dg_party)


lm.eng_nopa <- lm(st_eng_c ~ dg_age + political + dg_gender + dg_school + dg_party, data = nopa_state) 

  

nopa_state_robust <- compeng_std %>% 
  select(st_eng, dg_age, political, dg_gender, dg_school, dg_party)


lm.eng_nopa_robust <- lm(st_eng ~ dg_age + political + dg_gender + dg_school + dg_party, data = nopa_state_robust) 


# 
# tidy(lm.eng_nopa_robust) %>% 
#   select(term, estimate, std.error) %>% 
#   ggplot(aes(x= factor(term), y=estimate)) +
#   geom_point(size = 2) +
#   geom_errorbar(aes(ymin=estimate - 1.96*std.error ,ymax=estimate + 1.96*std.error),width=0, linewidth =1) +
#   
#   scale_x_discrete(limits = rev(c('dg_age', 'political1', 'dg_genderM',
#                                   'dg_school', 'dg_partyCDU', 'dg_partydie grünen', 'dg_partydie linke', 'dg_partyFDP', 'dg_partyother', 'dg_partySPD')),
#                    labels = rev(c('Age', 'Engagement (Political)',  'Gender (Male)', 
#                                   'School', 'Political party: CSU/CDU',
#                                   'Political party: Green',
#                                   'Political party: Die Linke',
#                                   'Political party: FDP',
#                                   'Political party: Other',
#                                   'Political party: SPD'))) +
#   
#   coord_flip() +
#   geom_hline(yintercept = 0, linetype="dashed", 
#              color = "pink", linewidth=1) + 
#   #shape of the graph
#   ylim(-5,5) +
#   #text
#   labs(subtitle = "",
#        title = "",
#        y = '',
#        x = "") +
#   #styling
#   #scale_color_manual() +
#   dezim::dezim_style() 
# 
# 
# 
# 

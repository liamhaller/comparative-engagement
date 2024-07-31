#figures
library(tidyverse)
library(dezim) #dezim package can be downloaded from devtools::install_github('liamhaller/dezim')
library(broom)
library(rethinking)
library(haven)
library(gtsummary)
library(purrr)


#Models are imported from modeldevelopment.R
source('modeldevelopment.R') #this file calls dataloading.R



# Table 1: Descriptive Statistics of Political and Socio-Demograph --------


# Function to calculate mean and sd for a given numeric column and political filter
calc_stats <- function(data, column, political_value) {
  mean_val <- data %>%
    filter(political == political_value) %>%
    pull(all_of(column)) %>%
    mean(., na.rm = TRUE)
  
  sd_val <- data %>%
    filter(political == political_value) %>%
    pull(all_of(column)) %>%
    sd(., na.rm = TRUE)
  
  list(mean = mean_val, sd = sd_val)
}

# Select all numeric columns
numeric_columns <- compeng %>% select(where(is.numeric)) %>% colnames()

# Calculate for political == 1
political_1_stats <- map_dfr(numeric_columns, ~{
  stats <- calc_stats(compeng, .x, 1)
  tibble(column = .x, operation = c("mean_political_1", "sd_political_1"), value = c(stats$mean, stats$sd))
})

# Calculate for political == 0
political_0_stats <- map_dfr(numeric_columns, ~{
  stats <- calc_stats(compeng, .x, 0)
  tibble(column = .x, operation = c("mean_political_0", "sd_political_0"), value = c(stats$mean, stats$sd))
})

# Combine results into one dataframe
results <- bind_rows(political_1_stats, political_0_stats)

# Display results
print(results, n = 52)

#income isn't actually a contiuous variable so here are the tables
compeng %>%
  filter(political == 1) %>%
  pull(dg_income) %>%
  table() %>% 
  prop.table()%>% 
  round(.,2)


compeng %>%
  filter(political == 0) %>%
  pull(dg_income) %>%
  table() %>% 
  prop.table() %>% 
  round(.,2)



# Function to calculate proportions for a factor/character columns and political filter
calc_proportions <- function(data, column, political_value) {
  prop_table <- data %>%
    filter(political == political_value) %>%
    pull(all_of(column)) %>%
    table() %>%
    prop.table()
  
  # Convert to a tibble for easier merging later
  prop_df <- as_tibble(prop_table, rownames = "category") %>%
    mutate(column = column, political = political_value)
  
  prop_df
}

# Select all factor and character columns
factor_char_columns <- compeng %>% select(where(is.factor) | where(is.character)) %>% colnames()

# Calculate for political == 1
political_1_props <- map_dfr(factor_char_columns, ~{
  calc_proportions(compeng, .x, 1)
})

# Calculate for political == 0
political_0_props <- map_dfr(factor_char_columns, ~{
  calc_proportions(compeng, .x, 0)
})

# Combine results into one dataframe
results_props <- bind_rows(political_1_props, political_0_props)

# Display results
print(results_props, n = 50)


# Figure 1: Mean coefficient values for logistic regression on the effect of motivations on type of engagement -----

figure1 <- tidy(engagment.motivation_full) %>% 
  filter(term != '(Intercept)') %>% 
  select(term, estimate, std.error) %>% 
  ggplot(aes(x= factor(term), y=estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin=estimate - 1.96*std.error ,ymax=estimate + 1.96*std.error),width=0, linewidth =1) +
  
  scale_x_discrete(limits = rev(c('dg_age', 'dg_genderM', 'dg_school',
                                  'm_democracy', 'm_religious', 
                                  'm_giveback', 'm_helpsociety', 
                                  'm_helpothers', 'm_helprefugees',
                                  'm_learnsth', 'm_improvecoexistence')),
                   labels = rev(c('Age', 'Gender (Male)',  'School', 
                                  'Motivation: Democracy', 'Motivation: Religion', 
                                  'Motivation: Give back', 'Motivation: Help society',
                              'Motivation: Help others', 'Motivation: Help refugees',
                              'Motivation: Learn something', "Motivation: Improve coexistence"))) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype="dashed", 
               color = "black", linewidth=1) + 
  #shape of the graph
  ylim(-2.5,2.5) +
  #text
  labs(subtitle = "",
       title = "",
       y = 'Likelihood to be politically engaged (standard deviations)',
       x = "",
       caption = "-2-0 = socially-humanitarian engagement; 0-2 = political engagement.") +
  dezim::theme_publish()


figure1




# Figure 2: Coefficient values from linear regression of type of engagement on role of the state ----------------------------


 figure2 <- tidy(lm.eng) %>% 
    select(term, estimate, std.error) %>% 
    ggplot(aes(x= factor(term), y=estimate)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin=estimate - 1.96*std.error ,ymax=estimate + 1.96*std.error),width=0, linewidth =1) +
    
    scale_x_discrete(limits = rev(c('dg_age', 'political1', 'dg_genderM',
                                    'dg_school', 'dg_partyCDU', 'dg_partydie grünen', 'dg_partydie linke', 'dg_partyFDP', 'dg_partyother', 'dg_partySPD')),
                     labels = rev(c('Age', 'Engagement (Political)',  'Gender (Male)', 
                                    'School', 'Political party: CSU/CDU',
                                    'Political party: Green',
                                    'Political party: Die Linke',
                                    'Political party: FDP',
                                    'Political party: Other',
                                    'Political party: SPD'))) +
    coord_flip() +
    geom_hline(yintercept = 0, linetype="dashed", 
               color = "black", linewidth=1) + 
    #shape of the graph
    ylim(-40,40) +
    #text
    labs(subtitle = "",
         title = "",
         y = '',
         x = "") +
    dezim::theme_publish() 
  
figure2 #intercept is not plotted





# Figure 3: Comparison of coefficient values from logistic regression on the effect of motivations on type of engagement --------

  # Tidy coefficients for engagment.motivation_full model
  coef_data1 <- tidy(engagment.motivation_full) %>% 
    select(term, estimate, std.error) %>% 
    mutate(model = "Full Sample") %>% 
    filter(term != '(Intercept)') 
    
  
  # Tidy coefficients for engagment.motivation_robust (without pa) model
  coef_data2 <- tidy(engagment.motivation_robust) %>% 
    select(term, estimate, std.error) %>% 
    mutate(model = "Without hosts") %>% 
    filter(term != '(Intercept)') 
    
  
  # Combine coefficients from both models
  combined_coef_data <- bind_rows(coef_data1, coef_data2)
  
  
  figure3 <- combined_coef_data %>% 
    ggplot(aes(x= factor(term), y=estimate, color = model, group = interaction(model, term))) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin=estimate - 1.96*std.error ,ymax=estimate + 1.96*std.error),width=0, linewidth =1) +
    
    scale_x_discrete(limits = rev(c('dg_age', 'dg_genderM', 'dg_school',
                                    'm_democracy', 'm_religious', 
                                    'm_giveback', 'm_helpsociety', 
                                    'm_helpothers', 'm_helprefugees',
                                    'm_learnsth', 'm_improvecoexistence')),
                     labels = rev(c('Age', 'Gender (Male)',  'School', 
                                    'Motivation: Democracy', 'Motivation: Religion', 
                                    'Motivation: Give back', 'Motivation: Help society',
                                    'Motivation: Help others', 'Motivation: Help refugees',
                                    'Motivation: Learn something', "Motivation: Improve coexistence"))) +
    
    coord_flip() +
    geom_hline(yintercept = 0, linetype="dashed", 
               color = "black", linewidth=1) + 
    #shape of the graph
    ylim(-2.5,2.5) +
    #text
    labs(subtitle = "",
         title = "",
         y = '',
         x = "",
         color = "") +
  ggplot2::scale_color_grey() +
        #styling
    dezim::theme_publish() 
  
  figure3
  
  


# Figure 4: Comparison of coefficient values from linear regression on effect of engagement type on view on role of the state -------------------------

  # Tidy coefficients for state perception model
  coef_data1 <- tidy(lm.eng) %>% 
    select(term, estimate, std.error) %>% 
    mutate(model = "Full Sample")  %>% 
    filter(term != '(Intercept)') 
  
  # Tidy coefficients for state perception without pa
  coef_data2 <- tidy(lm.eng_nopa) %>% 
    select(term, estimate, std.error) %>% 
    mutate(model = "Without hosts")  %>% 
    filter(term != '(Intercept)') 
  
  # Combine coefficients from both models
  combined_coef_data <- bind_rows(coef_data1, coef_data2)
  
  
  figure4 <- combined_coef_data %>% 
    ggplot(aes(x= factor(term), y=estimate, color = model, group = interaction(model, term))) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin=estimate - 1.96*std.error ,ymax=estimate + 1.96*std.error),width=0, linewidth =1) +
    
    scale_x_discrete(limits = rev(c('dg_age', 'political1', 'dg_genderM',
                                    'dg_school', 'dg_partyCDU', 'dg_partydie grünen', 'dg_partydie linke', 'dg_partyFDP', 'dg_partyother', 'dg_partySPD')),
                     labels = rev(c('Age', 'Engagement (Political)',  'Gender (Male)', 
                                    'School', 'Political party: CSU/CDU',
                                    'Political party: Green',
                                    'Political party: Die Linke',
                                    'Political party: FDP',
                                    'Political party: Other',
                                    'Political party: SPD'))) +
    
    coord_flip() +
    geom_hline(yintercept = 0, linetype="dashed", 
               color = "black", linewidth=1) + 
    #shape of the graph
    ylim(-50,50) +
    #text
    labs(subtitle = "",
         title = "",
         y = '',
         x = "",
         color = "") +
    ggplot2::scale_color_grey() +
    #styling
    dezim::theme_publish() 
  figure4
  
  
  #Appendix ------------------------------------------------------------
  
  # Table 1: Logistic regression: Effect of Motivation on type of engagement
  engagment.motivation_full %>% 
    tbl_regression(exponentiate = FALSE) %>% 
    as_flex_table()
  
  
  
  ##Table 2: Linear Regression: Effect of engagement on role of the state (Full dataset) 
  lm.eng %>% 
    tbl_regression(exponentiate = FALSE) %>% 
    as_flex_table()
  
  
  
  ##Table 3: Regression robustness check (imputation of independent variable): Effect of engagement on role of the state
  lm.eng_nopa_robust %>% 
  tbl_regression(exponentiate = FALSE) %>% 
    as_flex_table()
  
  


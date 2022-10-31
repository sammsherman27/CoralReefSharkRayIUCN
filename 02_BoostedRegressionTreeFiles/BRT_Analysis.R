# This script cleans the data, runs the boosted regression trees, and makes the figures
library(tidyverse)
library(countrycode)
library(reshape2) # make a correlation heatmap
library(PerformanceAnalytics) # to make a correltion matrix
library(xgboost) # run boosted regression tree
library(Matrix) # make a model matrix 
library(pdp) # partial dependence calculations
library(patchwork) # to plot the skewness of explanatory variables

# 1. CLEAN DATA -----------------------------------------------------------------------
# Clean covariates -------------------
# Coastal population
coast_pop <- 
  read_csv('../Data/CoralReef_EEZ_CoastalPop.csv') %>% 
  # Let's clean up the ISO column
  mutate(ISO3 = case_when(is.na(ISO_TER1) ~ ISO_SOV1,
                          TRUE ~ ISO_TER1)) %>% 
  # only care about two columns
  dplyr::select(ISO3, `_sum`) %>% 
  # one value per ISO3
  group_by(ISO3) %>% 
  summarise(coastal_pop = sum(`_sum`, na.rm = TRUE)) 

head(coast_pop)

# SST, primary productivity, and shelf area
sst <- 
  read_csv('../Data/20211008_chlora_sst_coralreef.csv') %>% 
  # Let's clean up the ISO column
  mutate(ISO3 = case_when(is.na(ISO_TER1) ~ ISO_SOV1,
                          TRUE ~ ISO_TER1)) %>% 
  dplyr::select(ISO3, AREA_KM2, SUM, MEDIAN) %>% 
  group_by(ISO3) %>% 
  summarise(ShelfArea_km2 = sum(AREA_KM2, na.rm = TRUE),
            Chla = sum(SUM, na.rm = TRUE),
            sst_med = median(MEDIAN, na.rm = TRUE))

head(sst)

# Gross domestic product
gdp <- 
  read_csv('../Data/GrossDomesticProduct.csv') %>% 
  dplyr::select(-Country) %>% 
  # take the median across the entire dataframe
  pivot_longer(-ISO3,
               names_to = 'year',
               values_to = 'gdp') %>% 
  group_by(ISO3) %>% 
  summarise(gdp = median(gdp, na.rm = TRUE))

head(gdp)

# World governance indicator
wgi <- 
  read_csv('../Data/WorldGovernanceIndicators.csv') %>% 
  dplyr::select(-Country) %>% 
  # median across years
  pivot_longer(-ISO3,
               names_to = 'year',
               values_to = 'wgi') %>% 
  group_by(ISO3) %>% 
  summarise(wgi = median(wgi, na.rm = TRUE))

head(wgi)

# Human development index 2019
hdi <- 
  read_csv('../Data/HumanDevelopmentIndex2019.csv') %>% 
  # add ISO3
  mutate(ISO3 = countrycode(Country, 'country.name', 'iso3c')) %>% 
  dplyr::select(-Country)

head(hdi)

# Marine protein consumption
protein <- 
  read_csv('../Data/MarineProteinConsumption.csv') %>% 
  dplyr::select(-Country) %>% 
  # median across years
  pivot_longer(-ISO3, 
               names_to = 'year',
               values_to = 'protein') %>% 
  group_by(ISO3) %>% 
  summarise(protein = median(protein, na.rm = TRUE))


head(protein)

# Catch, effort, CPUE
cpue <- 
  read_csv('../Data/SeaAroundUs_CatchEffortCPUE_Recalced.csv') %>% 
  # add ISO3
  mutate(ISO3 = countrycode(EEZ, 'country.name', 'iso3c')) %>% 
  #dplyr::filter(is.na(ISO3)) %>% 
  # manually infill countries without ISO
  mutate(ISO3 = case_when(EEZ == 'Chagos Archipelago (UK)' ~ 'IOT',
                          EEZ == 'Curacao (Netherlands)' ~ 'CUW',
                          EEZ == 'Guadeloupe (France)' ~ 'GLP',
                          EEZ == 'Guam (USA)' ~ 'GUM',
                          EEZ == 'Lord Howe Island' ~ 'AUS',
                          EEZ == 'Mayotte (France)' ~ 'MYT',
                          EEZ == 'New Caledonia (France)' ~ 'NCL',
                          EEZ == 'Niue (New Zealand)' ~ 'NIU',
                          EEZ == 'Northern Marianas (USA)' ~ 'MNP',
                          EEZ == 'Puerto Rico (USA)' ~ 'PRI',
                          EEZ == 'Reunion (France)' ~ 'REU',
                          EEZ == 'St Martin (France)' ~ 'MAF',
                          EEZ == 'Tokelau (New Zealand)' ~ 'TKL',
                          EEZ == 'US Virgin Isl.' ~ 'VIR',
                          EEZ == 'Wallis & Futuna Isl. (France)' ~ 'WLF',
                          EEZ == 'Yemen (Arabian Sea)' ~ 'YEM',
                          TRUE ~ ISO3)) %>% 
  # okay now let's take the mean of all values across years for each EEZ
  group_by(ISO3) %>% 
  summarise(catch = mean(Catch),
            effort = mean(Effort),
            cpue = mean(CPUE)) %>% 
  drop_na()

head(cpue)

# SPUE 
spue <- 
  read_csv('../Data/SharkRayFPInfo_BRT.csv') %>% 
  # replace British West Indies with Montserrat
  # and French West Indies with Martinique and Guadeloupe
  # turn Dutch Caribbean to Curacao
  mutate(location_name = case_when(location_name == 'British West Indies' ~ 'Montserrat',
                                   location_name == 'French West Indies' ~ 'Martinique',
                                   location_name == 'Dutch Caribbean' ~ 'Curacao',
                                   TRUE ~ location_name),
         # now let's add an integer to repeat the number of rows for Martinique
         # kinda janky but whatever
         rep_factor = case_when(location_name == 'Martinique' ~ 2,
                                TRUE ~ 1)) %>% 
  # add duplicate row
  uncount(rep_factor) %>% 
  # ok now I have two rows that are named identically - let's make them unique
  mutate(id = seq(1:nrow(.)),
         location_2 = paste(location_name, id, sep = '_'),
         #OK no we can specify two different locations
         location_name = case_when(location_2 == 'Martinique_18' ~ 'Martinique',
                                   location_2 == 'Martinique_19' ~ 'Guadeloupe',
                                   TRUE ~ location_name)) %>% 
  # now get rid of useless columns
  dplyr::select(-id, -location_2) %>% 
  # add ISO3 - fix up some of the territory names
  mutate(ISO3 = countrycode(location_name, 'country.name', 'iso3c')) %>% 
  # let's make a SharkRay column
  pivot_longer(c('spueshark', 'spueray'), names_to = 'SharkRay',
               values_to = 'spue') %>% 
  #relevel the variables
  mutate(SharkRay = dplyr::recode(SharkRay,
                                  'spueshark' = 'Shark',
                                  'spueray' = 'Ray')) %>% 
  dplyr::select(-location_name)

head(spue)

# Generate master dataframe ------------
all_data_raw <- 
  read_csv('../Data/SpeciesListWithCountries.csv') %>% 
  # add column of ISO3 codes
  mutate(ISO3 = countrycode(CountryName, 'country.name', 'iso3c'),
         # create a threat column
         threat = case_when(Status %in% c('CR', 'EN', 'VU') ~ 'threat',
                            TRUE ~ 'nothreat')) %>% 
  # want proportion threatened in each country for sharks and rays
  group_by(ISO3, SharkRay, threat) %>% 
  summarise(total = n()) %>% 
  pivot_wider(names_from = 'threat', values_from = 'total') %>% 
  replace(is.na(.), 0) %>% 
  mutate(total_spp = nothreat + threat,
         prop_threat = threat/total_spp) %>% 
  dplyr::select(ISO3, SharkRay, prop_threat) %>% 
  # Join covariate data
  left_join(., coast_pop, by = 'ISO3') %>% 
  left_join(., gdp, by = 'ISO3') %>% 
  left_join(., hdi, by = 'ISO3') %>% 
  left_join(., protein, by = 'ISO3') %>% 
  left_join(., wgi, by = 'ISO3') %>% 
  left_join(., sst, by = 'ISO3') %>% 
  left_join(., cpue, by = 'ISO3') %>% 
  left_join(., spue, by = c('ISO3', 'SharkRay')) %>% 
  ungroup() %>% 
  # now we'll remove the countries that don't have coral reefs:
  # New Zealand, Canada, Russia, Slovenia, UK, Albania, and Iceland
  dplyr::filter(!ISO3 %in% c('NZL', 'CAN', 'RUS',
                             'SVN', 'GBR', 'ALB', 'ISL'))

head(all_data_raw)

#write.csv(all_data_raw, '../Data/Full_data.csv', row.names = FALSE)

# Data processing --------------
# Let's check the collinearity among variables and their skewness

all_data <- 
  read_csv('../Data/Full_data.csv') %>% 
  mutate_at(vars('coastal_pop', 'gdp', 'protein', 'ShelfArea_km2',
                 'Chla', 'catch', 'effort', 'cpue', 'spue'), log1p)

# Let's check the skewness of all the data
pairs_skew <- 
  lapply(colnames(all_data_raw)[4:15], function(var_name) {
    
    the_df <- 
      all_data_raw %>% 
      dplyr::filter(SharkRay == 'Shark') %>% 
      dplyr::select(all_of(var_name)) %>% 
      dplyr::rename('variable_column' = 1)
    
    the_plot <- 
      the_df %>% 
      ggplot(aes(x = variable_column)) +
      geom_histogram(bins = 30) +
      labs(x = paste(var_name)) +
      theme_classic()
    
    return(the_plot)
    
  })

pairs_plots <- 
  pairs_skew [[1]] + pairs_skew [[2]] + pairs_skew [[3]] + pairs_skew [[4]] +
  pairs_skew [[5]] + pairs_skew [[6]] + pairs_skew [[7]] + pairs_skew [[8]] +
  pairs_skew [[9]] + pairs_skew [[10]] + pairs_skew [[11]] + pairs_skew [[12]] +
  plot_layout(ncol = 4)

pairs_plots

# So we'll want to log coastal population, gdp, protein consumption, shelf area
# chlorophyl-a, catch, effort, CPUE, and SPUE

# All correlation values are below 0.8, so we can move on with all the variables

# SHARKS ------------------------------------------------------------------------------
# 2. BOOSTED REGRESSION TREE ---------------------------------------------------------------
shark_data <- 
  dplyr::filter(all_data, SharkRay == 'Shark')

# Prep BRT objects
fmod <- formula(~ coastal_pop + gdp + HDI_2019 + protein + 
                  wgi + ShelfArea_km2 + sst_med + Chla + catch +
                  effort + cpue + spue)

# create model matrix
modmat <- 
  stats::model.matrix(fmod, model.frame(~ ., shark_data, na.action = na.pass))[, -1]

head(modmat)

# create a vector of response
labels <- shark_data$prop_threat

# Tune hyperparameters --------------------------
# Create a matrix of tuning variables
tune_grid_shark <- 
  expand.grid(eta = c(0.2, 0.5, 0.7), # learning rate
              gamma = c(0.2, 0.5, 0.7), # minimum loss reduction
              max_depth = c(5, 10, 15), # maximum tree depth
              subsample = c(0.3, 0.6, 0.9), # subsample ratio of the training instance
              rmse = NA) # empty column to infill

# Find best combination of hyperparameters
for(i in 1:nrow(tune_grid_shark)) {
  
  # create a list of parameters
  tune_params <- 
    list(eta = tune_grid_shark$eta,
         gamma = tune_grid_shark$gamma,
         max_depth = tune_grid_shark$max_depth,
         subsample = tune_grid_shark$subsample)
  
  # run the model with specific set of hyperparameers
  tune_brt <- 
    xgboost::xgboost(modmat, label = labels, nrounds = 150,
                     params = tune_params,
                     objective = 'reg:logistic',
                     verbose = 0)
  
  tune_grid_shark$rmse[i] <- 
    min(tune_brt$evaluation_log$train_rmse)
  
  cat(paste('Bootstrapping the model, round', i, '/81'), '\n')
  
}

# Find best combination of hyperparameters
tune_grid_shark <- 
  tune_grid_shark %>% 
  arrange(rmse)

head(tune_grid_shark)

# eta = 0.5, gamma = 0.5, max_depth = 15, subsample = 0.9 
# rmse = 0.078274 vs. 0.093391

# Run full boosted regression tree ------------------------------------------------
# Create list of output vectors
brt_rmse_shark <- list() # root mean squared error
rel_imp_shark <- list() # relative influence of predictors
test_preds_shark <- list() # predictions on the test set
pdp_values_shark <- list() # partial dependence values from each BRT

for(i in 1:1000) {
  
  # first, let's randomize the entire dataframe 
  brt_random <- 
    shark_data %>% 
    mutate(ID = row_number())
  
  # randomly split the data into 80-20% training-test set
  brt_train <- 
    brt_random %>% 
    dplyr::sample_frac(0.8)
  
  # test set
  brt_test <- 
    anti_join(brt_random, brt_train, by = 'ID')
  
  # create model matrix 
  modmat_train <- 
    stats::model.matrix(fmod, model.frame(~ ., brt_train, na.action=na.pass))[, -1]
  
  modmat_test <- 
    stats::model.matrix(fmod, model.frame(~ ., brt_test, na.action=na.pass))[, -1]
  
  # RUN THE MODEL
  params <- list(eta = 0.5, gamma = 0.5, max_depth = 15, subsample = 0.9)
  
  # the model
  brt_model <- xgboost::xgboost(modmat_train,
                                objective = 'reg:logistic',
                                label = brt_train$prop_threat,
                                params = params,
                                nrounds = 150,
                                verbose = 0)
  
  # extract rmse
  brt_rmse_shark[[i]] <- 
    tibble(N_round = paste(i),
           rmse = min(brt_model$evaluation_log$train_rmse))
  
  # relative importance values
  rel_imp_shark[[i]] <- 
    as_tibble(xgb.importance(colnames(modmat_train),
                             model = brt_model)) %>% 
    mutate(N_round = paste(i)) 
  
  # assess performance on testset
  test_preds_shark[[i]] <- 
    brt_test %>% 
    # create a column of predictions
    mutate(pred_threat = stats::predict(brt_model, newdata = modmat_test),
           # calculate bias
           threat_bias = prop_threat - pred_threat,
           N_round = paste(i)) %>% 
    dplyr::select(N_round, prop_threat, pred_threat, threat_bias) 
  
  # calculate partial dependence plot values
  pdp_dfs <- 
    lapply(colnames(modmat_train), function(variable_name) {
      
      df <- 
        as_tibble(pdp::partial(brt_model, pred.var = paste0(variable_name),
                               train = modmat_train)) %>% 
        mutate(variable = paste(variable_name)) %>% 
        dplyr::rename('value' = paste(variable_name))
      
      return(df)
      
    })
  
  pdp_values_shark[[i]] <- 
    do.call(rbind, pdp_dfs) %>% 
    mutate(N_round = paste(i))
  
  cat(paste('Bootstrapping the model, round', i), '\n')
  
}

beepr::beep()
# RMSE
rmse_clean_shark <- 
  do.call(rbind, brt_rmse_shark) %>% 
  mutate(SharkRay = 'Shark')

head(rmse_clean_shark)

write.csv(rmse_clean_shark, '../Data/BRT_rmse_shark.csv', row.names = FALSE)

# Relative influence
ri_clean_shark <- 
  do.call(rbind, rel_imp_shark) %>% 
  mutate(SharkRay = 'Shark')

head(ri_clean_shark)

write.csv(ri_clean_shark, '../Data/BRT_RelImp_shark.csv', row.names = FALSE)

# Test set Kmax biases
test_clean_shark <- 
  do.call(rbind, test_preds_shark) %>% 
  mutate(SharkRay = 'Shark')

head(test_clean_shark)

write.csv(test_clean_shark, '../Data/BRT_testset_pred_shark.csv', row.names = FALSE)

# Partial dependence values
pdp_clean_shark <- 
  do.call(rbind, pdp_values_shark) %>% 
  mutate(SharkRay = 'Shark')

head(pdp_clean_shark)

write.csv(pdp_clean_shark, '../Data/BRT_pdp_shark.csv', row.names = FALSE)
pdp_clean_shark <- read_csv('../Data/BRT_pdp_shark.csv')

# RAYS ------------------------------------------------------------------------------
# 2. BOOSTED REGRESSION TREE ---------------------------------------------------------------
ray_data <- 
  dplyr::filter(all_data, SharkRay == 'Ray')

modmat <- 
  stats::model.matrix(fmod, model.frame(~ ., ray_data, na.action=na.pass))[, -1]

head(modmat)

# create a vector of response
labels <- ray_data$prop_threat

# Tune hyperparameters --------------------------
# Create a matrix of tuning variables
tune_grid_ray <- 
  expand.grid(eta = c(0.2, 0.5, 0.7), # learning rate
              gamma = c(0.2, 0.5, 0.7), # minimum loss reduction
              max_depth = c(5, 10, 15), # maximum tree depth
              subsample = c(0.3, 0.6, 0.9), # subsample ratio of the training instance
              rmse = NA) # empty column to infill

# Find best combination of hyperparameters
for(i in 1:nrow(tune_grid_ray)) {
  
  # create a list of parameters
  tune_params <- 
    list(eta = tune_grid_ray$eta,
         gamma = tune_grid_ray$gamma,
         max_depth = tune_grid_ray$max_depth,
         subsample = tune_grid_ray$subsample)
  
  # run the model with specific set of hyperparameers
  tune_brt <- 
    xgboost::xgboost(modmat, label = labels, nrounds = 150,
                     params = tune_params,
                     objective = 'reg:logistic',
                     verbose = 0)
  
  tune_grid_ray$rmse[i] <- 
    min(tune_brt$evaluation_log$train_rmse)
  
  cat(paste('Bootstrapping the model, round', i, '/81'), '\n')
  
}

# Find best combination of hyperparameters
tune_grid_ray <- 
  tune_grid_ray %>% 
  arrange(rmse)

head(tune_grid_ray)

# eta = 0.2, gamma = 0.7, max_depth = 15, subsample = 0.3
# rmse = 0.062845 vs. 0.069056

# Run full boosted regression tree ------------------------------------------------
# Create list of output vectors
brt_rmse_ray <- list() # root mean squared error
rel_imp_ray <- list() # relative influence of predictors
test_preds_ray <- list() # predictions on the test set
pdp_values_ray <- list() # partial dependence values from each BRT

for(i in 1:1000) {
  
  # first, let's randomize the entire dataframe 
  brt_random <- 
    ray_data %>% 
    mutate(ID = row_number())
  
  # randomly split the data into 80-20% training-test set
  brt_train <- 
    brt_random %>% 
    dplyr::sample_frac(0.8)
  
  # test set
  brt_test <- 
    anti_join(brt_random, brt_train, by = 'ID')
  
  # create model matrix 
  modmat_train <- 
    stats::model.matrix(fmod, model.frame(~ ., brt_train, na.action=na.pass))[, -1]
  
  modmat_test <- 
    stats::model.matrix(fmod, model.frame(~ ., brt_test, na.action=na.pass))[, -1]
  
  # RUN THE MODEL
  params <- list(eta = 0.2, gamma = 0.7, max_depth = 15, subsample = 0.3)
  
  # the model
  brt_model <- xgboost::xgboost(modmat_train,
                                objective = 'reg:logistic',
                                label = brt_train$prop_threat,
                                params = params,
                                nrounds = 150,
                                verbose = 0)
  
  # extract rmse
  brt_rmse_ray[[i]] <- 
    tibble(N_round = paste(i),
           rmse = min(brt_model$evaluation_log$train_rmse))
  
  # relative importance values
  rel_imp_ray[[i]] <- 
    as_tibble(xgb.importance(colnames(modmat_train),
                             model = brt_model)) %>% 
    mutate(N_round = paste(i)) 
  
  # assess performance on testset
  test_preds_ray[[i]] <- 
    brt_test %>% 
    # create a column of predictions
    mutate(pred_threat = stats::predict(brt_model, newdata = modmat_test),
           # calculate bias
           threat_bias = prop_threat - pred_threat,
           N_round = paste(i)) %>% 
    dplyr::select(N_round, prop_threat, pred_threat, threat_bias) 
  
  # calculate partial dependence plot values
  pdp_dfs <- 
    lapply(colnames(modmat_train), function(variable_name) {
      
      df <- 
        as_tibble(pdp::partial(brt_model, pred.var = paste0(variable_name),
                               train = modmat_train)) %>% 
        mutate(variable = paste(variable_name)) %>% 
        dplyr::rename('value' = paste(variable_name))
      
      return(df)
      
    })
  
  pdp_values_ray[[i]] <- 
    do.call(rbind, pdp_dfs) %>% 
    mutate(N_round = paste(i))
  
  cat(paste('Bootstrapping the model, round', i), '\n')
  
}

beepr::beep()

# RMSE
rmse_clean_ray <- 
  do.call(rbind, brt_rmse_ray) %>% 
  mutate(SharkRay = 'Ray')

head(rmse_clean_ray)

write.csv(rmse_clean_ray, '../Data/BRT_rmse_ray.csv', row.names = FALSE)

# Relative influence
ri_clean_ray <- 
  do.call(rbind, rel_imp_ray) %>% 
  mutate(SharkRay = 'Ray')

head(ri_clean_ray)

write.csv(ri_clean_ray, '../Data/BRT_RelImp_ray.csv', row.names = FALSE)

# Test set Kmax biases
test_clean_ray <- 
  do.call(rbind, test_preds_ray) %>% 
  mutate(SharkRay = 'Ray')

head(test_clean_ray)

write.csv(test_clean_ray, '../Data/BRT_testset_pred_ray.csv', row.names = FALSE)

# Partial dependence values
pdp_clean_ray <- 
  do.call(rbind, pdp_values_ray) %>% 
  mutate(SharkRay = 'Ray')

head(pdp_clean_ray)

write.csv(pdp_clean_ray, '../Data/BRT_pdp_ray.csv', row.names = FALSE)

# Summary stats ---------------------------------------------------------
# Mean bias
mean_bias <- 
  test_clean %>% 
  group_by(SharkRay) %>% 
  summarise(mean = mean(threat_bias),
            sd = sd(threat_bias),
            se = sd/sqrt(36000),
            ci = 1.96*se,
            lower = mean - ci,
            upper = mean + ci)

mean_bias

# Mean RMSE
rmse_mean <- 
  rmse_clean %>% 
  group_by(SharkRay) %>% 
  summarise(mean = mean(rmse), 
            sd = sd(rmse), 
            se = sd/sqrt(1000), 
            ci = 1.96*se, 
            lower = mean - ci,
            upper = mean + ci)

rmse_mean

# Relative influence
ri_mean <- 
  ri_clean %>% 
  group_by(SharkRay, Feature) %>% 
  summarise(mean = mean(Gain),
            sd = sd(Gain),
            n = n(),
            se = sd/sqrt(n),
            ci = se * 1.96,
            lower = mean - ci,
            upper = mean + ci) %>% 
  dplyr::select(SharkRay, Feature, mean, lower, upper) %>% 
  arrange(SharkRay, -mean)

ri_mean




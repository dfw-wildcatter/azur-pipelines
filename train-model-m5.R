## m5 competition 
# https://kaggle.com/mayer79/m5-forecast-tidy-r-catboost

## initialize
#
suppressMessages({
  library(magrittr)
  library(tidyverse)
  library(RcppRoll)
  library(catboost)  
})

igc <- function() {
  invisible(gc())
}

as_int <- function(z) {
  as.integer(as.factor(z))
}

cum_rel <- function(z) {
  (z - cummin(z)) / (1 + cummax(z) - cummin(z))
}

#'input' %>%
#  list.files(full.names = TRUE) %>%
#  lapply(read_csv, col_types = cols()) %>%
#  setNames(c('calendar', 'sales', 'submission', 'prices')) %T>%
#  attach(warn.conflicts = FALSE) %>%
#  invisible()

## data prep
#

load('input.RData')

# calendar
calendar <- calendar %>%
  select(-date, -weekday, -event_name_2, -event_type_2) %>%
  mutate(d = as.integer(substring(d, 3))) %>%
  mutate_at(c('event_name_1', 'event_type_1'), as_int)

# sales prices
prices <- prices %>%
  group_by(store_id, item_id) %>%
  mutate(sell_price_rel_diff = sell_price / dplyr::lag(sell_price) -1,
         sell_price_cumrel = cum_rel(sell_price),
         sell_price_roll_sd7 = roll_sdr(sell_price, n=7)) %>%
  ungroup()

# sales
sales[, paste0('d_', 1913 + 1:56)] <- NA
sales <- sales %>%
  mutate(id = gsub('_validation', '', id)) %>%
  gather('d', 'demand', d_1:d_1969) %>%
  mutate(d = as.integer(substring(d, 3))) %>%
  filter (d >= 1000) %>%
  group_by(id) %>%
  mutate(lag_t28 = dplyr::lag(demand, 28),
         rolling_mean_t7 = roll_meanr(lag_t28, 7),
         rolling_mean_t500 = roll_meanr(lag_t28, 500), 
         rolling_sd_t7 = roll_sdr(lag_t28, 7),
         ) %>%
  ungroup() %>%
  filter(d >= 1914 | !is.na(rolling_mean_t500))

#save.image(file='input.RData')
#load('input.RData')

# merge all data
train <- sales %>% left_join(calendar, by = 'd') %>%
  left_join(prices, by = c('store_id', 'item_id', 'wm_yr_wk')) %>%
  select(-wm_yr_wk) %>%
  mutate_at(c('item_id', 'dept_id', 'cat_id', 'store_id', 'state_id'), as_int)

# clean up
rm(sales, prices, calendar)
#save.image(file='train.RData')
igc()

## modeling
#

# features
x <- c('item_id', 'dept_id', 'store_id', 'state_id', 'event_name_1', 'event_type_1',
       'cat_id', 'wday', 'month', 'year', 'snap_CA', 'snap_TX', 'snap_WI',
       'sell_price', 'sell_price_rel_diff', 'sell_price_cumrel', 'sell_price_roll_sd7',
       'lag_t28', 'rolling_mean_t7', 'rolling_mean_t500','rolling_sd_t7')

# split train and test, reconstruct id columns
test <- train %>%
  filter(d >= 1914) %>%
  mutate(id = paste(id, ifelse(d <= 1941, 'validation', 'evaluation'), sep = '_'),
         F = paste0('F', d - 1913 - 28 * (d > 1941)))

# train and validation set
train <- train %>%
  filter(d < 1914)
igc()

valid <- catboost.load_pool(data = train %>% filter(d >= 1914 - 28) %>% select_at(x), 
                            label =  train %>% filter(d >= 1914 -28) %>% pull(demand))
train <- train %>%
  filter(d < 1914 -28)
igc()

train <- catboost.load_pool(data = train %>% select_at(x), 
                            label =  train %>% pull(demand))
igc()

params <- list(iterations = 1000,
               metric_period = 100,
               loss_function = 'RMSE',
               eval_metric = 'RMSE',
               random_strength = 0.5,
               depth = 7,
               early_stopping_rounds= 400,
               learning_rate = 0.08,
               l2_leaf_reg = 0.1,
               random_seed = 1093)

fit <- catboost.train(train, valid, params = params)

catboost.get_feature_importance(fit, valid) %>%
  as.data.frame() %>%
  setNames('Importance') %>%
  rownames_to_column('Feature') %>%
  ggplot(aes(x=reorder(Feature, Importance), y=Importance)) + 
  geom_bar(stat = 'identity') +
  coord_flip()

# prediction
pred <- catboost.predict(fit, catboost.load_pool(test %>% select_at(x)))

# dark magic -> credits: https://www.kaggle.com/kyakovlev/m5-dark-magic
pred_calibrated <- pred / mean(pred[endsWith(test$id, 'validation')]) * 1.447147

submission <- test %>%
  mutate(demand = pred_calibrated) %>%
  select(id, F, demand) %>%
  spread(F, demand) %>%
  select_at(colnames(submission))

mean_pred <- submission %>%
  filter(endsWith(id, 'validation')) %>%
  select(-id) %>%
  as.matrix() %>%
  mean()

write_csv(submission, 'submission_cat.csv')
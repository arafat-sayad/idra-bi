linechart_policy
library(tidyverse)
library(tidymodels)
library(modeltime)
# remotes::install_github("business-science/timetk")
library(timetk)
# install.packages("modeltime.ensemble")
library(modeltime.ensemble)
library(lubridate)
# daily_policy_issue_



## monthly
monthly_policy <- linechart_policy %>%
    filter(company_name == "Metlife", policystartdate <= today()) %>%
    # group_by(policystartdate) %>% 
    # summarise(n = sum(total, na.rm = T)) %>%
    mutate(month = month(policystartdate, T),
           year= year(policystartdate)) %>%
    group_by(year, month) %>%
    mutate(n = sum(total),
           date = max(policystartdate)) %>%
    ungroup() %>%
    select(date, cnt = n) %>%
    distinct() %>% 
    ungroup()


monthly_policy %>% plot_time_series(.date_var = date, .value = cnt)

linechart_p_or


monthly_premium<- linechart_p_or %>%
    filter(company_name == "Metlife") %>%
    # group_by(ordate) %>% 
    # summarise(n = sum(total, na.rm = T)) %>%
    mutate(month = month(ordate, T),
           year= year(ordate)) %>%
    group_by(year, month) %>%
    mutate(n = sum(total),
           date = max(ordate)) %>%
    ungroup() %>%
    select(date, cnt = n) %>%
    distinct() %>% 
    ungroup()



monthly_premium %>% plot_time_series(.date_var = date, .value = cnt)

tail(monthly_premium)












monthly_7_model_ensemble_function <- function(df) {
    
    
    recipe1 <- recipe(cnt~., df) %>%
        step_timeseries_signature(date) %>%
        step_rm(matches("(.iso$)|(.xts$)|(day)|(hour)|(minute)|(second)|(am.pm)"))%>%
        step_normalize(date_index.num, date_year) %>%
        step_mutate(date_week = factor(date_week, ordered = T)) %>%
        step_dummy(all_nominal(), one_hot = T)
    
    # recipe1 %>% prep() %>% juice() %>% glimpse()
    
    
    recipe2 <- recipe1 %>%
        update_role(date, new_role = "ID")
    
    
    # recipe2 %>% prep() %>% summary()
    
    fit_prophet <- workflow() %>%
        add_model(
            prophet_reg() %>% set_engine("prophet")
        ) %>%
        add_recipe(recipe1) %>%
        fit(df)
    
    fit_xgboost <- workflow() %>%
        add_model(
            boost_tree() %>% set_engine("xgboost")
        ) %>%
        add_recipe(recipe2) %>%
        fit(df)
    
    
    
    
    
    fit_RF <- workflow() %>%
        add_model(
            rand_forest() %>% set_engine("ranger")
        ) %>%
        add_recipe(recipe2) %>%
        fit(df)
    
    
    fit_svm <- workflow() %>%
        add_model(
            svm_rbf() %>% set_engine("kernlab")
        ) %>%
        add_recipe(recipe2) %>%
        fit(df)
    
    
    fit_prophet_boost <- workflow() %>%
        add_model(
            prophet_boost(
                seasonality_daily = F,seasonality_weekly = F, seasonality_yearly = F,
            ) %>% set_engine("prophet_xgboost")
        ) %>%
        add_recipe(recipe1) %>%
        fit(df)
    
    
    
    fit_ets <- workflow() %>%
        add_model(
            exp_smoothing() %>% set_engine(engine = "ets")
        ) %>%
        add_recipe(recipe1) %>%
        fit(df)
    
    
    fit_mars <- workflow() %>%
        add_model(
            mars(mode = "regression") %>% set_engine("earth")
        ) %>%
        add_recipe(recipe2) %>%
        fit(df)
    
    # fit_lm <- workflow() %>%
    #     add_model(
    #         linear_reg() %>% set_engine("lm")
    #     ) %>%
    #     add_recipe(recipe1) %>%
    #     fit(df)
    # 
    # fit_arima_no_boost <- arima_reg() %>%
    #     set_engine(engine = "auto_arima") %>%
    #     fit(cnt ~ date, data = df)
    
    
    fit_table <- modeltime_table(
        fit_prophet,
        fit_xgboost,
        fit_RF,
        fit_svm,
        fit_prophet_boost,
        fit_ets,
        fit_mars
    )
    
    # fit_calibrated <- fit_table %>% 
    #   modeltime_calibrate(testing(splits))
    
    # fit_calibrated %>% modeltime_accuracy()
    
    
    # fit_calibrated %>%
    #     modeltime_forecast(
    #         new_data = testing(splits), actual_data = df, keep_data = T
    #     ) %>%
    #     plot_modeltime_forecast()
    
    
    
    ensemble_fit <- fit_table %>%
        ensemble_average(type = "median")
    
    ensemble_fit_table <- ensemble_fit %>%
        modeltime_table()
    
    
    # ensemble_fit_table %>%
    # combine_modeltime_tables(fit_table) %>%
    #     modeltime_accuracy(testing(splits))
    
    
    
    ensemble_refit_for_future <- ensemble_fit_table %>%
        modeltime_refit(df)
    
    
    
}



# prem_forecast_object <- monthly_7_model_ensemble_function(monthly_policy)




FORECAST_PERIOD <- 12

full_data <- monthly_policy %>% future_frame(.date_var = date, .length_out = FORECAST_PERIOD, .bind_data = T)

future_data <- full_data %>% filter(
    is.na(cnt)
)

prem_forecast_object %>% 
    modeltime_calibrate(monthly_policy)%>%
    modeltime_forecast(new_data = future_data, 
                       actual_data = monthly_policy, 
                       keep_data = T
    ) %>%
    plot_modeltime_forecast(.title = "Premium Forecast Plot")





















######### life weekly premium collection



weekly_premium<- linechart_p_or %>%
    filter(company_name == "Metlife") %>%
    # group_by(ordate) %>% 
    # summarise(n = sum(total, na.rm = T)) %>%
    mutate(month = month(ordate, T),
           year= year(ordate), 
           week = week(ordate)) %>%
    group_by(year, month, week) %>%
    mutate(n = sum(total),
           date = max(ordate)) %>%
    ungroup() %>%
    select(date, cnt = n) %>%
    distinct() %>% 
    ungroup()



weekly_premium %>% plot_time_series(.date_var = date, .value = cnt)



life_prem_forecast_object <- monthly_7_model_ensemble_function(weekly_premium)




FORECAST_PERIOD <- 12

full_data <- weekly_premium %>% future_frame(.date_var = date, .length_out = FORECAST_PERIOD, .bind_data = T)

future_data <- full_data %>% filter(
    is.na(cnt)
)

life_prem_forecast_object %>% 
    modeltime_calibrate(weekly_premium)%>%
    modeltime_forecast(new_data = future_data, 
                       actual_data = weekly_premium, 
                       keep_data = T
    ) %>%
    plot_modeltime_forecast(.title = "Weekly Premium Collection Forecast Plot")











###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################

forecast_object <- monthly_7_model_ensemble_function(monthly_policy)

## monthly
monthly_policy <- linechart_policy %>%
    filter(company_name == "Sandhani", policystartdate <= today()) %>%
    # group_by(policystartdate) %>% 
    # summarise(n = sum(total, na.rm = T)) %>%
    mutate(month = month(policystartdate, T),
           year= year(policystartdate)) %>%
    group_by(year, month) %>%
    mutate(n = sum(total),
           date = max(policystartdate)) %>%
    ungroup() %>%
    select(date, cnt = n) %>%
    distinct() %>% 
    ungroup()



companies <- c("Metlife", "Sandhani", "Alpha")


life_policy_forecastObjects <-list()
life_policy_forecast_data <- list()

for (i in companies ){
    print(i)
    
    monthly_policy <- linechart_policy %>%
        filter(company_name == i, policystartdate <= today(), policystartdate > "2005-01-01") %>%
        # group_by(policystartdate) %>% 
        # summarise(n = sum(total, na.rm = T)) %>%
        mutate(month = month(policystartdate, T),
               year= year(policystartdate)) %>%
        group_by(year, month) %>%
        mutate(n = sum(total),
               date = max(policystartdate)) %>%
        ungroup() %>%
        select(date, cnt = n) %>%
        distinct() %>% 
        ungroup()
    
    life_policy_forecast_data[[i]] <- monthly_policy
    
    forecast_object <- monthly_7_model_ensemble_function(monthly_policy)
    
    life_policy_forecastObjects[[i]] <- forecast_object
    
}






monthly_policy <- life_policy_forecast_data[["Alpha"]]
policy_forecast_object <- life_policy_forecastObjects[["Alpha"]]

FORECAST_PERIOD <- 12

full_data <- monthly_policy %>% future_frame(.date_var = date, .length_out = FORECAST_PERIOD, .bind_data = T)

future_data <- full_data %>% filter(
    is.na(cnt)
)

policy_forecast_object %>% 
    modeltime_calibrate(monthly_policy)%>%
    modeltime_forecast(new_data = future_data, 
                       actual_data = monthly_policy, 
                       keep_data = T
    ) %>%
    plot_modeltime_forecast(.title = "Premium Forecast Plot")






save(life_policy_forecastObjects, life_policy_forecast_data, file = "life_policy_forecast.RData")











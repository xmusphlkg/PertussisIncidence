

# drop

library(tidyverse)
library(lubridate)
library(ggsci)
library(patchwork)
library(forecast)
library(tseries)
library(openxlsx)
library(ggh4x)
library(bsts)

# data --------------------------------------------------------------------

fill_color <- paletteer_d("ggsci::nrc_npg")

# Load data
df_clean <- read.csv('./Outcome/Table S1.csv') |> 
     group_by(Country) |>
     mutate(Date = as.Date(Date),
            AnnualizedInci = case_when(all(is.na(Month)) ~ Incidence * 52.14,
                                       all(!is.na(Month)) ~ Incidence * 12))

# Function to calculate MAPE
smape <- function(actual, forecast) {
     n <- length(actual)
     sum_val <- sum(2 * abs(forecast - actual) / (abs(actual) + abs(forecast)))
     smape_val <- (1 / n) * sum_val * 100
     return(smape_val)
}

country_list <- sort(unique(df_clean$Country))

set.seed(20240601)

# model test ----------------------------------------------------------------

i <- 1

# Function to process time series analysis by country
analyze_country <- function(i) {
     df <- filter(df_clean, Country == country_list[i]) |> 
          arrange(Date)
     
     if (!all(is.na(df$Month))) {
          ts_data <- ts(df$Cases, start = c(2015, 1), frequency = 12)
     } else {
          ts_data <- ts(df$Cases, start = decimal_date(ymd(df$Date[1])), frequency = 365.25/7)
     }
     
     # Perform sequence tests and forecast
     adf_result <- adf.test(ts_data, alternative = 'stationary')
     bl_test <- Box.test(ts_data, lag = ifelse(frequency(ts_data) == 12, 12, 54), type = 'Ljung-Box')
     fig_acf <- autoplot(acf(ts_data, plot = F)) + 
          labs(caption = paste("ADF Test p-value: ", format.pval(adf_result$p.value, digits = 4), sep = ""),
               title = 'Autocorrelation Function')
     fig_pacf <- autoplot(pacf(ts_data, plot = F))+
          labs(caption = paste("Ljung-Box Test p-value: ", format.pval(bl_test$p.value, digits = 4), sep = ""),
               title = 'Partial Autocorrelation Function')
     
     # Forecasting model
     ts_train <- window(ts_data, start = 2015, end = 2019.5)
     ts_test <- window(ts_data, start = 2019.5, end = 2020)
     model <- auto.arima(ts_train, seasonal = TRUE, ic = 'aicc')
     forecasted <- forecast(model, h = length(ts_test))
     SMAPE <- smape(ts_test, forecasted$mean)
     fig_forecast <- autoplot(forecasted) + 
          geom_line(data = data.frame(date = time(ts_test), observed = as.matrix(ts_test)), aes(x = date, y = observed), color = "red") +
          labs(caption = paste("SMAPE: ", formatC(SMAPE, format = "f", digits = 2), "%"),
               title = 'Forecasting Validation',
               x = 'Date', y = 'Cases')
     
     # Save results
     ggsave(filename = paste0('./Outcome/S', i, '.png'),
            plot = fig_acf + fig_pacf + fig_forecast,
            width = 9, height = 3)
     
     # return results
     return(country_list[i])
}

outcome <- lapply(1:length(country_list), analyze_country)

# forecast 2021 -----------------------------------------------------------

analysis_2021 <- function(i) {
     df <- filter(df_clean, Country == country_list[i]) |> 
          arrange(Date)
     
     if (!all(is.na(df$Month))) {
          ts_data <- ts(df$AnnualizedInci, start = c(2015, 1), frequency = 12)
     } else {
          ts_data <- ts(df$AnnualizedInci, start = decimal_date(df$Date[1]), frequency = 365.25/7)
     }
     
     ts_train <- window(ts_data, end = 2019.99)
     ts_observed <- window(ts_data, start = 2020)
     model <- auto.arima(ts_train, seasonal = T, ic = 'aicc')
     outcome <- forecast(model, h = length(ts_observed))
     df_model <- data.frame(
          date = as.Date(date_decimal(as.numeric(time(ts_observed)))),
          mean = as.matrix(outcome$mean),
          lower_80 = outcome$lower[, "80%"],
          upper_80 = outcome$upper[, "80%"],
          lower_95 = outcome$lower[, "95%"],
          upper_95 = outcome$upper[, "95%"],
          observed = as.matrix(ts_observed),
          country = country_list[i],
          model = '2015-2019'
     )
     
     return(df_model)
}

outcome_2021 <- lapply(1:length(country_list), analysis_2021)
outcome_2021 <- bind_rows(outcome_2021)

# forecast 2023 -----------------------------------------------------------

analysis_2023 <- function(i) {
     df <- filter(df_clean, Country == country_list[i]) |> 
          arrange(Date)
     
     if (!all(is.na(df$Month))) {
          ts_data <- ts(df$AnnualizedInci, start = c(2015, 1), frequency = 12)
     } else {
          ts_data <- ts(df$AnnualizedInci, start = decimal_date(df$Date[1]), frequency = 365.25/7)
     }
     
     ts_train <- window(ts_data, end = 2023.5)
     ts_observed <- window(ts_data, start = 2023.5)
     
     model <- auto.arima(ts_train, seasonal = T, ic = 'aicc')
     outcome <- forecast(model, h = length(ts_observed))
     df_model <- data.frame(
          date = as.Date(date_decimal(as.numeric(time(ts_observed)))),
          mean = as.matrix(outcome$mean),
          lower_80 = outcome$lower[, "80%"],
          upper_80 = outcome$upper[, "80%"],
          lower_95 = outcome$lower[, "95%"],
          upper_95 = outcome$upper[, "95%"],
          observed = as.matrix(ts_observed),
          country = country_list[i],
          model = '2015-2022'
     )
     
     return(df_model)
}

outcome_2023 <- lapply(1:length(country_list), analysis_2023)
outcome_2023 <- bind_rows(outcome_2023)

# fig ---------------------------------------------------------------------

plot_data <- function(i){
     data_predict <- outcome_2021 |> 
          filter(country == country_list[i]) |> 
          arrange(date)
     
     plot_breaks <- pretty(c(0, data_predict$mean, data_predict$observed))
     plot_range <- range(plot_breaks)
     
     fig1 <- ggplot(data = data_predict, aes(x = date)) +
          geom_line(aes(y = mean, color = 'Predicted')) +
          geom_line(aes(y = observed, color = 'Observed'))+
          stat_difference(aes(ymin = observed, ymax = mean),
                          alpha = 0.3,
                          levels = c("Decreased", "Increased"))+
          coord_cartesian(xlim = as.Date(c('2020-1-1', '2024-6-1'))) +
          scale_color_manual(values = fill_color[1:3]) +
          scale_fill_manual(values = fill_color[6:5]) +
          scale_x_date(expand = expansion(add = c(0, 0)),
                       date_labels = "%Y") +
          scale_y_continuous(expand = c(0, 0),
                             breaks = plot_breaks,
                             limits = range(plot_breaks)) +
          theme_bw() +
          theme(plot.title.position = "plot",
                plot.caption.position = "plot",
                plot.title = element_text(face = "bold", size = 14, hjust = 0),
                panel.grid.major.y = element_blank(),
                panel.grid.minor = element_blank(),
                legend.text = element_text(face = "bold", size = 12),
                legend.title = element_text(face = "bold", size = 12),
                legend.box.background = element_rect(fill = "transparent", colour = "transparent"),
                legend.background = element_rect(fill = "transparent", colour = "transparent"),
                legend.position = 'bottom',
                axis.title = element_text(face = "bold", size = 12, color = "black"),
                axis.text.y = element_text(size = 12, color = "black"),
                axis.text.x = element_text(size = 12, color = "black"),
                plot.background = element_blank())+
          labs(title = paste0(LETTERS[i], ': ', country_list[i]),
               x = 'Date', y = 'Annualized Incidence',
               color = 'Stage', fill = 'Stage')
     
     fig1
}

fig <- lapply(1:length(country_list), plot_data) |> 
     wrap_plots(ncol = 2, widths = c(1, 1), guides = 'collect')&
     theme(legend.position = 'bottom')

ggsave(filename = './Outcome/Fig4.pdf',
       plot = fig,
       width = 12,
       height = 10, 
       device = cairo_pdf,
       family = 'Times New Roman')


# Loading packages -----------------------------------------------------------------------

if (require(pacman)) {
  p_load(
    data.table, 
    forecast,
    tseries,
    xts,
    tidyverse,
    ggalt,
    fable,
    tsibble,
    readxl,
    stringi,
    stringr
    )
}

# Importing data -------------------------------------------------------------------------

df <- fread("combined_incidence.csv")
df2022_animal <- read_excel("animal_brucellosis_2022.xlsx")
df2023_animal <- read_excel("animal_brucellosis_2023.xlsx")
human_22_23 <- fread('brucella_2022_2023.csv')


human_pop <- fread("kenya_pop_2020_2025.csv") |> 
  filter(Year == "2022") |> 
  select(pop)

animal_pop <- read_csv('animal_pop_2019.csv') %>% 
  pivot_wider(
    names_from = "species",
    values_from = "species_num"
  ) %>% 
  setNames(c("county", paste0(colnames(.[,2:7]), "_pop") %>% str_to_lower())) |> 
  summarise(across(where(is.numeric), ~sum(.))) |> 
  select(-pigs_pop, -donkeys_pop)

# Cleaning 2022 and 2023 human brucella data -------------------------------------------------------------------

## Human
df22_human <- human_22_23 |>
  select(periodname, dataname, Kenya) |> 
  pivot_wider(id_cols = periodname, names_from = dataname, values_from = Kenya) |> 
  mutate(date = dmy(paste("01", periodname, sep = "-")) |> ymd()) |>
  rowwise() |>
  mutate(hum_cases = sum(Brucellosis, `MOH 706_Brucella`)) |> 
  select(date, hum_cases) |> 
  cbind(human_pop) |> 
  mutate(human_incidence = hum_cases/pop * 1e3) |> 
  select(-hum_cases, -pop)

# Animals
df22 <- df2022_animal |> 
  select(month, year...13, `Nature of Diagnosis`, `Number Sick`, `Species Affected`) |> 
  mutate(month =
           ifelse(
             str_detect(month, "[:digit:]"),
             month.name[as.numeric(month[str_detect(month, "[:digit:]")])],
             month
           )) |> 
  filter(!is.na(month)) |> 
  mutate(date = ymd(paste(year...13, month, "01", sep = "-"))) |> 
  group_by(date, `Species Affected`) |> 
  summarise(count = sum(`Number Sick`, na.rm = T)) |> 
  mutate(`Species Affected` = ifelse(
    str_detect(`Species Affected`, "caprine|Caprine|CAPRINE"),
    "Goats",
    `Species Affected`
  ),
  count = as.numeric(count)
  
  ) |> 
  filter(`Species Affected` %in% c("Cattle", "Goats", "Camel", "Sheep")) |> 
  ungroup() |> 
  distinct() |> 
  pivot_wider(
    id_cols = "date",
    values_from = count,
    names_from = `Species Affected`,
    values_fn = sum
  ) |> 
  rowwise() |> 
  mutate(animal_cases = sum(Cattle, Goats, Sheep, Camel, na.rm = T)) |> 
  cbind(animal_pop) |> 
  mutate(
    animal_pop = sum(cattle_pop, goats_pop, sheep_pop, camels_pop, na.rm = T),
    animal_incidence = animal_cases/animal_pop*1e6,
    date = ymd((date))
    )


# Cleaning df2023 data -------------------------------------------------------------------

# Animals
df23 <- df2023_animal |> 
  select(month = Month, year...13 = Year, `Nature of Diagnosis`, `Number Sick`, `Species Affected`) |> 
  mutate(month =
           ifelse(
             str_detect(month, "[:digit:]"),
             month.name[as.numeric(month[str_detect(month, "[:digit:]")])],
             month
           )) |> 
  filter(!is.na(month)) |> 
  mutate(date = ymd(paste(year...13, month, "01", sep = "-"))) |> 
  group_by(date, `Species Affected`) |> 
  summarise(count = sum(`Number Sick`, na.rm = T)) |> 
  mutate(`Species Affected` = ifelse(
    str_detect(`Species Affected`, "caprine|Caprine|CAPRINE"),
    "Goats",
    `Species Affected`
  ),
  count = as.numeric(count)
  
  ) |> 
  filter(`Species Affected` %in% c("Cattle", "Goats", "Camel", "Sheep")) |> 
  ungroup() |> 
  distinct() |> 
  pivot_wider(
    id_cols = "date",
    values_from = count,
    names_from = `Species Affected`,
    values_fn = sum
  ) |> 
  rowwise() |> 
  mutate(animal_cases = sum(Cattle, Goats, Sheep, Camel, na.rm = T)) |> 
  cbind(animal_pop) |> 
  mutate(
    animal_pop = sum(cattle_pop, goats_pop, sheep_pop, camels_pop, na.rm = T),
    animal_incidence = animal_cases/animal_pop*1e6,
    date = ymd((date))
  )

all_y <- df22 |> 
  rbind(df23) |> 
  merge(df22_human, by = "date") |> 
  select(date, human_incidence, animal_incidence)



df_complete1 <- df |> 
  select(-V1) |> 
  mutate(date = ymd(date)) |> 
  rbind(all_y) |> 
  arrange(date) |> 
  mutate(human_incidence = ifelse(human_incidence > 2, median(human_incidence), human_incidence))

# Testing for stationary ---------------------------------------------------------------------------

# Test for human incidence
adf.test(df_complete1$human_incidence) # Not stationary

# Test for animal incidence
adf.test(df_complete1$animal_incidence)



# Lagging ------------------------------------------------------------

df2.1 <- df_complete1 |>
  mutate(date = as.Date(date),
         animal_incidence = lag(animal_incidence, 3)
         ) |>
  na.omit()
adf.test(df2.1$human_incidence)


df_xts1 <- as.xts(x = cbind(df2.1$human_incidence, df2.1$animal_incidence), order.by = df2.1$date) |>
  setNames(c("human_incidence", "animal_incidence"))

plot(df_xts1)

# Data for train and testing -------------------------------------------------------------

df_xts <- window(df_xts1, start ='2014-04-01', end = '2022-12-01')
forecast.df <- window(df_xts1, start ='2023-01-01', end = '2023-12-01')

df2 <- df2.1 |> 
  filter(date < as.Date('2023-01-01'))
# Our training and testing data reaches 2022-12-01


# Investigating the time series ----------------------------------------------------------

# Correlation between human incidence and the previous lags
dd   |>
  gg_lag(human_incidence, geom = "point") +
  theme_light()
#> Lag 1 seems to have a strong correlation with the current value of human incidence

# Decomposing the time series(human incidence) to get the trend, and seasonality

dcmp <- dd |>
  model(stl = STL(human_incidence))
components(dcmp)

# The trend
components(dcmp) |>
  as_tsibble() |>
  ggplot(aes(x = date)) +
  geom_xspline(aes(y = human_incidence, colour = "Human Incidence")) +
  geom_xspline(aes(y = trend, color = 'Trend')) +
  labs(y = "Incidence (human cases per 1000 pop)",
       title = "Human Brucellois incidence") +
  theme_light() +
  theme(
    axis.title = element_text(color = 'black'),
    axis.text = element_text(color = 'black'),
    plot.title = element_text(color = 'black', hjust = .5),
    legend.position = 'bottom'
  ) +
  scale_color_manual(values = c("Human Incidence" = "gray", "Trend" = "#D55E00")) +
  labs(color = 'Series:')
  
# Seasonality
components(dcmp) |>
  as_tsibble() |>
  ggplot(aes(x = date)) +
  #geom_xspline(aes(y = human_incidence, colour = "Human Incidence")) +
  geom_xspline(aes(y = season_year, color = 'Season-year')) +
  labs(y = "Incidence (human cases per 1000 pop)",
       title = "Human Brucellois incidence") +
  theme_light() +
  theme(
    axis.title = element_text(color = 'black'),
    axis.text = element_text(color = 'black'),
    plot.title = element_text(color = 'black', hjust = .5),
    legend.position = 'bottom'
  ) +
  scale_color_manual(values = c("Human Incidence" = "gray", "Season-year" = "#D55E00")) +
  labs(color = 'Series:')

# All of them together
components(dcmp) |>
  autoplot() +
  theme_light()

# The plot without seasonality
components(dcmp) |>
  as_tsibble() |>
  ggplot(aes(x = date)) +
  geom_xspline(aes(y = human_incidence, colour = "Human Incidence")) +
  geom_xspline(aes(y = season_adjust, color = 'Human Incidence without the seasonality')) +
  labs(y = "Incidence (human cases per 1000 pop)",
       title = "Human Brucellois incidence") +
  theme_light() +
  theme(
    axis.title = element_text(color = 'black'),
    axis.text = element_text(color = 'black'),
    plot.title = element_text(color = 'black', hjust = .5),
    legend.position = 'bottom'
  ) +
  scale_color_manual(values = c("Human Incidence" = "gray", "Human Incidence without the seasonality" = "#D55E00")) +
  labs(color = 'Series:')

# The plot without seasonality and trend
components(dcmp) |>
  as_tsibble() |>
  ggplot(aes(x = date)) +
  #geom_xspline(aes(y = human_incidence, colour = "Human Incidence")) +
  geom_xspline(aes(y = season_adjust - trend, color = 'Human Incidence without the seasonality')) +
  labs(y = "Incidence (human cases per 1000 pop)",
       title = "Human Brucellois incidence") +
  theme_light() +
  theme(
    axis.title = element_text(color = 'black'),
    axis.text = element_text(color = 'black'),
    plot.title = element_text(color = 'black', hjust = .5),
    legend.position = 'bottom'
  ) +
  scale_color_manual(values = c("Human Incidence" = "gray", "Human Incidence without the seasonality" = "#D55E00")) +
  labs(color = 'Series:')

# Troughs and peakedness
dd |>
  features(human_incidence, feat_stl) 


# difference ---------------------------------------------------------------------------

ts_diff <- diff(df_xts) |> na.omit()

# Splitting ------------------------------------------------------------------------------

# train <- window(ts_diff, start = as.Date("2014-04-01"), end = "2021-12-01")
# test <- window(df_xts, start = as.Date("2022-01-01"), end = as.Date("2022-12-01"))

# We will take 70% for training, 15% for validation anf 15% for testing.
nrow(df_xts) # our data has 120 rows, thus 70% of this is 84 rows for training
nrow_80 <- floor(.8 * nrow(df_xts))
train <- df_xts[1:nrow_80,]
test <- df_xts[(nrow_80 + 1):nrow(df_xts),]

# Plotting for differenced case ----------------------------------------------------------------

# Humans
diff_human <- ts_diff |>
  as.data.frame() %>%
  mutate(date = row.names(.) |>
           as.Date()) |>
  ggplot(aes(x = date)) +
  geom_xspline(aes(y = human_incidence)) +
  theme_light() +
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    axis.ticks = element_line(color = "black"),
  ) +
  xlab("Year") +
  ylab("Human Brucellosis Incidence")
diff_human
dev.off()
ggsave(
  "images/diff_human.png",
  width = 10,
  height = 6,
  dpi = 1e3
)

# Animal
diff_animal <- ts_diff |>
  as.data.frame() %>%
  mutate(date = row.names(.) |>
           as.Date()) |>
  ggplot(aes(x = date)) +
  geom_xspline(aes(y = animal_incidence)) +
  theme_light() +
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    axis.ticks = element_line(color = "black"),
  ) +
  xlab("Year") +
  ylab("Human Brucellosis Incidence")
diff_animal
dev.off()
ggsave(
  "images/diff_animal.png",
  width = 10,
  height = 6,
  dpi = 1e3
)

# ACF and PACF plots ---------------------------------------------------------------------

acf(train$human_incidence)

#> shows both trend and seasonality.
#> The slow decrease in the ACF as the lags increase is due to the trend, while the “scalloped” shape
#> is due to the seasonality. Time series that show no autocorrelation are called white noise. 

pacf(train$human_incidence)

# Fitting arima model for human incidence without covariates --------------------------------------------------------------------

# ARIMA for human incidence alone
model_arima_human <- auto.arima(train$human_incidence)

summary(model_arima_human)

# ETS
# Smoothing Parameters:
# Alpha (α) = 0.7570879: This is the smoothing parameter for the level component. 
# It determines how much weight to give to the most recent observation when updating the level.
# Beta (β) = 0.0001000072: This is the smoothing parameter for the trend component. 
# It determines how much weight to give to the most recent trend estimate when updating the trend.
# Initial States:
# Initial Level (l[0]) = 0.1824089: This is the initial estimate for the level component at 
# the beginning of the series.
# Initial Trend (b[0]) = 0.009429334: This is the initial estimate for the trend component at 
# the beginning of the series.

model <- train |>
  as.data.frame() %>%
  mutate(date = row.names(.) |>
           zoo::as.yearmon() |> 
           yearmonth()) |> 
  as_tsibble() |>
  model(
    ETS = ETS(human_incidence ~ trend("A")),
    ARIMA = ARIMA(human_incidence)
  ) 

model |> 
 glance()

# Fitting arima model for human incidence with covariates --------------------------------------------------------------------

# # ARIMA now with animal incidence
model_arima_covariates <- auto.arima(train$human_incidence,
                     xreg = train$animal_incidence
                     )

pred.actual <- data.frame(
  date = row.names(data.frame(train)),
  actual = train[, 'human_incidence'],
  fitted = model_arima_covariates$fitted
) |> 
  setNames(c('date', 'actual', 'fitted')) |> 
  mutate(date = as.Date(date))
row.names(pred.actual) <- NULL

pred.actual_plt <- pred.actual %>%
  pivot_longer(cols = -date, names_to = 'series') |> 
  ggplot(aes(x = date)) +
  geom_xspline(aes(y = value, col = series)) +
  scale_color_manual(
    values = c(
      actual = 'red2',
      fitted = 'green4'
    )

  ) +
  theme_light() +
  xlab('Year') +
  ylab('Incidence')
pred.actual_plt


# Forecasting for time series with covariate ----------------------------------------------------------------------------

forecast_data <-
  forecast::forecast(model_arima_covariates, xreg = test[, 2]) |>
  as.data.frame() |>
  mutate(date = row.names(as.data.frame(test)) |> ymd()) %>%
  setNames(c("PointForecast", "Lo80", "Hi80", "Lo95", "Hi95", "date"))

original_data <- df_xts |>
  as.data.frame() %>%
  mutate(date = row.names(.) |> ymd())

# forecast_plot <- ggplot(original_data) +
#   geom_ribbon(data = forecast_data, aes(date, ymin = Lo95, ymax = Hi95, fill = "95% CI"), alpha = 0.25) +
#   geom_ribbon(data = forecast_data, aes(date, ymin = Lo80, ymax = Hi80, fill = "80% CI"), alpha = 1) +
#   geom_xspline(data = forecast_data, aes(date, PointForecast, colour = "Forecasted human incidence")) +
#   geom_xspline(aes(date, human_incidence, color = "Actual human incidence")) +
#   geom_xspline(data = pred.actual, aes(date, fitted, color = "Fitted human incidence")) +
#   theme_light() +
#   theme(axis.text = element_text(color = "black"),
#         axis.title = element_text(color = "black"),
#         axis.ticks = element_line(color = "black"),
#         legend.position = "bottom"
#         ) +
#   xlab("Year") +
#   ylab("Human Incidence") +
#   scale_color_manual(
#     values = c(
#       "Forecasted human incidence" = "blue",
#       "Actual human incidence" = "green4",
#       'Fitted human incidence' = 'red'
#     )
#   ) +
#   scale_fill_manual(
#     values = c(
#       "95% CI" = "tomato",
#       "80% CI" = "tomato"
#     ),
#         guide = guide_legend(
#       override.aes = list(alpha = c(0.25, 1))
#     )
#   ) +
#   labs(col = NULL, fill = NULL)
# forecast_plot

forecast_plot <- ggplot(original_data) +
  geom_line(data = forecast_data, aes(date, y = Hi95, linetype = "95% CI"), col = 'black') +
  geom_line(data = forecast_data, aes(date, y = Lo95), linetype = 2, col = 'black') +
  geom_vline(xintercept = as.Date('2021-03-01'), linetype = 3) +
  geom_xspline(data = forecast_data, aes(date, PointForecast, colour = "Predicted human incidence")) +
  geom_xspline(aes(date, human_incidence, color = "Actual human incidence")) +
  geom_xspline(data = pred.actual, aes(date, fitted, color = "Fitted human incidence")) +
  theme_light() +
  theme(axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        legend.position = "bottom"
  ) +
  xlab("Year") +
  ylab("Human Incidence") +
  scale_color_manual(
    values = c(
      "Predicted human incidence" = "blue",
      "Actual human incidence" = "green4",
      'Fitted human incidence' = 'red'
    )
  ) +
  scale_linetype_manual(
    name = NULL,
    values = 2,
    labels = '95% Confidence Interval'
  ) +
  labs(col = NULL, fill = NULL)

forecast_plot

dev.off()
ggsave(
  "images/forecast_plot.png",
  width = 10,
  height = 6,
  dpi = 1e3
)


## The MAE is here
MAE(forecast_data$PointForecast, test$human_incidence)

    
# Forecasting for time series with no covariate (human alone) ---------------------------------------------------------------------

forecast_data_human <-
  forecast::forecast(model_arima_human, h = nrow(test)) |>
  as.data.frame() |>
  mutate(date = row.names(as.data.frame(test)) |> ymd()) %>%
  setNames(c("PointForecast", "Lo80", "Hi80", "Lo95", "Hi95", "date"))

original_data <-   df_xts |>
  as.data.frame() %>%
  mutate(date = row.names(.) |> ymd())

# forecast_plot_human <- ggplot(original_data) +
#   geom_ribbon(data = forecast_data_human, aes(date, ymin = Lo95, ymax = Hi95, fill = "95% CI"), alpha = 0.25) +
#   geom_ribbon(data = forecast_data_human, aes(date, ymin = Lo80, ymax = Hi80, fill = "80% CI"), alpha = 1) +
#   geom_xspline(data = forecast_data_human, aes(date, PointForecast, colour = "Forecasted human incidence")) +
#   geom_xspline(aes(date, human_incidence, color = "Actual human incidence")) +
#   theme_light() +
#   theme(axis.text = element_text(color = "black"),
#         axis.title = element_text(color = "black"),
#         axis.ticks = element_line(color = "black"),
#         legend.position = "bottom"
#         ) +
#   xlab("Year") +
#   ylab("Human Incidence") +
#   scale_color_manual(
#     values = c(
#       "Forecasted human incidence" = "blue",
#       "Actual human incidence" = "black"
#     )
#   ) +
#   scale_fill_manual(
#     values = c(
#       "95% CI" = "tomato",
#       "80% CI" = "tomato"
#     ),
#         guide = guide_legend(
#       override.aes = list(alpha = c(0.25, 1))
#     )
#   ) +
#   labs(col = NULL, fill = NULL)
# forecast_plot_human

forecast_plot_human <- ggplot(original_data) +
  geom_line(data = forecast_data_human, aes(date, y = Hi95, linetype = "95% CI"), col = 'black') +
  geom_line(data = forecast_data_human, aes(date, y = Lo95), linetype = 2, col = 'black') +
  geom_vline(xintercept = as.Date('2021-03-01'), linetype = 3) +
  geom_xspline(data = forecast_data_human, aes(date, PointForecast, colour = "Forecasted human incidence")) +
  geom_xspline(aes(date, human_incidence, color = "Actual human incidence")) +
  #geom_xspline(data = pred.actual, aes(date, fitted, color = "Fitted human incidence")) +
  theme_light() +
  theme(axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        legend.position = "bottom"
  ) +
  xlab("Year") +
  ylab("Human Incidence") +
  scale_color_manual(
    values = c(
      "Forecasted human incidence" = "blue",
      "Actual human incidence" = "green4",
      'Fitted human incidence' = 'red'
    )
  ) +
  scale_linetype_manual(
    name = NULL,
    values = 2,
    labels = '95% Confidence Interval'
  ) +
  labs(col = NULL, fill = NULL)

forecast_plot_human

dev.off()
ggsave(
  "images/forecast_plot_human.png",
  width = 10,
  height = 6,
  dpi = 1e3
)

## The MAE is here
MAE(forecast_data_human$PointForecast, test$human_incidence)

# The low the MAE, the better, so the model with animal incidence actually did better!

# SARIMA with seasonal component ----------------------------------------------------------

set.seed(123)
model_sarima <- train |>
  as.data.frame() %>%
  mutate(date = row.names(.) |>
           zoo::as.yearmon() |>
           yearmonth()) |>
  as_tsibble() |>
  model(
    ARIMA(human_incidence,
          ic = "aic",
          stepwise = T
          )
  ) |>
  report()

fitted <- augment(model_sarima) |> 
  as.data.frame() %>%
  select(date, fitted =.fitted, residuals = .resid) |> 
  mutate(date = ym(date) 
         ) 

CI <-  hilo(forecast::forecast(model_sarima, h = nrow(test))) |> 
  as.data.frame()

CI80 <- CI$`80%` |> 
  lapply(FUN = first) |> unlist() |>
  matrix(ncol = 3, byrow = T) |> 
  as.data.frame() |> 
  select(-V3) |> 
  setNames(c("Lo80", "Hi80"))


CI95 <- CI$`95%` |> 
  lapply(FUN = first) |> unlist() |>
  matrix(ncol = 3, byrow = T) |> 
  as.data.frame() |> 
  select(-V3) |> 
  setNames(c("Lo95", "Hi95"))

forecast_data_human_sarima <-
  data.frame(
    date = row.names(as.data.frame(test)) |>
      ymd(),
    PointForecast = CI$.mean,
    CI80,
    CI95
  )

forecast_plot_human_sarima <- ggplot(original_data) +
  geom_ribbon(data = forecast_data_human_sarima, aes(date, ymin = Lo95, ymax = Hi95, fill = "95% CI"), alpha = 0.1) +
  geom_xspline(data = fitted, aes(date, y = fitted, color = "fitted")) +
  geom_ribbon(data = forecast_data_human_sarima, aes(date, ymin = Lo80, ymax = Hi80, fill = "80% CI"), alpha = .4) +
  geom_xspline(data = forecast_data_human_sarima, aes(date, PointForecast, colour = "Predicted human incidence")) +
  geom_xspline(aes(date, human_incidence, color = "Actual human incidence")) +
  theme_light() +
  theme(axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        legend.position = "bottom",
        plot.title = element_text(color = "black", hjust = .5)
        ) +
  xlab("Year") +
  ylab("Human Incidence") +
  scale_color_manual(
    values = c(
      "Predicted human incidence" = "red",
      "Actual human incidence" = "black",
      'fitted' = 'blue'
    )
  ) +
  scale_fill_manual(
    values = c(
      "95% CI" = "blue",
      "80% CI" = "blue"
    ),
        guide = guide_legend(
      override.aes = list(alpha = c(0.25, 1))
    )
  ) +
  labs(col = NULL, fill = NULL)
#plotly::ggplotly( forecast_plot_human_sarima)
forecast_plot_human_sarima

dev.off()
ggsave(
  "images/forecast_plot_human_sarima.png",
  width = 10,
  height = 6,
  dpi = 1e3
)


# Accuracy
test_acc <- test |>
  as.data.frame() %>%
  mutate(date = rownames(.) |> ymd() |> yearmonth()) |>
  as_tsibble() |>
  select(-animal_incidence)
  


model_sarima_accuracy <- accuracy(forecast::forecast(model_sarima, h = nrow(test)), test_acc,
  measures = list(
    point_accuracy_measures,
    interval_accuracy_measures,
    distribution_accuracy_measures
  )
)

# SARIMA with seasonal component And Covariate --------------------------------------------

set.seed(123)
model_sarima_covariates <- train |>
  as.data.frame() %>%
  mutate(date = row.names(.) |>
           zoo::as.yearmon() |>
           yearmonth()) |>
  as_tsibble() |>
  model(ARIMA(
    human_incidence ~ animal_incidence,
    ic = "aic",
    stepwise = T
  )) |>
  report()

#>  The ARIMA model is specified as ARIMA(0,1,0)(2,0,0)[12], which indicates a seasonal
#>  ARIMA model with seasonal period 12 (monthly data). 
  

fitted_covariate <- augment(model_sarima_covariates) |> 
  as.data.frame() %>%
  select(date, fitted =.fitted, residuals = .resid) |> 
  mutate(date = ym(date) 
  ) 


test_tsibble <- test |> 
  as.data.frame() %>%
  mutate(date = rownames(.) |> ymd() |> yearmonth()) |> 
  as_tsibble() |>
  select(-human_incidence)
  

CI_covariate <- model_sarima_covariates |>
  forecast(new_data = test_tsibble) |> 
  hilo()

CI_covariate80 <- CI_covariate$`80%` |> 
  lapply(FUN = first) |> 
  unlist() |>
  matrix(ncol = 3, byrow = T) |> 
  as.data.frame() |> 
  select(-V3) |> 
  setNames(c("Lo80", "Hi80"))


CI_covariate95 <- CI_covariate$`95%` |> 
  lapply(FUN = first) |> unlist() |>
  matrix(ncol = 3, byrow = T) |> 
  as.data.frame() |> 
  select(-V3) |> 
  setNames(c("Lo95", "Hi95"))

forecast_data_covariate_sarima <-
  data.frame(
    date = row.names(as.data.frame(test)) |>
      ymd(),
    PointForecast = CI_covariate$.mean,
    CI_covariate80,
    CI_covariate95
  )



forecast_plot_covariate_sarima <- ggplot(original_data) +
  geom_ribbon(data = forecast_data_covariate_sarima, aes(date, ymin = Lo95, ymax = Hi95, fill = "95% CI"), alpha = 0.1) +
  geom_ribbon(data = forecast_data_covariate_sarima, aes(date, ymin = Lo80, ymax = Hi80, fill = "80% CI"), alpha = .4) +
  geom_xspline(data = fitted_covariate, aes(date, y = fitted, color = "Fitted")) +
  geom_xspline(data = forecast_data_covariate_sarima, aes(date, PointForecast, colour = "Predicted human incidence")) +
  geom_xspline(aes(date, human_incidence, color = "Actual human incidence")) +
  theme_light() +
  theme(axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        legend.position = "bottom",
                plot.title = element_text(color = "black", hjust = .5)
        ) +
  xlab("Year") +
  ylab("Human Incidence") +
  scale_color_manual(
    values = c(
      "Predicted human incidence" = "red",
      "Actual human incidence" = "black",
      'Fitted' = 'blue'
    )
  ) +
  scale_fill_manual(
    values = c(
      "95% CI" = "blue",
      "80% CI" = "blue"
    ),
    guide = guide_legend(
      override.aes = list(alpha = c(0.25, 1))
    )
  ) +
  labs(col = NULL, fill = NULL)
forecast_plot_covariate_sarima

dev.off()
ggsave(
  "images/forecast_plot_covariate_sarima.png",
  width = 10,
  height = 6,
  dpi = 1e3
)

model_sarima_covariates_coefficients <-
  tidy(model_sarima_covariates) |>
  select(-.model) %>%
  as_tibble() %>%
  group_by(term) 


# Accuracy
test_acc2 <- test |>
  as.data.frame() %>%
  mutate(date = rownames(.) |> ymd() |> yearmonth()) |>
  as_tsibble() |>
  select(-animal_incidence)

model_sarima_covariate_accuracy <-
  accuracy(
    model_sarima_covariates |>
      forecast(new_data = test_tsibble) ,
    test_acc,
    measures = list(
      point_accuracy_measures,
      interval_accuracy_measures,
      distribution_accuracy_measures
    )
  )

model_sarima_accuracy2 <- model_sarima_accuracy |> 
    mutate(model = "Model without Covariate") |> 
  select(-.model, -.type) |> 
  select(model, RMSE, MAE, MAPE)

model_sarima_covariate_accuracy2 <- model_sarima_covariate_accuracy |> 
  mutate(model = "Model with Covariate") |> 
  select(-.model, -.type) |> 
  select(model, RMSE, MAE, MAPE)

metrics_df <- rbind(model_sarima_accuracy2, model_sarima_covariate_accuracy2)
write.csv(metrics_df, "metrics_df.csv", row.names = F)

forecast_plot_human_sarima_title <- forecast_plot_human_sarima +
  ggtitle("Forecast without exogenous variable")

dev.off()
ggsave(
  "images/forecast_plot_human_sarima_title.png",
  width = 10,
  height = 6,
  dpi = 1e3
)


# Creating the time series with all data -------------------------------------------------
# We chose the time series with covariate because it was better based on the RMSE, MAPE from the metrics.df
# Time series so that we can be able to forecast 2023.
set.seed(123)
full.model <- df_xts |> 
  as.data.frame() %>%
  mutate(date = row.names(.) |>
           zoo::as.yearmon() |>
           yearmonth()) |>
  as_tsibble() |>
  model(ARIMA(
    human_incidence ~ animal_incidence,
    ic = "aic",
    stepwise = T
  )) |>
  report()

fitted_full <- augment(full.model) |> 
  as.data.frame() %>%
  select(date, fitted =.fitted, residuals = .resid) |> 
  mutate(date = ym(date) 
  ) 

original_data2 <-   df_xts |>
  rbind(forecast.df) |> 
  as.data.frame() %>%
  mutate(date = row.names(.) |> ymd())

test_tsibble <- forecast.df |> 
  as.data.frame() %>%
  mutate(date = rownames(.) |> ymd() |> yearmonth()) |> 
  as_tsibble() |>
  select(-human_incidence)


CI_full <- full.model |>
  forecast(new_data = test_tsibble) |> 
  hilo()

CI_full80 <- CI_full$`80%` |> 
  lapply(FUN = first) |> 
  unlist() |>
  matrix(ncol = 3, byrow = T) |> 
  as.data.frame() |> 
  select(-V3) |> 
  setNames(c("Lo80", "Hi80"))


CI_full95 <- CI_full$`95%` |> 
  lapply(FUN = first) |> unlist() |>
  matrix(ncol = 3, byrow = T) |> 
  as.data.frame() |> 
  select(-V3) |> 
  setNames(c("Lo95", "Hi95"))

forecast_data_full_sarima <-
  data.frame(
    date = row.names(as.data.frame(forecast.df)) |>
      ymd(),
    PointForecast = CI_full$.mean,
    CI_full80,
    CI_full95
  )



forecast_plot_full_sarima <- ggplot(original_data2) +
  geom_ribbon(data = forecast_data_full_sarima, aes(date, ymin = Lo95, ymax = Hi95, fill = "95% CI"), alpha = 0.1) +
  geom_ribbon(data = forecast_data_full_sarima, aes(date, ymin = Lo80, ymax = Hi80, fill = "80% CI"), alpha = .4) +
  geom_xspline(data = fitted_full, aes(date, y = fitted, color = "Fitted")) +
  geom_xspline(data = forecast_data_full_sarima, aes(date, PointForecast, colour = "Predicted human incidence")) +
  geom_xspline(aes(date, human_incidence, color = "Actual human incidence")) +
  theme_light() +
  theme(axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        legend.position = "bottom",
        plot.title = element_text(color = "black", hjust = .5)
  ) +
  xlab("Year") +
  ylab("Human Incidence") +
  scale_color_manual(
    values = c(
      "Predicted human incidence" = "red",
      "Actual human incidence" = "black",
      'Fitted' = 'blue'
    )
  ) +
  scale_fill_manual(
    values = c(
      "95% CI" = "blue",
      "80% CI" = "blue"
    ),
    guide = guide_legend(
      override.aes = list(alpha = c(0.25, 1))
    )
  ) +
  labs(col = NULL, fill = NULL)
forecast_plot_full_sarima

dev.off()
ggsave(
  "images/forecast_plot_full_sarima.png",
  width = 15,
  height = 6,
  dpi = 1e3
)

full.model_coefficients <-
  tidy(full.model) |>
  select(-.model) %>%
  as_tibble() %>%
  group_by(term)

# Forecasted and Actual data
fore.actual <- forecast_data_full_sarima |> 
  mutate(Actual = as.vector(forecast.df$human_incidence),
         Date = as.Date(date) |> 
           zoo::as.yearmon() |>
           yearmonth()) |> 
  select(Date = Date, 
         Forecasted = PointForecast,
         Actual,
         `Lower 95% CI` = Lo95,
         `Upper 95% CI` = Hi95,
         ) |> 
  mutate(across(where(is.numeric), ~as.numeric(round(., 3)))) 

# Supplementary plots --------------------------------------------------------------------


forecast_plot_covariate_sarima_title <- forecast_plot_covariate_sarima +
  ggtitle("Forecast with exogenous variable")

dev.off()
ggsave(
  "images/forecast_plot_covariate_sarima_title.png",
  width = 13,
  height = 6,
  dpi = 1e3
)

require(patchwork)
all <- forecast_plot_human_sarima/forecast_plot_covariate_sarima
all
dev.off()
ggsave(
  "images/all_without_title_sarima.png",
  width = 13,
  height = 6,
  dpi = 1e3
)

all_title <- forecast_plot_human_sarima_title/forecast_plot_covariate_sarima_title
all_title
dev.off()
ggsave(
  "images/all_with_title_sarima.png",
  width = 13,
  height = 6,
  dpi = 1e3
)




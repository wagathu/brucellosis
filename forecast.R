
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
df2022_human <- fread("human_2022_brucellosis.csv")
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

# Cleaning df2022 data -------------------------------------------------------------------

## Human
df22_human <- df2022_human |>
  select(periodname, Brucellosis, `MOH 706_Brucella`) |>
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

df22_animal_human <- df22 |> 
  select(date, animal_incidence) |> 
  merge(df22_human, by = "date")


df_complete <- df |> 
  select(-V1) |> 
  mutate(date = ymd(date)) |> 
  rbind(df22_animal_human)

# Testing for stationary ---------------------------------------------------------------------------

# Test for human incidence
adf.test(df_complete$human_incidence)

# Test for animal incidence
adf.test(df_complete$animal_incidence)

# Difference for stationary ------------------------------------------------------------

df2 <- df_complete |> 
  mutate(date = as.Date(date),
         animal_incidence = lag(animal_incidence, 3)
         ) |> 
  na.omit()

df_xts <- as.xts(x = cbind(df2$human_incidence, df2$animal_incidence), order.by = df2$date) |> 
  setNames(c("human_incidence", "animal_incidence"))


# Differencing ---------------------------------------------------------------------------

ts_diff <- diff(df_xts) |> na.omit()

# Splitting ------------------------------------------------------------------------------

train <- window(df_xts, start = as.Date("2014-04-01"), end = "2021-12-01")
test <- window(df_xts, start = as.Date("2022-01-01"), end = as.Date("2022-12-01"))

# Plotting -------------------------------------------------------------------------------

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

dev.off()
ggsave(
  "images/diff_animal.png",
  width = 10,
  height = 6,
  dpi = 1e3
)

# ACF and PACF plots ---------------------------------------------------------------------

acf(train$human_incidence)
pacf(train$human_incidence)

# Fitting arima model --------------------------------------------------------------------

# ARIMA for human incidence alone
model_arima_human <- auto.arima(train$human_incidence, d = 1)

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

# # ARIMA now with animal incidence
model_arima_covariates <- auto.arima(train$human_incidence,
                                     d = 1,
                     xreg = train$animal_incidence
                     )

# Forecasting for time series with covariate ----------------------------------------------------------------------------

forecast_data <-
  forecast::forecast(model_arima_covariates, xreg = test[, 2]) |>
  as.data.frame() |>
  as.data.frame() |>
  mutate(date = row.names(as.data.frame(test)) |> ymd()) %>%
  setNames(c("PointForecast", "Lo80", "Hi80", "Lo95", "Hi95", "date"))

original_data <-   df_xts |>
  as.data.frame() %>%
  mutate(date = row.names(.) |> ymd())

forecast_plot <- ggplot(original_data) +
  geom_ribbon(data = forecast_data, aes(date, ymin = Lo95, ymax = Hi95, fill = "95% CI"), alpha = 0.25) +
  geom_ribbon(data = forecast_data, aes(date, ymin = Lo80, ymax = Hi80, fill = "80% CI"), alpha = 1) +
  geom_xspline(data = forecast_data, aes(date, PointForecast, colour = "Forecasted human incidence")) +
  geom_xspline(aes(date, human_incidence, color = "Actual human incidence")) +
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
      "Actual human incidence" = "black"
    )
  ) +
  scale_fill_manual(
    values = c(
      "95% CI" = "tomato",
      "80% CI" = "tomato"
    ),
        guide = guide_legend(
      override.aes = list(alpha = c(0.25, 1))
    )
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

    
# Forecasting for time series with no covariate (human alone) ----------------------------------------------------------------------------

forecast_data_human <-
  forecast::forecast(model_arima_human, h = 12) |>
  as.data.frame() |>
  mutate(date = row.names(as.data.frame(test)) |> ymd()) %>%
  setNames(c("PointForecast", "Lo80", "Hi80", "Lo95", "Hi95", "date"))

original_data <-   df_xts |>
  as.data.frame() %>%
  mutate(date = row.names(.) |> ymd())

forecast_plot_human <- ggplot(original_data) +
  geom_ribbon(data = forecast_data_human, aes(date, ymin = Lo95, ymax = Hi95, fill = "95% CI"), alpha = 0.25) +
  geom_ribbon(data = forecast_data_human, aes(date, ymin = Lo80, ymax = Hi80, fill = "80% CI"), alpha = 1) +
  geom_xspline(data = forecast_data_human, aes(date, PointForecast, colour = "Forecasted human incidence")) +
  geom_xspline(aes(date, human_incidence, color = "Actual human incidence")) +
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
      "Actual human incidence" = "black"
    )
  ) +
  scale_fill_manual(
    values = c(
      "95% CI" = "tomato",
      "80% CI" = "tomato"
    ),
        guide = guide_legend(
      override.aes = list(alpha = c(0.25, 1))
    )
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

# ARIMA with seasonal component ----------------------------------------------------------


model_sarima <- train |>
  as.data.frame() %>%
  mutate(date = row.names(.) |>
           zoo::as.yearmon() |>
           yearmonth()) |>
  as_tsibble() |>
  model(
    ARIMA(human_incidence)
  ) |>
  report()

CI <-  hilo(forecast::forecast(model_sarima, h = "12 months")) |> 
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
  geom_ribbon(data = forecast_data_human_sarima, aes(date, ymin = Lo95, ymax = Hi95, fill = "95% CI"), alpha = 0.25) +
  geom_ribbon(data = forecast_data_human_sarima, aes(date, ymin = Lo80, ymax = Hi80, fill = "80% CI"), alpha = 1) +
  geom_line(data = forecast_data_human_sarima, aes(date, PointForecast, colour = "Forecasted human incidence")) +
  geom_line(aes(date, human_incidence, color = "Actual human incidence")) +
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
      "Actual human incidence" = "black"
    )
  ) +
  scale_fill_manual(
    values = c(
      "95% CI" = "tomato",
      "80% CI" = "tomato"
    ),
        guide = guide_legend(
      override.aes = list(alpha = c(0.25, 1))
    )
  ) +
  labs(col = NULL, fill = NULL)
plotly::ggplotly( forecast_plot_human_sarima)
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
  


model_sarima_accuracy <- accuracy(forecast::forecast(model_sarima, h = "12 months"), test_acc,
  measures = list(
    point_accuracy_measures,
    interval_accuracy_measures,
    distribution_accuracy_measures
  )
)

# ARIMA with seasonal component And Covariate --------------------------------------------

model_sarima_covariates <- train |>
  as.data.frame() %>%
  mutate(date = row.names(.) |>
           zoo::as.yearmon() |>
           yearmonth()) |>
  as_tsibble() |>
  model(
    ARIMA(human_incidence ~ animal_incidence)
  ) |>
  report()

#>  The ARIMA model is specified as (0,1,0)(2,0,0)[12], which indicates a seasonal
#>  ARIMA model with seasonal period 12 (monthly data). Specifically, it has a seasonal
#>  moving average component with lags at 1 and 2, and no non-seasonal differences
#>  or non-seasonal autoregressive terms.

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
  geom_ribbon(data = forecast_data_covariate_sarima, aes(date, ymin = Lo95, ymax = Hi95, fill = "95% CI"), alpha = 0.25) +
  geom_ribbon(data = forecast_data_covariate_sarima, aes(date, ymin = Lo80, ymax = Hi80, fill = "80% CI"), alpha = 1) +
  geom_line(data = forecast_data_covariate_sarima, aes(date, PointForecast, colour = "Forecasted human incidence")) +
  geom_line(aes(date, human_incidence, color = "Actual human incidence")) +
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
      "Actual human incidence" = "black"
    )
  ) +
  scale_fill_manual(
    values = c(
      "95% CI" = "tomato",
      "80% CI" = "tomato"
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
  select(model, ME, RMSE, MPE, MAPE)

model_sarima_covariate_accuracy2 <- model_sarima_covariate_accuracy |> 
  mutate(model = "Model with Covariate") |> 
  select(-.model, -.type) |> 
  select(model, ME, RMSE, MPE, MAPE)

metrics_df <- rbind(model_sarima_accuracy2, model_sarima_covariate_accuracy2)





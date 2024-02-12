model_arima_covariates <- train |>
  as.data.frame() %>%
  mutate(date = row.names(.) |>
           zoo::as.yearmon() |>
           yearmonth()) |>
  as_tsibble() |>
  model(
    ARIMA(human_incidence)
  ) |>
  report()




CI <-  hilo(forecast::forecast(model_arima_covariates, h = 12)) |> 
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

forecast_data_human <-
  data.frame(
    date = row.names(as.data.frame(test)) |>
      ymd(),
    PointForecast = CI$.mean,
    CI80,
    CI95
  )



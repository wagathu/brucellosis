# Importing Packages ------------------------------------------------------

if (require(pacman))
{
  p_load(
    tidyverse,
    tseries,
    data.table,
    scales,
    zoo,
    forecast,
    sf,
    patchwork,
    grid,
    fable,
    patchwork,
    xts,
    feasts,
    cowplot,
    broom,
    kableExtra,
    readxl,
    stringi,
    stringr,
    rKenyaCensus,
    knitr,
    purrr,
    RColorBrewer,
    tscount,
    lme4
  )
}

# Importing data ----------------------------------------------------------

p09 <- read_excel("Projections.xlsx",
                  sheet = "2009")
p19 <- read_excel("Projections.xlsx",
                  sheet = "2019")
df_incidence <- fread("all_bruc_incidence.csv")
df_numbers <- fread("all_bruc_pop.csv")
shp <- st_read("shapefiles/County.shp", quiet = T)
eco_zones <- fread("eco_zones_county.csv")

shp <- st_read("shapefiles/County.shp", quiet = T)

individual <- fread("individual_incidence_cases.csv")
combined <- fread("combined_incidence_cases.csv")
individual_county <- fread("df_tot_cases_spatial_month.csv")
combined_county <- fread("df_spatial_cum_month.csv")


# Complete
county_indivi_complete <- fread("df_1_complete.csv") |> 
  mutate(hum_cases = round(hum_cases))
county_comb_complete <- fread("df_cum_complete.csv") |> 
  mutate(hum_cases = round(hum_cases))

# Creating the proportion ----------------------------------------------------------------

# Individual: Incidence
# The cattle and goat incidence were calculated 10M per population
df_indivi_inci <- county_indivi_complete |> 
  mutate(across(c(catt_incidence, goat_incidence), ~ round(.*10))) |> 
  mutate(human_incidence = round(human_incidence * 10)) |> 
  select(date, county, contains("incidence")) |>
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)))

# Individual: Proportion
df_indivi_inci_prop <- county_indivi_complete |> 
  mutate(across(c(catt_incidence, goat_incidence), ~ .*10)) |> 
  mutate(human_incidence = human_incidence/1e3) |> 
  select(date, county, contains("incidence")) |>
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)))

# Individual: Cases
df_indivi_cases <- county_indivi_complete |> 
  select(date, county, contains("cases")) |>
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)))


# Combined : Incidence
df_comb_inci <- county_comb_complete |> 
  mutate(human_incidence = round(human_incidence*10),
         animal_incidence = round(animal_incidence*10)
         ) |> 
  select(date, county, animal_incidence, human_incidence) |> 
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)))

# combined: Proportion
df_comb_inci_prop <- county_comb_complete |> 
  mutate(human_incidence = human_incidence/1e3,
         animal_incidence = animal_incidence*10
         ) |> 
  select(date, county, animal_incidence, human_incidence) |> 
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)))

# Combined: Cases
df_comb_cases <- county_comb_complete |> 
  select(date, county, animal_cases, hum_cases) 

# Individual Incidence
indivi_models <- function(df, type, max_lag, ...) {
  # Type is either prop or incidence
  result_df <- tibble()
  if (type == "prop") {
    for (lag_value in 0:max_lag) {
      df_lagged <- df |>
        as_tibble() %>%
        mutate_at(
          vars(
            catt_incidence,
            goat_incidence
          ),
          list(~ lag(., n = lag_value))
        ) |>
        na.omit() |>
        mutate(date = as.Date(date))
      
      mod <-
        glmer(
          human_incidence ~ catt_incidence + goat_incidence + 
            (1 |
               county),
          data = df_lagged,
          family = binomial(link = "logit"),
          nAGQ = 0
        )
      
      mod_results <- broom.mixed::tidy(mod) %>%
        as_tibble() %>%
        mutate(
          term = case_when(
            term == "goat_incidence" ~ "Goat Incidence",
            term == "catt_incidence" ~ "Cattle incidence",
            term == "sd__(Intercept)" ~ "Sd - Random Intercept",
            TRUE ~ as.character(term)
          ),
          effect = case_when(
            effect == "ran_pars" ~ "Random",
            TRUE ~ effect
          ),
          variable = term
        ) |> 
        rename(`log odds` = estimate) |> 
        mutate(odds = exp(`log odds`),
               odds = ifelse(effect == "Random", NA, odds)
               ) |> 
        select(1, 8,4, 9, 5:8) |> 
        group_by(variable) %>%
        mutate(
          conf_low = min(`log odds` - std.error * 1.645),
          conf_high = max(`log odds` + std.error * 1.645)
        ) %>%
        mutate(lag = lag_value)
      
      metrics <- broom.mixed::glance(mod) %>%
        select(nobs, AIC, BIC)
      
      mod_results <- bind_cols(mod_results, metrics) |>
        mutate(across(
          c(
            `log odds`,
            odds,
            std.error,
            statistic,
            p.value,
            conf_low,
            conf_high,
            AIC,
            BIC
          ),
          ~ round(., 3)
        )) |>
        mutate(significance = ifelse(conf_low * conf_high > 0, "Significant", "Not Significant")) 
      cat(paste("Runnning model for lag", lag_value), "\n")
      result_df <- bind_rows(result_df, mod_results)
    }
    
  } else if (type == "inci") {
    for (lag_value in 0:max_lag) {
      df_lagged <- df |>
        as_tibble() %>%
        mutate_at(
          vars(
            catt_incidence,
            goat_incidence
          ),
          list(~ lag(., n = lag_value))
        ) |>
        na.omit() |>
        mutate(date = as.Date(date))
      
      mod <-
         glmer.nb(
          human_incidence ~ catt_incidence + goat_incidence +
            (1 |
               county),
          data = df_lagged,
          nAGQ = 0
        )
      
      mod_results <- broom.mixed::tidy(mod) %>%
        as_tibble() %>%
        mutate(
          term = case_when(
            term == "goat_incidence" ~ "Goat Incidence",
            term == "catt_incidence" ~ "Cattle incidence",
            term == "sd__(Intercept)" ~ "Sd - Random Intercept",
            TRUE ~ as.character(term)
          ),
          effect = case_when(
            effect == "ran_pars" ~ "Random",
            TRUE ~ effect
          ),
          variable = term
        ) |> 
        rename(`log IRR` = estimate) |> 
        mutate(IRR = exp(`log IRR`),
               IRR = ifelse(effect == "Random", NA, IRR)
               ) |> 
        select(1, 8,4, 9, 5:8) |> 
        group_by(variable) %>%
        mutate(
          conf_low = min(`log IRR` - std.error * 1.645),
          conf_high = max(`log IRR` + std.error * 1.645)
        ) %>%
        mutate(lag = lag_value)

      metrics <- broom.mixed::glance(mod) %>%
        select(nobs, AIC, BIC)
      
      mod_results <- bind_cols(mod_results, metrics) |>
        mutate(across(
          c(
            `log IRR`,
            IRR,
            std.error,
            statistic,
            p.value,
            conf_low,
            conf_high,
            AIC,
            BIC
          ),
          ~ round(., 4)
        )) |>
        mutate(significance = ifelse(conf_low * conf_high > 0, "Significant", "Not Significant"))
      cat(paste("Runnning model for lag", lag_value), "\n")
      result_df <- bind_rows(result_df, mod_results)
    }
    
  }
  
  return(result_df)

}

df_indivi_model_inci = indivi_models(df = df_indivi_inci, type = "inci", max_lag = 3)
#df_indivi_model_prop = indivi_models(df = df_indivi_inci_prop, type = "prop", max_lag = 3)
write.csv(df_indivi_model_inci, "df_indivi_model_inci.csv")

# Combined Cases
comb_models <- function(df, type, max_lag, ...) {
  # Type is either prop or incidence
  result_df <- tibble()
  if (type == "prop") {
    for (lag_value in 0:max_lag) {
      df_lagged <- df |>
        as_tibble() %>%
        mutate_at(
          vars(
            animal_incidence,
          ),
          list(~ lag(., n = lag_value))
        ) |>
        na.omit() |>
        mutate(date = as.Date(date))
      
      mod <-
        glmer(
          human_incidence ~ animal_incidence + 
            (1 |
               county),
          data = df_lagged,
          family = binomial(link = "logit"),
          nAGQ = 0
        )
      
      mod_results <- broom.mixed::tidy(mod) %>%
        as_tibble() %>%
        mutate(
          term = case_when(
            term == "animal_incidence" ~ "Animal incidence",
            term == "sd__(Intercept)" ~ "Sd - Random Intercept",
            TRUE ~ as.character(term)
          ),
          effect = case_when(
            effect == "ran_pars" ~ "Random",
            TRUE ~ effect
          ),
          variable = term
        ) |> 
        rename(`log odds` = estimate) |> 
        mutate(odds = exp(`log odds`),
               odds = ifelse(effect == "Random", NA, odds)
               ) |> 
        select(1, 8,4, 9, 5:8) |> 
        group_by(variable) %>%
        mutate(
          conf_low = min(`log odds` - std.error * 1.645),
          conf_high = max(`log odds` + std.error * 1.645)
        ) %>%
        mutate(lag = lag_value)
      
      metrics <- broom.mixed::glance(mod) %>%
        select(nobs, AIC, BIC)
      
      mod_results <- bind_cols(mod_results, metrics) |>
        mutate(across(
          c(
            `log odds`,
            odds,
            std.error,
            statistic,
            p.value,
            conf_low,
            conf_high,
            AIC,
            BIC
          ),
          ~ round(., 3)
        )) |>
        mutate(significance = ifelse(conf_low * conf_high > 0, "Significant", "Not Significant")) 
      cat(paste("Runnning model for lag", lag_value), "\n")
      result_df <- bind_rows(result_df, mod_results)
    }
    
  } else if (type == "inci") {
    for (lag_value in 0:max_lag) {
      df_lagged <- df |>
        as_tibble() %>%
        mutate_at(
          vars(
         animal_incidence
          ),
          list(~ lag(., n = lag_value))
        ) |>
        na.omit() |>
        mutate(date = as.Date(date))
      
      mod <-
         glmer.nb(
          human_incidence ~ animal_incidence +
            (1 |
               county),
          data = df_lagged,
          nAGQ = 0
        )
      
      mod_results <- broom.mixed::tidy(mod) %>%
        as_tibble() %>%
        mutate(
          term = case_when(
            term == "animal_incidence" ~ "Animal incidence",
            term == "sd__(Intercept)" ~ "Sd - Random Intercept",
            TRUE ~ as.character(term)
          ),
          effect = case_when(
            effect == "ran_pars" ~ "Random",
            TRUE ~ effect
          ),
          variable = term
        ) |> 
        rename(`log IRR` = estimate) |> 
        mutate(IRR = exp(`log IRR`),
               IRR = ifelse(effect == "Random", NA, IRR)
               ) |> 
        select(1, 8,4, 9, 5:8) |> 
        group_by(variable) %>%
        mutate(
          conf_low = min(`log IRR` - std.error * 1.645),
          conf_high = max(`log IRR` + std.error * 1.645)
        ) %>%
        mutate(lag = lag_value)
      
      metrics <- broom.mixed::glance(mod) %>%
        select(nobs, AIC, BIC)
      
      mod_results <- bind_cols(mod_results, metrics) |>
        mutate(across(
          c(
            `log IRR`,
            IRR,
            std.error,
            statistic,
            p.value,
            conf_low,
            conf_high,
            AIC,
            BIC
          ),
          ~ round(., 4)
        )) |>
        mutate(significance = ifelse(conf_low * conf_high > 0, "Significant", "Not Significant"))
      cat(paste("Runnning model for lag", lag_value), "\n")
      result_df <- bind_rows(result_df, mod_results)
    }
    
  }
  
  return(result_df)

}

df_combined_model_inci = comb_models(df = df_comb_inci, type = "inci", max_lag = 3)
#df_combined_model_prop = comb_models(df = df_comb_inci_prop, type = "prop", max_lag = 3)

# Selecting counties with many rows ------------------------------------------------------

arranged_table1 <- df_indivi_inci %>%
  count(county) %>%
  arrange(desc(n)) |> 
  filter(n > 2)
counties <- arranged_table1$county

arranged_table2 <- df_comb_inci %>%
  count(county) %>%
  arrange(desc(n)) |> 
  filter(n > 2)

counties <- arranged_table1$county
counties2 <- arranged_table2$county

sub_indivi_inci <- df_indivi_inci |> 
  filter(county %in% counties)

sub_indivi_inci_prop <- df_indivi_inci_prop |> 
  filter(county %in% counties)

sub_comb_inci <- df_comb_inci |> 
  filter(county %in% counties2)

sub_comb_inci_prop <- df_comb_inci_prop |> 
  filter(county %in% counties2)

# Model for the subset data (individual incidence)
subset_indivi_inci <- indivi_models(df = sub_indivi_inci, type = "inci", max_lag = 3)
subset_indivi_prop <- indivi_models(df = sub_indivi_inci_prop, type = "prop", max_lag = 3)


# Model for the subset data (combined incidence)
subset_comb_inci = comb_models(df = sub_comb_inci, type = "inci", max_lag = 3)
subset_comb_prop = comb_models(df = sub_comb_inci_prop, type = "prop", max_lag = 3)

# County with the highest data

bomet_indivi <- df_indivi_inci |> 
  filter(county == "Bomet")

bomet_comb <- df_comb_inci |> 
  filter(county == "Bomet")


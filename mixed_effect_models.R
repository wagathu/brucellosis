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


# Creating the proportion ----------------------------------------------------------------

df_indivi_inci <- individual_county |> 
  mutate(across(c(catt_incidence, cam_incidence, goat_incidence), ~ ./1e6)) |> 
  mutate(human_incidence = human_incidence/1e3) |> 
  select(date, county, contains("incidence")) |>
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)))

df_indivi_cases <- individual_county |> 
  mutate(across(c(catt_incidence, cam_incidence, goat_incidence), ~ ./1e6)) |> 
  mutate(human_incidence = human_incidence/1e3) |> 
  select(date, county, contains("cases")) |>
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)))


# Combined
df_comb_inci <- combined_county |> 
  mutate(human_incidence = human_incidence/1e3,
         animal_incidence = animal_incidence/1e6
         ) |> 
  select(date, county, animal_incidence, human_incidence) |> 
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)))


df_comb_cases <- combined_county |> 
  mutate(human_incidence = human_incidence/1e3,
         animal_incidence = animal_incidence/1e6
         ) |> 
  select(date, county, animal_cases, hum_cases) |> 
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)))



# Lags for individual incidence -----------------------------------------------------------------------------------

# lag 1
lag1_indivi_inci <- df_indivi_inci |> 
  group_by(county) |> 
  arrange(date) |> 
    mutate_at(vars(catt_incidence, cam_incidence, goat_incidence, shp_incidence),
            list( ~ lag(., n = 1)))
  # filter(!is.na(catt_incidence) & !is.na(cam_incidence) & !is.na(goat_incidence) & !is.na(shp_incidence) )

# Lag 2
lag2_indivi_inci <- df_indivi_inci |> 
    group_by(county) |> 
  arrange(date) |> 
    mutate_at(vars(catt_incidence, cam_incidence, goat_incidence, shp_incidence),
            list( ~ lag(., n = 2)))

# lag 3
lag3_indivi_inci <- df_indivi_inci |> 
    group_by(county) |> 
  arrange(date) |> 
    mutate_at(vars(catt_incidence, cam_incidence, goat_incidence, shp_incidence),
            list( ~ lag(., n = 3)))

# lag 4
lag4_indivi_inci <- df_indivi_inci |> 
    group_by(county) |> 
  arrange(date) |> 
    mutate_at(vars(catt_incidence, cam_incidence, goat_incidence, shp_incidence),
            list( ~ lag(., n = 4)))

# Lags for individual cases -----------------------------------------------------------------------------------

# lag 1
lag1_indivi_cases <- df_indivi_cases |> 
    group_by(county) |> 
  arrange(date) |> 
    mutate_at(vars(catt_cases, cam_cases, goat_cases, shp_cases),
            list( ~ lag(., n = 1)))

# Lag 2
lag2_indivi_cases <- df_indivi_cases |> 
    group_by(county) |> 
  arrange(date) |> 
    mutate_at(vars(catt_cases, cam_cases, goat_cases, shp_cases),
            list( ~ lag(., n = 2)))

# lag 3
lag3_indivi_cases <- df_indivi_cases |> 
    group_by(county) |> 
  arrange(date) |> 
    mutate_at(vars(catt_cases, cam_cases, goat_cases, shp_cases),
            list( ~ lag(., n = 3)))

# lag 4
lag4_indivi_cases <- df_indivi_cases |> 
    group_by(county) |> 
  arrange(date) |> 
    mutate_at(vars(catt_cases, cam_cases, goat_cases, shp_cases),
            list( ~ lag(., n = 4)))

# Lags for combined incidence -----------------------------------------------------------------------------------

# lag 1
lag1_comb_inci <- df_comb_inci |>
    group_by(county) |> 
  arrange(date) |> 
  mutate(animal_incidence = lag(animal_incidence, n = 1))


# Lag 2
lag2_comb_inci <- df_comb_inci |>
    group_by(county) |> 
  arrange(date) |> 
  mutate(animal_incidence = lag(animal_incidence, n = 2))

# lag 3
lag3_comb_inci <- df_comb_inci |> 
    group_by(county) |> 
  arrange(date) |> 
    mutate(animal_incidence = lag(animal_incidence, n = 3))

# lag 4
lag4_comb_inci <- df_comb_inci |> 
    group_by(county) |> 
  arrange(date) |> 
    mutate(animal_incidence = lag(animal_incidence, n = 4))

# Lags for individual cases -----------------------------------------------------------------------------------

# lag 1
lag1_comb_cases <- df_comb_cases |>
  mutate(animal_cases = lag(animal_cases, n = 1))

# Lag 2
lag2_comb_cases <- df_comb_cases |>
  mutate(animal_cases = lag(animal_cases, n = 2))

# lag 3
lag3_comb_cases <- df_comb_cases |> 
    mutate(animal_cases = lag(animal_cases, n = 3))

# lag 4
lag4_comb_cases <- df_comb_cases |> 
    mutate(animal_cases = lag(animal_cases, n = 4))

# Models ---------------------------------------------------------------------------------

# Individual animal incidence
## No mixed effect binomial
binom_fixed_indi <- glm(human_incidence ~ catt_incidence + goat_incidence + cam_incidence + shp_incidence,
                        family = binomial(link = "logit"),
                        data = lag1_indivi_inci
                        )
summary(binom_fixed_indi)

## mixed effect binomial
binom_mixed_indi <- glmer(human_incidence ~ catt_incidence + goat_incidence + cam_incidence + 
                            (1 | county + date), data = lag1_indivi_inci, family = binomial(link = "logit") )
summary(binom_mixed_indi)

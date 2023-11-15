
# Importing Packages ------------------------------------------------------

if (require(pacman)) {
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
    stringr,
    stringi,
    recipes,
    caret
  )
}

# Importing data ----------------------------------------------------------

df_incidence <- fread("all_bruc_incidence.csv")
df_numbers <- fread("all_bruc_pop.csv")
shp <- st_read("shapefiles/County.shp", quiet = T)
eco_zones <- fread("eco_zones_county.csv")

# Preprocessing data ------------------------------------------------------

# 1. Selecting the required columns
df_pop <- df_incidence |> 
  select(date, county, contains("pop")) |> 
  distinct()

df_ <- df_incidence |> 
  group_by() |> 
  filter(!is.na(date)) |>
  select(-y,-diseases,-contains("incidence")) |> 
  group_by(date, county) |> 
  summarise(across(contains("cases"), ~sum(., na.rm = T))) |> 
  merge(df_pop, by = c("date", "county"))

df_1 <- df_ |> 
  mutate(
    human_incidence = (hum_cases / hum_pop),
    catt_incidence = (catt_cases / catt_pop) * 1000000,
    cam_incidence = (cam_cases / cam_pop) * 1000000,
    goat_incidence = (goat_cases / goat_pop) * 1000000,
    shp_incidence = (shp_cases / sheep_pop) * 1000000 
  ) |>
  ungroup()

# Replacing NaN cases (0/0) with 0
df_1 <- df_1 |>
  mutate(across(is.numeric, ~ ifelse(is.na(.), 0, .))) |>
  select(date, county, contains("incidence"), hum_pop)

df_2 <- df_ |>
  rowwise() |>
  mutate(animal_cases = sum(catt_cases, goat_cases, cam_cases, shp_cases),
         animal_pop = sum(catt_pop, goat_pop, cam_pop, sheep_pop),
         animal_incidence = (animal_cases/animal_pop)* 100000,
         human_incidence = (hum_cases/hum_pop)
  ) |> 
  select(date, county, animal_incidence, human_incidence)

# Cleaning the ecological zones -------------------------------------------

setdiff(eco_zones$county, df_1$county)
eco_zones <- eco_zones |>
  mutate(county = ifelse(county == "Elgeiyo Marakwet", "Elgeyo Marakwet", county))

# Cleaning the ecological zones
df_zones = eco_zones |>
  mutate(
    split1 =  str_split(eco_zones$eco_zone, ";", simplify = T)[, 1] |> str_squish(),
    split2 =  str_split(eco_zones$eco_zone, ";", simplify = T)[, 2] |> str_squish(),
    farming = ifelse(split1 == "Farming" |
                       split2 == "Farming", 1, 0),
    agropastoral = ifelse(split1 == "Agropastoral" |
                            split2 == "Agropastoral", 1, 0),
    pastoral = ifelse(split1 == "Pastoral" |
                        split2 == "Pastoral", 1, 0),
    fishing = ifelse(split1 == "Fishing" |
                       split2 == "Fishing", 1, 0),
    riverine = ifelse(split1 == "Riverine" |
                        split2 == "Riverine", 1, 0),
  ) |>
  select(-eco_zone,-split1,-split2)

# Creating the zones as factors and defining the reference level
df_zones <- df_zones %>%
  mutate(across(
    c(farming, agropastoral, fishing, riverine, pastoral),
    ~ relevel(factor(
      .x, levels = c(0, 1), labels = c("No", "Yes")
    ), ref = "No")
  ))



# Merging with the incidence data set
df_full <- df_1 |>
  merge(df_zones, by = "county")

df_full2 <- df_2 |> 
  merge(df_zones, by = 'county')

# Scaling the predictors --------------------------------------------------

df_ <- df_full |> 
  recipe(human_incidence ~.) |>
  step_YeoJohnson(all_double ()) |> 
  step_center(all_numeric_predictors()) |> 
  step_scale(all_numeric_predictors()) |> 
  prep(trainiing = df_full) |> 
  bake(df_full)

# Relationship between human incidence and the predictors ----------------

df_1 |>
  filter(human_incidence < 800) |> 
  filter(goat_incidence < 300) |>
  ggplot(aes(x = goat_incidence)) +
  geom_point(aes(y = human_incidence)) +
  geom_smooth(aes(y = human_incidence), method = "lm", se = F)

# Binomial proportion -----------------------------------------------------

df_full <- df_full |> 
  filter(human_incidence < 1) |> 
  as_tibble()

df_full2 <- df_full2 |> 
  filter(human_incidence < 1) |> 
  as_tibble()

df_full2$human_incidence <- lead(df_full2$human_incidence)

modd <- glm(
  human_incidence ~ 
    catt_incidence +
    goat_incidence +
    cam_incidence +
    shp_incidence +
    farming +
    pastoral +
    fishing +
    agropastoral +
    riverine-1 ,
  family = "binomial",
  data = df_full
)
summary(modd)
modd1.1 <- MASS::stepAIC(modd, direction = "both")
summary(modd1.1)


modd2 <- glm(
  human_incidence ~ 
    animal_incidence +
    farming +
    pastoral +
    fishing +
    agropastoral +
    riverine,
  family = binomial(link = "logit"),
  data = df_full2
)
summary(modd2)
exp(modd2$coefficients) |> round(3)
odds_ci <- exp(confint(modd2))
modd2.1 <- MASS::stepAIC(modd2, direction = "both")
summary(modd2.1)

# Beta Regression
modd3 <- glm(
  human_incidence ~ 
    animal_incidence +
    farming +
    pastoral +
    fishing +
    agropastoral +
    riverine,
  data = df_full
)
summary(modd3)
exp(modd3$coefficients) |> round(4)
odds_ci <- exp(confint(modd3)) |> round(2)

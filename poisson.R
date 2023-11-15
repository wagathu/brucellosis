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

df_1 <- df_incidence  |>
  filter(!is.na(date)) |>
  select(-y,-diseases,-contains("incidence"), -contains("pop")) |>
  group_by(date, county) |>
  summarise(across(where(is.numeric), ~ sum(., na.rm = TRUE)))

# Cleaning the ecological zones -------------------------------------------

setdiff(eco_zones$county, df_1$county)
eco_zones <- eco_zones |>
  mutate(county = ifelse(county == "Elgeiyo Marakwet", "Elgeyo Marakwet", county))

# Cleaning the ecological zones
df_zones = eco_zones |>
  mutate(
    split1 =  str_split(eco_zones$eco_zone, ";", simplify = T)[, 1] |> 
      str_squish() |> 
      str_to_title(),
    split2 =  str_split(eco_zones$eco_zone, ";", simplify = T)[, 2] |> 
      str_squish()  |> 
      str_to_title(),
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
  dplyr::select(-eco_zone,-split1,-split2)

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

df_full$hum_cases <- lead(df_full$hum_cases, n = 5) 

jj = df_full |>
  dplyr::select(-date, -county) |> 
  rowwise() |> 
  mutate(ani_cases = sum(catt_cases, cam_cases, goat_cases, shp_cases))

mod <- glm(round(hum_cases) ~ ani_cases
           +
             
             farming*agropastoral +
             farming * pastoral +
             farming * fishing +
             farming * riverine +
             agropastoral * pastoral +
             agropastoral * fishing +
             agropastoral * riverine +
             pastoral * fishing +
             pastoral * riverine -1
           
           , data = jj, family = "poisson")
summary(mod)

jj = MASS::stepAIC(mod)
summary(jj)
library(lme4)

df_full <- df_full |>
  mutate(month = month.name[month(date)] |> 
           as.factor() 
         
         ) |> 
  rowwise() |> 
  mutate(
    animal_cases = sum(catt_cases, cam_cases, goat_cases, shp_cases)
  )
df_full$month

# Leading the Dependent variable
df_full$hum_cases <- lead(df_full$hum_cases, n = 2) 
df_full2$human_incidence <- lead(df_full2$human_incidence, n = 6) 


df_full <- df_full |> 
  na.omit()

mod <- glm(round(hum_cases) ~ 
             # catt_cases
             # + cam_cases +
             #   goat_cases +
             #   shp_cases +
             #   farming +
             #   agropastoral
             animal_cases + 
             + fishing +
               riverine +
               month +
               pastoral,
             family = "poisson",
             data = df_full)

summary(mod)
mod <- glmer(round(hum_cases) ~ catt_cases
             + cam_cases +
               goat_cases +
               shp_cases +
               farming +
               agropastoral
             + fishing +
               riverine +
               month +
               pastoral +
               (1 | county) +
               pastoral * month,
             family = "poisson",
             data = df_full)
library(glm)
library(MASS)
stepAIC(mod)

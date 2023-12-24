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
    knitr
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

# Population --------------------------------------------------------------

# 2009
colnames(p09) <- p09[1,] #Make the 1st row the header
p09 <- p09[-1, ]  #Remove the 1st row

p09_clean <- p09 |>
  filter(
    !County %in% c(
      "Kenya",
      "Central",
      "Coast",
      "Eastern",
      "Western",
      "N. Eastern",
      "Nyanza",
      "R. Valley"
    )
  ) |> 
  distinct() |>
  select(County, "2014", "2015") 

# 2019
p19_clean <- p19 |>
  select(County, "2020", "2021", "2022", "2023", "2024", "2025") |>
  setNames(c("County", "pop2020", "pop2021", "pop2022", "pop2023", "pop2024", "pop2025")) |> 
  na.omit() |> 
  mutate(across(2:7, ~ as.numeric(str_remove(., ","))),
         across(2:7, ~ . * 1000))

p19_clean2 <- p19_clean |> 
  filter(County != "Kenya") |> 
  pivot_longer(
    cols = -County,
    names_to = "Year",
    values_to = "pop"
  ) |>
  arrange()

# Growth Rate
p19_growth <- p19_clean2 |> 
  group_by(County) |> 
  arrange(County, Year) |> 
  mutate(growth_rate = (pop - lag(pop)) / lag(pop) * 100) |> 
  group_by(County) |>
  summarise(growth_rate = mean(growth_rate, na.rm = T) |> round(2)) 
write.csv(p19_growth, "growthrate.csv")

# 2019 Population
pop <- rKenyaCensus::V1_T2.2 |> 
  select(County, pop2019 = Total) |> 
  filter(!County %in% c("Total")) |>
  arrange(County) |> 
  mutate(County = case_when(
    County == "Elgeyo/Marakwet" ~ "Elgeyo Marakwet",
    County == "Nairobi City" ~ "Nairobi",
    County == "Taita/Taveta" ~ "Taita Taveta",
    County == "Tharaka-Nithi" ~ "Tharaka Nithi",
    TRUE ~ County
  )) |> 
  merge(p19_growth, by = "County") |> 
  mutate(
    pop2018 = round(pop2019 * (1 - (growth_rate / 100))),
    pop2017 = round(pop2018 * (1 - (growth_rate / 100))),
    pop2016 = round(pop2017 * (1 - (growth_rate / 100))),
    pop2015 = round(pop2016 * (1 - (growth_rate / 100))),
    pop2014 = round(pop2015 * (1 - (growth_rate / 100)))
  ) |> 
  merge(p19_clean |> select(1:3), by = "County") |> 
  select(County,
         pop2014,
         pop2015,
         pop2016,
         pop2017,
         pop2018,
         pop2019,
         pop2020,
         pop2021
  ) |>
  pivot_longer(
    cols = -County,
    values_to = "pop"
  ) |> 
  mutate(year = case_when(
    name == "pop2014" ~ 2014,
    name == "pop2015" ~ 2015,
    name == "pop2016" ~ 2016,
    name == "pop2017" ~ 2017,
    name == "pop2018" ~ 2018,
    name == "pop2019" ~ 2019,
    name == "pop2020" ~ 2020,
    name == "pop2021" ~ 2021
  )) |> 
  select(county = County, year, pop)

# Grouping data -----------------------------------------------------------

# Cases per year per county
df_incidence2 <- df_incidence |> 
  #filter(diagnosis == "Lab confirmed") |>
  select(date, 
         county, 
         diseases, 
         diagnosis, 
         contains("cases"), 
         catt_pop, 
         goat_pop, 
         sheep_pop, 
         cam_pop) |>
  mutate(year = year(date)) |> 
  merge(pop, by = c("county", "year")) 

df_tot_cases <- df_incidence2 |>
  group_by(date, county) |>
  summarise(across(contains("cases"), ~ sum(., na.rm = T)))

# Population per year, per county
df_pop <- df_incidence2 |> 
  select(date, county, contains("pop")) %>%
  distinct(.) |>
  as_tibble() |>
  group_by(date, county) %>%
  summarise(across(where(is.numeric), ~unique(.)))

df_1 <- df_tot_cases |>
  merge(df_pop, by = c("date", "county")) |> 
  filter(!is.na(date)) |>
  mutate(
    human_incidence = round((hum_cases / pop) * 1000, 4),
    catt_incidence = round((catt_cases / catt_pop) * 1000000, 4),
    cam_incidence = round((cam_cases / cam_pop) * 1000000, 4),
    goat_incidence = round((goat_cases / goat_pop) * 1000000, 4),
    shp_incidence = round((shp_cases / sheep_pop) * 1000000, 4)
  ) |>
  select(date, county, contains("incidence")) |>
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .))) |> 
  as_tibble() |> 
  arrange(county)


df_cum <- df_tot_cases |>
  merge(df_pop, by = c("date", "county")) |> 
  filter(!is.na(date)) |> 
  rowwise() |> 
  mutate(
    animal_cases = sum(catt_cases, goat_cases, shp_cases, cam_cases, na.rm = T),
    animal_pop = sum(catt_pop, goat_pop, sheep_pop, cam_pop, na.rm = T),
    animal_incidence = round((animal_cases / animal_pop) * 1000000, 4),
    human_incidence = round((hum_cases / pop) * 1000, 4)
  ) |> 
  select(date, county, contains("incidence")) |> 
  as_tibble()

# Cleaning Ecological zones
setdiff(eco_zones$county, df_cum$county)
eco_zones <- eco_zones |>
  mutate(county = ifelse(county == "Elgeiyo Marakwet", "Elgeyo Marakwet", county))
setdiff(eco_zones$county, df_cum$county)

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

# Model -------------------------------------------------------------------

mod_df <- df_full |> 
  as_tibble() |> 
  distinct() |> 
  mutate_at(vars(catt_incidence, cam_incidence, goat_incidence, shp_incidence),
            list( ~ dplyr::lag(., n = 2))) |>
  na.omit() |> 
  mutate(date = as.Date(date))

mod_df |> 
  as_tsibble() |>
  model(
    forecast::tslm(
      human_incidence ~ catt_incidence + cam_incidence + goat_incidence + shp_incidence +
        farming + agropastoral + pastoral + fishing + riverine  
    )
  )



















# Importing Packages ---------------------------------------------------------------------

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
    RColorBrewer
  )
}

# Importing Datasets ---------------------------------------------------------------------

df_22_human <- fread('county_human_2022_brucellosis.csv')
df_22_animal <- read_excel("animal_brucellosis_2022.xlsx")
p09 <- read_excel("Projections.xlsx", sheet = "2009")
p19 <- read_excel("Projections.xlsx", sheet = "2019")
df_incidence <- fread("all_bruc_incidence.csv")
df_numbers <- fread("all_bruc_pop.csv")
shp <- st_read("shapefiles/County.shp", quiet = T)
eco_zones <- fread("eco_zones_county.csv")
shp <- st_read("shapefiles/County.shp", quiet = T)

# Cleaning population data ---------------------------------------------------------------

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
  dplyr::select(County, "2014", "2015") 

# 2019
p19_clean <- p19 |>
  dplyr::select(County, "2020", "2021", "2022", "2023", "2024", "2025") |>
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


p19_clean2_kenya <- p19_clean |> 
  filter(County == "Kenya") |> 
  pivot_longer(
    cols = -County,
    names_to = "Year",
    values_to = "pop"
  ) |>
  arrange() |> 
  dplyr::select(County,Year, pop) |> 
  mutate(Year = str_replace(Year, "pop", "") |> str_squish() |> as.numeric())

write.csv(p19_clean2_kenya, "kenya_pop_2020_2025.csv")

# Growth Rate
p19_growth <- p19_clean2 |> 
  group_by(County) |> 
  arrange(County, Year) |> 
  mutate(growth_rate = (pop - lag(pop)) / lag(pop) * 100) |> 
  group_by(County) |>
  summarise(growth_rate = mean(growth_rate, na.rm = T) |> round(2)) 
write.csv(p19_growth, "growthrate.csv")

# County Population for 20222-2023
pop_22_23 <- p19_clean2 %>%
  setNames(c(str_to_lower(colnames(.)))) |> 
  mutate(year = str_remove(year, '[A-Za-z]+') |> 
           str_squish() |> 
           as.numeric()) |> 
  filter(year %in% c(2022, 2023))

# 2019 Population
pop <- rKenyaCensus::V1_T2.2 |> 
  dplyr::select(County, pop2019 = Total) |> 
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
  merge(p19_clean |> dplyr::select(1:4), by = "County") |> 
  dplyr::select(County,
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
  dplyr::select(county = County, year, pop) |> 
  rbind(pop_22_23 |>  filter(year != 2023))
write.csv(pop, "population_county_2014_2021.csv")

# Cleaning 2022 dataset alone --------------------------------------------------------

animal_pop2019 <- read_csv('animal_pop_2019.csv') %>%
  pivot_wider(names_from = "species", values_from = "species_num") %>%
  setNames(c("county", paste0(colnames(.[, 2:7]), "_pop") %>% str_to_lower())) |>
  summarise(across(where(is.numeric), ~ sum(.))) |>
  dplyr::select(-pigs_pop, -donkeys_pop) |>
  setNames(c('sheep_pop', 'goat_pop', 'cam_pop', 'catt_pop'))

df22_human <- df_22_human |>
  dplyr::select(county = organisationunitname, periodname, Brucellosis, `MOH 706_Brucella`) |>
  mutate(date = dmy(paste("01", periodname, sep = "-")) |> ymd()) |>
  dplyr::select(-periodname) |>
  pivot_longer(
    cols = -c(county, date),
    names_to = 'diagnosis',
    values_to = 'hum_cases'
  ) |>
  mutate(diagnosis = case_when(
    diagnosis == 'Brucellosis' ~ 'Clinically confirmed',
    TRUE ~ 'Lab confirmed'
  )) |>
  dplyr::select(county, date, diagnosis, hum_cases) |>
  mutate(
    county = str_remove(county, ' County') %>%
      ifelse(. == "Muranga", "Murang'a", .),
    year = year(date)
  ) |>
  filter(county != 'Kenya') |>
  merge(pop_22_23, by = c('county', 'year'))

df_2022 <- df_22_animal |>
  dplyr::select(
    county = County,
    month,
    year...13,
    diseases = `Disease/Condition`,
    diagnosis = `Nature of Diagnosis`,
    `Number Sick`,
    `Species Affected`
  ) |>
  mutate(month =
           ifelse(str_detect(month, "[:digit:]"), month.name[as.numeric(month[str_detect(month, "[:digit:]")])], month)) |>
  filter(!is.na(month)) |>
  mutate(date = ymd(paste(year...13, month, "01", sep = "-")), year = year(date)) |>
  group_by(county, year, date, diseases, diagnosis, `Species Affected`) |>
  summarise(count = sum(`Number Sick`, na.rm = T)) |>
  mutate(
    `Species Affected` = ifelse(
      str_detect(`Species Affected`, "caprine|Caprine|CAPRINE"),
      "Goats",
      `Species Affected`
    ),
    count = as.numeric(count)
  ) |>
  filter(`Species Affected` %in% c("Cattle", "Goats", "Camel", "Sheep")) |> 
  mutate(
    `Species Affected` = case_when(
      `Species Affected` == 'Cattle' ~ 'catt',
      `Species Affected` == 'Sheep' ~ 'shp',
      `Species Affected` == 'Goats' ~ 'goat',
      `Species Affected` == 'Camel' ~ 'cam',
      TRUE ~ `Species Affected`
    ),
    diagnosis = case_when(
      diagnosis == 'Laboratory' ~  'Lab confirmed',
      diagnosis == 'Clinical' ~ 'Clinically confirmed'
    ),
    diseases = str_to_sentence(diseases)
  ) |> 
  pivot_wider(
    id_cols = c('county', 'year', "date", 'diagnosis', 'diseases'),
    values_from = count,
    names_from = `Species Affected`,
    values_fn = sum
  ) %>%
  setNames(c(
    'county',
    'year',
    'date',
    'diagnosis',
    'diseases',
    str_to_lower(paste0(colnames(.[, 6:9]), '_cases'))
  )) |> 
  mutate(county = str_to_title(county)) |> 
  merge(df22_human |>
          filter(year == 2022),
        by = c('county', 'date', 'diagnosis', 'year'), all.y = T) |> 
  cbind(animal_pop2019) |> 
  dplyr::select(
    county,
    year,
    date,
    diseases,
    diagnosis,
    hum_cases,
    catt_cases,
    cam_cases,
    goat_cases,
    shp_cases,
    catt_pop,
    goat_pop,
    sheep_pop,
    cam_pop,
    pop
  ) |>
  as_tibble() 

# Cleaning data for 2014 to 2021 ---------------------------------------------------------

# Cases per year per county
df_incidence2.1 <- df_incidence |>
  dplyr::select(
    date,
    county,
    diseases,
    diagnosis,
    contains("cases"),
    catt_pop,
    goat_pop,
    sheep_pop,
    cam_pop
  ) |>
  mutate(year = lubridate::year(date), date = as.Date(date)) |>
  merge(pop |> filter(year != 2022), by = c("county", "year"), all = T) |>
  filter(!is.na(year)) |>
  rbind(df_2022) |> 
  arrange(date)

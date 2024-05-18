
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
    stringr,
    forecast
  )
}

# Importing data -------------------------------------------------------------------------

df1 <- fread("combined_incidence.csv")
df2022_animal <- read_excel("animal_brucellosis_2022.xlsx")
df2023_animal <- read_excel("animal_brucellosis_2023.xlsx")
human_22_23 <- fread('brucella_2022_2023.csv')

df <- df1 |> 
  filter(date < '2022-01-01')

human_pop <- fread("kenya_pop_2020_2025.csv") |> 
  filter(Year == "2022") |> 
  dplyr::select(pop)

animal_pop <- read_csv('animal_pop_2019.csv') %>% 
  pivot_wider(
    names_from = "species",
    values_from = "species_num"
  ) %>% 
  setNames(c("county", paste0(colnames(.[,2:7]), "_pop") %>% str_to_lower())) |> 
  summarise(across(where(is.numeric), ~sum(.))) |> 
  dplyr::select(-pigs_pop, -donkeys_pop)

# Cleaning 2022 and 2023 human brucella data -------------------------------------------------------------------

## Human
df22_human <- human_22_23 |>
  dplyr::select(periodname, dataname, Kenya) |> 
  pivot_wider(id_cols = periodname, names_from = dataname, values_from = Kenya) |> 
  mutate(date = dmy(paste("01", periodname, sep = "-")) |> ymd()) |>
  rowwise() |>
  mutate(hum_cases = sum(Brucellosis, `MOH 706_Brucella`)) |> 
  dplyr::select(date, hum_cases) |> 
  cbind(human_pop) |> 
  mutate(human_incidence = hum_cases/pop * 1e3) |> 
  dplyr::select(-hum_cases, -pop)

# Animals
df22 <- df2022_animal |> 
  dplyr::select(month, year...13, `Nature of Diagnosis`, `Number Sick`, `Species Affected`) |> 
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
  rowwise() |> 
  mutate(
    animal_pop = sum(cattle_pop, goats_pop, sheep_pop, camels_pop, na.rm = T),
    animal_incidence = animal_cases/animal_pop*1e6,
    date = ymd((date))
  )


# Cleaning df2023 data -------------------------------------------------------------------

# Animals
df23 <- df2023_animal |> 
  dplyr::select(month = Month, year...13 = Year, `Nature of Diagnosis`, `Number Sick`, `Species Affected`) |> 
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
  rowwise() |> 
  mutate(
    animal_pop = sum(cattle_pop, goats_pop, sheep_pop, camels_pop, na.rm = T),
    animal_incidence = animal_cases/animal_pop*1e6,
    date = ymd((date))
  )

all_y <- df22 |> 
  rbind(df23) |> 
  merge(df22_human, by = "date") |> 
  dplyr::select(date, human_incidence, animal_incidence)

df_complete1 <- df |> 
  dplyr::select(-V1) |> 
  mutate(date = ymd(date)) |> 
  rbind(all_y) |> 
  arrange(date) |> 
  mutate(human_incidence = ifelse(human_incidence > 2, median(human_incidence), human_incidence))

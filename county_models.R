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
    RColorBrewer
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
 # mutate(across(where(is.numeric), ~ifelse(. == 0, NA, .)))

# Identifying the outliers in the number of cases
numeric_columns <- df_incidence2 %>%
  select(contains('cases')) |> 
  keep(is.numeric)

numeric_columns %>%
  map(~summary(.))

numeric_columns %>%
  imap(
    ~ ggplot(data = data.frame(y = .x)) +
      geom_boxplot(aes(x = 1, y = y)) +
      labs(title = .y) +
      theme_light()
  )


df_incidence2 <- df_incidence2 |> 
  mutate(
    catt_cases = ifelse(catt_cases >= 69, mean(catt_cases), catt_cases),
    goat_cases = ifelse(goat_cases > 28, mean(goat_cases), goat_cases),
  )

numeric_columns <- df_incidence2 %>%
  select(contains('cases')) |> 
  keep(is.numeric)

numeric_columns %>%
  map(~summary(.))

numeric_columns %>%
  imap(
    ~ ggplot(data = data.frame(y = .x)) +
      geom_boxplot(aes(x = 1, y = y)) +
      labs(title = .y) +
      theme_light()
  )


df_tot_cases <- df_incidence2 |>
  group_by(date, county) |>
  summarise(across(contains("cases"), ~ sum(., na.rm = T))) 
  #mutate(across(contains('cases'), ~ifelse(. == 0, NA, .)))

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
  #mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .))) |> 
  as_tibble() |> 
  arrange(county)

# Outliers
numeric_columns <- df_1 %>%
  select(contains('incidence')) |> 
  keep(is.numeric)

numeric_columns %>%
  map(~summary(.))

numeric_columns %>%
  imap(
    ~ ggplot(data = data.frame(y = .x)) +
      geom_boxplot(aes(x = 1, y = y)) +
      labs(title = .y) +
      theme_light()
  )


df_cum <- df_tot_cases |>
  merge(df_pop, by = c("date", "county")) |> 
  filter(!is.na(date)) |> 
  rowwise() |> 
  mutate(
    animal_cases = sum(catt_cases, goat_cases, shp_cases, cam_cases, na.rm = T),
    animal_pop = sum(catt_pop, goat_pop, sheep_pop, cam_pop, na.rm = T),
    animal_cases = ifelse(animal_cases == 0, NA, animal_cases),
    animal_incidence = round((animal_cases / animal_pop) * 1000000, 4),
    human_incidence = round((hum_cases / pop) * 1000, 4)
  ) |> 
  select(date, county, contains("incidence")) |> 
  as_tibble() |> 
  mutate()

# Trend -------------------------------------------------------------------

df_incidence2_trend <- df_incidence2 |> 
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

df_tot_cases_trend <- df_incidence2_trend |>
  group_by(date) |>
  summarise(across(contains("cases"), ~ sum(., na.rm = T))) |> 
  mutate(across(contains('cases'), ~ifelse(. == 0, NA, .)))

df_tot_pop_trend <- df_incidence2_trend |>
  select(date, county, contains("pop")) %>%
  distinct(.) |>
  as_tibble() |>
  group_by(date) %>%
  summarise(
    sheep_pop = sum(sheep_pop),
    goat_pop = sum(goat_pop),
    cam_pop = sum(cam_pop),
    catt_pop = sum(catt_pop),
    hum_pop = sum(pop)
  ) %>%
  as_tibble()


df_1_trend <- df_tot_cases_trend |>
  merge(df_tot_pop_trend, by = "date") |>
  filter(!is.na(date)) |>
  mutate(
    human_incidence = round((hum_cases / hum_pop) * 1000, 4),
    catt_incidence = round((catt_cases / catt_pop) * 1000000, 4),
    cam_incidence = round((cam_cases / cam_pop) * 1000000, 4),
    goat_incidence = round((goat_cases / goat_pop) * 1000000, 4),
    shp_incidence = round((shp_cases / sheep_pop) * 1000000, 4),
    date = as.Date(date)
  ) |>
  select(date, contains("incidence")) |>
  as_tibble() |> 
  mutate(
    
  )

## The differenced one
date <- df_1_trend$date[-1]

df_1_trend_diff <- df_1_trend |> 
  reframe(across(contains("incidence"), ~ diff(., na.rm = T))) |> 
  mutate(date = as.Date(date))


df_cum_trend <- df_tot_cases_trend |>
  merge(df_tot_pop_trend, by = "date") |>
  filter(!is.na(date)) |>
  rowwise() |> 
  mutate(animal_cases = sum(catt_cases, goat_cases, shp_cases, cam_cases, na.rm = T),
         animal_pop = sum(catt_pop, goat_pop, sheep_pop, cam_pop, na.rm = T),
        # animal_cases = ifelse(animal_cases == 0, NA, animal_cases),
         human_incidence = round((hum_cases / hum_pop) * 1000, 4),
         animal_incidence = round((animal_cases / animal_pop) * 1000000, 4)
  ) |> 
  select(date, contains("incidence"))

# The differenced one
df_cum_trend_diff <- df_cum_trend |> 
  arrange(date) |>  
  as.data.frame() |>
  reframe(across(c(human_incidence, animal_incidence), ~ diff(., 1, na.rm = T))) |> 
  mutate(date = as.Date(date))

trend_data <- df_1_trend %>%
  pivot_longer(cols = -date) %>%
  mutate(
    name = factor(name, levels = unique(name)),
    name = factor(name, labels = c(
      "Human Incidence", "Cattle Incidence", "Goat Incidence",
      "Sheep Incidence", "Camel Incidence"
    ))
  )

all_plus_hum <- df_cum_trend |> 
  #filter(!is.na(animal_incidence)) |> 
  mutate(animal_incidence = ifelse(is.na(animal_incidence), 0, animal_incidence)) |> 
  ggplot(aes(date)) +
  geom_point(aes(y = animal_incidence), col = "black", size = 1) +
  geom_line(aes(y = animal_incidence), col = "black", linewidth = 1) +
  theme_light() +
  theme(
    strip.background = element_rect(fill = "white", colour = "grey"),
    strip.text = element_text(color = "black", size = 12),
    axis.title = element_text(colour = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    plot.title = element_text(color = "black", hjust = 0.5, size = 12),
    axis.title.y = element_text(color = "black", size = 10),
    legend.position = "bottom",
    legend.text = element_text(color = "black")
  ) +
  ylab("Animal Incidence") +
  ggtitle('Animal Incidence')
all_plus_hum

# All except humans
species_plt <- trend_data %>%
  filter(name != "Human Incidence") |>
  mutate(value = ifelse(is.na(value), 0, value)) |> 
  ggplot(aes(x = date)) +
  geom_line(aes(y = value, col = name), linewidth = 1) +
  geom_point(aes(y = value, col = name), size = 2) +
  theme_light() +
  #facet_wrap(~name, scales = "free", ncol = 3) +
  theme(
    strip.background = element_rect(fill = "white", colour = "grey"),
    strip.text = element_text(color = "black", size = 12),
    axis.title = element_text(colour = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    plot.title = element_text(color = "black", hjust = 0.5, size = 12),
    axis.title.y = element_text(color = "black", size = 10),
    legend.position = "bottom",
    legend.text = element_text(color = "black")
  ) +
  ylab("Incidence/1,000,000 population") +
  xlab("Year") +
  labs(col = "Species", title = "Incidence rate for cattle, goats, sheep and camels")

species_plt

# Humans
humans_plt <- trend_data %>%
  filter(name == "Human Incidence") |>
  mutate(value = ifelse(is.na(value), 0, value)) |> 
  
  ggplot(aes(x = date)) +
  geom_line(aes(y = value), linewidth = 1) +
  geom_point(aes(y = value), size = 2) +
  theme_light() +
  #facet_wrap(~name, scales = "free", ncol = 3) +
  theme(
    strip.background = element_rect(fill = "white", colour = "grey"),
    strip.text = element_text(color = "black", size = 12),
    axis.title = element_text(colour = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    plot.title = element_text(color = "black", hjust = 0.5, size = 12),
    axis.title.y = element_text(color = "black", size = 10),
    legend.position = "bottom",
    legend.text = element_text(color = "black")
  ) +
  ylab("Incidence/1,000 population") +
  xlab("Year") +
  labs(col = "Species", title = "Incidence rate for humans")
humans_plt


# Testing for stationary -------------------------------------------------

# At county Level
adf1 <- adf.test(na.omit(df_1$human_incidence))
adf2 <- adf.test(na.omit(df_1$catt_incidence))
adf3 <- adf.test(na.omit(df_1$cam_incidence))
adf4 <- adf.test(na.omit(df_1$goat_incidence))
adf5 <- adf.test(na.omit(df_1$shp_incidence))

adf_res <- data.frame(
  variable = c(
    "Human Incidence",
    "Cattle Incidence",
    "Camel Incidence",
    "Goat Incidence",
    "Sheep Incidence"
  ),
  statistic = c(
    adf1$statistic,
    adf2$statistic,
    adf3$statistic,
    adf4$statistic,
    adf5$statistic
  ),
  
  `P Value` = c(
    adf1$p.value,
    adf2$p.value,
    adf3$p.value,
    adf4$p.value,
    adf5$p.value
  )
  
  
) %>%
  mutate(across(where(is.numeric), ~round(., 3))) |> 
knitr::kable(
    align = "l",
    caption = "Results of Augmented Dickey-Fuller Test for Stationarity at County Level",
    format = "pipe",
    latex_options = "hold_position"
  )

# At National Level
adf_trend1 <- adf.test(na.omit(df_1_trend$human_incidence))
adf_trend2 <- adf.test(na.omit(df_1_trend$catt_incidence))
adf_trend3 <- adf.test(na.omit(df_1_trend$cam_incidence))
adf_trend4 <- adf.test(na.omit(df_1_trend$goat_incidence))
adf_trend5 <- adf.test(na.omit(df_1_trend$shp_incidence))

adf_trend_res <- data.frame(
  Variable = c(
    "Human Incidence",
    "Cattle Incidence",
    "Camel Incidence",
    "Goat Incidence",
    "Sheep Incidence"
  ),
  Statistic = c(
    adf_trend1$statistic,
    adf_trend2$statistic,
    adf_trend3$statistic,
    adf_trend4$statistic,
    adf_trend5$statistic
  ),
  
  `P Value` = c(
    adf_trend1$p.value,
    adf_trend2$p.value,
    adf_trend3$p.value,
    adf_trend4$p.value,
    adf_trend5$p.value
  )
  
  
) |>
  mutate(across(where(is.numeric), ~round(., 3))) |> 
  knitr::kable(
    align = "l",
    caption = "Results of Augmented Dickey-Fuller Test for Stationarity at National Level",
    format = "pipe",
    latex_options = "hold_position"
  )

## All incidences combined

adf_combined <- adf.test(na.omit(df_cum$animal_incidence))
adf_combined_trend <- adf.test(df_cum_trend$animal_incidence)


adf_combined_results <- data.frame(
  "Level" = c("County", "National"),
  "Statistic" = c(adf_combined$statistic, adf_combined_trend$statistic),
  "P Value" = c(adf_combined$p.value, adf_combined_trend$p.value)
)

# Moving Average Smoothing ------------------------------------------------


# Select columns containing "incidence"
incidence_cols <- grep("incidence", names(df_1_trend), value = TRUE)

# Apply moving average smoothing to selected columns
smoothed_df <- df_1_trend %>%
  mutate(across(incidence_cols, ~ifelse(is.na(.), 0, .))) |> 
  mutate(across(all_of(incidence_cols), ~zoo::rollmean(., k = 4, fill = NA), .names = "smoothed_{.col}")) |> 
  na.omit()

# Print the first few rows of the smoothed data
head(smoothed_df)

order <- c(
  "smoothed_human_incidence",
  "smoothed_catt_incidence",
  "smoothed_goat_incidence",
  "smoothed_shp_incidence",
  "smoothed_cam_incidence"
)

trend_data_smoothed <- smoothed_df %>%
  select(date, contains("smoothed")) %>%
  pivot_longer(cols = -date) %>%
  mutate(
    name = factor(name, levels = order),
    name = factor(name, labels = c(
      "Human Incidence", "Cattle Incidence", "Goat Incidence",
      "Sheep Incidence", "Camel Incidence"
    ))
  ) 

species_sm_plt <- trend_data_smoothed |> 
  filter(name != "Human Incidence") |> 
  ggplot(aes(x = date)) +
  geom_line(aes(y = value, col = name), linewidth = 1) +
  # facet_wrap(~name, scales = "free", ncol = 3) +
  theme_light() +
  theme(
    strip.background = element_rect(fill = "white", colour = "grey"),
    strip.text = element_text(color = "black", size = 12),
    axis.title = element_text(colour = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    plot.title = element_text(color = "black", hjust = 0.5, size = 12),
    axis.title.y = element_text(color = "black", size = 10),
    legend.position = "bottom",
    legend.text = element_text(color = "black")
  ) +
  ylab("Incidence/1,000,000 population") +
  xlab("Year") +
  labs(col = "Species", title = "Smoothed Incidence rate for cattle, goats, sheep and camels")

species_sm_plt

# With points
species_sm_plt_points <- trend_data_smoothed |> 
  filter(name != "Human Incidence") |> 
  ggplot(aes(x = date)) +
  geom_line(aes(y = value, col = name), linewidth = 1) +
  geom_point(data = trend_data |> 
               mutate(value = ifelse(is.na(value), 0, value)) |> 
               filter(name != "Human Incidence"),
             
             aes(y = value, col = name), size = 1) +
  # facet_wrap(~name, scales = "free", ncol = 3) +
  theme_light() +
  theme(
    strip.background = element_rect(fill = "white", colour = "grey"),
    strip.text = element_text(color = "black", size = 12),
    axis.title = element_text(colour = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    plot.title = element_text(color = "black", hjust = 0.5, size = 12),
    axis.title.y = element_text(color = "black", size = 10),
    legend.position = "bottom",
    legend.text = element_text(color = "black")
  ) +
  ylab("Incidence/1,000,000 population") +
  xlab("Year") +
  labs(col = "Species", title = "Smoothed Incidence rate for cattle, goats, sheep and camels")

species_sm_plt_points

humans_sm_plt <- trend_data_smoothed |> 
  filter(name == "Human Incidence") |> 
  ggplot(aes(x = date)) +
  geom_line(aes(y = value), linewidth = 1) +
  geom_point(data = df_1_trend, aes(y = human_incidence), col = "red") +
  # facet_wrap(~name, scales = "free", ncol = 3) +
  theme_light() +
  theme(
    strip.background = element_rect(fill = "white", colour = "grey"),
    strip.text = element_text(color = "black", size = 12),
    axis.title = element_text(colour = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    plot.title = element_text(color = "black", hjust = 0.5, size = 12),
    axis.title.y = element_text(color = "black", size = 10),
    legend.position = "bottom",
    legend.text = element_text(color = "black")
  ) +
  ylab("Incidence/1,000 population") +
  xlab("Year") +
  labs(col = "Species", title = "Smoothed incidence rate for humans")
humans_sm_plt

## For all animal incidence combined
df_cum_trend <- df_cum_trend |> 
  mutate(date = as.Date(date))

incidence_cols_cum <- grep("incidence", names(df_cum_trend), value = TRUE)

# Apply moving average smoothing to selected columns
# Select columns containing "incidence"
incidence_cols <- grep("incidence", names(df_cum_trend), value = TRUE)

# Apply moving average smoothing to selected columns
smoothed_df_combined <- df_cum_trend %>%
  mutate(animal_incidence = ifelse(is.na(animal_incidence), 0, animal_incidence)) |> 
  as_tibble() |>
  mutate(across(
    all_of(incidence_cols),
    ~ zoo::rollmean(., k = 2, fill = NA),
    .names = "smoothed_{.col}"
  )) |>
  na.omit()

animal_sm_plt <- smoothed_df_combined |> 
  ggplot(aes(x = date)) +
  geom_line(aes(y = smoothed_animal_incidence), linewidth = 1) +
  geom_point(data = df_cum_trend |> 
               mutate(animal_incidence = ifelse(is.na(animal_incidence), 0, animal_incidence)) 
               , aes(y = animal_incidence), col = "red") +
  # facet_wrap(~name, scales = "free", ncol = 3) +
  theme_light() +
  theme(
    strip.background = element_rect(fill = "white", colour = "grey"),
    strip.text = element_text(color = "black", size = 12),
    axis.title = element_text(colour = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    plot.title = element_text(color = "black", hjust = 0.5, size = 12),
    axis.title.y = element_text(color = "black", size = 10),
    legend.position = "bottom",
    legend.text = element_text(color = "black")
  ) +
  ylab("Incidence/1,000,000 population") +
  xlab("Year") +
  labs(col = "Species", title = "Smoothed incidence rate for animal incidence combined")
animal_sm_plt

# Descriptive Statistics  -------------------------------------------------

table1.1 <- df_incidence |> 
  select(county, diagnosis, contains("cases")) |> 
  pivot_longer(cols = -c(county, diagnosis)) %>%
  group_by(name) |> 
  group_by(name, Diagnosis = diagnosis) |> 
  summarise(Cases = sum(value, na.rm = T)) |> 
  mutate(
    Species = recode(
      name,
      "cam_cases" = 'Camels',
      "hum_cases" = 'Humans',
      "goat_cases" = 'Goats',
      "shp_cases" = "Sheep",
      "catt_cases" = "Cattle"
    ) 
  ) |> 
  ungroup() |> 
  select(-name) |> 
  group_by(Species, Diagnosis) |> 
  group_by(Species) |> 
  mutate(`Percent(%)` = round((Cases/sum(Cases)) * 100, 2)) |> 
  select(3, 1,2,4) |> 
  knitr::kable(align = "l", 
               caption = "Number of cases according to the type of Diagnosis", 
               format = "pipe",
               latex_options = "hold_position")



# The descriptive statistics are for the Incidence Rate National Wide
table2 <- df_1 %>%
  select(county, contains("incidence")) |> 
  pivot_longer(cols = -1) %>%
  group_by(name) %>%
  summarise(
    `Mean Incidence Rate` = mean(value, na.rm = TRUE),
    minimum = min(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE)
  ) %>%
  arrange(desc(`Mean Incidence Rate`)) %>%
  mutate(
    name = c("Human", "Goat", "Cattle", "Camel", "Sheep"),
    Cases = comma(`Mean Incidence Rate`),
    Minimum = comma(minimum),
    Median = comma(median),
    Maximum = comma(max),
    `Standard Deviation` = comma(sd)
  ) %>%
  select(Species = name, `Mean Incidence Rate`, Minimum, Median, Maximum, `Standard Deviation`) |> 
  knitr::kable(align = "l", 
               caption = "Descriptive Statistics for Incidence Rate", 
               format = "pipe", latex_options = "hold_position")




# Spatial -----------------------------------------------------------------


# Cases per year per county
df_tot_cases_spatial <- df_incidence2 |>
  group_by(year = year(date), county) |>
  summarise(across(contains("cases"), ~ sum(., na.rm = T))) |> 
  mutate(across(contains('cases'), ~ifelse(. == 0, NA, .)))


# Population per year, per county
df_pop_spatial <- df_incidence2 |> 
  select(date, county, contains("pop")) %>%
  distinct(.) |>
  as_tibble() |>
  group_by(year = year(date), county) %>%
  summarise(across(where(is.numeric), ~unique(.))) |> 
  mutate(across(contains('cases'), ~ifelse(. == 0, NA, .)))

df_spatial <- df_tot_cases_spatial |>
  merge(df_pop_spatial, by = c("year", "county")) |> 
  filter(!is.na(year)) |>
  mutate(
    human_incidence = round((hum_cases / pop) * 1000, 4),
    catt_incidence = round((catt_cases / catt_pop) * 1000000, 4),
    cam_incidence = round((cam_cases / cam_pop) * 1000000, 4),
    goat_incidence = round((goat_cases / goat_pop) * 1000000, 4),
    shp_incidence = round((shp_cases / sheep_pop) * 1000000, 4)
  ) |>
  select(year, county, contains("incidence")) |>
  #mutate(across(is.numeric, ~ifelse(is.na(.), 0, .))) |> 
  as_tibble()

df_spatial_cum <- df_tot_cases_spatial |>
  merge(df_pop_spatial, by = c("year", "county")) |> 
  rowwise() |> 
  mutate(
    animal_cases = sum(catt_cases, goat_cases, shp_cases, cam_cases, na.rm = T),
    animal_pop = sum(catt_pop, goat_pop, sheep_pop, cam_pop, na.rm = T),
    animal_cases = ifelse(animal_cases == 0, NA, animal_cases),
    animal_incidence = round((animal_cases / animal_pop) * 1000000, 4),
    human_incidence = round((hum_cases / pop) * 1000, 4)
  ) |> 
  select(year, county, contains("incidence")) |> 
  as_tibble()

# Checking for mismatch of county names in the shapefiles and in our data
setdiff(shp$Name, df_spatial$county)

# Replacing Muranga to Murang'a
shp <- shp |> 
  mutate(Name = ifelse(Name == "Muranga", "Murang'a", Name))
setdiff(shp$Name, df_spatial$county)

length(unique(df_spatial$county))

# Merging

df_spatial_merged <- df_spatial |> 
  merge(shp, by.x = "county", by.y = 'Name') 
indi_incidence_columns <- grep("incidence", names(df_spatial_merged), value = TRUE)

df_spatial_merged <- df_spatial_merged |> 
    mutate(across(all_of(indi_incidence_columns), 
                ~ cut(., breaks = quantile(., na.rm = TRUE), include.lowest = TRUE),
                .names = "{col}_range")) |> 
  st_as_sf()

df_spatial_merged_cum <- df_spatial_cum |> 
  merge(shp, by.x = "county", by.y = 'Name') 

all_incidence_columns <- grep("incidence", names(df_spatial_merged_cum), value = TRUE)
df_spatial_merged_cum <- df_spatial_merged_cum |> 
      mutate(across(all_of(all_incidence_columns), 
                ~ cut(., breaks = quantile(., na.rm = TRUE), include.lowest = TRUE),
                .names = "{col}_range")) |> 
  st_as_sf()

# Plotting

# Convert year to factor for better plotting
df_spatial_merged$year <- as.factor(df_spatial_merged$year)
df_spatial_merged_cum$year <- as.factor(df_spatial_merged_cum$year)

# All animals incidence
cate_animal <- length(levels(df_spatial_merged_cum$animal_incidence_range))
animals <- df_spatial_merged_cum |>
  mutate(animal_incidence_range = ifelse(is.na(animal_incidence_range), 
                                         "0", 
                                         as.character(animal_incidence_range)) %>%
           factor(., levels = c(
             "0",
             "[0.113,0.865]",
             "(0.865,1.85]", 
             "(1.85,5.62]",
             "(5.62,67.4]"
           ))) |> 
  ggplot() +
  geom_sf(aes(fill = animal_incidence_range)) +
  scale_fill_manual(values = c("white", brewer.pal(cate_animal, "YlOrRd")),
                    labels = c(
                      "0" = "0",
                      "[0.113,0.865]" = "0.113 - 0.865",
                      "(0.865,1.85]" = "0.865 - 1.85",
                      "(1.85,5.62]" = "1.85 - 5.62",
                      "(5.62,67.4]"  = "5.62 - 67.4"
                    ),
                    
                    na.value = "white") +
  theme_void() +
  facet_wrap( ~ year, nrow = 1) +
  theme(
    plot.title = element_text(
      color = "black",
      hjust = .5,
      size = 16
    ),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10, colour = "black"),
    legend.key.size = unit(0.3, "cm"),
    strip.text = element_text(colour = "black", size = 16)
  ) +
  ggtitle("Animals") +
  labs(fill = "Animals") 
animals

# Humans
cate_human <-
  length(levels(df_spatial_merged_cum$human_incidence_range))
humans <- df_spatial_merged_cum |>
  # mutate(human_incidence_range = ifelse(is.na(human_incidence_range),
  #                                        "0",
  #                                        as.character(human_incidence_range)) %>%
  #          factor(., levels = c(
  #            "0",
  #            "[0.113,0.865]",
  #            "(0.865,1.85]",
  #            "(1.85,5.62]",
  #            "(5.62,67.4]"
  #          ))) |>
  ggplot() +
  geom_sf(aes(fill = human_incidence_range)) +
  scale_fill_manual(
    values = c(brewer.pal(cate_human, "YlOrRd")),
    labels =
      function(breaks) {
        str_replace_all(breaks, "\\[|\\)|\\]|\\(", "") %>% str_replace_all(., ",", " - ")
      }
    
    ,
    
    na.value = "white"
  ) +
  theme_void() +
  facet_wrap(~ year, nrow = 1) +
  theme(
    plot.title = element_text(
      color = "black",
      hjust = .5,
      size = 16
    ),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10, colour = "black"),
    legend.key.size = unit(0.3, "cm"),
    strip.text = element_text(colour = "black", size = 16)
  ) +
  ggtitle("Humans") +
  labs(fill = "Humans")
humans



animals_humans <-
  wrap_plots(humans,
             animals,
             ncol = 1,
             guides = "keep") +
  plot_annotation(caption = "For humans, the incidence rate is per
                  1,000 population while for animal,
                  the incidence rate is per 1,000,000 population
                  ") &
  theme(plot.caption = element_text(size = 16, colour = "black"))

cate_catt <- length(levels(df_spatial_merged$catt_incidence_range))
cattle <- df_spatial_merged |>
  mutate(catt_incidence_range = ifelse(is.na(catt_incidence_range), 
                                         "0", 
                                         as.character(catt_incidence_range)) %>%
           factor(., levels = c(
             "0",
             "[0.704,2.38]",
             "(2.38,5.45]", 
             "(5.45,10]",
             "(10,235]"
           ))) |> 
  ggplot() +
  geom_sf(aes(fill = catt_incidence_range)) +
  scale_fill_manual(values = c("white", brewer.pal(cate_catt, "YlOrRd")),
                    # labels = c(
                    #   "0" = "0",
                    #   "[0.113,0.865]" = "0.113 - 0.865",
                    #   "(0.865,1.85]" = "0.865 - 1.85",
                    #   "(1.85,5.62]" = "1.85 - 5.62",
                    #   "(5.62,67.4]"  = "5.62 - 67.4"
                    # ),
                        labels =
      function(breaks) {
        str_replace_all(breaks, "\\[|\\)|\\]|\\(", "") %>% str_replace_all(., ",", " - ")
      },
    
                    na.value = "white") +
  theme_void() +
  facet_wrap( ~ year, nrow = 1) +
  theme(
    plot.title = element_text(
      color = "black",
      hjust = .5,
      size = 16
    ),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10, colour = "black"),
    legend.key.size = unit(0.3, "cm"),
    strip.text = element_text(colour = "black", size = 16)
  ) +
  ggtitle("Cattle") +
  labs(fill = "Cattle") 
cattle

# Goats
cate_goat <- length(levels(df_spatial_merged$goat_incidence_range))
goat <- df_spatial_merged |>
  mutate(goat_incidence_range = ifelse(is.na(goat_incidence_range), 
                                         "0", 
                                         as.character(goat_incidence_range)) %>%
           factor(., levels = c(
             "0",
             "[0.13,0.724]",
             "(0.724,1.63]", 
             "(1.63,8.05]",
             "(8.05,24.3]"
           ))) |> 
  ggplot() +
  geom_sf(aes(fill = goat_incidence_range)) +
  scale_fill_manual(values = c("white", brewer.pal(cate_goat, "YlOrRd")),
                        labels =
      function(breaks) {
        str_replace_all(breaks, "\\[|\\)|\\]|\\(", "") %>% str_replace_all(., ",", " - ")
      },
    
                    na.value = "white") +
  theme_void() +
  facet_wrap( ~ year, nrow = 1) +
  theme(
    plot.title = element_text(
      color = "black",
      hjust = .5,
      size = 16
    ),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10, colour = "black"),
    legend.key.size = unit(0.3, "cm"),
    strip.text = element_text(colour = "black", size = 16)
  ) +
  ggtitle("Goats") +
  labs(fill = "Goat") 
goat


#sheep
cate_shp <- length(levels(df_spatial_merged$shp_incidence_range))
sheep <- df_spatial_merged |>
  mutate(shp_incidence_range = ifelse(is.na(shp_incidence_range), 
                                         "0", 
                                         as.character(shp_incidence_range)) %>%
           factor(., levels = c(
             "0",
             "[0.182,0.701]",
             "(0.701,2.48]", 
             "(2.48,4.65]",
             "(4.65,45.1]"
           ))) |> 
  ggplot() +
  geom_sf(aes(fill = shp_incidence_range)) +
  scale_fill_manual(values = c("white", brewer.pal(cate_shp, "YlOrRd")),
                        labels =
      function(breaks) {
        str_replace_all(breaks, "\\[|\\)|\\]|\\(", "") %>% str_replace_all(., ",", " - ")
      },
    
                    na.value = "white") +
  theme_void() +
  facet_wrap( ~ year, nrow = 1) +
  theme(
    plot.title = element_text(
      color = "black",
      hjust = .5,
      size = 16
    ),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10, colour = "black"),
    legend.key.size = unit(0.3, "cm"),
    strip.text = element_text(colour = "black", size = 16)
  ) +
  ggtitle("Sheep") +
  labs(fill = "Sheep") 
sheep

# Camels
cate_cam <- length(levels(df_spatial_merged$cam_incidence_range))
camels <- df_spatial_merged |>
  mutate(cam_incidence_range = ifelse(is.na(cam_incidence_range), 
                                         "0", 
                                         as.character(cam_incidence_range)) %>%
           factor(., levels = c(
             "0",
             "[0.613,1.9]",
             "(1.9,2.84]", 
             "(2.84,33]",
             "(33,122]"
           ))) |> 
  ggplot() +
  geom_sf(aes(fill = cam_incidence_range)) +
  scale_fill_manual(values = c("white", brewer.pal(cate_cam, "YlOrRd")),
                        labels =
      function(breaks) {
        str_replace_all(breaks, "\\[|\\)|\\]|\\(", "") %>% str_replace_all(., ",", " - ")
      },
    
                    na.value = "white") +
  theme_void() +
  facet_wrap( ~ year, nrow = 1) +
  theme(
    plot.title = element_text(
      color = "black",
      hjust = .5,
      size = 16
    ),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10, colour = "black"),
    legend.key.size = unit(0.3, "cm"),
    strip.text = element_text(colour = "black", size = 16)
  ) +
  ggtitle("Camels") +
  labs(fill = "Camels") 
camels


all_plots <-
  wrap_plots(humans,
             cattle,
             goat,
             sheep,
             camels,
             ncol = 1,
             guides = "keep") +
  plot_annotation(caption = "For humans, the incidence rate is per 1,000 population while for other species,
                  the incidence rate is per 1,000,000 population
                  ") &
  theme(plot.caption = element_text(size = 16, colour = "black"))



# Correlation -------------------------------------------------------------
# library(data.table)
# urca::ca.jo(df[, .SD, .SDcols != ('human incidence')])
# library(data.table)
# 
# dd = data.table::setDT(df_2)
# 
# dd = dd[, .SD, .SDcols = !c("human_incidence", 'date')]
# 
# library(urca)
# KK = ca.jo(dd)
# summary(KK)

# Correlation Plot
df_1_trend <- df_1_trend |> 
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)))

cor_lag <- df_1_trend %>%
  as_tibble() %>%
  select(-date) %>%
  setNames(c("Human",
             "Cattle",
             "Camel",
             "Goat",
             "Sheep")) |>
  cor() %>%
  ggcorrplot::ggcorrplot(type = "upper",
                         lab = TRUE,
                         lab_size = 6) +
  theme_light() +
  labs(subtitle = "Correlation between human incidence \nand animal incidences",
       x = NULL,
       y = NULL) +
  guides(fill = "none") +
  theme(
    strip.background = element_rect(fill = "white", colour = "grey"),
    strip.text = element_text(color = "black", size = 12),
    axis.title = element_text(colour = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    plot.title = element_text(
      color = "black",
      hjust = 0.5,
      size = 35
    ),
    plot.subtitle = element_text(
      color = "black",
      hjust = 0.5,
      size = 14
    ),
    axis.title.y = element_text(color = "black", size = 10),
    legend.position = "right",
    legend.text = element_text(color = "black")
  )
cor_lag

# Correlation Plot at lag 1
cor_lag1 <- df_1_trend %>%
  as_tibble() %>%
  mutate_at(vars(catt_incidence, cam_incidence, goat_incidence, shp_incidence),
            list( ~ lag(., n = 1))) |>
  na.omit() |>
  select(-date) %>%
  setNames(c("Human",
             "Cattle",
             "Camel",
             "Goat",
             "Sheep")) |>
  cor() %>%
  ggcorrplot::ggcorrplot(type = "upper",
                         lab = TRUE,
                         lab_size = 6) +
  theme_light() +
  labs(subtitle = "Correlation between\nhuman incidence \nand animal incidences at lag 1",
       x = NULL,
       y = NULL) +
  guides(fill = "none") +
  theme(
    strip.background = element_rect(fill = "white", colour = "grey"),
    strip.text = element_text(color = "black", size = 12),
    axis.title = element_text(colour = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    plot.title = element_text(
      color = "black",
      hjust = 0.5,
      size = 20
    ),
    axis.title.y = element_text(color = "black", size = 10),
    legend.position = "right",
    legend.text = element_text(color = "black")
  )
cor_lag1

# Correlation Plot at lag 2
cor_lag2 <- df_1_trend %>%
  as_tibble() %>%
  mutate_at(vars(catt_incidence, cam_incidence, goat_incidence, shp_incidence),
            list( ~ lag(., n = 2))) |>
  na.omit() |>
  select(-date) %>%
  setNames(c("Human",
             "Cattle",
             "Camel",
             "Goat",
             "Sheep")) |>
  cor() %>%
  ggcorrplot::ggcorrplot(type = "upper",
                         lab = TRUE,
                         lab_size = 6) +
  theme_light() +
  labs(subtitle = "Correlation between\nhuman incidence \nand animal incidences at lag 2",
       x = NULL,
       y = NULL) +
  guides(fill = "none") +
  theme(
    strip.background = element_rect(fill = "white", colour = "grey"),
    strip.text = element_text(color = "black", size = 12),
    axis.title = element_text(colour = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    plot.title = element_text(
      color = "black",
      hjust = 0.5,
      size = 20
    ),
    axis.title.y = element_text(color = "black", size = 10),
    legend.position = "right",
    legend.text = element_text(color = "black")
  )
cor_lag2

# Correlation Plot at lag 3
cor_lag3 <- df_1_trend %>%
  as_tibble() %>%
  mutate_at(vars(catt_incidence, cam_incidence, goat_incidence, shp_incidence),
            list( ~ lag(., n = 3))) |>
  na.omit() |>
  select(-date) %>%
  setNames(c("Human",
             "Cattle",
             "Camel",
             "Goat",
             "Sheep")) |>
  cor() %>%
  ggcorrplot::ggcorrplot(type = "upper",
                         lab = TRUE,
                         lab_size = 6) +
  theme_light() +
  labs(subtitle = "Correlation between\nhuman incidence \nand animal incidences at lag 3",
       x = NULL,
       y = NULL) +
  guides(fill = "none") +
  theme(
    strip.background = element_rect(fill = "white", colour = "grey"),
    strip.text = element_text(color = "black", size = 12),
    axis.title = element_text(colour = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    plot.title = element_text(
      color = "black",
      hjust = 0.5,
      size = 20
    ),
    axis.title.y = element_text(color = "black", size = 10),
    legend.position = "right",
    legend.text = element_text(color = "black")
  )
cor_lag3

# Correlation Plot at lag 4
cor_lag4 <- df_1_trend %>%
  as_tibble() %>%
  mutate_at(vars(catt_incidence, cam_incidence, goat_incidence, shp_incidence),
            list( ~ lag(., n = 4))) |>
  na.omit() |>
  select(-date) %>%
  setNames(c("Human",
             "Cattle",
             "Camel",
             "Goat",
             "Sheep")) |>
  cor() %>%
  ggcorrplot::ggcorrplot(type = "upper",
                         lab = TRUE,
                         lab_size = 6) +
  theme_light() +
  labs(subtitle = "Correlation between\nhuman incidence \nand animal incidences at lag 4",
       x = NULL,
       y = NULL) +
  guides(fill = "none") +
  theme(
    strip.background = element_rect(fill = "white", colour = "grey"),
    strip.text = element_text(color = "black", size = 12),
    axis.title = element_text(colour = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    plot.title = element_text(
      color = "black",
      hjust = 0.5,
      size = 20
    ),
    axis.title.y = element_text(color = "black", size = 10),
    legend.position = "right",
    legend.text = element_text(color = "black")
  )
cor_lag4

# Correlation Plot at lag 5
cor_lag5 <- df_1_trend %>%
  as_tibble() %>%
  mutate_at(vars(catt_incidence, cam_incidence, goat_incidence, shp_incidence),
            list( ~ lag(., n = 5))) |>
  na.omit() |>
  select(-date) %>%
  setNames(c("Human",
             "Cattle",
             "Camel",
             "Goat",
             "Sheep")) |>
  cor() %>%
  ggcorrplot::ggcorrplot(type = "upper",
                         lab = TRUE,
                         lab_size = 6) +
  theme_light() +
  labs(subtitle = "Correlation between\nhuman incidence \nand animal incidences at lag 5",
       x = NULL,
       y = NULL) +
  guides(fill = "none") +
  theme(
    strip.background = element_rect(fill = "white", colour = "grey"),
    strip.text = element_text(color = "black", size = 12),
    axis.title = element_text(colour = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    plot.title = element_text(
      color = "black",
      hjust = 0.5,
      size = 20
    ),
    axis.title.y = element_text(color = "black", size = 10),
    legend.position = "right",
    legend.text = element_text(color = "black")
  )
cor_lag5

# Correlation Plot at lag 6
cor_lag6 <- df_1_trend %>%
  as_tibble() %>%
  mutate_at(vars(catt_incidence, cam_incidence, goat_incidence, shp_incidence),
            list( ~ lag(., n = 6))) |>
  na.omit() |>
  select(-date) %>%
  setNames(c("Human",
             "Cattle",
             "Camel",
             "Goat",
             "Sheep")) |>
  cor() %>%
  ggcorrplot::ggcorrplot(type = "upper",
                         lab = TRUE,
                         lab_size = 6) +
  theme_light() +
  labs(subtitle = "Correlation between\nhuman incidence \nand animal incidences at lag 6",
       x = NULL,
       y = NULL) +
  theme(
    strip.background = element_rect(fill = "white", colour = "grey"),
    strip.text = element_text(color = "black", size = 12),
    axis.title = element_text(colour = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    plot.title = element_text(
      color = "black",
      hjust = 0.5,
      size = 20
    ),
    axis.title.y = element_text(color = "black", size = 10),
    legend.position = "right",
    legend.text = element_text(color = "black")
  )
cor_lag6

all_cols <- wrap_plots(
  cor_lag,
  cor_lag1,
  cor_lag2,
  cor_lag3,
  cor_lag4,
  cor_lag5,
  cor_lag6,
  ncol = 3,
  guides = "collect"
) |> 
  plot_grid(
    rel_widths = c(7, 7,7)
  ) 
all_cols <- all_cols + theme(plot.title = element_text(size = 16),
                             axis.text.y = element_text(color = 'black', size = 13))


# Choosing the model with the highest correlation ----------------------------------------

lag_values <- 0:6  # Assuming you want lag values from 0 to 6
cor_dats <- list(

    cor_lag$data,
    cor_lag1$data,
    cor_lag2$data,
    cor_lag3$data,
    cor_lag4$data,
    cor_lag5$data,
    cor_lag6$data
  )
result_table <- tibble(
  Lag = lag_values,
  `Average Correlation` = cor_dats %>%
    map(~ filter(.x, Var1 == "Human")) %>%
    map_dbl(~mean(.$value))
) |>
  knitr::kable(align = "l", 
               caption = "Average correlation between human incidence and other species incidence", 
               format = "pipe",
               latex_options = "hold_position")

print(result_table)
 # This helps us to choose the lag with the highest average correlation

# General Model without and with differencing --------------------------------------

## This model fits the data without differencing, at difference lags, (0-6) and for individual 
## animal incidences.

run_lag_models <- function(df, max_lag = 6, ...) {
  
  result_df <- tibble()
  
  for (lag_value in 0:max_lag) {
    df_lagged <- df |>
      as_tibble() %>%
      mutate_at(vars(catt_incidence, cam_incidence, goat_incidence, shp_incidence),
                list( ~ lag(., n = lag_value))) |>
      na.omit() |>
      mutate(date = as.Date(date))
    
    mod <- df_lagged |>
      as_tsibble() |>
      model(
        TSLM(
          (human_incidence) ~ cam_incidence + shp_incidence + catt_incidence + goat_incidence
        )
      ) |>
      report()
    
    mod_results <- tidy(mod) %>%
      select(-.model) %>%
      as_tibble() %>%
      mutate(term = case_when(
        term == "goat_incidence" ~ "Goat Incidence",
        term == "catt_incidence" ~ "Cattle incidence",
        term == "shp_incidence" ~ "Sheep incidence",
        term == "cam_incidence" ~ "Camel incidence",
        TRUE ~ as.character(term) 
      ),
      variable = term
      ) |>  select(6, 2:5) |> 
      group_by(variable) %>%
      mutate(
        conf_low = min(estimate - std.error * 1.645),
        conf_high = max(estimate + std.error * 1.645)
      ) %>%
      mutate(lag = lag_value)
    
    adj_r_squared <- glance(mod) %>%
      select(r_squared, AIC, adj_r_squared)
    
    mod_results <- bind_cols(mod_results, adj_r_squared) |>
      mutate(across(c(estimate, std.error, statistic, p.value, conf_low, conf_high, adj_r_squared), ~round(., 3))) |>
      mutate(significance = ifelse(conf_low * conf_high > 0, "Significant", "Not Significant")) |> 
      select(c(1:8, 12, 9:11))
      
    result_df <- bind_rows(result_df, mod_results)
  }
  
  return(result_df)
}

non_diff_indivi <- run_lag_models(df_1_trend 
                                    #mutate(across(contains('incidence'), ~ifelse(is.na(.), 0, .)))
                                    )
diff_indivi <- run_lag_models(df_1_trend_diff |> 
                                mutate(across(contains('incidence'), ~ifelse(is.na(.), 0, .))))
write_csv(non_diff_indivi, "non_diff_individual.csv")
write_csv(diff_indivi, "diff_individual.csv")

# If you want a model at a specific lag, you can use the following code
df_2 <- df_1_trend |> 
  as_tibble() %>%
  mutate_at(vars(catt_incidence, cam_incidence, goat_incidence, shp_incidence),
            list( ~ lag(., n = 2))) |>
  na.omit() |>
  mutate(date = as.Date(date))

mod_lag <- df_2 |> 
  as_tsibble() |>
  model(
    TSLM(
      (human_incidence) ~ cam_incidence + shp_incidence + catt_incidence + goat_incidence
    )
  ) |>
  report()

## The following function fits the model, without differencing, at difference lags, (0-6) and for
## animal incidences combined

lag_models_full <- function(df, max_lag = 6, ...) {
  
  result_df <- tibble()
  
  for (lag_value in 0:max_lag) {
    df_lagged <- df |>
      as_tibble() %>%
      mutate_at(vars(animal_incidence),
                list( ~ lag(., n = lag_value))) |>
      na.omit() |>
      mutate(date = as.Date(date))
    
    mod <- df_lagged |>
      as_tsibble() |>
      model(
        TSLM(
          (human_incidence) ~ animal_incidence
        )
      ) |>
      report()
    
    mod_results <- tidy(mod) %>%
      select(-.model) %>%
      as_tibble() %>%
      mutate(term = case_when(
        term == "animal_incidence" ~ "Animal Incidence",
        TRUE ~ as.character(term) 
      ),
      variable = term
      ) |>  select(6, 2:5) |> 
      group_by(variable) %>%
      mutate(
        conf_low = min(estimate - std.error * 1.645),
        conf_high = max(estimate + std.error * 1.645)
      ) %>%
      mutate(lag = lag_value)
    
    adj_r_squared <- glance(mod) |> 
      select(r_squared, adj_r_squared, AIC) |> 
      mutate(lag = lag_value)
    
    result_df <- bind_rows(result_df, mod_results) |> 
      mutate(across(c(estimate, std.error, statistic, p.value, conf_low, conf_high), ~round(., 3))) |> 
      mutate(significance = ifelse(conf_low * conf_high > 0, "Significant", "Not Significant")) 
        result_df <- bind_rows(result_df, adj_r_squared)

  }
  
  return(result_df)
}


non_diff_full <- lag_models_full(df_cum_trend |> 
                                 mutate(across(contains('incidence'), ~ifelse(is.na(.), 0, .))))
diff_full <- lag_models_full(df_cum_trend_diff)

write_csv(non_diff_full, "non_diff_full.csv")
write_csv(diff_full, "diff_full.csv")

## Getting the AIC, R-Squared and Adjusted R squared for each lag

Table_lag_indivi <- non_diff_indivi |> 
  select(Lag = lag, `R-Squared(%)` = r_squared, `Adjusted R-Squared(%)` = adj_r_squared, AIC) |> 
  mutate(across(c(`R-Squared(%)` , `Adjusted R-Squared(%)`, ), ~ (. * 100))) |> 
    mutate(across(where(is.numeric), ~round(., 2))) |> 
  unique() |> 
  arrange(desc(`Adjusted R-Squared(%)`)) |> 
  knitr::kable(
    align = "l",
    caption = "The AIC, R-Squared and Adjusted R-Squared for each lag for individual species. The data as been arranged in decreasing order of Adjusted R-Squared",
    format = "pipe",
    latex_options = "hold_position"
  )


Table_lag_full <- non_diff_full |> 
  select(Lag = lag, `R-Squared(%)` = r_squared, `Adjusted R-Squared(%)` = adj_r_squared, AIC) |> 
  mutate(across(c(`R-Squared(%)` , `Adjusted R-Squared(%)`, ), ~ (. * 100))) |> 
    mutate(across(where(is.numeric), ~round(., 2))) |> 
  unique() |> 
  arrange(desc(`Adjusted R-Squared(%)`)) |> 
    knitr::kable(
    align = "l",
    caption = "The AIC, R-Squared and Adjusted R-Squared for each lag for combined species. The data as been arranged in decreasing order of Adjusted R-Squared",
    format = "pipe",
    latex_options = "hold_position"
  )


# All counties model --------------------------------------------------------------

# Lag 2
df_2_2 <- df_1 |> 
  as_tibble() %>%
  mutate_at(vars(catt_incidence, cam_incidence, goat_incidence, shp_incidence),
            list( ~ dplyr::lag(., n = 2))) |>
  na.omit() |> 
  mutate(date = as.Date(date))

# Lag 4
df_2_4 <- df_1 |> 
  as_tibble() %>%
  mutate_at(vars(catt_incidence, cam_incidence, goat_incidence, shp_incidence),
            list( ~ dplyr::lag(., n = 4))) |>
  na.omit() |> 
  mutate(date = as.Date(date))


fit_county_model <- function(county_name, data, type) {
  # Subset the data for the specific county
  county_data <- filter(data, county == county_name)
  
  if(type == "full")
  {
    # Check if all incidences are zero
    if (all(county_data$animal_incidence == 0)) {
      message(paste("Skipping model for", county_name, "as all incidences are zero."))
      return(NULL)
    }
    
    # Fit the model
    mod_county <- county_data |>
      as_tsibble() |>
      model(
        TSLM(
          human_incidence ~ animal_incidence
        )
      ) |>
      tidy() |>
      select(-.model) |>
      as_tibble() |>
      mutate(term = case_when(
        term == "animal_incidence" ~ "Animal Incidence",
        TRUE ~ as.character(term) 
      ),
      variable = term
      ) |>  
      select(6, 2:5) |> 
      group_by(variable) %>%
      mutate(
        conf_low = min(estimate - std.error * 1.645),
        conf_high = max(estimate + std.error * 1.645)
      )
  }
  else if(type == "individual") {
  # Check if all incidences are zero
  if (all(county_data$catt_incidence == 0 &
          county_data$cam_incidence == 0 &
          county_data$goat_incidence == 0 &
          county_data$shp_incidence == 0)) {
    message(paste("Skipping model for", county_name, "as all incidences are zero."))
    return(NULL)
  }
  
  # Fit the model
  mod_county <- county_data |>
    as_tsibble() |>
    model(
      TSLM(
        human_incidence ~ catt_incidence + goat_incidence + shp_incidence +cam_incidence
      )
    ) |>
    tidy() |>
    select(-.model) |>
    as_tibble() |>
    mutate(term = case_when(
      term == "goat_incidence" ~ "Goat Incidence",
      term == "catt_incidence" ~ "Cattle incidence",
      term == "shp_incidence" ~ "Sheep incidence",
      term == "cam_incidence" ~ "Camel incidence",
      TRUE ~ as.character(term) 
    ),
    variable = term
    ) |>  
    select(6, 2:5) |> 
    group_by(variable) %>%
    mutate(
      conf_low = min(estimate - std.error * 1.645),
      conf_high = max(estimate + std.error * 1.645)
    )
  }
  return(mod_county)
}

county_names <- unique(df_2_2$county)

# Create a list to store the models for each county
models_list <- list()

# Initialize the data frame for each county
coefficients_df_lag2 <- data.frame(county = character(), 
                              variable = character(),
                              estimate = numeric(),
                              stringsAsFactors = FALSE)

# At lag 2
for (county_name in county_names) {
  message(paste("Fitting model for", county_name))
  

  
  # Fit the model
  mod_county <- fit_county_model(county_name, df_2_2, type = "individual")
  
  # Check if model fitting was successful
  if (!is.null(mod_county)) {
    # Extract coefficients, round off, and add to the data frame
    coefficients_df_lag2 <- bind_rows(coefficients_df_lag2, 
                                 mod_county %>% 
                                   mutate(county = county_name,
                                          estimate = round(estimate, 3)))
  }
}

coefficients_df_lag2 <- coefficients_df_lag2 |>
  mutate(significant = ifelse(conf_low * conf_high > 0, "Significant", "Not Significant")) |> 
  mutate(across(c(3:8), ~round(., 3)))
write_csv(coefficients_df_lag2, "individual_animal_incidence_per_county_lag2.csv")

# At lag 4
coefficients_df_lag4 <- data.frame(county = character(), 
                                   variable = character(),
                                   estimate = numeric(),
                                   stringsAsFactors = FALSE)

for (county_name in county_names) {
  message(paste("Fitting model for", county_name))
  
  
  
  # Fit the model
  mod_county <- fit_county_model(county_name, df_2_4, type = "individual")
  
  # Check if model fitting was successful
  if (!is.null(mod_county)) {
    # Extract coefficients, round off, and add to the data frame
    coefficients_df_lag4 <- bind_rows(coefficients_df_lag4, 
                                 mod_county %>% 
                                   mutate(county = county_name,
                                          estimate = round(estimate, 3)))
  }
}

coefficients_df_lag4 <- coefficients_df_lag4 |>
  mutate(significant = ifelse(conf_low * conf_high > 0, "Significant", "Not Significant")) |> 
  mutate(across(c(3:8), ~round(., 3)))
write_csv(coefficients_df_lag4, "individual_animal_incidence_per_county_lag4.csv")

# Animal incidence combined -----------------------------------------------

# Lag 2
df_cum_2_2 <- df_cum |> 
  as_tibble() %>%
  mutate_at(vars(animal_incidence),
            list( ~ dplyr::lag(., n = 2))) |>
  na.omit() |>
  mutate(date = as.Date(date))

df_cum_2_4 <- df_cum |> 
  as_tibble() %>%
  mutate_at(vars(animal_incidence),
            list( ~ dplyr::lag(., n = 4))) |>
  na.omit() |>
  mutate(date = as.Date(date))

# At lag 2
county_names2_2 <- unique(df_cum_2_2$county)

# Create a list to store the models for each county
models_list <- list()

# Initialize the data frame for each county
coefficients_df2_lag2 <- data.frame(county = character(), 
                              variable = character(),
                              estimate = numeric(),
                              stringsAsFactors = FALSE)

for (county_name in county_names2_2) {
  message(paste("Fitting model for", county_name))
  
  
  
  # Fit the model
  mod_county <- fit_county_model(county_name, df_cum_2_2, type = "full")
  
  # Check if model fitting was successful
  if (!is.null(mod_county)) {
    # Extract coefficients, round off, and add to the data frame
    coefficients_df2_lag2 <- bind_rows(coefficients_df2_lag2, 
                                 mod_county %>% 
                                   mutate(county = county_name,
                                          estimate = round(estimate, 3)))
  }
}

coefficients_df2_lag2 <- coefficients_df2_lag2 |>
  mutate(across(c(3:8), ~round(., 3))) |> 
  mutate(significant = ifelse(conf_low * conf_high > 0, "Significant", "Not Significant")) |> 
  as_tibble()
write_csv(coefficients_df2_lag2, "all_animal_incidence_per_county_lag2.csv")

# At lag 4
county_names2_4 <- unique(df_cum_2_4$county)

# Create a list to store the models for each county
models_list <- list()

# Initialize the data frame for each county
coefficients_df2_lag4 <- data.frame(county = character(), 
                                    variable = character(),
                                    estimate = numeric(),
                                    stringsAsFactors = FALSE)

for (county_name in county_names2_4) {
  message(paste("Fitting model for", county_name))
  
  
  
  # Fit the model
  mod_county <- fit_county_model(county_name, df_cum_2_4, type = "full")
  
  # Check if model fitting was successful
  if (!is.null(mod_county)) {
    # Extract coefficients, round off, and add to the data frame
    coefficients_df2_lag4 <- bind_rows(coefficients_df2_lag4, 
                                       mod_county %>% 
                                         mutate(county = county_name,
                                                estimate = round(estimate, 3)))
  }
}

coefficients_df2_lag4 <- coefficients_df2_lag4 |>
  mutate(across(c(3:8), ~round(., 3))) |> 
  mutate(significant = ifelse(conf_low * conf_high > 0, "Significant", "Not Significant")) |> 
  as_tibble()
write_csv(coefficients_df2_lag4, "all_animal_incidence_per_county_lag4.csv")

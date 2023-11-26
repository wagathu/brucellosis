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
    rKenyaCensus
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

# We're grouping data by date so that we can obtain a time series

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
  group_by(date) |>
  summarise(across(contains("cases"), ~ sum(., na.rm = T)))

df_tot_pop <- df_incidence2 |>
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

df_1 <- df_tot_cases |>
  merge(df_tot_pop, by = "date") |>
  filter(!is.na(date)) |>
  mutate(
    human_incidence = round((hum_cases / hum_pop) * 1000, 4),
    catt_incidence = round((catt_cases / catt_pop) * 100000000, 4),
    cam_incidence = round((cam_cases / cam_pop) * 100000000, 4),
    goat_incidence = round((goat_cases / goat_pop) * 100000000, 4),
    shp_incidence = round((shp_cases / sheep_pop) * 100000000, 4),
    date = as.Date(date)
  ) |>
  select(date, contains("incidence")) |>
  as_tibble()


# Test for stationarity
adf.test(df_1$human_incidence)
adf.test(df_1$catt_incidence)
adf.test(df_1$cam_incidence)
adf.test(df_1$goat_incidence)
adf.test(df_1$shp_incidence)

date <- df_1$date[-1]

df_diff <- df_1 |> 
  reframe(across(contains("incidence"), ~ diff(.))) |> 
  mutate(date = as.Date(date))

df_cum <- df_tot_cases |>
  merge(df_tot_pop, by = "date") |>
  filter(!is.na(date)) |>
  rowwise() |> 
  mutate(animal_cases = sum(catt_cases, goat_cases, shp_cases, cam_cases),
         animal_pop = sum(catt_pop, goat_pop, sheep_pop, cam_pop),
         human_incidence = round((hum_cases / hum_pop) * 1000, 4),
         animal_incidence = round((animal_cases / animal_pop) * 10000000, 4)
  ) |> 
  select(date, contains("incidence"))

df_cum_diff <- df_cum |> 
  arrange(date) |>  
  as.data.frame() |>
  reframe(across(c(human_incidence, animal_incidence), ~ diff(., 1))) |> 
  mutate(date = as.Date(date))

# Cases per year per county
df_tot_cases_spatial <- df_incidence2 |>
  group_by(year = year(date), county) |>
  summarise(across(contains("cases"), ~ sum(., na.rm = T)))

# Population per year, per county
df_pop_spatial <- df_incidence2 |> 
  select(date, county, contains("pop")) %>%
  distinct(.) |>
  as_tibble() |>
  group_by(year = year(date), county) %>%
  summarise(across(where(is.numeric), ~unique(.)))

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
  mutate(across(is.numeric, ~ifelse(is.na(.), 0, .))) |> 
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
  merge(shp, by.x = "county", by.y = 'Name') |> 
  st_as_sf()

# Plotting

# Convert year to factor for better plotting
df_spatial_merged$year <- as.factor(df_spatial_merged$year)

# Display the first few rows of the updated data frame
ylorrd_palette <- colorRampPalette(
  c(
    "white",
    "#FFEDA0",
    "#FED976",
    "#FFCC00",
    "#FEB24C",
    "#FFAA00",
    "#FD8D3C",
    "#FC4E2A",
    "#E31A1C",
    "#BD0026",
    "#800026"
  ),
  interpolate = "linear",
  space = "rgb"
)

human <- df_spatial_merged |>
  mutate(
    human_incidence_range = cut(
      human_incidence,
      breaks = c(0, 1, 5, 10, 20, 30, 40, 50, 60, Inf),
      labels = c("0", "1-5", "5-10", "10-20", "20-30", "30-40", "40-50", "50-60", "> 60"),
      include.lowest = TRUE
    ) |>
      as.factor()
  ) |>
  ggplot() +
  geom_sf(aes(fill = human_incidence_range)) +
  scale_fill_manual(values = ylorrd_palette(9)) +
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
  ggtitle("Humans") +
  labs(fill = "Humans")
human

ylorrd_palette2 <-
  colorRampPalette(
    c(
      "white",
      "#FFFFCC",
      "#FEB24C",
      "#FD8D3C",
      "#FC4E2A",
      "#BD0026",
      "#800026"
    )
  )

cattle <- df_spatial_merged %>%
  mutate(catt_incidence_range = cut(round(catt_incidence, 1),
                                    breaks = c(0, 0.1, 1, 2, 4, 6, 11, Inf),
                                    labels = c("0", "0.1-1", "1-2", "2-4", "4-6", "6-11", "> 11"),
                                    include.lowest = TRUE) %>%
           as.factor()
  ) |>
  ggplot() +
  geom_sf(aes(fill = catt_incidence_range)) +
  scale_fill_manual(values = ylorrd_palette(7)) +
  theme_void() +
  facet_wrap(~year, nrow = 1) +
  theme(
    plot.title = element_text(color = "black", hjust = .5, size = 16),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10, colour = "black"),
    legend.key.size = unit(0.3, "cm"),
    strip.text = element_text(colour = "black", size = 16)
  ) +
  ggtitle("Cattle") +
  labs(fill = "Cattle")
cattle

ylorrd_palette3 <-
  colorRampPalette(
    c(
      "white",
      "#FEB24C",
      "#FD8D3C",
      "#FC4E2A",
      "#BD0026",
      "#800026"
    )
  )

goat <- df_spatial_merged %>%
  mutate(goat_incidence_range = cut(round(goat_incidence, 1),
                                    breaks = c(0, 0.1, 0.5, 1, 2, Inf),
                                    labels = c("0", "0.1-0.5", "0.5-1", "1-2", "> 2"),
                                    include.lowest = TRUE) %>%
           as.factor()
  ) |>
  ggplot() +
  geom_sf(aes(fill = goat_incidence_range)) +
  scale_fill_manual(values = ylorrd_palette(5)) +
  theme_void() +
  facet_wrap(~year, nrow = 1) +
  theme(
    plot.title = element_text(color = "black", hjust = .5, size = 16),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10, colour = "black"),
    legend.key.size = unit(0.3, "cm"),
    strip.text = element_text(colour = "black", size = 16)
  ) +
  ggtitle("Goat") +
  labs(fill = "Goat")
goat


ylorrd_palette4 <-
  colorRampPalette(
    c(
      "white",
      "#FEB24C",
      "#FC4E2A",
      "#800026"
    )
  )

sheep <- df_spatial_merged %>%
  mutate(shp_incidence_range = cut(round(shp_incidence, 2),
                                   breaks = c(0, 0.1, 0.2, 0.3, Inf),
                                   labels = c("0", "0.1-0.2", "0.2-0.3", "> 0.3"),
                                   include.lowest = TRUE) %>%
           as.factor()
  ) |>
  ggplot() +
  geom_sf(aes(fill = shp_incidence_range)) +
  scale_fill_manual(values = ylorrd_palette(4)) +
  theme_void() +
  facet_wrap(~year, nrow = 1) +
  theme(
    plot.title = element_text(color = "black", hjust = .5, size = 16),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10, colour = "black"),
    legend.key.size = unit(0.3, "cm"),
    strip.text = element_text(colour = "black", size = 16)
  ) +
  ggtitle("Sheep") +
  labs(fill = "Sheep")
sheep


ylorrd_palette5 <-
  colorRampPalette(
    c(
      "white",
      "#FEB24C",
      "#FC4E2A",
      "#800026"
    )
  )

camels <- df_spatial_merged %>%
  mutate(cam_incidence_range = cut(round(cam_incidence, 2),
                                   breaks = c(0, 0.01, 0.1, 0.2, Inf),
                                   labels = c("0", "0.01-0.1", "0.1-0.2", "> 0.2"),
                                   include.lowest = TRUE) %>%
           as.factor()
  ) |>
  ggplot() +
  geom_sf(aes(fill = cam_incidence_range)) +
  scale_fill_manual(values = ylorrd_palette(4)) +
  theme_void() +
  facet_wrap(~year, nrow = 1) +
  theme(
    plot.title = element_text(color = "black", hjust = .5, size = 16),
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
  wrap_plots(human, cattle, goat, sheep, camels, ncol = 1, guides = "keep") +
  plot_annotation(caption = "For humans, the incidence rate is per 1,000 population while for other species,
                  the incidence rate is per 1,000,000 population
                  "
  ) &
  theme(plot.caption = element_text(size = 16, colour = "black"))


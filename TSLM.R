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
    kableExtra
  )
}

# Importing data ----------------------------------------------------------

df_incidence <- fread("all_bruc_incidence.csv")
df_numbers <- fread("all_bruc_pop.csv")
shp <- st_read("shapefiles/County.shp", quiet = T)
eco_zones <- fread("eco_zones_county.csv")
shp <- st_read("shapefiles/County.shp", quiet = T)

# Grouping data -----------------------------------------------------------

# We're grouping data by date so that we can obtain a time series
df_tot_cases <- df_incidence |>
  group_by(date) |>
  summarise(across(contains("cases"), ~ sum(., na.rm = T)))

df_tot_pop <- df_incidence |>
  select(date, county, contains("pop")) %>%
  distinct(.) |>
  as_tibble() |>
  group_by(date) %>%
  summarise(
    sheep_pop = sum(sheep_pop),
    goat_pop = sum(goat_pop),
    cam_pop = sum(cam_pop),
    catt_pop = sum(catt_pop),
    hum_pop = sum(hum_pop)
  ) %>%
  as_tibble()

df_1 <- df_tot_cases |>
  merge(df_tot_pop, by = "date") |>
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
         animal_incidence = round((animal_cases / animal_pop) * 1000000, 4)
         ) |> 
  select(date, contains("incidence"))

# Trend -------------------------------------------------------------------

trend_data <- df_1 %>%
  pivot_longer(cols = -date) %>%
  mutate(
    name = factor(name, levels = unique(name)),
    name = factor(name, labels = c(
      "Human Incidence", "Cattle Incidence", "Goat Incidence",
      "Sheep Incidence", "Camel Incidence"
    ))
  )


# All except humans
species_plt <- trend_data %>%
  filter(name != "Human Incidence") |>
  ggplot(aes(x = date)) +
  geom_line(aes(y = value, col = name), linewidth = 1) +
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
  ggplot(aes(x = date)) +
  geom_line(aes(y = value), linewidth = 1) +
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


# Spatial-temporal --------------------------------------------------------

# Cases per year per county
df_tot_cases_spatial <- df_incidence |>
  group_by(year = year(date), county) |>
  summarise(across(contains("cases"), ~ sum(., na.rm = T)))

# Population per year, per county
df_pop_spatial <- df_incidence |> 
  select(date, county, contains("pop")) %>%
  distinct(.) |>
  as_tibble() |>
  group_by(year = year(date), county) %>%
  summarise(across(is.numeric, ~unique(.)))

df_spatial <- df_tot_cases_spatial |>
  merge(df_pop_spatial, by = c("year", "county")) |> 
  filter(!is.na(year)) |>
  mutate(
    human_incidence = round((hum_cases / hum_pop) * 1000, 4),
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
head(df_spatial_merged)

ylorrd_palette <-
  colorRampPalette(
    c(
      "white",
      "#FFEDA0",
      "#FED976",
      "#FEB24C",
      "#FD8D3C",
      "#FC4E2A",
      "#E31A1C",
      "#BD0026",
      "#800026"
    )
  )

human <- df_spatial_merged |>
  filter(human_incidence < 1000) |> 
  mutate(
    human_incidence_range = cut(
      human_incidence,
      breaks = c(0, 10, 20, 30, 40, 50, 60, Inf),
      labels = c("0", "10-20", "20-30", "30-40", "40-50", "50-60", "> 60"),
      include.lowest = TRUE
    ) |>
      as.factor()
  ) |>
  ggplot() +
  geom_sf(aes(fill = human_incidence_range)) +
  scale_fill_manual(values = ylorrd_palette(7)) +
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
  )|>
  ggplot() +
  geom_sf(aes(fill = catt_incidence_range)) +
  scale_fill_manual(values = ylorrd_palette2(6)) +
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
  scale_fill_manual(values = ylorrd_palette3(5)) +
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
  scale_fill_manual(values = ylorrd_palette4(4)) +
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
  scale_fill_manual(values = ylorrd_palette4(4)) +
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
cor_lag <- df_diff %>%
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
  labs(subtitle = "Correlation between\nhuman incidence \nand animal incidences",
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
cor_lag

# Correlation Plot at lag 1
cor_lag1 <- df_diff %>%
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
cor_lag2 <- df_diff %>%
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
cor_lag3 <- df_diff %>%
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
cor_lag4 <- df_diff %>%
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
cor_lag5 <- df_diff %>%
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
cor_lag6 <- df_diff %>%
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
                             axis.text.y = element_text(color = 'black', size = 13)
                             )

# Time series linear model ------------------------------------------------
# Lag 6 seems the best for the model
df_2 <- df_diff |>
  as_tibble() %>%
  mutate_at(vars(catt_incidence, cam_incidence, goat_incidence, shp_incidence),
            list( ~ lag(., n = 3))) |>
  na.omit() |>
  mutate(date = as.Date(date))


# The model
mod_lag6 <- df_2 |>
  as_tsibble() |>
  model(
    TSLM(
      (human_incidence) ~ catt_incidence + cam_incidence +  shp_incidence + goat_incidence
    )
  ) |>
  report()

mod_lag6_results <- tidy(mod_lag6) %>%
  select(-.model) %>%
  as_tibble() %>%
  mutate(term = case_when(
    term == "goat_incidence" ~ "Goat Incidence",
    term == "catt_incidence" ~ "Cattle incidence",
    term == "shp_incidence" ~ "Sheep incidence",
    term == "cam_incidence" ~ "Camel incidence",
    TRUE ~ as.character(term)  # Keep unchanged if not specified
  ),
  variable = term
  ) |>  select(6, 2:5) |> 
  group_by(variable) %>%
  mutate(
    conf_low = min(estimate - std.error * 1.96),
    conf_high = max(estimate + std.error * 1.96)
  )


# The model after removing the goat incidence
mod_lag6.1 <- df_2 |>
  as_tsibble() |>
  model(TSLM(
    human_incidence ~   cam_incidence +  shp_incidence +goat_incidence
  )) |>
  report()

mod_lag6_results2 <- tidy(mod_lag6.1) %>%
  select(-.model) %>%
  as_tibble() %>%
  mutate(term = case_when(
    term == "catt_incidence" ~ "Cattle incidence",
    term == "shp_incidence" ~ "Sheep incidence",
    term == "cam_incidence" ~ "Camel incidence",
    TRUE ~ as.character(term)  # Keep unchanged if not specified
  ),
  variable = term
  ) |>  select(6, 2:5) |> 
  group_by(variable) %>%
  mutate(
    conf_low = min(estimate - std.error * 1.96),
    conf_high = max(estimate + std.error * 1.96)
  )


# Individual models -------------------------------------------------------

# Human incidence and cattle
catt_mod <-  df_2 |>
  as_tsibble() |>
  model(
    TSLM(
      (human_incidence) ~ catt_incidence
    )
  ) |>
  report()

catt_mod_results <- tidy(catt_mod) %>%
  select(-.model) %>%
  as_tibble() %>%
  mutate(term = case_when(
    term == "catt_incidence" ~ "Cattle Incidence",
    TRUE ~ as.character(term)  # Keep unchanged if not specified
  ),
  variable = term
  ) |>  select(6, 2:5) |> 
  group_by(variable) %>%
  mutate(
    conf_low = min(estimate - std.error * 1.96),
    conf_high = max(estimate + std.error * 1.96)
  )


# Human incidence and goat
goat_mod <-  df_2 |>
  as_tsibble() |>
  model(
    TSLM(
      (human_incidence) ~ goat_incidence
    )
  ) |>
  report()

goat_mod_results <- tidy(goat_mod) %>%
  select(-.model) %>%
  as_tibble() %>%
  mutate(term = case_when(
    term == "goat_incidence" ~ "Goat Incidence",
    TRUE ~ as.character(term) 
  ),
  variable = term
  ) |>  select(6, 2:5) |> 
  group_by(variable) %>%
  mutate(
    conf_low = min(estimate - std.error * 1.96),
    conf_high = max(estimate + std.error * 1.96)
  )


# Human incidence and sheep
shp_mod <-  df_2 |>
  as_tsibble() |>
  model(
    TSLM(
      (human_incidence) ~ shp_incidence
    )
  ) |>
  report()

shp_mod_results <- tidy(shp_mod) %>%
  select(-.model) %>%
  as_tibble() %>%
  mutate(term = case_when(
    term == "shp_incidence" ~ "Sheep Incidence",
    TRUE ~ as.character(term)
  ),
  variable = term
  ) |>  select(6, 2:5) |> 
  group_by(variable) %>%
  mutate(
    conf_low = min(estimate - std.error * 1.96),
    conf_high = max(estimate + std.error * 1.96)
  )


# Human incidence and camel
cam_mod <-  df_2 |>
  as_tsibble() |>
  model(
    TSLM(
      (human_incidence) ~ cam_incidence
    )
  ) |>
  report()

cam_mod_results <- tidy(cam_mod) %>%
  select(-.model) %>%
  as_tibble() %>%
  mutate(term = case_when(
    term == "cam_incidence" ~ "Camel Incidence",
    TRUE ~ as.character(term) 
  ),
  variable = term
  ) |>  select(6, 2:5) |> 
  group_by(variable) %>%
  mutate(
    conf_low = min(estimate - std.error * 1.96),
    conf_high = max(estimate + std.error * 1.96)
  )


# Model validation --------------------------------------------------------

# 1. The full model
augment(mod_lag6) %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  labs(
    y = "Residuals",
    x = "Fitted"
  ) +
  theme_light()

# Predicted time series versus the actual
p1 <- augment(mod_lag6) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = human_incidence, color = "Observed"), linewidth = 1) +
  geom_line(aes(y = .fitted, color = "Fitted"), linewidth = 1) +
  theme_light() +
  scale_color_manual(values = c("#4E79A7", "#E41A1C")) +
  theme(
    strip.background = element_rect(fill = "white", colour = "grey"),
    strip.text = element_text(color = "black", size = 12),
    axis.title = element_text(colour = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    plot.title = element_text(
      color = "black",
      hjust = 0.5,
      size = 12
    ),
    axis.title.y = element_text(color = "black", size = 10),
    legend.position = "bottom",
    legend.text = element_text(color = "black")
  ) +
  labs(color = "") +
  ylab("human incidence") +
  guides(col = "none")

# Predicted Versus Actual
p2 <- augment(mod_lag6) %>%
  ggplot(aes(x = human_incidence, y = .fitted)) +
  geom_point(color = "goldenrod", size = 1) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = 2,
    size = 1,
    color = "dark gray"
  ) +
  theme_light() +
  labs(subtitle = "Predicted vs actuals")

p = p1 + p2 +
  patchwork::plot_annotation(title = "Fitted values plots")

# 2. Without Goat incidence
augment(mod_lag6.1) %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  theme_light()

# Predicted time series versus the actual
q1 <- augment(mod_lag6.1) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = human_incidence, color = "Observed"), size = 1) +
  geom_line(aes(y = .fitted, color = "Fitted"), size = 1) +
  theme_light() +
  scale_color_manual(values = c("#4E79A7", "#E41A1C")) +  # Blue and Red
  theme(
    strip.background = element_rect(fill = "white", colour = "grey"),
    strip.text = element_text(color = "black", size = 12),
    axis.title = element_text(colour = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    plot.title = element_text(
      color = "black",
      hjust = 0.5,
      size = 12
    ),
    axis.title.y = element_text(color = "black", size = 10),
    legend.position = "right",
    legend.text = element_text(color = "black")
  ) +
  labs(color = "") +
  ylab("Human incidence")

q1
# Predicted Versus Actual
q2 <- augment(mod_lag6.1) %>%
  ggplot(aes(x = human_incidence, y = .fitted)) +
  geom_point(size = 1) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = 2,
    size = 1,
    color = "dark gray"
  ) +
  theme_light() +
  theme(
    strip.background = element_rect(fill = "white", colour = "grey"),
    strip.text = element_text(color = "black", size = 12),
    axis.title = element_text(colour = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    plot.title = element_text(
      color = "black",
      hjust = 0.5,
      size = 12
    ),
    axis.title.y = element_text(color = "black", size = 10),
    legend.position = "bottom",
    legend.text = element_text(color = "black")
  ) +
  labs(subtitle = "Predicted vs actuals")

q = q1 + q2 +
  patchwork::plot_annotation(title = "Fitted values plots")
pq <- wrap_plots(p1 + q1, guides = "collect")


# The acf plot

residuals <- residuals(mod_lag6)$.resid 
acf(residuals)

var(residuals)

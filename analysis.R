
# Importing Packages ------------------------------------------------------

if (require(pacman)) p_load(tidyverse, tseries, data.table, scales, zoo, forecast, sf, patchwork, grid)

# Importing data ----------------------------------------------------------

df_inci <- fread("all_bruc_incidence.csv")
df_numbers <- fread("all_bruc_pop.csv")
shp <- st_read("shapefiles/County.shp", quiet = T)
eco_zones <- fread("eco_zones_county.csv")

# Descriptive Statistics --------------------------------------------------

# Number of Cases Per County
df_county <- df_inci %>%
  group_by(county) %>%
  mutate(across(contains(c("cases")), ~sum(., na.rm = TRUE))) 

# Incidence Per County
df_inci_county <- df_county |> 
  mutate(
    human_incidence = round((hum_cases/hum_pop) * 1000),
    catt_incidence = round((catt_cases / catt_pop) * 1000000, 4),
    cam_incidence = round((cam_cases / cam_pop) * 1000000, 4),
    goat_incidence = round((goat_cases / goat_pop) * 1000000, 4),
    shp_incidence = round((shp_cases / sheep_pop) * 1000000, 4)
  )

# The descriptive statistics are for the number of cases National Wide
table1 <- df_county %>%
  select(county, contains("cases")) %>%
  pivot_longer(cols = -1) %>%
  group_by(name) %>%
  summarise(
    cases = sum(value, na.rm = TRUE),
    minimum = min(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE)
  ) %>%
  arrange(desc(cases)) %>%
  mutate(
    name = c("Human", "Goat", "Cattle", "Sheep", "Camel"),
    Cases = comma(cases),
    Minimum = comma(minimum),
    Median = comma(median),
    Maximum = comma(max),
    `Standard Deviation` = comma(sd)
  ) %>%
  select(Species = name, Cases, Minimum, Median, Maximum, `Standard Deviation`) |> 
  knitr::kable(align = "c", caption = "Descriptive statistics for the number of cases", format = "pipe")

order <- c("Humans", 'Goats', "Cattle", "Sheep", "Camels")
table1.1 <- df_inci |> 
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
  knitr::kable(align = "c", caption = "Number of cases according to the type of Diagnosis", format = "pipe")



# The descriptive statistics are for the Incidence Rate National Wide
table2 <- df_inci_county %>%
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
  knitr::kable(align = "c", caption = "Descriptive Statistics for Incidence Rate", format = "pipe")

table(df_inci$diagnosis)


# Trend -------------------------------------------------------------------

# df_trend <- df_inci |> 
#   filter(!is.na(date)) |> 
#   group_by(date) |> 
#   summarise(across(contains(c("cases", "pop")), ~ sum(., na.rm = T))) |> 
# mutate(
#   human_incidence = round((hum_cases/hum_pop) * 1000, 2),
#   catt_incidence = round((catt_cases / catt_pop) * 1000000, 2), # The incidence rate per 10,000,000
#   cam_incidence = round((cam_cases / cam_pop) * 1000000, 2),
#   goat_incidence = round((goat_cases / goat_pop) * 1000000, 2),
#   shp_incidence = round((shp_cases / sheep_pop) * 1000000, 2)
# )


df_pop <- df_inci %>%
  select(date, contains("pop")) %>%
  distinct(date, .keep_all = TRUE)

df_ <- df_inci |> 
  filter(!is.na(date)) |>
  select(-y,-diseases,-contains("incidence")) |> 
  group_by(date) |> 
  summarise(across(contains("cases"), ~sum(., na.rm = T))) |> 
  merge(df_pop, by = c("date"))

df_ <- df_ |> 
  mutate(
    human_incidence = (hum_cases / hum_pop),
    catt_incidence = (catt_cases / catt_pop),
    cam_incidence = (cam_cases / cam_pop),
    goat_incidence = (goat_cases / goat_pop),
    shp_incidence = (shp_cases / sheep_pop) 
  ) |>
  ungroup()

order <- c(
  "human_incidence",
  "catt_incidence",
  "goat_incidence",
  "shp_incidence",
  "cam_incidence"
)

trend_data <- df_ %>%
  select(date, contains("incidence")) %>%
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
humans_plt

# Loess smoothing
df_trend <- df_inci |> 
  filter(!is.na(date)) |> 
  group_by(date) |> 
  summarise(across(contains(c("cases", "pop")), ~ sum(., na.rm = T))) |> 
  mutate(
    human_incidence = round((hum_cases/hum_pop) * 1000, 2),
    catt_incidence = round((catt_cases / catt_pop) * 1000000, 2), # The incidence rate per 10,000,000
    cam_incidence = round((cam_cases / cam_pop) * 1000000, 2),
    goat_incidence = round((goat_cases / goat_pop) * 1000000, 2),
    shp_incidence = round((shp_cases / sheep_pop) * 1000000, 2)
  )

order <- c(
  "human_incidence",
  "catt_incidence",
  "goat_incidence",
  "shp_incidence",
  "cam_incidence"
)

trend_loess_plt <- df_trend %>%
  select(date, contains("incidence")) %>%
  pivot_longer(cols = -date) %>%
  mutate(
    name = factor(name, levels = order),
    name = factor(name, labels = c(
      "Human Incidence", "Cattle Incidence", "Goat Incidence",
      "Sheep Incidence", "Camel Incidence"
    ))
  ) %>%
  ggplot(aes(x = date)) +
  geom_smooth(aes(y = value, col = name), se = F, linewidth = 1) +
  facet_wrap(~name, scales = "free", ncol = 3) +
  theme_light() +
  theme(
    strip.background = element_rect(fill = "white", colour = "grey"),
    strip.text = element_text(color = "black", size = 12),
    axis.title = element_text(colour = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    plot.title = element_text(color = "black", hjust = 0.5, size = 12)
  ) +
  guides(col = "none")

trend_loess_plt

# Moving Average Smoothing ------------------------------------------------


# Select columns containing "incidence"
incidence_cols <- grep("incidence", names(df_trend), value = TRUE)

# Apply exponential smoothing to selected columns
smoothed_df <- df_trend %>%
  mutate(across(all_of(incidence_cols), ~zoo::rollmean(., k = 3, fill = NA), .names = "smoothed_{.col}")) |> 
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

humans_sm_plt <- trend_data_smoothed |> 
  filter(name == "Human Incidence") |> 
  ggplot(aes(x = date)) +
  geom_line(aes(y = value), linewidth = 1) +
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

# spatial -------------------------------------------------------------------

df_spatial <- df_inci |> 
  filter(!is.na(date)) |> 
  group_by(year = year(date), county) |> 
  summarise(across(contains(c("cases", "pop")), ~sum(., na.rm = T))) |> 
  mutate(
    human_incidence = round((hum_cases/hum_pop) * 1000, 3),
    catt_incidence = round((catt_cases / catt_pop) * 1000000, 3), # The incidence rate per 1,000,000
    cam_incidence = round((cam_cases / cam_pop) * 1000000, 3) ,
    goat_incidence = round((goat_cases / goat_pop) * 1000000, 3),
    shp_incidence = round((shp_cases / sheep_pop) * 1000000, 3)
  ) |> 
  select(year, county, contains("incidence"))

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
# 
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


# human <- df_spatial_merged |> 
#   ggplot() +
#   geom_sf(aes(fill = human_incidence)) +
#   theme_void() +
#   scale_fill_gradient(low = "white",
#                       high = "tomato"
#   ) + 
#   facet_wrap(~year, nrow = 1) +
#   theme(
#     plot.title = element_text(color = "black", hjust = .5, size = 16),
#     legend.position = "bottom",
#     legend.text = element_text(size = 10), 
#     legend.title = element_text(size = 7),
#     legend.key.size = unit(0.3, "cm"),
#     strip.text = element_text(colour = "black", size = 16)
#   ) + ggtitle("Humans") +
#   labs(fill = "Humans") +
#   guides(fill = "none")
# human
# 
# cattle <- df_spatial_merged |> 
#   ggplot() +
#   geom_sf(aes(fill = catt_incidence)) +
#   theme_void() +
#   scale_fill_gradient(low = "white",
#                       high = "tomato"
#   ) + 
#   facet_wrap(~year, nrow = 1) +
#   theme(
#     plot.title = element_text(color = "black", hjust = .5, size = 16),
#     legend.position = "bottom",
#     legend.text = element_text(size = 8), 
#     legend.title = element_text(size = 7),
#     legend.key.size = unit(0.3, "cm") ,
#     strip.text = element_text(colour = "black", size = 16)
#   ) +
#   labs(fill = "Cattle") +
#   ggtitle("Cattle")  +
#   guides(fill = "none")
# cattle
# 
# goat <- df_spatial_merged |> 
#   ggplot() +
#   geom_sf(aes(fill = goat_incidence)) +
#   theme_void() +
#   scale_fill_gradient(low = "white",
#                       high = "tomato"
#   ) + 
#   facet_wrap(~year, nrow = 1) +
#   theme(
#     plot.title = element_text(color = "black", hjust = .5, size = 16),
#     legend.position = "bottom",
#     legend.text = element_text(size = 8), 
#     legend.title = element_text(size = 7),
#     legend.key.size = unit(0.3, "cm") ,
#     strip.text = element_text(colour = "black", size = 16)
#   ) + ggtitle("Humans") +
#   labs(fill = "Goats") +
#   ggtitle("Goats") +
#   guides(fill = "none")
# goat
# 
# sheep <- df_spatial_merged |> 
#   ggplot() +
#   geom_sf(aes(fill = shp_incidence)) +
#   theme_void() +
#   scale_fill_gradient(low = "white",
#                       high = "tomato"
#   ) + 
#   facet_wrap(~year, nrow = 1) +
#   theme(
#     plot.title = element_text(color = "black", hjust = .5, size = 16),
#     legend.position = "bottom",
#     legend.text = element_text(size = 8), 
#     legend.title = element_text(size = 7),
#     legend.key.size = unit(0.3, "cm") ,
#     strip.text = element_text(colour = "black", size = 16)
#   ) + ggtitle("Humans") +
#   labs(fill = "Sheep") +
#   ggtitle("Sheep") +
#   guides(fill = "none")
# sheep
# 
# camels <- df_spatial_merged |> 
#   ggplot() +
#   geom_sf(aes(fill = cam_incidence)) +
#   theme_void() +
#   scale_fill_gradient(low = "white",
#                       na.value  = "white",
#                       high = "tomato"
#   ) +  
#   facet_wrap(~year, nrow = 1) +
#   theme(
#     plot.title = element_text(color = "black", hjust = .5, size = 16),
#     legend.position = "bottom",
#     legend.text = element_text(size = 8), 
#     legend.title = element_text(size = 7),
#     legend.key.size = unit(0.3, "cm") ,
#     strip.text = element_text(colour = "black", size = 16)
#   ) + ggtitle("Humans") +
#   labs(fill = "Camels") +
#   ggtitle("Camels") +
#   guides(fill = "none")
# camels

all_plots <-
  wrap_plots(human, cattle, goat, sheep, camels, ncol = 1, guides = "keep") +
  plot_annotation(caption = "For humans, the incidence rate is per 1,000 population while for other species,
                  the incidence rate is per 1,000,000 population
                  "
  ) &
  theme(plot.caption = element_text(size = 16, colour = "black"))

# row_label_1 <- wrap_elements(panel = textGrob('Human', x = unit(0.95, "npc")))
# row_label_2 <- wrap_elements(panel = textGrob('Cattle', x = unit(0.95, "npc")))
# row_label_3 <- wrap_elements(panel = textGrob('Goats', x = unit(0.95, "npc")))
# row_label_4 <- wrap_elements(panel = textGrob('Sheep', x = unit(0.95, "npc")))
# row_label_5 <- wrap_elements(panel = textGrob('Camels', x = unit(0.95, "npc")))
# 
# 
# 
# (
#   row_label_1 |
#     human
# ) / (row_label_2 | cattle) /(row_label_3 | goat)/(row_label_4 | sheep)/ (row_label_5 | camels) 
#   plot_layout(widths = c(.1, 1, 1)) 
# 
# patchwork::inset_element()

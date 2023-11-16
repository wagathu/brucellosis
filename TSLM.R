# Importing Packages ------------------------------------------------------

if (require(pacman))
{
  p_load(tidyverse,
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
         feasts
         )
}

# Importing data ----------------------------------------------------------

df_incidence <- fread("all_bruc_incidence.csv")
df_numbers <- fread("all_bruc_pop.csv")
shp <- st_read("shapefiles/County.shp", quiet = T)
eco_zones <- fread("eco_zones_county.csv")

# Grouping data -----------------------------------------------------------

# We're grouping data by date so that we can obtain a time series
df_tot_cases <- df_incidence |> 
  group_by(date) |> 
  summarise(across(contains("cases"), ~sum(., na.rm = T)))

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
    human_incidence = round((hum_cases/hum_pop) * 1000, 4),
    catt_incidence = round((catt_cases / catt_pop) * 1000000, 4),
    cam_incidence = round((cam_cases / cam_pop) * 1000000, 4),
    goat_incidence = round((goat_cases / goat_pop) * 1000000, 4),
    shp_incidence = round((shp_cases / sheep_pop) * 1000000, 4)
  ) |> 
  select(date, contains("incidence")) |> 
  as_tibble()

# Correlation -------------------------------------------------------------
library(data.table)
urca::ca.jo(df[, .SD, .SDcols != ('human incidence')])
library(data.table)

dd = data.table::setDT(df_2)

dd = dd[, .SD, .SDcols = !c("human_incidence", 'date')]

library(urca)
KK = ca.jo(dd)
summary(KK)

df_2 |> 
  select( -human_incidence)


# Correlation Plot
cor_lag <- df_1 %>%
  as_tibble() %>%
  select(-date) %>%
  setNames(
    c(
      "Human",
      "Cattle",
      "Camel",
      "Goat",
      "Sheep"
    )
  ) |>
  cor() %>%
  ggcorrplot::ggcorrplot(type = "upper",
                         lab = TRUE,
                         lab_size = 3) +
  theme_light() +
  labs(
    subtitle = "Correlation between human incidence \nand animal incidences",
    x = NULL,
    y = NULL
  ) + 
  guides(fill = "none") +
  theme(
    strip.background = element_rect(fill = "white", colour = "grey"),
    strip.text = element_text(color = "black", size = 12),
    axis.title = element_text(colour = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    plot.title = element_text(color = "black", hjust = 0.5, size = 12),
    axis.title.y = element_text(color = "black", size = 10),
    legend.position = "right",
    legend.text = element_text(color = "black")
  )
cor_lag

# Correlation Plot at lag 1
cor_lag1 <- df_1 %>%
  as_tibble() %>%
  mutate_at(
    vars(catt_incidence, cam_incidence, goat_incidence, shp_incidence),
    list(~lag(., n = 1))
  ) |> 
  na.omit() |> 
  select(-date) %>%
  setNames(
    c(
      "Human",
      "Cattle",
      "Camel",
      "Goat",
      "Sheep"
    )
  ) |>
  cor() %>%
  ggcorrplot::ggcorrplot(type = "upper",
                         lab = TRUE,
                         lab_size = 3) +
  theme_light() +
  labs(
    subtitle = "Correlation between human incidence \nand animal incidences at lag 1",
    x = NULL,
    y = NULL
  ) + 
  guides(fill = "none") +
  theme(
    strip.background = element_rect(fill = "white", colour = "grey"),
    strip.text = element_text(color = "black", size = 12),
    axis.title = element_text(colour = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    plot.title = element_text(color = "black", hjust = 0.5, size = 12),
    axis.title.y = element_text(color = "black", size = 10),
    legend.position = "right",
    legend.text = element_text(color = "black")
  )
cor_lag1

# Correlation Plot at lag 2
cor_lag2 <- df_1 %>%
  as_tibble() %>%
  mutate_at(
    vars(catt_incidence, cam_incidence, goat_incidence, shp_incidence),
    list(~lag(., n = 2))
  ) |> 
  na.omit() |> 
  select(-date) %>%
  setNames(
    c(
      "Human",
      "Cattle",
      "Camel",
      "Goat",
      "Sheep"
    )
  ) |>
  cor() %>%
  ggcorrplot::ggcorrplot(type = "upper",
                         lab = TRUE,
                         lab_size = 3) +
  theme_light() +
  labs(
    subtitle = "Correlation between human incidence \nand animal incidences at lag 2",
    x = NULL,
    y = NULL
  ) + 
  guides(fill = "none") +
  theme(
    strip.background = element_rect(fill = "white", colour = "grey"),
    strip.text = element_text(color = "black", size = 12),
    axis.title = element_text(colour = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    plot.title = element_text(color = "black", hjust = 0.5, size = 12),
    axis.title.y = element_text(color = "black", size = 10),
    legend.position = "right",
    legend.text = element_text(color = "black")
  )
cor_lag2

# Correlation Plot at lag 3
cor_lag3 <- df_1 %>%
  as_tibble() %>%
  mutate_at(
    vars(catt_incidence, cam_incidence, goat_incidence, shp_incidence),
    list(~lag(., n = 3))
  ) |> 
  na.omit() |> 
  select(-date) %>%
  setNames(
    c(
      "Human",
      "Cattle",
      "Camel",
      "Goat",
      "Sheep"
    )
  ) |>
  cor() %>%
  ggcorrplot::ggcorrplot(type = "upper",
                         lab = TRUE,
                         lab_size = 3) +
  theme_light() +
  labs(
    subtitle = "Correlation between human incidence \nand animal incidences at lag 3",
    x = NULL,
    y = NULL
  ) + 
  guides(fill = "none") +
  theme(
    strip.background = element_rect(fill = "white", colour = "grey"),
    strip.text = element_text(color = "black", size = 12),
    axis.title = element_text(colour = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    plot.title = element_text(color = "black", hjust = 0.5, size = 12),
    axis.title.y = element_text(color = "black", size = 10),
    legend.position = "right",
    legend.text = element_text(color = "black")
  )
cor_lag3

# Correlation Plot at lag 4
cor_lag4 <- df_1 %>%
  as_tibble() %>%
  mutate_at(
    vars(catt_incidence, cam_incidence, goat_incidence, shp_incidence),
    list(~lag(., n = 4))
  ) |> 
  na.omit() |> 
  select(-date) %>%
  setNames(
    c(
      "Human",
      "Cattle",
      "Camel",
      "Goat",
      "Sheep"
    )
  ) |>
  cor() %>%
  ggcorrplot::ggcorrplot(type = "upper",
                         lab = TRUE,
                         lab_size = 3) +
  theme_light() +
  labs(
    subtitle = "Correlation between human incidence \nand animal incidences at lag 4",
    x = NULL,
    y = NULL
  ) + 
  guides(fill = "none") +
  theme(
    strip.background = element_rect(fill = "white", colour = "grey"),
    strip.text = element_text(color = "black", size = 12),
    axis.title = element_text(colour = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    plot.title = element_text(color = "black", hjust = 0.5, size = 12),
    axis.title.y = element_text(color = "black", size = 10),
    legend.position = "right",
    legend.text = element_text(color = "black")
  )
cor_lag4

# Correlation Plot at lag 5
cor_lag5 <- df_1 %>%
  as_tibble() %>%
  mutate_at(
    vars(catt_incidence, cam_incidence, goat_incidence, shp_incidence),
    list(~lag(., n = 5))
  ) |> 
  na.omit() |> 
  select(-date) %>%
  setNames(
    c(
      "Human",
      "Cattle",
      "Camel",
      "Goat",
      "Sheep"
    )
  ) |>
  cor() %>%
  ggcorrplot::ggcorrplot(type = "upper",
                         lab = TRUE,
                         lab_size = 3) +
  theme_light() +
  labs(
    subtitle = "Correlation between human incidence \nand animal incidences at lag 5",
    x = NULL,
    y = NULL
  ) + 
  guides(fill = "none") +
  theme(
    strip.background = element_rect(fill = "white", colour = "grey"),
    strip.text = element_text(color = "black", size = 12),
    axis.title = element_text(colour = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    plot.title = element_text(color = "black", hjust = 0.5, size = 12),
    axis.title.y = element_text(color = "black", size = 10),
    legend.position = "right",
    legend.text = element_text(color = "black")
  )
cor_lag5

# Correlation Plot at lag 6
cor_lag6 <- df_1 %>%
  as_tibble() %>%
  mutate_at(
    vars(catt_incidence, cam_incidence, goat_incidence, shp_incidence),
    list(~lag(., n = 6))
  ) |> 
  na.omit() |> 
  select(-date) %>%
  setNames(
    c(
      "Human",
      "Cattle",
      "Camel",
      "Goat",
      "Sheep"
    )
  ) |>
  cor() %>%
  ggcorrplot::ggcorrplot(type = "upper",
                         lab = TRUE,
                         lab_size = 3) +
  theme_light() +
  labs(
    subtitle = "Correlation between human incidence \nand animal incidences at lag 6",
    x = NULL,
    y = NULL
  ) +
  theme(
    strip.background = element_rect(fill = "white", colour = "grey"),
    strip.text = element_text(color = "black", size = 12),
    axis.title = element_text(colour = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    plot.title = element_text(color = "black", hjust = 0.5, size = 12),
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
) 

# Time series linear model ------------------------------------------------
# Lag 6 seems the best for the model
df_2 <- df_1 |> 
  as_tibble() %>%
  mutate_at(
    vars(catt_incidence, cam_incidence, goat_incidence, shp_incidence),
    list(~lag(., n = 6))
  ) |> 
  na.omit() |> 
  mutate(date = as.Date(date))


# The model 
mod_lag6 <- df_1 |>
  as_tsibble() |>
  model(
    TSLM(
      diff(human_incidence) ~ catt_incidence + cam_incidence +  shp_incidence + goat_incidence + season()-1
    )
  ) |>
  report()

# The model after removing the goat incidence
mod_lag6.1 <- df_2 |>
  as_tsibble() |>
  model(
    TSLM(
      human_incidence ~ catt_incidence + cam_incidence +  shp_incidence-1 
    )
  ) |>
  report()

# Model validation --------------------------------------------------------

# 1. The full model
augment(mod_lag6) %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  labs(title = "There is no relationship between residuals and fitted values.",
       subtitle = "otherwise the response variable may require transformation.",
       y = "Residuals", x = "Fitted") +
  theme_light()

# Predicted time series versus the actual
p1 <- augment(mod_lag6) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = human_incidence, color = "Observed"), size = 1) +
  geom_line(aes(y = .fitted, color = "Fitted"), size = 1) +
  theme_light() +
  scale_color_manual(values = c("#4E79A7", "#E41A1C")) + 
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
  labs(color = "") +
  ylab("human incidence") +
  guides(col = "none")

# Predicted Versus Actual
p2 <- augment(mod_lag6) %>%
  ggplot(aes(x = human_incidence, y = .fitted)) +
  geom_point(color = "goldenrod", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2, size = 1, color = "dark gray") +
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
    plot.title = element_text(color = "black", hjust = 0.5, size = 12),
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
  geom_abline(intercept = 0, slope = 1, linetype = 2, size = 1, color = "dark gray") +
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
  labs(subtitle = "Predicted vs actuals") 

q = q1 + q2 +
  patchwork::plot_annotation(title = "Fitted values plots")
pq <- wrap_plots(p1 + q1, guides = "collect")



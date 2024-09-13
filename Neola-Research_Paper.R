rm(list=ls())
library(tidyverse)
library(stringr)
library(broom)
library(gridExtra)
library(ggthemes)
library(stargazer)
options(scipen = 999) # avoids scientific notation

faostat <- readRDS("~/Google Drive/Shared drives/AREC280F23/Data/FAOSTAT/FAO_crop_production_e_all_data.rds")
tmp <- read_csv("Google Drive/Shared drives/AREC280F23/Data/CRUclimate/tmp.csv")
pre <- read_csv("Google Drive/Shared drives/AREC280F23/Data/CRUclimate/pre.csv")

# gathering production data
prod <- faostat %>%
  filter(area %in% c("India", "Bangladesh", "Sri Lanka"), item == "Tea leaves", year >= 1976, element == "production") %>%
  select(area_code, area, item, year, value) %>%
  group_by(area, year) %>%
  mutate(annual_production = mean(value)) 

prod <- prod %>%
  group_by(area) %>% # says that all the countries are related
  arrange(year) %>% 
  filter(annual_production != 0) %>%
  mutate(prod_growth = log(annual_production) - log(lag(annual_production)),
         growth_percent = prod_growth * 100)

# gathering temperature data
wider_tmp <- tmp %>%
  filter(country_name %in% c("India", "Bangladesh", "Sri_Lanka"), year >= 1976, time %in% c("MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC", "JAN", "FEB")) %>%
  pivot_wider(names_from = time, values_from = value) 

wider_tmp$country_name <- str_replace(wider_tmp$country_name, "Sri_Lanka", "Sri Lanka")

# calculating temperature variation by month
wider_tmp_sum <- wider_tmp %>%
  group_by(country_name,year)%>%
  summarize(min_monthly_average = min(MAR:NOV),
            max_monthly_average = max(MAR:NOV),
            avg_monthly_average = mean(MAR:NOV),
            var_monthly_average = sd(MAR:NOV)^2, # 0/NA is no variance, the larger it is the more variant
            avg_monthly_average_growing = mean(MAR:NOV),
            avg_monthly_average_off = mean(JAN, FEB, DEC),
            months_below = rowSums(across(MAR:NOV, ~ .x < 13)),
            months_above = rowSums(across(MAR:NOV, ~ .x > 28)),
            months_in_range = 9 - months_below - months_above) %>% 
  replace_na(list(var_monthly_average = 0))

tmp_joined <- wider_tmp %>%
  left_join(wider_tmp_sum, by = c("country_name", "year")) %>%
  mutate(avg_log_difference = log(avg_monthly_average) - log(lag(avg_monthly_average)))

hist(tmp_joined$months_below) # all 0
hist(tmp_joined$months_above) # values from 0 to 8, 2 is max
hist(tmp_joined$months_in_range) # values from 2 to 9, 7 is max

# doing the same for precipitation
wider_pre <- pre %>%
  filter(country_name %in% c("India", "Bangladesh", "Sri_Lanka"), year >= 1976, time %in% c("MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC", "JAN", "FEB")) %>%
  pivot_wider(names_from = time, values_from = value)

wider_pre$country_name <- str_replace(wider_pre$country_name, "Sri_Lanka", "Sri Lanka")

wider_pre_sum <- wider_pre %>%
  group_by(country_name,year) %>% 
  summarize(min_monthly_average = min(MAR:NOV),
            max_monthly_average = max(MAR:NOV),
            avg_monthly_average_growing = mean(MAR:NOV),
            avg_monthly_average_off = mean(JAN, FEB, DEC),
            var_monthly_average = sd(MAR:NOV)^2,
            annual_sum = rowSums(across(JAN:DEC)),
            growing_sum = rowSums(across(MAR:NOV)),
            off_sum = annual_sum - growing_sum,
            is_year_in_range = ifelse((annual_sum <= 3000) & (annual_sum >= 2000), 1, 0),
            months_below = rowSums(across(MAR:NOV, ~ .x < 125)),
            months_above = rowSums(across(MAR:NOV, ~ .x > 208)),
            months_in_range = 9 - months_below - months_above) %>% 
  replace_na(list(var_monthly_average = 0))

pre_joined <- wider_pre %>%
  left_join(wider_pre_sum, by = c("country_name", "year"))

hist(pre_joined$months_below) # values from 1 to 8, 5 is max
hist(pre_joined$months_above) # values from 0 to 6, 2 is max
hist(pre_joined$months_in_range) # values from 0 to 4, 2 is max

# joining production data with climate data
wider_joined <- prod %>%
  left_join(tmp_joined, by = c("area" = "country_name", "year")) %>%
  left_join(pre_joined, by = c("area" = "country_name", "year"), suffix = c("_tmp", "_pre"))

# STATISTICAL (normal) GRAPHS
# GRAPH 1: climate factors based on growing season vs off-season
# 1. make data frame with necessary data
precipitation_variability_df <- pre_joined %>%
  select(country_name, year, avg_monthly_average_growing, avg_monthly_average_off) %>%
  pivot_longer(avg_monthly_average_growing:avg_monthly_average_off, names_to = "season", values_to = "avg_monthly_pre")

precipitation_variability_df$season <- str_replace(precipitation_variability_df$season, "avg_monthly_average_growing", "growing_season")
precipitation_variability_df$season <- str_replace(precipitation_variability_df$season, "avg_monthly_average_off", "off_season")

temp_variability_df <- tmp_joined %>%
  select(country_name, year, avg_monthly_average_growing, avg_monthly_average_off) %>%
  pivot_longer(avg_monthly_average_growing:avg_monthly_average_off, names_to = "season", values_to = "avg_monthly_tmp")

temp_variability_df$season <- str_replace(temp_variability_df$season, "avg_monthly_average_growing", "growing_season")
temp_variability_df$season <- str_replace(temp_variability_df$season, "avg_monthly_average_off", "off_season")

variability_df <- precipitation_variability_df %>%
  left_join(temp_variability_df, by = c("country_name", "year", "season")) %>%
  pivot_longer(avg_monthly_pre:avg_monthly_tmp, names_to = "climate_factor", values_to = "value")

variability_df$climate_factor <- str_replace(variability_df$climate_factor, "avg_monthly_pre", "precipitation")
variability_df$climate_factor <- str_replace(variability_df$climate_factor, "avg_monthly_tmp", "temperature")

# new facet label names
climate_factor.labs <- c("Average Precipitation (mm/month)", "Average Temperature (°C)")
names(climate_factor.labs) <- c("precipitation", "temperature")

season.labs <- c("Growing Season (MAR-NOV)", "Off Season (DEC-FEB)")
names(season.labs) <- c("growing_season", "off_season")

# 2. making the graph
variability_plot <- ggplot(variability_df, aes(year, value, color = country_name)) +
  geom_point(size = 2) +
  geom_line(size = 1, alpha = 0.5) +
  geom_smooth(method = "lm", alpha = 0.5) +
  scale_color_brewer(palette = "Set2") +
  facet_grid(climate_factor ~ season, scales = "free_y", 
             labeller = labeller(climate_factor = climate_factor.labs, season = season.labs)) +
  expand_limits(y = 0) +
  labs(
    title = "Average Temperature and Precipitation Over Time by Season",
    y = "Value",
    x = "Year",
    color = "Country"
  ) + 
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = rel(1))
  )

# GRAPH 2: how many months DURING THE GROWING SEASON are in the ideal range
# 1. making necessary data frames
tmp_months_df <- wider_joined %>%
  select(area, year, months_below_tmp, months_in_range_tmp, months_above_tmp) %>%
  pivot_longer(-c(area, year), names_to = "suitability", values_to = "months_tmp")

tmp_months_df$suitability <- str_replace(tmp_months_df$suitability, "months_below_tmp", "below_range")
tmp_months_df$suitability <- str_replace(tmp_months_df$suitability, "months_in_range_tmp", "in_range")
tmp_months_df$suitability <- str_replace(tmp_months_df$suitability, "months_above_tmp", "above_range")

pre_months_df <- wider_joined %>%
  select(area, year, months_below_pre, months_in_range_pre, months_above_pre) %>%
  pivot_longer(-c(area, year), names_to = "suitability", values_to = "months_pre")

pre_months_df$suitability <- str_replace(pre_months_df$suitability, "months_below_pre", "below_range")
pre_months_df$suitability <- str_replace(pre_months_df$suitability, "months_in_range_pre", "in_range")
pre_months_df$suitability <- str_replace(pre_months_df$suitability, "months_above_pre", "above_range")


months_df <- tmp_months_df %>%
  # annual_sum, growing_sum, off_sum
  left_join(pre_months_df, by = c("area", "year", "suitability")) %>%
  pivot_longer(4:5, names_to = "climate_factor", values_to = "months")

months_df$climate_factor <- str_replace(months_df$climate_factor, "months_pre", "Precipitation")
months_df$climate_factor <- str_replace(months_df$climate_factor, "months_tmp", "Temperature")

# reordering data
months_df$suitability <- factor(months_df$suitability, levels = c("above_range", "in_range", "below_range"))

# new facet label names
climate_factor_2.labs <- c("Precipitation", "Temperature)")
names(climate_factor_2.labs) <- c("precipitation", "temperature")

# 2. making the graph
months_range_area_plot <- ggplot(months_df, aes(year, months, fill = suitability)) +
  geom_area(color = NA, alpha = 0.4) +
  scale_fill_brewer(palette = "Set2", labels = c("Above the range", "Within the range", "Below the range")) +
  geom_line(position = "stack", size = 0.2) +
  scale_y_continuous(limits = c(0, 9), breaks = seq(0, 9, 1)) +
  facet_grid(climate_factor ~ area) +
  theme_bw() +
  labs(
    title = "Number of Months Being Within the Suitable Range of Each Climate Factor During the Growing Season",
    x = "Year",
    y = "Number of Months",
    fill = "The months are ...",
  ) + 
  theme(
    strip.text = element_text(face = "bold", size = rel(1))
  )
  
# GRAPHS: how the max temperature changes over time - how min and max precipitation changes over time
max_temp <- ggplot(wider_joined, aes(year, max_monthly_average_tmp, color = area)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Maximum Monthly Temperature Over Time",
    x = "Year",
    y = "Value (°C)",
    color = "Country"
  ) +
  theme_bw()

min_pre <- ggplot(wider_joined, aes(year, min_monthly_average_pre, color = area)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Minimum Monthly Precipitation Over Time",
    x = "Year",
    y = "Value (mm)",
    color = "Country"
  ) +
  theme_bw()

max_pre <- ggplot(wider_joined, aes(year, max_monthly_average_pre, color = area)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Maximum Monthly Precipitation Over Time",
    x = "Year",
    y = "Value (mm)",
    color = "Country"
  ) +
  theme_bw()

mix_max_pre_plots <- grid.arrange(min_pre, max_pre)

# LINEAR MODELING
# calculate growth for different variables
modeling_df <- wider_joined %>%
  select(area, year, growth_percent, avg_monthly_average_growing_tmp, avg_monthly_average_growing_pre, growing_sum) %>%
  mutate(tmp_growth = log(avg_monthly_average_growing_tmp) - lag(log(avg_monthly_average_growing_tmp)),
         pre_growth = log(avg_monthly_average_growing_pre) - lag(log(avg_monthly_average_growing_pre)),
         sum_growth = log(growing_sum) - lag(log(growing_sum)))

pre_growth_plot <- ggplot(modeling_df, aes(pre_growth, growth_percent)) +
  geom_point(size = 1.5, color = "#b35806") +
  geom_line(alpha = 0.5, size = 1, color = "#f1a340") +
  theme_bw() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, color = "#998ec3") +
  facet_wrap(~ area) +
  labs(
    title = "Production Growth Over Lagged Average Monthly Precipitation",
    x = "Precipitation Rate (mm/month)",
    y = "Production Growth (tonnes)"
  )

tmp_growth_plot <- ggplot(modeling_df, aes(tmp_growth * 100, growth_percent)) +
  geom_point(size = 1.5, color = "#b35806") +
  geom_line(alpha = 0.5, size = 1, color = "#f1a340") +
  theme_bw() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, color = "#998ec3") +
  facet_wrap(~ area) +
  labs(
    title = "Production Growth Over Lagged Average Monthly Temperature",
    x = "Temperature Increase (°C)",
    y = "Production Growth (tonnes)"
  )

pre_tmp_growth_plots <- grid.arrange(pre_growth_plot, tmp_growth_plot)

# doing some modeling to see what the coefficients are
modB <- lm(growth_percent ~ months_in_range_pre + months_above_pre, wider_joined)
tidy(modB, conf.int = T, conf.level = 0.90) # positive ^ coefficient
summary(modB)

modC <- lm(growth_percent ~ months_in_range_pre + months_below_pre, wider_joined)
tidy(modC, conf.int = T, conf.level = 0.90) # negative ^ coefficient
summary(modC)

modD <- lm(growth_percent ~ months_in_range_tmp + months_above_tmp, wider_joined)
tidy(modD, conf.int = T, conf.level = 0.90) # NA ^ coefficient
summary(modD)

stargazer(modB, modC, modD, type = "text", out = "G:/Shared drives/AREC280F23/Team1/Neola/Research_SummaryStats.html",
          title = "Summary Statistics with Production Growth as a Factor of Climatic Months", align = TRUE,
          dep.var.labels = "Production Growth Percent",
          covariate.labels = c("Precipitation: Months in Range", "Precipitation: Months Above Range", 
                               "Precipitation: Months Below Range", "Temperature: Months in Range",
                               "Temperature: Months Above Range"))

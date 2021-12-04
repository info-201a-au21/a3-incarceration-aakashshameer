library(tidyverse)
library(ggplot2)
library(lintr)
library(styler)
library(datasets)
library(mapproj)
library(dplyr)

# An Analysis of the Vera Institute of Justice Incarceration Trends Dataset


data <- read.csv(paste0("https://raw.githubusercontent.com/vera-institute/",
                        "incarceration-trends/master/incarceration_trends.csv"))

# Black Population Proportion from the Year 2016.
black_pop_year_2016 <- data %>%
                        group_by(year) %>%
                        select(black_jail_pop_rate) %>%
                        filter(year == "2016") %>%
                        summarise(average = mean(black_jail_pop_rate,
                                                 na.rm = TRUE)) %>%
                        pull(average)

# Black Population Proportion from all years
black_pop_year <- data %>%
                   group_by(year) %>%
                   select(black_jail_pop_rate) %>%
                   summarise(black_year = mean(black_jail_pop_rate,
                                               na.rm = TRUE))

# Latinx Population Proportion from the Year 2016.
latinx_pop_year_2016 <- data %>%
                         group_by(year) %>%
                         select(latinx_jail_pop_rate) %>%
                         summarise(average = mean(latinx_jail_pop_rate,
                                                  na.rm = T)) %>%
                         filter(year == "2016") %>%
                         pull(average)

# Latinx Population Proportion from all years
latinx_pop_year <- data %>%
                    group_by(year) %>%
                    select(latinx_jail_pop_rate) %>%
                    summarise(latinx_year = mean(latinx_jail_pop_rate,
                                                 na.rm = TRUE))

# White Population Proportion from the Year 2016.
white_pop_year_2016 <- data %>%
                        group_by(year) %>%
                        select(white_jail_pop_rate) %>%
                        summarise(average = mean(white_jail_pop_rate,
                                                 na.rm = T)) %>%
                        filter(year == "2016") %>%
                        pull(average)

# White Population Proportion from all years
white_pop_year <- data %>%
                   group_by(year) %>%
                   select(white_jail_pop_rate) %>%
                   summarise(white_year = mean(white_jail_pop_rate, na.rm = T))

# Asian Population Proportion from the Year 2016.
asian_pop_year <- data %>%
                   group_by(year) %>%
                   select(aapi_jail_pop_rate) %>%
                   summarise(asian_year = mean(aapi_jail_pop_rate, na.rm = T))

# Native Population Proportion from the Year 2016.
native_pop_year <- data %>%
                    group_by(year) %>%
                    select(native_jail_pop_rate) %>%
                    summarise(native_year = mean(native_jail_pop_rate,
                                                 na.rm = T))

# Proportion of the Population of Races
pop_by_races <- left_join(asian_pop_year, black_pop_year, by = "year") %>%
                left_join(latinx_pop_year, by = "year") %>%
                left_join(native_pop_year, by = "year") %>%
                left_join(white_pop_year, by = "year") %>%
                filter(year >= 1990 & year <= 2018) %>%
                rename(
                  Asian = asian_year, Black = black_year, Latinx = latinx_year,
                  Native = native_year, White = white_year
                )

# Data of Population Proportion of Black Based on States
black_pop_state <- data %>%
                    group_by(state) %>%
                    select(black_jail_pop_rate) %>%
                    summarise(average = mean(black_jail_pop_rate, na.rm = T))

# Average Data of Population Proportion of Black Based on States
mean_black_pop_state <- black_pop_state %>%
                            summarise(average_state = mean(average,
                                                           na.rm = T)) %>%
                            pull(average_state)

# Data of Population Proportion of Latinx Based on States
latinx_pop_state <- data %>%
                    group_by(state) %>%
                    select(latinx_jail_pop_rate) %>%
                    summarise(average = mean(latinx_jail_pop_rate,
                                             na.rm = T)) %>%
                    summarise(average_state = mean(average, na.rm = T)) %>%
                    pull(average_state)

# Data of Population Proportion of White Based on States
white_pop_state <- data %>%
                    group_by(state) %>%
                    select(white_jail_pop_rate) %>%
                    summarise(average = mean(white_jail_pop_rate, na.rm = T))

# Average Data of Population Proportion of Black Based on States
mean_white_pop_state <- white_pop_state %>%
                        summarise(average_state = mean(average, na.rm = T)) %>%
                        pull(average_state)

# Data of ICE detainees on states by year
ice_year <- data %>%
            group_by(year) %>%
            select(year, total_jail_from_ice) %>%
            summarise(average_year = sum(total_jail_from_ice, na.rm = T))

# Data of ICE detainees on states from 1990 to 2000
ice_1990_to_2000 <- ice_year %>%
                    filter(year >= 1990 & year <= 2000) %>%
                    summarise(average = mean(average_year)) %>%
                    pull(average)

# Data of ICE detainees on states from 2001 to 2010
ice_2001_to_2010 <- ice_year %>%
                    filter(year >= 2001 & year <= 2010) %>%
                    summarise(average = mean(average_year)) %>%
                    pull(average)

# Data of ICE detainees on states from 2011 to 2018
ice_2011_to_2018 <- ice_year %>%
                    filter(year >= 2011 & year <= 2018) %>%
                    summarise(average = mean(average_year)) %>%
                    pull(average)

# Jail Capacity Rate
jail_cap_rate <- data %>%
                  group_by(year) %>%
                  select(year, jail_rated_capacity, total_jail_pop) %>%
                  summarise(
                    jail_rate = sum(jail_rated_capacity, na.rm = T),
                    jail_pop = sum(total_jail_pop, na.rm = T)
                  ) %>%
                  mutate(prop = (jail_pop / jail_rate)) %>%
                  round(digits = 3)

# Jail Capacity Rate for the year 1990
jail_cap_rate_1990 <- jail_cap_rate %>%
                      filter(year == 1990) %>%
                      pull(prop)

# Jail Capacity Rate for the year 2000
jail_cap_rate_2000 <- jail_cap_rate %>%
                      filter(year == 2000) %>%
                      pull(prop)

# Jail Capacity Rate for the year 2010
jail_cap_rate_2010 <- jail_cap_rate %>%
                      filter(year == 2010) %>%
                      pull(prop)

# Jail Capacity Rate for the year 2018
jail_cap_rate_2018 <- jail_cap_rate %>%
                      filter(year == 2018) %>%
                      pull(prop)

# Jail Capacity for the year 2000
jail_cap_2000 <- data %>%
                  filter(year == 2000) %>%
                  select(year, jail_rated_capacity) %>%
                  summarise(total = sum(jail_rated_capacity, na.rm = T)) %>%
                  pull(total)

# Jail Capacity for the year 2010
jail_cap_2010 <- data %>%
                  filter(year == 2010) %>%
                  select(year, jail_rated_capacity) %>%
                  summarise(total = sum(jail_rated_capacity, na.rm = T)) %>%
                  pull(total)

# Jail Capacity for the year 2018
jail_cap_2018 <- data %>%
                  filter(year == 2018) %>%
                  select(year, jail_rated_capacity) %>%
                  summarise(total = sum(jail_rated_capacity, na.rm = T)) %>%
                  pull(total)

# Jail Capacity based on States
jail_cap_state <- data %>%
                  select(year, state, jail_rated_capacity) %>%
                  group_by(state) %>%
                  summarise("Jail Capcity" = mean(jail_rated_capacity,
                                                  na.rm = T))

# Jail Race Trends Over Time Chart
pop_by_races_long <- gather(
  pop_by_races,
  key = races,
  value = no_of_people,
  c("Asian", "Black", "White", "Latinx", "Native")
)

jail_race_trends_over_time <-
  ggplot(data = pop_by_races_long) +
  geom_line(mapping = aes(
    x = year,
    y = no_of_people,
    group = races,
    color = races
  )) +
  labs(
    title = "No of people in jail based on race from 1990 to 2018",
    x = "Year",
    y = "No of people",
    color = "Races"
  )

# Variable Comparison Chart
jail_cap_black <- data %>%
mutate(location = paste(county_name, state, sep = ", ")) %>%
filter(year >= 1990) %>%
select(year, jail_rated_capacity, black_jail_pop, location, state, region) %>%
group_by(location) %>%
summarise(jail_rated_capacity = mean(jail_rated_capacity),
          black_jail_pop = mean(black_jail_pop),
          state = state, region = region) %>%
distinct(location, .keep_all = T) %>%
rename(Region = region) %>%
filter(black_jail_pop <= 4000)

jail_cap_white <- data %>%
mutate(location = paste(county_name, state, sep = ", ")) %>%
filter(year >= 1990) %>%
select(year, jail_rated_capacity, white_jail_pop, location, state, region) %>%
group_by(location) %>%
summarise(
  jail_rated_capacity = mean(jail_rated_capacity),
  white_jail_pop = mean(white_jail_pop), state = state, region = region
) %>%
distinct(location, .keep_all = T) %>%
rename(Region = region) %>%
filter(white_jail_pop <= 4000) %>%
filter(jail_rated_capacity <= 8000)

var_comp_black <- ggplot(data = jail_cap_black) +
geom_point(
  mapping = aes(x = jail_rated_capacity, y = black_jail_pop, color = Region)
) +
labs(
  title = "Jail Rated Capacity vs Black people in Jail",
  x = "Jail Rated Capacity",
  y = "Black People in Jail"
)

var_comp_white <- ggplot(data = jail_cap_white) +
geom_point(
  mapping = aes(x = jail_rated_capacity, y = white_jail_pop, color = Region)
) +
labs(
  title = "Jail Rated Capacity vs White people in Jail",
  x = "Jail Rated Capacity",
  y = "White People in Jail"
)

# Map
state_name <- cbind(state.abb, state.name) %>%
              write.csv("state_name.csv")

state_name <- read.csv("state_name.csv") %>%
              rename(state_short = state.abb, state_long = state.name) %>%
              mutate(state_long = tolower(state_long))

black_people_state <- data %>%
    filter(year == 2018) %>%
    select(year, state, black_jail_pop, black_pop_15to64) %>%
    mutate(black_rate = (black_jail_pop / black_pop_15to64)) %>%
    rename(state_short = state) %>%
    left_join(state_name, by = "state_short") %>%
    group_by(state_long) %>%
    summarise(
      black_jail_pop = sum(black_jail_pop, na.rm = T),
      black_pop_15to64 = sum(black_pop_15to64, na.rm = T)
    )

white_people_state <- data %>%
    filter(year == 2018) %>%
    select(year, state, white_jail_pop, white_pop_15to64) %>%
    mutate(black_rate = (white_jail_pop / white_pop_15to64)) %>%
    rename(state_short = state) %>%
    left_join(state_name, by = "state_short") %>%
    group_by(state_long) %>%
    summarise(
      white_jail_pop = sum(white_jail_pop, na.rm = T),
      white_pop_15to64 = sum(white_pop_15to64, na.rm = T)
    )


state_shape_black <- map_data("state") %>%
                      rename(state_long = region) %>%
                      left_join(black_people_state, by = "state_long")

state_shape_white <- map_data("state") %>%
                      rename(state_long = region) %>%
                      left_join(white_people_state, by = "state_long")

blank_theme <- theme_bw() +
                theme(
                  axis.line = element_blank(), # remove axis lines
                  axis.text = element_blank(), # remove axis labels
                  axis.ticks = element_blank(), # remove axis ticks
                  axis.title = element_blank(), # remove axis titles
                  plot.background = element_blank(), # remove gray background
                  panel.grid.major = element_blank(), # remove major grid lines
                  panel.grid.minor = element_blank(), # remove minor grid lines
                  panel.border = element_blank() # remove border around plot
                )

# Create a blank map of U.S. states
map_jail_black <- ggplot(state_shape_black) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop),
    color = "white",
    size = .1
  ) +
  labs(
    title = "No of Black people in Jail based on State"
  ) +
  coord_map() + # use a map-based coordinate system
  scale_fill_continuous(low = "#132B43", high = "Red") +
  labs(fill = "Black People in Jail") +
  blank_theme

map_jail_white <- ggplot(state_shape_white) +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = white_jail_pop),
      color = "white",
      size = .1
    ) +
    labs(
      title = "No of White people in Jail based on State"
    ) +
    coord_map() + # use a map-based coordinate system
    scale_fill_continuous(low = "#132B43", high = "Green") +
    labs(fill = "White People in Jail") +
    blank_theme

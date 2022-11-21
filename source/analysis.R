# Load libraries and data
library(tidyverse)
library(scales)
library(plotly)
library(choroplethr)
library(maps)
library(gridExtra)
prison_records <- read.csv("../data/incarceration_trends.csv")

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Data Summary
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

# Generate summary
summary <- function() {
  f_jail_mean <- round(mean(prison_records[["female_jail_pop"]], na.rm = TRUE), 2)
  m_jail_mean <- round(mean(prison_records[["male_jail_pop"]], na.rm = TRUE), 2)
  # 48465 - Val Verde, 48443 - Terrell
  val_verde_ice <- prison_records %>%
    filter(fips == 48465, year %in% (2008:2018)) %>%
    group_by(fips) %>%
    summarise(sum = sum(total_jail_from_ice, na.rm = TRUE)) %>%
    pull(sum)
  terrell_ice <- prison_records %>%
    filter(fips == 48443, year %in% (2008:2018)) %>%
    group_by(fips) %>%
    summarise(sum = sum(total_jail_from_ice, na.rm = TRUE)) %>%
    pull(sum)
  summary <- data.frame(stat = c("f_mean", "m_mean", "val_verde_ice", "terrell_ice"),
                        value = c(f_jail_mean, m_jail_mean, val_verde_ice, terrell_ice))
  return(summary)
}

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#

# Return data frame of jail population per year
get_year_jail_pop <- function() {
  year_pop <- prison_records %>%
    select(year, total_jail_pop) %>%
    group_by(year) %>%
    summarise(pop = sum(total_jail_pop, na.rm = TRUE))
  return(year_pop)   
}

# Return chart
plot_jail_pop_for_us <- function() {
  year_pop <- get_year_jail_pop()
  plot <- ggplot(year_pop, aes(x = year, y = pop)) +
    geom_bar(stat = "identity") +
    labs(title = "Increase of Jail Population in U.S. (1970-2018)",
         x = "Year",
         y = "Total Jail Population") +
    scale_y_continuous(labels = comma) +
    theme(plot.title = element_text(size = 10))
  return(plot)
}

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
#----------------------------------------------------------------------------#

# Return data frame of state populations, takes in a vector of states
get_jail_pop_by_states <- function(states) {
  states_pop <- prison_records %>%
    filter(state %in% states) %>%
    group_by(year, state) %>%
    summarise(state_total = sum(total_jail_pop, na.rm = TRUE), .groups = "keep")
  return(states_pop)
}

# Return chart, takes in a vector of states
plot_jail_pop_by_states <- function(states) {
  states_pop <- get_jail_pop_by_states(states)
  plot <- ggplot(states_pop, aes(x = year, y = state_total, group = state, color = state)) +
    geom_line(stat = "identity") +
    labs(title = "U.S. State Jail Population Trends (1970-2018)",
         x = "Year",
         y = "Jail Population",
         fill = "State"
    )
}

## Section 5  ---- 
#----------------------------------------------------------------------------#
# Disproportionate Gender Jail Populations
#----------------------------------------------------------------------------#

# Return data frame
gender_prison_jail <- function(states) {
  genders <- prison_records %>%
    filter(year == 2018) %>%
    select(state,
           county_name,
           female_jail_pop,
           female_pop_15to64,
           male_jail_pop,
           male_pop_15to64) %>%
    rename(count_f_jail = female_jail_pop,
           count_f_prison = female_pop_15to64,
           count_m_jail = male_jail_pop,
           count_m_prison = male_pop_15to64) %>%
    mutate(type_f_jail = "f_jail_pop", .before = count_f_jail) %>%
    mutate(type_f_prison = "f_prison_pop", .before = count_f_prison) %>%
    mutate(type_m_jail = "m_jail_pop", .before = count_m_jail) %>%
    mutate(type_m_prison = "m_prison_pop", .before = count_m_prison)
  f_jail <- genders %>%
    select(type_f_jail, count_f_jail) %>%
    rename(type = type_f_jail, count = count_f_jail)
  f_prison <- genders %>%
    select(type_f_prison, count_f_prison) %>%
    rename(type = type_f_prison, count = count_f_prison)
  m_jail <- genders %>%
    select(type_m_jail, count_m_jail) %>%
    rename(type = type_m_jail, count = count_m_jail)
  m_prison <- genders %>%
    select(type_m_prison, count_m_prison) %>%
    rename(type = type_m_prison, count = count_m_prison)
  total <- rbind(f_jail, f_prison, m_jail, m_prison) %>%
    filter(!is.na(count))
  return(total)
}

# Box Plots
plot_gender_prison_jail <- function() {
  genders <- gender_prison_jail()
  genders$count <- log(genders$count)
  genders <- genders %>%
    filter(is.finite(count))
  genders$type <- str_replace_all(genders$type, "f_jail_pop", "Female-Jail")
  genders$type <- str_replace_all(genders$type, "f_prison_pop", "Female-Prison")
  genders$type <- str_replace_all(genders$type, "m_jail_pop", "Male-Jail")
  genders$type <- str_replace_all(genders$type, "m_prison_pop", "Male-Prison")
  plot <- ggplot(genders, aes(x = type, y = count, fill = type)) +
    geom_boxplot(color = "#666666", outlier.size = 0.5) +
    labs(title = "U.S. Jail and Prison Populations by Gender (2018)",
         x = "Type",
         y = "Log Transformed Population",
         fill = "Type") +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5),
          plot.title = element_text(size = 12),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white")) +
    scale_fill_manual(values = c("#A0E7E5", "#FFAEBC", "#B4F8C8", "#FFCF9C"))
}

# Stat tests of f-m jail population, returns t test results hidden
perform_prison_jail_test <- function() {
  test <- gender_prison_jail()
  f_jail <- test %>%
    filter(type == "f_jail_pop")
  m_jail <- test %>%
    filter(type == "m_jail_pop")
  results <- t.test(f_jail$count, m_jail$count, paired = TRUE)
  return(invisible(results))
}

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

# Return data frame
ice_jail_county <- function() {
  df_ice <- prison_records %>%
    filter(year %in% (2008:2018)) %>%
    select(fips, total_jail_from_ice) %>%
    group_by(fips) %>%
    summarise(value = sum(total_jail_from_ice, na.rm = TRUE)) %>%
    filter(fips != 2158, fips != 46102) %>%
    rename(region = fips)
  return(df_ice)
}

# Returns map plot
plot_ice_jail_county <- function() {
  df_ice <- ice_jail_county()
  county_ice <- county_choropleth(df_ice,
                                  title = "Population Jailed due to ICE by County (1970~2018)",
                                  legend = "Number of People",
                                  num_colors = 7,
                                  state_zoom = c("new mexico", "arizona", "texas")) +
    theme(plot.title = element_text(size = 10, hjust = 1),
          legend.title = element_text(size = 8, hjust = 0.5))
  return(county_ice)
}

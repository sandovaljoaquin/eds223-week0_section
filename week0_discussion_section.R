# Install necessary packages

packages <- c("here", "janitor", "tidyverse", "sf", "terra", "tmap", "spData", "spDataLarge", "geodata", "kableExtra", "viridisLite")
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

library(here)
library(janitor)
library(tidyverse)
library(sf)
library(kableExtra)

# Load data 

gdw_df <- read_csv(here("data", "gdw.csv")) |> 
  janitor::clean_names()

# Data Exploration 
# Show the first and last 10 rows of gdw_df and use kable() to make html table 

head(gdw_df, n = 10) |> 
  kable()

tail(gdw_df, n = 10) |> 
  kable()

# Number of rows 
nrow(gdw_df)

# Number of cols 
ncol(gdw_df)

#Dimensions 
dim(gdw_df)

# Names of column names 

names(gdw_df)

# 4. Index, Summarize, Subset Data

# Use indexing brackets to extract column containing country name as vector and dataframe  
country_df <- gdw_df[, "country"]
country_vec <- gdw_df[["country"]]

# Use group_by() and summarise() to find the number of dams by dam type 

gdw_df |> 
  group_by(dam_type) |> 
  summarise(count = n()) |> 
  ungroup()
  
# Make a subset called sub_dam that only contains entries for dam_tye == "Dam"

dam_sub <- gdw_df |> 
  filter(dam_type == "Dam")


# Re-order gdw_df by ascending order of installation year arrange function 

gdw_df <- gdw_df |> 
  arrange(year_dam)

# Data Visualization 

# Make bar graph of avg height of dam/barrier in meters by country 

gdw_df |>
  group_by(country) |>
  summarize(mean_dam_hgt_m = mean(dam_hgt_m, na.rm = TRUE)) |>
  ungroup() |> 
  ggplot(aes(x = country, y = mean_dam_hgt_m)) +
  geom_bar(stat = "identity") +
  labs(x = "Country",
       y = "Average height of dam/barrier in meters") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


ggplot(gdw_df, 
       aes(x = cap_mcm, 
           y = dam_hgt_m)) + 
  geom_point() 

# gdw_df also has a shape column which contains point coordinates 

# Print first 3 rows of this column and look at class 

head(gdw_df$shape, n = 3)
class(gdw_df$shape)

# dataframe needs to be spatially enabled to be a spatial object. Use sf package to do this 

gdw_st <- st_read(here("data", "gdw.gdb")) |> 
  clean_names()



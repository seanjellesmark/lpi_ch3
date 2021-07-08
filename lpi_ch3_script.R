### Exploratory analysis of the LPI data for the 3rd chapter.
library(purrr)
library(readxl)
library(dplyr)
library(broom)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(tibble)
library(openxlsx)
library(rtrim)
library(tidyverse)
library(patchwork)
library(MatchIt)
library(rlpi)

# set wd for infile creation

setwd("C:/Users/seanj/OneDrive - University College London/Articles from Thesis/3. Assessing the effect of global conservation/LPI_files")

# Load LPI data

lpi <- read_excel("C:/Users/seanj/OneDrive - University College London/Articles from Thesis/3. Assessing the effect of global conservation/Data/LPD_output_20201116.xlsx")

# Conservation sheet

cons<- read_excel("C:/Users/seanj/OneDrive - University College London/Articles from Thesis/3. Assessing the effect of global conservation/Data/Conservation_LPD_worksheet05072021.xlsx") %>% 
  select(everything(), -(13:15)) %>% 
  filter(cons_action_1 != "?")

  # Problem with cons variables being rounded down to 2.29999 (albeint being characters) and decimals are off, probably some Excel witchcraft.
  # Fix by converting to numeric, rounding and then convert to factor

cons <- cons %>% 
  mutate(across(cons_action_1:Alternative_cons,
                ~ case_when(.x == "1.1000000000000001" ~ "1.1",
                            .x == "2.2000000000000002" ~ "2.2",
                            .x == "2.2999999999999998" ~ "2.3",
                            TRUE ~ (.x))))

# Check the different management types and validate whether these can be recategorised - all of this is out dated and have been done

lpi$Management_type <- factor(lpi$Management_type)

list<-levels(lpi$Management_type)

list_man <- lpi %>% 
  group_by(Management_type) %>% 
  summarise(number_of_pops = n()) # Looks surprisingly manageable - 1025 unique management comments
# write.csv(list_man, "C:/Users/seanj/OneDrive - University College London/Articles from Thesis/3. Assessing the effect of global conservation/Data/man_com.csv")


# Try the same but include species and location
list_man_ext <- lpi %>% 
  filter(Managed ==1) %>% 
  group_by(Management_type, Common_name, Location) %>% 
  summarise(number_of_pops = n()) 


## To ease categorisation, we add common name and location to each unique comment
 # First, we need to shorten the original LPI data down. No need to add all the data

name_location <- lpi %>% 
  select(Common_name, Location, Management_type)
 
 # Exclude the comments with more than one pop as name and location doesn't apply here
 cons_exp <- cons %>% 
   filter(number_of_pops == 1)

name_location_exp <- left_join(cons_exp, name_location, by = "Management_type")
 # Add names and locations to cons sheet and export

# Export

# write.csv(name_location_exp, "C:/Users/seanj/OneDrive - University College London/Articles from Thesis/3. Assessing the effect of global conservation/Data/man_com_name_loc.csv")

# Check if any pops have management comments but managed variable as 2 or 0
 # 81 pops in total, most management comments are cryptic so easiest to just remove

lpi_error <- lpi %>% 
  filter(Managed == 0 & Management_type != "NULL" | Managed == 2 & Management_type != "NULL")
  
# Remove populations without management info

lpi_work <- lpi %>% 
  filter(Managed != 2)

# Remember to filter for other conditions so aggregated national trends are removed. Have to figure out the best way to do so - Update
#  Easiest done using the Scale Field in the LPD.

lpi_work %>%
  group_by(Scale) %>% 
  summarise(n_obs = n()) 
 # Global            99
 # National/Ocean  1227
 # Regional        4867
 # Smaller unit    8454
 # Unknown           11

# Number of managed and not not managed populations

lpi_work %>% 
  count(Managed) # 9296 unmanaged pops and 5362 managed

# Join the conservation sheet to the lpi work sheet

lpi_work1 <- left_join(lpi_work, cons, by = "Management_type") 

# Create variables for starting year, number of ops and duration of time series
 # First thing to do is to transform the df into a long format

lpi_work2 <- lpi_work1 %>% 
  pivot_longer('1950':'2019',
               names_to = "year",
               values_to = "value") # Goes from 14658 rows to 1026060 rows
 
lpi_work2$year <- as.integer(lpi_work2$year)
lpi_work2$value <- as.numeric(lpi_work2$value)

  
 # Group and filter away NA values to create the variables and select the variables we need 

lpi_work3 <- lpi_work2 %>% 
  group_by(ID) %>%
  filter(!is.na(value)) %>% 
  mutate(start_year = min(year),
         last_year = max(year),
         ts_length = last_year - start_year) %>% 
  select(ID, start_year, last_year, ts_length) %>% 
  distinct() %>% 
  ungroup()

 # Join the three new variables to the lpi data

lpi_work4 <- left_join(lpi_work1, lpi_work3, by = "ID") 

# Remove all duplicate columns (from not specifying all similar joining variables in the joins) ending with x

lpi_work5 <- lpi_work4 %>% 
  select(everything(), -ends_with(".x")) # Removes duplicate name and location variables

# Remove variables where conservation management is unknown or cannot be categorized

lpi_work6 <- lpi_work5 %>% 
  filter(Management_type != "NA" | Management_type != "Unknown") %>% 
  mutate(treatment = if_else(Managed == 1, 1, 0))# Drops 210 pops

# Contruct pre-match object to check initial balance. 
# Most variables are categorical and will be exact but potentially a few continuous (that we just created) - Update: This takes forever, even 
# when specified super simple

 # Make sure all variables are in right format

lpi_work6$Binomial <- factor(lpi_work6$Binomial)

lpi_work6$Country <- factor(lpi_work6$Country)

# Add X to the year columns as the LPImain functions doesn't work otherwise

lpi_work6 <- lpi_work6 %>% 
  rename_with(.cols = '1970':'2018',function(x){paste0("X", x)})


 # Reduce the number of rows in order to check if whether the matching method works properly without having to run forever

lpi_sample <- sample_n(lpi_work6, 100)

 # Reduce number of variables - should reduce time running. Updated and using full sample as the problem was with specifying multiple methods

lpi_sample <- lpi_sample %>% 
  select(treatment, Binomial, Country, ID)

pre_match <- matchit(treatment ~ Species + Country, data = lpi_work6,
                  method = "exact", distance = "glm")

 # Extract matched data

lpi_matched <- match.data(pre_match)

# Create infile and LPImain

# creatre infile and LPI main

create_infile(lpi_matched, name = "lpi_matched",  start_col_name = 'X1970', end_col_name = 'X2018')

lpi_matched<-LPIMain(infile = "lpi_matched_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

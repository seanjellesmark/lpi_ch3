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
library(ggmap)
library(maptools)
library(maps)

# set wd for infile creation

setwd("C:/Users/seanj/OneDrive - University College London/Articles from Thesis/3. Assessing the effect of global conservation/LPI_files")

# Load LPI data

lpi <- read_excel("C:/Users/seanj/OneDrive - University College London/Articles from Thesis/3. Assessing the effect of global conservation/Data/LPD_output_20201116.xlsx")

# Conservation sheet

cons<- read_excel("C:/Users/seanj/OneDrive - University College London/Articles from Thesis/3. Assessing the effect of global conservation/Data/Conservation_LPD_worksheet05072021.xlsx") %>% 
  select(everything(), -(13:15)) %>% 
  filter(cons_action_1 != "?")

  # Problem with cons variables being rounded down to 2.29999 (albeit being characters) and decimals are off, probably some Excel witchcraft.
  # Fix by correcting wrong decimals into right category

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
         ts_length = last_year - start_year,
         n_obs = sum(year > 0)) %>% 
  select(ID, start_year, last_year, ts_length, n_obs) %>% 
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
# when specified super simple. Something is wrong

 # Make sure all variables are in right format

lpi_work6$Binomial <- factor(lpi_work6$Binomial)

lpi_work6$Country <- factor(lpi_work6$Country)

## Check distribution of conservations in the LPI

# Add X to the year columns as the LPImain functions doesn't work otherwise

lpi_work6 <- lpi_work6 %>% 
  rename_with(.cols = '1970':'2018',function(x){paste0("X", x)})

# Create conservation variables - not a very elegant solution...

lpi_work7 <- lpi_work6 %>% 
  mutate(site_area_protection_1.1 = case_when(cons_action_1 == "1.1" ~ 1, 
                                              cons_action_2 == "1.1" ~ 1,
                                              cons_action_3 == "1.1" ~ 1,
                                              cons_action_4 == "1.1" ~ 1, 
                                              TRUE ~ 0),
         site_area_management_2.1 = case_when(cons_action_1 == "2.1" ~ 1, 
                                              cons_action_2 == "2.1" ~ 1,
                                              cons_action_3 == "2.1" ~ 1,
                                              cons_action_4 == "2.1" ~ 1, 
                                              TRUE ~ 0),
         invasive_problem_species_2.2 = case_when(cons_action_1 == "2.2" ~ 1, 
                                              cons_action_2 == "2.2" ~ 1,
                                              cons_action_3 == "2.2" ~ 1,
                                              cons_action_4 == "2.2" ~ 1, 
                                              TRUE ~ 0),
         habitat_natural_process_restoration_2.3 = case_when(cons_action_1 == "2.3" ~ 1, 
                                              cons_action_2 == "2.3" ~ 1,
                                              cons_action_3 == "2.3" ~ 1,
                                              cons_action_4 == "2.3" ~ 1, 
                                              TRUE ~ 0),
         harvest_management_3.1.1 = case_when(cons_action_1 == "3.1.1" ~ 1, 
                                              cons_action_2 == "3.1.1" ~ 1,
                                              cons_action_3 == "3.1.1" ~ 1,
                                              cons_action_4 == "3.1.1" ~ 1, 
                                              TRUE ~ 0),
         trade_management_3.1.2 = case_when(cons_action_1 == "3.1.2" ~ 1, 
                                              cons_action_2 == "3.1.2" ~ 1,
                                              cons_action_3 == "3.1.2" ~ 1,
                                              cons_action_4 == "3.1.2" ~ 1, 
                                              TRUE ~ 0),
         limit_pop_growth_3.1.3 = case_when(cons_action_1 == "3.1.3" ~ 1, 
                                              cons_action_2 == "3.1.3" ~ 1,
                                              cons_action_3 == "3.1.3" ~ 1,
                                              cons_action_4 == "3.1.3" ~ 1, 
                                              TRUE ~ 0),
         species_recovery_3.2 = case_when(cons_action_1 == "3.2" ~ 1, 
                                              cons_action_2 == "3.2" ~ 1,
                                              cons_action_3 == "3.2" ~ 1,
                                              cons_action_4 == "3.2" ~ 1, 
                                              TRUE ~ 0),
         reintroduction_3.3.1 = case_when(cons_action_1 == "3.3.1" ~ 1, 
                                              cons_action_2 == "3.3.1" ~ 1,
                                              cons_action_3 == "3.3.1" ~ 1,
                                              cons_action_4 == "3.1.1" ~ 1, 
                                              TRUE ~ 0),
         benign_reintroduction_3.3.2 = case_when(cons_action_1 == "3.3.2" ~ 1, 
                                              cons_action_2 == "3.3.2" ~ 1,
                                              cons_action_3 == "3.3.2" ~ 1,
                                              cons_action_4 == "3.3.2" ~ 1, 
                                              TRUE ~ 0),
         training_4.2 = case_when(cons_action_1 == "4.2" ~ 1, 
                                              cons_action_2 == "4.2" ~ 1,
                                              cons_action_3 == "4.2" ~ 1,
                                              cons_action_4 == "4.2" ~ 1, 
                                              TRUE ~ 0),
         awareness_communication_4.3 = case_when(cons_action_1 == "4.3" ~ 1, 
                                                                 cons_action_2 == "4.3" ~ 1,
                                                                 cons_action_3 == "4.3" ~ 1,
                                                                 cons_action_4 == "4.3" ~ 1, 
                                                                 TRUE ~ 0),
         international_legislation_5.1.1 = case_when(cons_action_1 == "5.1.1" ~ 1, 
                                                                 cons_action_2 == "5.1.1" ~ 1,
                                                                 cons_action_3 == "5.1.1" ~ 1,
                                                                 cons_action_4 == "5.1.1" ~ 1, 
                                                                 TRUE ~ 0),
         national_legislation_5.1.2 = case_when(cons_action_1 == "5.1.2" ~ 1, 
                                               cons_action_2 == "5.1.2" ~ 1,
                                               cons_action_3 == "5.1.2" ~ 1,
                                               cons_action_4 == "5.1.2" ~ 1, 
                                               TRUE ~ 0),
         regional_legislation_5.1.3 = case_when(cons_action_1 == "5.1.3" ~ 1, 
                                               cons_action_2 == "5.1.3" ~ 1,
                                               cons_action_3 == "5.1.3" ~ 1,
                                               cons_action_4 == "5.1.3" ~ 1, 
                                               TRUE ~ 0),
         unspecified_legislation_5.1.4 = case_when(cons_action_1 == "5.1.4" ~ 1, 
                                            cons_action_2 == "5.1.4" ~ 1,
                                            cons_action_3 == "5.1.4" ~ 1,
                                            cons_action_4 == "5.1.4" ~ 1, 
                                            TRUE ~ 0),
         policies_regulation_5.2 = case_when(cons_action_1 == "5.2" ~ 1, 
                                                   cons_action_2 == "5.2" ~ 1,
                                                   cons_action_3 == "5.2" ~ 1,
                                                   cons_action_4 == "5.2" ~ 1, 
                                                   TRUE ~ 0),
         complience_enforcement_5.4 = case_when(cons_action_1 == "5.4" ~ 1, 
                                             cons_action_2 == "5.4" ~ 1,
                                             cons_action_3 == "5.4" ~ 1,
                                             cons_action_4 == "5.4" ~ 1, 
                                             TRUE ~ 0),
         linked_enterprises_livelihood_alternatives_6.1 = case_when(cons_action_1 == "6.1" ~ 1, 
                                             cons_action_2 == "6.1" ~ 1,
                                             cons_action_3 == "6.1" ~ 1,
                                             cons_action_4 == "6.1" ~ 1, 
                                             TRUE ~ 0),
         conservation_payments_6.4 = case_when(cons_action_1 == "6.4" ~ 1, 
                                                                    cons_action_2 == "6.4" ~ 1,
                                                                    cons_action_3 == "6.4" ~ 1,
                                                                    cons_action_4 == "6.4" ~ 1, 
                                                                    TRUE ~ 0),
         institutional_civil_society_development_7.1 = case_when(cons_action_1 == "7.1" ~ 1, 
                                                                    cons_action_2 == "7.1" ~ 1,
                                                                    cons_action_3 == "7.1" ~ 1,
                                                                    cons_action_4 == "7.1" ~ 1, 
                                                                    TRUE ~ 0),
         alliance_partnership_development_7.2 = case_when(cons_action_1 == "7.2" ~ 1, 
                                                                 cons_action_2 == "7.2" ~ 1,
                                                                 cons_action_3 == "7.2" ~ 1,
                                                                 cons_action_4 == "7.2" ~ 1, 
                                                                 TRUE ~ 0),
         pop_size_dist_past_trends_r1.2 = case_when(cons_action_1 == "r1.2" ~ 1, 
                                                                 cons_action_2 == "r1.2" ~ 1,
                                                                 cons_action_3 == "r1.2" ~ 1,
                                                                 cons_action_4 == "r1.2" ~ 1, 
                                                                 TRUE ~ 0),
         life_hist_ecology_r1.3 = case_when(cons_action_1 == "r1.3" ~ 1, 
                                                    cons_action_2 == "r1.3" ~ 1,
                                                    cons_action_3 == "r1.3" ~ 1,
                                                    cons_action_4 == "r1.3" ~ 1, 
                                                    TRUE ~ 0),
         threats_r1.5 = case_when(cons_action_1 == "r1.5" ~ 1, 
                                                    cons_action_2 == "r1.5" ~ 1,
                                                    cons_action_3 == "r1.5" ~ 1,
                                                    cons_action_4 == "r1.5" ~ 1, 
                                                    TRUE ~ 0),
         species_action_recovery_plan_r2.1 = case_when(cons_action_1 == "r2.1" ~ 1, 
                                                    cons_action_2 == "r2.1" ~ 1,
                                                    cons_action_3 == "r2.1" ~ 1,
                                                    cons_action_4 == "r2.1" ~ 1, 
                                                    TRUE ~ 0),
         area_based_management_plan_r2.2 = case_when(cons_action_1 == "r2.2" ~ 1, 
                                                       cons_action_2 == "r2.2" ~ 1,
                                                       cons_action_3 == "r2.2" ~ 1,
                                                       cons_action_4 == "r2.2" ~ 1, 
                                                       TRUE ~ 0),
         harvest_trade_management_plan_r2.3 = case_when(cons_action_1 == "r2.3" ~ 1, 
                                                     cons_action_2 == "r2.3" ~ 1,
                                                     cons_action_3 == "r2.3" ~ 1,
                                                     cons_action_4 == "r2.3" ~ 1, 
                                                     TRUE ~ 0),
         population_trends_r3.1 = case_when(cons_action_1 == "r3.1" ~ 1, 
                                                     cons_action_2 == "r3.1" ~ 1,
                                                     cons_action_3 == "r3.1" ~ 1,
                                                     cons_action_4 == "r3.1" ~ 1, 
                                                     TRUE ~ 0),
         harvest_trends_r3.2 = case_when(cons_action_1 == "r3.2" ~ 1, 
                                            cons_action_2 == "r3.2" ~ 1,
                                            cons_action_3 == "r3.2" ~ 1,
                                            cons_action_4 == "r3.2" ~ 1, 
                                            TRUE ~ 0))

# Create general categories that describe the conservation category - sorry for the wide code

lpi_work8 <- lpi_work7 %>% 
  mutate(land_water_protection = if_else(site_area_protection_1.1 == 1, 1, 0),
         land_water_management = if_else((site_area_management_2.1 == 1 | invasive_problem_species_2.2 == 1 | habitat_natural_process_restoration_2.3 == 1), 1, 0),
         species_management = if_else((harvest_management_3.1.1 == 1 | trade_management_3.1.2 == 1 | limit_pop_growth_3.1.3 == 1 | species_recovery_3.2 == 1 | reintroduction_3.3.1 == 1 | benign_reintroduction_3.3.2 ==1), 1, 0),
         education_awareness = if_else((training_4.2 == 1 | awareness_communication_4.3 ==1), 1, 0),
         law_policy = if_else((international_legislation_5.1.1 == 1 | national_legislation_5.1.2 == 1 | regional_legislation_5.1.3 == 1 | unspecified_legislation_5.1.4 == 1 | policies_regulation_5.2 == 1| complience_enforcement_5.4 == 1), 1, 0),
         incentives = if_else((linked_enterprises_livelihood_alternatives_6.1 == 1 | conservation_payments_6.4 == 1), 1, 0),
         external_capacity = if_else((institutional_civil_society_development_7.1 == 1 | alliance_partnership_development_7.2 == 1), 1, 0),
         research = if_else((str_starts(cons_action_1, "r") | str_starts(cons_action_2, "r") | str_starts(cons_action_3, "r") | str_starts(cons_action_4, "r")),1 , 0),
)

 # Correct NAs in the research field to zeros 
lpi_work8 <- lpi_work8 %>% 
  mutate(research = case_when(research == 1 ~ 1,
                              research == 0 ~ 0,
                              TRUE ~ 0))



           
# Add Cites data from the lpi - UPDATE: Doesn't make sense. equal number of pops in both groups (approx 2500 in total) are listed.

lpi_work9 <- lpi_work8 %>% 
  mutate(international_legislation_5.1.1 = if_else(CITES != "Not listed", 1, international_legislation_5.1.1))
           
 # Reduce the number of rows in order to check if whether the matching method works properly without having to run forever
 # Update - Not necessary. Problem is adding multiple conditions to the matching, for some reason forces matching procedure into an endless loop

 lpi_sample <- sample_n(lpi_work8, 1000)

 # Reduce number of variables - should reduce time running. Updated and using full sample as the problem was with specifying multiple methods

 lpi_sample <- lpi_sample %>% 
  select(treatment, Class, Binomial, Country, Region, ID, ts_length, start_year)

# Create matched sample
  
pre_match <- matchit(treatment ~ Binomial + Country, data = lpi_work8,
                     method = "exact")

pre_match_liberal <- matchit(treatment ~ Genus + Region, data = lpi_work8,
                             method = "exact")


pre_match_stringent <- matchit(treatment ~ Region + Class + start_year + last_year + n_obs, method = "nearest",
                                exact = c("Region","Class"), data = lpi_work8)

# pre_match_strict <- matchit(treatment ~ Species + Country + start_year, data = lpi_sample,
#                            distance = "mahalanobis", replace = FALSE, exact = ~ Species + Country)

 # Extract matched data

# Liberal

lpi_match_lib <- match.data(pre_match_liberal)

lpi_cons_lib <- lpi_match_lib %>% 
  filter(treatment == 1)

lpi_control_lib <- lpi_match_lib %>% 
  filter(treatment == 0)

# Benchmark

lpi_match <- match.data(pre_match)

lpi_cons <- lpi_match %>% 
  filter(treatment == 1) #%>% 
  #filter(Binomial != "Castor_fiber") # Excluding Eurasian Beaver

lpi_control <- lpi_match %>% 
  filter(treatment == 0) #%>% 
  #filter(Binomial != "Castor_fiber")

# Continous strict counterfactual


lpi_match_stringent <- match.data(pre_match_stringent)

lpi_cons_stringent <- lpi_match_stringent %>% 
  filter(treatment == 1) #%>% 
#filter(Binomial != "Castor_fiber") # Excluding Eurasian Beaver

lpi_control_stringent <- lpi_match_stringent %>% 
  filter(treatment == 0) #%>% 
#filter(Binomial != "Castor_fiber")

# Full sample after excluding uncertain management

lpi_full_cons <- lpi_work8 %>% 
  filter(treatment == 1)

lpi_full_cont <- lpi_work8 %>% 
  filter(treatment == 0)

# Descriptive stuff for start of result section

 # Countries
lpi_full_cont %>% 
  summarise(country_n = n_distinct(Country))

 # Conservation group
lpi_full_cons %>% 
  group_by(Class) %>% 
  count()

 # Control group
lpi_full_cont %>% 
  group_by(Class) %>% 
  count()

 # Plot points
 

mapWorld <- borders("world", colour="gray50", fill="white")
mp <- ggplot() + mapWorld

lpi_loc <- lpi_work8 %>% 
  mutate(Conservation = if_else(treatment == 1, "Conservation", "Control"))

(cons_locations <- mp + geom_point(data = lpi_loc, aes(x = Longitude, y = Latitude, color = Conservation, shape = Conservation), size = 1.5) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size=30),
        text=element_text(size=30),
        legend.position = "top") + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  scale_color_viridis_d(end = 0.7))
 
#ggsave(filename = "C:/Users/seanj/OneDrive - University College London/Articles from Thesis/3. Assessing the effect of global conservation/Plots and tables/loc_cons.tiff",
#       plot = cons_locations, compression = "lzw", width = 60, height = 40, dpi = 400, units = "cm")

 # How many are utilized

lpi_full_cons %>% 
  summarise(n_util = sum(Utilised == 1))

# Create infile and LPImain

# creatre infile and LPI main

# Liberal

create_infile(lpi_cons_lib, name = "lpi_cons_lib",  start_col_name = 'X1970', end_col_name = 'X2015')

lpi_matched_cons_lib<-LPIMain(infile = "lpi_cons_lib_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

create_infile(lpi_control_lib, name = "lpi_control_lib",  start_col_name = 'X1970', end_col_name = 'X2015')

lpi_matched_control_lib<-LPIMain(infile = "lpi_control_lib_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

ggplot_lpi(lpi_matched_cons_lib, title = "Conservation", ylim=c(0.9, 3))+ggplot_lpi(lpi_matched_control_lib, title ="Without conservation")


# Benchmark

create_infile(lpi_cons, name = "lpi_cons",  start_col_name = 'X1970', end_col_name = 'X2018')

lpi_matched_cons<-LPIMain(infile = "lpi_cons_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

create_infile(lpi_control, name = "lpi_control",  start_col_name = 'X1970', end_col_name = 'X2018')

lpi_matched_control<-LPIMain(infile = "lpi_control_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

ggplot_lpi(lpi_matched_cons, title = "Conservation", ylim=c(0.9, 4.7))+ggplot_lpi(lpi_matched_control, title ="Without conservation")

# Run lpi trend creation on all species targeted by conservation and all not targeted individually
 
 # Not targeted by conservation

create_infile(lpi_full_cont, name = "lpi_full_cont",  start_col_name = 'X1970', end_col_name = 'X2015')

lpi_full_cont_trend<-LPIMain(infile = "lpi_full_cont_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

no_conservation_full <- ggplot_lpi(lpi_full_cont_trend, title ="Without conservation - full sample")

 # Targeted by conservation

create_infile(lpi_full_cons, name = "lpi_full_cons",  start_col_name = 'X1970', end_col_name = 'X2015')

lpi_full_cons_trend<-LPIMain(infile = "lpi_full_cons_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

conservation_full <- ggplot_lpi(lpi_full_cons_trend, title ="Conservation - full sample", ylim=c(0.8, 2.5))


conservation_full + no_conservation_full

# Assume that all conservation targeted species had remained stable in the absence of conservation

 # First we check how th lpi looks for the full sample after preparation

lpi_all <- lpi%>% 
  rename_with(.cols = '1970':'2018',function(x){paste0("X", x)})


create_infile(lpi_all, name = "lpi_all",  start_col_name = 'X1970', end_col_name = 'X2015')

lpi_all_trend<-LPIMain(infile = "lpi_all_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

lpi_all_trend <- lpi_all_trend %>% 
  mutate(trend = "normal", 
         year = 1970:2018) %>% 
  filter(year < 2016)

ggplot_lpi(lpi_all_trend)

# Assign lpi data and start correcting it

lpi_all_corrected <- lpi_all

# Create a variable that indicates if population increase were caused by conservation 
# REMEMBER TO DISCUSS WHICH VARIABLES TO INCLUDE WITH lOUISE AND MIKE

lpi_all_corrected <- lpi_all_corrected %>% 
  mutate(conservation_increase = if_else(Introduction == 1 | Recolonisation == 1 | Recruitment == 1 | Removal_of_threat == 1 | 
                                           `Rural_to_urban migration` == 1 | Reintroduction == 1 | Range_shift == 1 | 
                                           Legal_protection == 1 | Management == 1 | Unknown...163 == 1 | Other...164 == 1, 1, 0))

# Alternative

lpi_all_corrected <- lpi_all_corrected %>% 
  mutate(conservation_increase = if_else(Introduction == 1 | Recolonisation == 1 | Recruitment == 1 | Removal_of_threat == 1 | 
                                           Reintroduction == 1 | Range_shift == 1 | 
                                           Legal_protection == 1 | Management == 1, 1, 0))


 # Replace all observed population counts for the managed group with a constant number and re-create the trend

lpi_all_corrected <- lpi_all_corrected %>% 
 mutate(across(X1970:X2018,
                ~ if_else( Managed == 1 & conservation_increase == 1 & .x != "NULL", '5', .x)))


create_infile(lpi_all_corrected, name = "lpi_all_corrected",  start_col_name = 'X1970', end_col_name = 'X2015')

lpi_all_trend_corrected<-LPIMain(infile = "lpi_all_corrected_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

lpi_all_trend_corrected <- lpi_all_trend_corrected %>% 
  mutate(trend = "stable conservation pops",
         year = 1970:2018) %>% 
  filter(year < 2016)

 # Bind the two together

lpi_merged_trend <- bind_rows(lpi_all_trend, lpi_all_trend_corrected)

(global_cons_impact <- lpi_merged_trend %>% 
  ggplot(., aes(x = year, y = LPI_final, color = trend, fill = trend)) + 
  geom_line(size = 2) +
  geom_ribbon(aes(ymin=CI_low, ymax=CI_high), linetype=3, alpha=0.1) +
  theme_bw() +
  theme(legend.title = element_blank(),
        text=element_text(size=30)))
  
#ggsave(filename = "C:/Users/seanj/OneDrive - University College London/Articles from Thesis/3. Assessing the effect of global conservation/Plots and tables/global_cons_trend.tiff",
#       plot = global_cons_impact, compression = "lzw", width = 50, height = 40, dpi = 400, units = "cm")

# Plot conservation - Using same basic representation as in Bolam et al 2020 

 # First, transform lpi_work8 into long format, stretching primary conservation and sub categories

lpi_plot <- lpi_work8 %>% 
  pivot_longer(land_water_protection:research,
               names_to = "primary_action",
               values_to = "action_type") 

lpi_plot_filtered <- lpi_plot1 %>% 
  pivot_longer(land_water_protection:research,
               names_to = "primary_action",
               values_to = "action_type") %>% 
  filter(action_type == 1)

lpi_plot1 <- lpi_work8 %>% 
  pivot_longer(site_area_protection_1.1:harvest_trends_r3.2 ,
               names_to = "cons_action",
               values_to = "active") %>%
  filter(active == 1)
  

df <- lpi_plot1 %>%
  group_by(cons_action) %>%
  summarise(counts = n(),
            percentage = (n()/nrow(lpi_plot1))*100)
df

lpi_plot_filtered %>% 
  group_by(primary_action, Region) %>% 
  summarise(counts = n()) %>% 
  ggplot(., aes(x = counts, y = primary_action)) +
  geom_bar(position = "stack", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean() +
  facet_wrap(~Region)

  
# Create the bar plot. Use theme_pubclean() [in ggpubr]
ggplot(lpi_plot_filtered, aes(x = counts, y = cons_type)) +
  geom_bar(position = "stack", fill = cons_type, stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean() +
  facet_wrap(~Region)

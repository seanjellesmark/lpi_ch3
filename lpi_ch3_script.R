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
library(stringr)
library(INLA)

# Testing if updated token works - it does

# set wd for infile creation

setwd("C:/Users/seanj/OneDrive - University College London/Articles from Thesis/3. Assessing the effect of global conservation/LPI_files")

# Load LPI data

lpi <- read_excel("C:/Users/seanj/OneDrive - University College London/Articles from Thesis/3. Assessing the effect of global conservation/Data/LPD_output_20201116.xlsx")

# Add common class names

lpi <- lpi %>% 
  mutate(common_class = case_when(Class %in% c("Actinopteri", "Coelacanthi", "Dipneusti", "Elasmobranchii", "Holocephali", "Myxini", "Petromyzonti") ~ "Fishes",
                                  Class == "Amphibia" ~ "Amphibians",
                                  Class == "Aves" ~ "Birds",
                                  Class == "Mammalia" ~ "Mammals",
                                  Class == "Reptilia" ~ "Reptiles"))


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
#  Easiest done using the Scale Field in the LPD. Update - Not necessary at national aggregated trends won't have conservation actions recorded

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

# Create variables for starting year, number of obs and duration of time series
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

# Remove pops where conservation management is unknown or cannot be categorized

lpi_work6 <- lpi_work5 %>% 
  filter(Management_type != "NA") %>% 
  filter(Management_type != "Unknown") %>% 
  mutate(treatment = if_else(Managed == 1, 1, 0))# Drops 210 pops - It doesn't filter unknown management properly when doing it in one 
# line for whatever reason

# Contruct pre-match object to check initial balance. 
# Most variables are categorical and will be exact but potentially a few continuous (that we just created) - Update: This takes forever, even 
# when specified super simple. Something is wrong

 # Make sure all variables are in right format

lpi_work6$Binomial <- factor(lpi_work6$Binomial)

lpi_work6$Country <- factor(lpi_work6$Country)

## Check distribution of conservation in the LPI

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

#lpi_work9 <- lpi_work8 %>% 
#  mutate(international_legislation_5.1.1 = if_else(CITES != "Not listed", 1, international_legislation_5.1.1))
           
 # Reduce the number of rows in order to check if whether the matching method works properly without having to run forever
 # Update - Not necessary. Problem is adding multiple conditions to the matching, for some reason forces matching procedure into an endless loop

 # pi_sample <- sample_n(lpi_work8, 1000)

 # Reduce number of variables - should reduce time running. Updated and using full sample as the problem was with specifying multiple methods

 # lpi_sample <- lpi_sample %>% 
 # select(treatment, Class, Binomial, Country, Region, ID, ts_length, start_year)

# Create matched sample
  
pre_match_bench <- matchit(treatment ~ Binomial + Country, data = lpi_work8,
                     method = "exact")

pre_match_liberal <- matchit(treatment ~ Genus + Region, data = lpi_work8,
                             method = "exact")


pre_match_stringent <- matchit(treatment ~ Region + Class + ts_length + start_year, method = "nearest",
                                exact = c("Region","Class"), data = lpi_work8)

# pre_match_strict <- matchit(treatment ~ Species + Country + start_year, data = lpi_sample,
#                            distance = "mahalanobis", replace = FALSE, exact = ~ Species + Country)

 # Extract matched data

# Liberal

lpi_match_lib <- match.data(pre_match_liberal)

lpi_lib_cons <- lpi_match_lib %>% 
  filter(treatment == 1)

lpi_lib_cont <- lpi_match_lib %>% 
  filter(treatment == 0)

# Benchmark

lpi_match_bench <- match.data(pre_match_bench)

lpi_bench_cons <- lpi_match_bench %>% 
  filter(treatment == 1) #%>% 
  #filter(Binomial != "Castor_fiber") # Excluding Eurasian Beaver

lpi_bench_cont <- lpi_match_bench %>% 
  filter(treatment == 0) #%>% 
  #filter(Binomial != "Castor_fiber")

# "Stringent" counterfactual


lpi_match_stringent <- match.data(pre_match_stringent)

lpi_string_cons <- lpi_match_stringent %>% 
  filter(treatment == 1) #%>% 
#filter(Binomial != "Castor_fiber") # Excluding Eurasian Beaver

lpi_string_cont <- lpi_match_stringent %>% 
  filter(treatment == 0) #%>% 
#filter(Binomial != "Castor_fiber")

# Full sample after excluding uncertain management

lpi_full_cons <- lpi_work8 %>% 
  filter(treatment == 1)

lpi_full_cont <- lpi_work8 %>% 
  filter(treatment == 0)

# Descriptive stuff for start of result section

 # Countries
lpi_full_cons %>% 
  summarise(country_n = n_distinct(Country))

 # Conservation group
lpi_full_cons %>% 
  group_by(Class) %>% 
  count()

 # Control group
lpi_full_cont %>% 
  group_by(Class) %>% 
  count()

# nr of species and pops

# Conservation group
lpi_full_cons %>% group_by(common_class) %>% 
  summarise(n_species = n_distinct(Binomial))


lpi_full_cons %>% group_by(common_class) %>% 
  summarise(n_pops = n())


# Control group
lpi_full_cont %>% 
  group_by(Class) %>% 
  count()

# Plot taxonomy in each matched scenario
 # 
lpi_lib_pop <- bind_rows(lpi_lib_cons, lpi_lib_cont) 

lpi_lib_pop <- lpi_lib %>% 
  group_by(Class, treatment) %>% 
  summarise(n_class = n())

(plot<- ggplot(lpi_lib_pop, aes(x="", y=n_class, fill=Class))+
  geom_bar(width = 1, stat = "identity") +
  facet_wrap(~factor(treatment)))

# Species

 # Scenario 1
lpi_lib_species <- lpi_match_lib %>%
  group_by(Class, treatment) %>% 
  mutate(treatment = recode(treatment, '1' = "Conservation", '0' = "Without conservation")) %>% 
  summarise(n_species = n_distinct(Binomial))

(plot_lib<- ggplot(lpi_lib_species, aes(x="", y=n_species, fill=Class))+
    geom_bar(width = 1, stat = "identity") +
    scale_fill_viridis_d() +
    facet_wrap(~factor(treatment)) +
    ggtitle("Scenario 1") +
    xlab("") +
    ylab("Number of species"))


# Scenario 2
lpi_bench_species <- lpi_match_bench %>%
  group_by(Class, treatment) %>% 
  mutate(treatment = recode(treatment, '1' = "Conservation", '0' = "Without conservation")) %>% 
  summarise(n_species = n_distinct(Binomial))

(plot_bench<- ggplot(lpi_bench_species, aes(x="", y=n_species, fill=Class))+
    geom_bar(width = 1, stat = "identity") +
    scale_fill_viridis_d() +
    facet_wrap(~factor(treatment)) +
    ggtitle("Scenario 2") +
    xlab("") +
    ylab(""))

# Scenario 3
lpi_string_species <- lpi_match_stringent %>%
  group_by(Class, treatment) %>% 
  mutate(treatment = recode(treatment, '1' = "Conservation", '0' = "Without conservation")) %>% 
  summarise(n_species = n_distinct(Binomial))

(plot_string<- ggplot(lpi_string_species, aes(x="", y=n_species, fill=Class))+
    geom_bar(width = 1, stat = "identity") +
    scale_fill_viridis_d() +
    facet_wrap(~factor(treatment)) +
    ggtitle("Scenario 3")+
  xlab("") +
  ylab("Number of species"))

# Scenario 4
lpi_full_species <- lpi_work8 %>%
  group_by(Class, treatment) %>% 
  mutate(treatment = recode(treatment, '1' = "Conservation", '0' = "Without conservation")) %>% 
  summarise(n_species = n_distinct(Binomial))

(plot_full<- ggplot(lpi_full_species, aes(x="", y=n_species, fill=Class))+
    geom_bar(width = 1, stat = "identity") +
    scale_fill_viridis_d() +
    facet_wrap(~factor(treatment)) +
    ggtitle("Scenario 4")+
    xlab("") +
    ylab(""))

(plot_lib + plot_bench) / (plot_string +plot_full)

# Plot points

mapWorld <- borders("world", colour="gray50", fill="white")
mp <- ggplot() + mapWorld

lpi_loc <- lpi_work8 %>% 
  mutate(Conservation = if_else(treatment == 1, "Conservation", "Control"))

(cons_locations <- mp + geom_point(data = lpi_loc, aes(x = Longitude, y = Latitude, color = Conservation, shape = Conservation), size = 1.5) +
    theme_void() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size=30),
        text=element_text(size=30),
        legend.position = "top") + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  scale_color_viridis_d(end = 0.7))
  
  
 
#ggsave(filename = "C:/Users/seanj/OneDrive - University College London/Articles from Thesis/3. Assessing the effect of global conservation/Plots and tables/loc_cons_simple.tiff",
#       plot = cons_locations, compression = "lzw", width = 60, height = 40, dpi = 400, units = "cm")

 # How many are utilized

lpi_full_cons %>% 
  summarise(n_util = sum(Utilised == 1))

# Create infile and LPImain

# creatre infile and LPI main

# Liberal - scenario 1

create_infile(lpi_lib_cons, name = "lpi_lib_cons",  start_col_name = 'X1970', end_col_name = 'X2016')

lpi_matched_cons_lib<-LPIMain(infile = "lpi_lib_cons_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

create_infile(lpi_lib_cont, name = "lpi_lib_cont",  start_col_name = 'X1970', end_col_name = 'X2016')

lpi_matched_control_lib<-LPIMain(infile = "lpi_lib_cont_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

ggplot_lpi(lpi_matched_cons_lib, title = "Conservation", ylim=c(0.9, 3))+ggplot_lpi(lpi_matched_control_lib, title ="Without conservation")


# Benchmark - scenario 2

create_infile(lpi_bench_cons, name = "lpi_bench_cons",  start_col_name = 'X1970', end_col_name = 'X2016')

lpi_bench_cons<-LPIMain(infile = "lpi_bench_cons_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

create_infile(lpi_bench_cont, name = "lpi_bench_cont",  start_col_name = 'X1970', end_col_name = 'X2016')

lpi_bench_cont<-LPIMain(infile = "lpi_bench_cont_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

ggplot_lpi(lpi_bench_cons, title = "Conservation", ylim=c(0.9, 4.7)) + ggplot_lpi(lpi_bench_cont, title ="Without conservation")

a <- ggplot_lpi(lpi_matched_cons, title = "Conservation", ylim=c(0.9, 4.7)) 
b <- ggplot_lpi(lpi_matched_control, title ="Without conservation")

# Stringent - scenario 3

create_infile(lpi_string_cons, name = "lpi_string_cons",  start_col_name = 'X1970', end_col_name = 'X2016')

lpi_matched_cons_stringent<-LPIMain(infile = "lpi_string_cons_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

create_infile(lpi_string_cont, name = "lpi_string_cont",  start_col_name = 'X1970', end_col_name = 'X2016')

lpi_matched_control_stringent<-LPIMain(infile = "lpi_string_cont_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

ggplot_lpi(lpi_matched_cons_stringent, title = "Conservation", ylim=c(0.9, 3))+ggplot_lpi(lpi_matched_control_stringent, title ="Without conservation")

# Run lpi trend creation on full LPI for all species targeted by conservation and all that are not targeted
 
 # Not targeted by conservation - scenario 4

create_infile(lpi_full_cont, name = "lpi_full_cont",  start_col_name = 'X1970', end_col_name = 'X2016')

lpi_full_cont_trend<-LPIMain(infile = "lpi_full_cont_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

no_conservation_full <- ggplot_lpi(lpi_full_cont_trend, title ="Without conservation - full sample")

 # Targeted by conservation

create_infile(lpi_full_cons, name = "lpi_full_cons",  start_col_name = 'X1970', end_col_name = 'X2016')

lpi_full_cons_trend<-LPIMain(infile = "lpi_full_cons_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

conservation_full <- ggplot_lpi(lpi_full_cons_trend, title ="Conservation - full sample", ylim=c(0.8, 2.5))


conservation_full + no_conservation_full

# Create a nicer visual containing all samples 
 # First, gather all the indices, add scenario information and conservation info. We do this using a function

combiner <- function (data, conservation_status, scenario){
  data %>% 
    mutate("conservation status" = conservation_status,
           "Scenario" = scenario,
           year = 1970:2018)
}

libcons <- combiner(lpi_matched_cons_lib, "Conservation", "Scenario 1")

libcont <- combiner(lpi_matched_control_lib, "Counterfactual", "Scenario 1")  

benchcons <- combiner(lpi_bench_cons, "Conservation", "Scenario 2")

benchcont <- combiner(lpi_bench_cont, "Counterfactual", "Scenario 2")

stringcons <- combiner(lpi_matched_cons_stringent, "Conservation", "Scenario 3")

stringcont <- combiner(lpi_matched_control_stringent, "Counterfactual", "Scenario 3")
  
fullcons <- combiner(lpi_full_cons_trend, "Conservation", "Scenario 4")

fullcont <- combiner(lpi_full_cont_trend, "Counterfactual", "Scenario 4")

# Bind into one df for easy plotting

cons_data <- bind_rows(list(libcons, libcont, benchcons, benchcont, stringcons, stringcont, fullcons, fullcont), .id = 'source')

cons_data <- cons_data %>% 
  filter(year < 2017)

# plot

(impact_plot <- cons_data %>% 
    ggplot(., aes(x = year, y = LPI_final, color = `conservation status`, fill = `conservation status`)) + 
    geom_line(size = 2) +
    geom_ribbon(aes(ymin=CI_low, ymax=CI_high), linetype=3, alpha=0.5) +
    theme_classic() +
    geom_hline(yintercept = 1, linetype=2) +
  facet_wrap(~Scenario)) +
  theme(legend.title = element_blank(),
        text=element_text(size=30),
        legend.position = "bottom") +
  ylab("Vertebrate population index (1970 = 1)")+ xlab("Year") +
  scale_color_viridis_d(option = "D", begin = 0, end = 0.7, aesthetics = c("color", "fill"), direction = -1)

## The approximation of the 2010 science paper by Hoffmann et al 2010 ----

# Assume that all conservation targeted species had remained stable in the absence of conservation

 # First we check how th lpi looks for the full sample after preparation

lpi_all <- lpi%>% 
  rename_with(.cols = '1970':'2018',function(x){paste0("X", x)})


create_infile(lpi_all, name = "lpi_all",  start_col_name = 'X1970', end_col_name = 'X2016')

lpi_all_trend<-LPIMain(infile = "lpi_all_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

lpi_all_trend <- lpi_all_trend %>% 
  mutate(trend = "normal", 
         year = 1970:2018) %>% 
  filter(year < 2017)

ggplot_lpi(lpi_all_trend)

# Assign lpi data and start correcting it

lpi_all_corrected <- lpi_all

# Create a variable that indicates if population increase were caused by conservation 
# REMEMBER TO DISCUSS WHICH VARIABLES TO INCLUDE WITH lOUISE AND MIKE

 # Using all pops 
# lpi_all_corrected <- lpi_all_corrected %>% 
#  mutate(conservation_increase = if_else(Introduction == 1 | Recolonisation == 1 | Recruitment == 1 | Removal_of_threat == 1 | 
#                                         `Rural_to_urban migration` == 1 | Reintroduction == 1 | Range_shift == 1 | 
#                                         Legal_protection == 1 | Management == 1 | Unknown...163 == 1 | Other...164 == 1, 1, 0))

# Alternative restricted one - currently using this one

lpi_all_corrected <- lpi_all_corrected %>% 
  mutate(conservation_increase = if_else(Introduction == 1 | Recolonisation == 1 | Recruitment == 1 | Removal_of_threat == 1 | 
                                           Reintroduction == 1 | Range_shift == 1 | 
                                           Legal_protection == 1 | Management == 1, 1, 0)) # Remove recruitment, recolonisation and range shift

# With recruitment, recolonization and range shift removed
#lpi_all_corrected <- lpi_all_corrected %>% 
#  mutate(conservation_increase = if_else(Introduction == 1 | Removal_of_threat == 1 | 
#                                           Reintroduction == 1 | Legal_protection == 1 | Management == 1, 1, 0))
 
# Replace all observed population counts for the managed group with a constant number and re-create the trend

lpi_all_corrected <- lpi_all_corrected %>% 
 mutate(across(X1970:X2018,
                ~ if_else( Managed == 1 & conservation_increase == 1 & .x != "NULL", '5', .x)))

# Check how many pops

lpi_all_corrected %>% 
  filter(Managed == 1 & conservation_increase == 1) %>% 
  summarise(n = n(),
            n_species = n_distinct(Binomial))

create_infile(lpi_all_corrected, name = "lpi_all_corrected",  start_col_name = 'X1970', end_col_name = 'X2016')

lpi_all_trend_corrected<-LPIMain(infile = "lpi_all_corrected_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

lpi_all_trend_corrected <- lpi_all_trend_corrected %>% 
  mutate(trend = "stable conservation pops",
         year = 1970:2018) %>% 
  filter(year < 2017)

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


# Create alternative counterfactual that also acocunts for collateral conservation

# Replace all observed population counts for the managed group and for populations inside PAs with conservation as reason for increase 
# with a constant number and re-create the trend

 # First run used this but we have altered the code so this part is outdated.
#lpi_all_corrected_PA <- lpi_all_corrected %>%
#  mutate(PA = if_else(Ramsar == 1 | WHS == 1 | Biosphere == 1 | Other_PA == 1, 1, 0))

# Insted, we use a simplified version drawing directly on the whether a population is recorded as being inside a PA

lpi_all_corrected_PA <- lpi_all_corrected %>%
  mutate(PA = if_else(Protected_status == c("Yes", "Both"), 1, 0))


lpi_all_corrected_PA <- lpi_all_corrected_PA %>%
  mutate(across(X1970:X2018,
                ~ if_else( Managed == 1 & conservation_increase == 1 & .x != "NULL" | PA ==1  & conservation_increase == 1 & .x != "NULL", '5', .x)))

# check how many pops are changed

lpi_all_corrected_PA %>% 
  filter(Managed == 1 & conservation_increase == 1 | PA ==1  & conservation_increase == 1) %>% 
  summarise(n = n(),
            n_species = n_distinct(Binomial)) # 600 pops

create_infile(lpi_all_corrected_PA, name = "lpi_all_corrected_PA",  start_col_name = 'X1970', end_col_name = 'X2016')

lpi_all_trend_corrected_PA<-LPIMain(infile = "lpi_all_corrected_PA_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

lpi_all_trend_corrected_PA <- lpi_all_trend_corrected_PA %>% 
  mutate(trend = "PA stable conservation pops",
         year = 1970:2018) %>% 
  filter(year < 2017)

# Richard talked about adding one without conservation so we create a global index where conservation targeted populations have been excluded.
 
 # First, filter away conservation targeted populations

excluding_cons <- anti_join(lpi_all, lpi_full_cons, by = "ID")

# Create a trend to check how it looks

create_infile(excluding_cons, name = "excluding_cons",  start_col_name = 'X1970', end_col_name = 'X2016')

excluding_cons_trend<-LPIMain(infile = "excluding_cons_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

excluding_cons_trend <- excluding_cons_trend %>% 
  mutate(trend = "Excluding cons trend",
         year = 1970:2018) %>% 
  filter(year < 2017)


# bind together for easier plotting

lpi_merged_trend_PA <- bind_rows(lpi_all_trend, lpi_all_trend_corrected, lpi_all_trend_corrected_PA, excluding_cons_trend)

# Recode trend levels. I haven't named them well

lpi_merged_trend_PA <- lpi_merged_trend_PA %>%  
  mutate(trend = recode(lpi_merged_trend_PA$trend, normal = "Reference index", `stable conservation pops` = "Stable conservation populations", 
                        `PA stable conservation pops` = "Stable conservation population & PAs"))

(global_cons_impact <- lpi_merged_trend_PA %>% 
    ggplot(., aes(x = year, y = LPI_final, color = trend, fill = trend)) + 
    geom_hline(yintercept = 1, linetype=2) +
    geom_line(size = 2) +
    geom_ribbon(aes(ymin=CI_low, ymax=CI_high), linetype=3, alpha=0.1) +
    theme_classic() +
    ylab("Index (1970 = 1)") +
    theme(legend.title = element_blank(),
          text=element_text(size=30),
          legend.position = "bottom")+
    scale_color_viridis_d(option = "D", begin = 0, end = 0.9, aesthetics = c("color", "fill"), direction = -1))

# Create a plot where improvements are shown relative to the unweighted LPI

lpi_merged_trend_PA1 <- lpi_merged_trend_PA %>% 
  pivot_wider(values_from = c(LPI_final, CI_low, CI_high),
              names_from = trend) %>% 
  mutate("Conservation targeted populations" = .[[2]] - .[[3]],
         "Conservation targeted populations and inside PAs" = .[[2]] - .[[4]],
         "Excluding conservation targeted populations" = .[[2]] - .[[5]],) %>% 
  select(1, 14:16) %>% 
  pivot_longer(cols = 2:4,
               names_to="Index",
               values_to = "Improvement")

# Plot improvements


(global_cons_improvement <- lpi_merged_trend_PA1 %>% 
  ggplot(., aes(x = year, y = Improvement, color = Index)) + 
  geom_hline(yintercept = 0, linetype=2) +
  geom_line(size = 2) +
  theme_classic() +
  ylab("Index Improvement") +
    ylim(-0.1, 0.2)+
  theme(legend.title = element_blank(),
        text=element_text(size=20),
        legend.position = "bottom")+
  scale_color_viridis_d(option = "D", begin = 0, end = 0.9, aesthetics = c("color", "fill"), direction = -1))

ggsave(filename = "C:/Users/seanj/OneDrive - University College London/Articles from Thesis/3. Assessing the effect of global conservation/Plots and tables/global_cons_improvement.tiff",
       plot = global_cons_improvement, compression = "lzw", width = 40, height = 20, dpi = 400, units = "cm")



## # Relabel factors for balloon plot. UPDATE not necessary, done below

#lpi_work9 <- lpi_work8 %>% 
#  mutate(across(land_water_management:research,
#                ~case_when(.x == 0 ~ "Not targeted",
#                           TRUE ~ "Targeted")))

#lpi_work10 <- lpi_work9 %>% 
#  mutate(across(Introduction:'Other...164',
#                ~case_when(.x == "NULL" ~ "Unknown",
#                           TRUE ~ "Reason for increase")))

## Test frequency of co-occurence between conservation actions and reasons for population increases ----

# First, redo the lpi_work8 df into a format that's appropriate for the balloon plot

baloon <- lpi_work8 %>% 
  select(land_water_protection, land_water_management,species_management, 
         education_awareness, law_policy, incentives, external_capacity, research, Introduction, Recolonisation, Recruitment, 
         Removal_of_threat, Reintroduction, Range_shift, Legal_protection, Management)

# pivot conservation actions longer

baloon1 <- baloon %>% 
  pivot_longer((1:8),
               names_to = "Conservation_lvl_1",
               values_to = "active_cons")

# pivot reasons for increase longer

baloon2 <- baloon1 %>% 
  pivot_longer((1:8),
               names_to = "Reason_increase",
               values_to = "active_reason")

# select pops with conservation and reason for increase listed

baloon3 <- baloon2 %>% 
  filter(active_cons ==1 & active_reason == 1)

# Count frequency of combinations

baloon4 <- baloon3 %>% 
  count(Conservation_lvl_1, Reason_increase) %>% 
  spread(Reason_increase, n) # spread is similar to pivot_wider but is older tidyr syntax. Rewrite to new syntax when 
# time to spare

# Save first col as row names

baloon5 <- data.frame(baloon4, row.names = 1)

# Plot - Looks like it shows decent overlap between reasons for increase and the conservation actions we would expect - good.

ggballoonplot(baloon5, size = "value", show.label = TRUE)
  
# Test sensitivity of these pops following similar approach as below(ts > (5 & 10) and remove upper and lower quantiles) ----
# Discuss with Mike and Louise whether this makes sense before doing it. Do regressions instead 30/08/2021.
 # 5 years
lpi_all5 <- lpi_all %>% 
  filter(ts_length)

lpi_all_corrected5 <- lpi_all_corrected %>% 
  filter(ts_length > 5)

lpi_all_corrected_PA5 <- lpi_all_corrected_PA %>% 
  filter(ts_length > 5)
# Plot conservation - Using same basic representation as in Bolam et al 2020 

 # First, transform lpi_work8 into long format, stretching primary conservation and sub categories

# Not used. Instead mutate combined with case_when to create a primary category for each conservation action
#lpi_plot_filtered <- lpi_plot1 %>% 
#  pivot_longer(land_water_protection:research,
#               names_to = "primary_action",
#               values_to = "action_type") %>% 
#  filter(action_type == 1)

# Long format so that each conservation action is a row. We need this to plot conservation actions grouped by primary category. Otherwise, each row
# has a separate column for each primary category which is a mess to plot

lpi_work8_long <- lpi_work8 %>% 
  pivot_longer(site_area_protection_1.1:harvest_trends_r3.2 ,
               names_to = "cons_action",
               values_to = "active") %>%
  filter(active == 1)

lpi_work8_long1 <-lpi_work8_long %>% 
  mutate(primary_cons_category = case_when(cons_action == "site_area_protection_1.1" ~ "Land & water protection",
                                           str_detect(cons_action, regex("_2", ignore_case=TRUE)) ~ "Land & water management",
                                           str_detect(cons_action, regex("_3", ignore_case=TRUE)) ~ "Species management",
                                           str_detect(cons_action, regex("_4", ignore_case=TRUE)) ~ "Education & awareness",
                                           str_detect(cons_action, regex("_5", ignore_case=TRUE)) ~ "Law & policy",
                                           str_detect(cons_action, regex("_6", ignore_case=TRUE)) ~ "Livelihood, economic and other incentives",
                                           str_detect(cons_action, regex("_7", ignore_case=TRUE)) ~ "External capacity building",
                                           str_detect(cons_action, regex("_r", ignore_case=TRUE)) ~ "Research",
                                           TRUE ~ "other"))  

df <- lpi_work8_long1 %>%
  group_by(cons_action, primary_cons_category, Class) %>%
  summarise(counts = n()) %>% 
  group_by(Class) %>% 
  mutate(percentage = (counts/sum(counts))*100)
            
df

df %>% 
  filter(primary_cons_category != "Research") %>% 
  filter(Class %in% c("Actinopteri","Aves","Mammalia")) %>% 
  ggplot(aes(x = percentage, y = reorder(cons_action, percentage), fill = primary_cons_category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = counts), size = 6, color = "black") + 
  theme_pubclean() +
  facet_wrap(~Class) +
  theme(text=element_text(size=21),
        legend.title = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_viridis_d()

# Plot using "common classes"
df_mod <- lpi_work8_long1 %>%
  group_by(cons_action, primary_cons_category, common_class) %>%
  summarise(counts = n()) %>% 
  group_by(common_class) %>% 
  mutate(percentage = (counts/sum(counts))*100)

df_mod %>% 
  filter(primary_cons_category != "Research") %>% 
  filter(common_class %in% c("Fishes","Birds","Mammals")) %>% 
  ggplot(aes(x = percentage, y = reorder(cons_action, percentage), fill = primary_cons_category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = counts), size = 6, color = "black") + 
  theme_pubclean() +
  facet_wrap(~common_class) +
  theme(text=element_text(size=21),
        legend.title = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_viridis_d()

# Plotting primary actions  
df1 <- lpi_work8_long1 %>%
  group_by(primary_cons_category, Class) %>%
  summarise(counts = n()) %>% 
  group_by(Class) %>% 
  mutate(percentage = (counts/sum(counts))*100)

df1 %>% 
  filter(Class %in% c("Actinopteri","Aves","Mammalia")) %>% 
  ggplot(aes(x = percentage, y = reorder(primary_cons_category, percentage), fill = primary_cons_category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = counts), size = 6, color = "black") + 
  theme_pubclean() +
  facet_wrap(~Class) +
  theme(text=element_text(size=21),
        legend.title = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_viridis_d()

# Plot using common class

df1_mod <- lpi_work8_long1 %>%
  group_by(primary_cons_category, common_class) %>%
  summarise(counts = n()) %>% 
  group_by(common_class) %>% 
  mutate(percentage = (counts/sum(counts))*100)

main_cons_plot <-df1_mod %>% 
  filter(common_class %in% c("Fishes","Birds","Mammals")) %>% 
  ggplot(aes(x = percentage, y = reorder(primary_cons_category, percentage), fill = primary_cons_category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = counts), size = 6, color = "black") + 
  theme_pubclean() +
  facet_wrap(~common_class) +
  xlab("Percentage") +
  theme(legend.position = "none",
        text=element_text(size=21),
        legend.title = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_viridis_d()

#ggsave(filename = "C:/Users/seanj/OneDrive - University College London/Articles from Thesis/3. Assessing the effect of global conservation/Plots and tables/main_cons_plot.tiff",
#       plot = main_cons_plot, compression = "lzw", width = 40, height = 20, dpi = 400, units = "cm")

# Plot main and sub for all classes
 # main cons actions
df2 <- lpi_work8_long1 %>%
  group_by(primary_cons_category, Class) %>%
  summarise(counts = n()) %>% 
  group_by(Class) %>% 
  mutate(percentage = (counts/sum(counts))*100)

df2 %>% 
  ggplot(aes(x = percentage, y = reorder(primary_cons_category, percentage), fill = primary_cons_category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = counts), size = 6, color = "black") + 
  theme_pubclean() +
  facet_wrap(~Class) +
  theme(text=element_text(size=21),
        legend.title = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_viridis_d()

 # secondary actions

df3 <- lpi_work8_long1 %>%
  group_by(cons_action, primary_cons_category, Class) %>%
  summarise(counts = n()) %>% 
  group_by(Class) %>% 
  mutate(percentage = (counts/sum(counts))*100)

df3 <-df3 %>% 
  ggplot(aes(x = percentage, y = reorder(cons_action, percentage), fill = primary_cons_category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = counts), size = 6, color = "black") + 
  theme_pubclean() +
  facet_wrap(~Class) +
  theme(text=element_text(size=21),
        legend.title = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_viridis_d()

# Save plot

#ggsave(filename = "C:/Users/seanj/OneDrive - University College London/Articles from Thesis/3. Assessing the effect of global conservation/Plots and tables/all_cons.tiff",
#       plot = df3, compression = "lzw", width = 60, height = 40, dpi = 400, units = "cm")

# Check number of species in each class - excluding populations exclusively targeted by the research category

lpi_work8_long1 %>% 
  filter(primary_cons_category != "Research") %>% 
  group_by(Class) %>% 
  summarise(n_distinct(Binomial))


# Check number of pops in each class

lpi_work8_long1 %>% 
  filter(primary_cons_category != "Research") %>% 
  group_by(Class) %>% 
  summarise(n_distinct(ID))

# Check number of populations with research as the only management action - 41 pops
lpi_work8_long1 %>% 
  filter(primary_cons_category != "Research") %>% 
  summarise(n_populations = n_distinct(ID))

lpi_work8_long1 %>%  
  summarise(n_populations = n_distinct(ID))

# Mean length of time series 

lpi_work8 %>% 
  group_by(treatment) %>% 
  summarise(mean_ts_length = mean(ts_length))

# map showing length of ts

mapWorld <- borders("world", colour="gray50", fill="white")
mp <- ggplot() + mapWorld

lpi_loc <- lpi_work8 %>% 
  mutate(Conservation = if_else(treatment == 1, "Conservation", "Control"))

(cons_length <- mp + geom_point(data = lpi_loc, aes(x = Longitude, y = Latitude, color = ts_length, shape = Conservation), size = 2) +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.text = element_text(size=20),
          text=element_text(size=30),
          legend.position = "top") + 
    guides(shape = guide_legend(override.aes = list(size = 5))) +
    scale_color_viridis_c(end = 1))



#ggsave(filename = "C:/Users/seanj/OneDrive - University College London/Articles from Thesis/3. Assessing the effect of global conservation/Plots and tables/length_cons.tiff",
#       plot = cons_length, compression = "lzw", width = 60, height = 40, dpi = 400, units = "cm")

# map showing starting year of ts

(cons_start <- mp + geom_point(data = lpi_loc, aes(x = Longitude, y = Latitude, color = start_year, shape = Conservation), size = 2) +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.text = element_text(size=10),
          text=element_text(size=30),
          legend.position = "top") + 
    guides(shape = guide_legend(override.aes = list(size = 5))) +
    scale_color_viridis_c(end = 1))
  

#ggsave(filename = "C:/Users/seanj/OneDrive - University College London/Articles from Thesis/3. Assessing the effect of global conservation/Plots and tables/start_cons.tiff",
#       plot = cons_start, compression = "lzw", width = 60, height = 40, dpi = 400, units = "cm")


# Sensitivity analysis ----

# Function for binding indices together and another function for plotting them accordingly

 # Wrangle and bind
sens <- function(index1, index2, conservation, counterfactual, scenario, sensitivity) {
  index1 <- index1 %>% 
    drop_na() %>% 
    mutate("Scenario" = scenario,
           "Sensitivity" = sensitivity,
           conservation_status = "Conservation",
           year = 1970:2016)
  
  index2 <- index2 %>% 
    drop_na() %>% 
    mutate("Scenario" = scenario,
           "Sensitivity" = sensitivity,
           conservation_status = "Counterfactual",
           year = 1970:2016)
  
  bind_rows(index1, index2)
}
 
 # Plot

plot_sens <- function(df, scenario, sensitivity) {
  df %>% 
    ggplot(., aes(x = year, y = LPI_final, color = conservation_status, fill = conservation_status)) + 
    geom_line(size = 2) +
    geom_ribbon(aes(ymin=CI_low, ymax=CI_high), linetype=3, alpha=0.5) +
    theme_classic() +
    geom_hline(yintercept = 1, linetype=2)+
    theme(legend.title = element_blank(),
          text=element_text(size=30),
          legend.position = "bottom") +
    labs(title = scenario,
         subtitle = sensitivity) +
    ylab("Index (1970 = 1)")+ xlab("Year") +
    scale_color_viridis_d(option = "D", begin = 0, end = 0.7, aesthetics = c("color", "fill"), direction = -1)
}

# Full sample - Scenario 4  ----
# Start with 5 year restriction

# select ts > 5 years

lpi_full_cons5 <- lpi_work8 %>% 
  filter(treatment == 1 & ts_length > 5)

lpi_full_cont5 <- lpi_work8 %>% 
  filter(treatment == 0 & ts_length > 5)
# create infiles

create_infile(lpi_full_cons5, name = "lpi_full_cons5",  start_col_name = 'X1970', end_col_name = 'X2016')

full_cons5<-LPIMain(infile = "lpi_full_cons5_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

create_infile(lpi_full_cont5, name = "lpi_full_cont5",  start_col_name = 'X1970', end_col_name = 'X2016')

full_cont5<-LPIMain(infile = "lpi_full_cont5_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

ggplot_lpi(full_cons5, title = "Conservation", ylim=c(0.9, 4.7))+ggplot_lpi(full_cont5, title ="Without conservation")

full_cons5_plot <- ggplot_lpi(full_cons5, title = "Full LPI - Conservation", ylim = c(0.9, 2.5))
full_cont5_plot <- ggplot_lpi(full_cont5,title ="Without conservation", ylim = c(0.7, 1.2))
full_cons5_plot + full_cont5_plot

# Using the sens_plot function

full5 <- sens(full_cons5, full_cont5, scenario = 'Scenario 4', sensitivity = '>5')

# Plot with <5 year excluded

scenario4_5<-plot_sens(full5, "Scenario 4", "Excluding time series < 5")

# As above but restricted to ts > 10 

lpi_full_cons10 <- lpi_work8 %>% 
  filter(treatment == 1 & ts_length > 10)

lpi_full_cont10 <- lpi_work8 %>% 
  filter(treatment == 0 & ts_length > 10)
# create infiles

create_infile(lpi_full_cons10, name = "lpi_full_cons10",  start_col_name = 'X1970', end_col_name = 'X2016')

full_cons10<-LPIMain(infile = "lpi_full_cons10_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

create_infile(lpi_full_cont10, name = "lpi_full_cont10",  start_col_name = 'X1970', end_col_name = 'X2016')

full_cont10<-LPIMain(infile = "lpi_full_cont10_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

ggplot_lpi(full_cons10, title = "Conservation", ylim=c(0.9, 2.5))+ggplot_lpi(full_cont5, title ="Without conservation")

full_cons10_plot <- ggplot_lpi(full_cons10, ylim=c(0.9, 2.5))
full_cont10_plot <- ggplot_lpi(full_cont10, ylim = c(0.7, 1.2))
full_cons10_plot + full_cont10_plot

# plot

full10 <- sens(full_cons10, full_cont10, scenario = 'Scenario 4', sensitivity = '>10')

# Plot with <10 year excluded

(scenario4_10 <- plot_sens(full10, "", "Excluding time series < 10"))

# lastly, excluding 1% quantile to test the sensitivity to the extremes

### Load lambda file (removing 1970) for the index you want to look at
lambda_cons <- read.csv("lpi_full_cons_pops_Lambda.txt")
lambda_cont <- read.csv("lpi_full_cont_pops_Lambda.txt")



### Remove column with species names
lambda2_cons <- lambda_cons[,-1]

lambda2_cont <- lambda_cont[,-1]


### Calculate the 95th or 99th quartile of the data set
q95_cons <- quantile(as.matrix(lambda2_cons), c(0.05, 0.95), na.rm=TRUE)
q99_cons <- quantile(as.matrix(lambda2_cons), c(0.01, 0.99), na.rm=TRUE)
hist(as.numeric(as.vector(as.matrix(lambda2_cons))), breaks=10000)
abline(v = q95_cons[1], col="red")
abline(v = q95_cons[2], col="red")
abline(v = q99_cons[1], col="pink")
abline(v = q99_cons[2], col="pink")

### Calculate the 95th or 99th quartile of the data set
q95_cont <- quantile(as.matrix(lambda2_cont), c(0.05, 0.95), na.rm=TRUE)
q99_cont <- quantile(as.matrix(lambda2_cont), c(0.01, 0.99), na.rm=TRUE)
hist(as.numeric(as.vector(as.matrix(lambda2_cont))), breaks=10000)
abline(v = q95_cont[1], col="red")
abline(v = q95_cont[2], col="red")
abline(v = q99_cont[1], col="pink")
abline(v = q99_cont[2], col="pink")

lower_cons <- q99_cons[1]
higher_cons <- q99_cons[2]

removed_cons <- as.matrix(lambda2_cons)


lower_cont <- q99_cont[1]
higher_cont <- q99_cont[2]

removed_cont <- as.matrix(lambda2_cont)

### Mark outliers with -9999
removed_cons[removed_cons < lower_cons | removed_cons > higher_cons] = -9999
range(removed_cons, na.rm =T)

removed_cont[removed_cont < lower_cont | removed_cont > higher_cont] = -9999
range(removed_cont, na.rm =T)

### Replaces the numbers with the species names as rownames
rownames(removed_cons) <- lambda_cons[, 1]

rownames(removed_cont) <- lambda_cont[, 1]

### Find species with outliers
indices_cons <- which(removed_cons == -9999, arr.ind=TRUE)
sp_to_be_removed_cons <- unique(rownames(indices_cons))
sp_to_be_removed_cons

indices_cont <- which(removed_cont == -9999, arr.ind=TRUE)
sp_to_be_removed_cont <- unique(rownames(indices_cont))
sp_to_be_removed_cont

### How many outlier values are there
sum(removed_cons == -9999, na.rm = T)

sum(removed_cont == -9999, na.rm = T)


#### Re-run the index calculation (print datapoint files) with the dataset cleared from outliers
full_cons_nooutliers  = lpi_full_cons %>% 
  filter(!Binomial %in% sp_to_be_removed_cons)

create_infile(full_cons_nooutliers, name = "full_cons_nooutliers", start_col_name = 'X1970', end_col_name = 'X2016')
full_cons_index_nooutliers <- LPIMain("full_cons_nooutliers_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2016)

full_cont_nooutliers  = lpi_full_cont %>% 
  filter(!Binomial %in% sp_to_be_removed_cont)

create_infile(full_cont_nooutliers, name = "full_cont_nooutliers", start_col_name = 'X1970', end_col_name = 'X2016')
full_cont_index_nooutliers <- LPIMain("full_cont_nooutliers_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2016)

full_cons1percent_plot <- ggplot_lpi(full_cons_index_nooutliers, ylim=c(0.9, 2.5))
full_cont1percent_plot <- ggplot_lpi(full_cont_index_nooutliers, ylim = c(0.7, 1.2))

# Plot full with outliers removed

fullout <- sens(full_cons_index_nooutliers, full_cont_index_nooutliers, scenario = 'Scenario 4', sensitivity = '1% quantiles')

# Plot with 1% outliers excluded

(scenario4_quant <- plot_sens(fullout, "", "Excluding outliers"))

# Plots with single window for each index

(full_cons5_plot + labs(subtitle = 'Time series > 5 year') + full_cont5_plot + labs(subtitle = 'Time series > 5 year'))/ 
  (full_cons10_plot + labs(subtitle = 'Time series > 10 year') + full_cont10_plot + labs(subtitle = 'Time series > 10 year'))/ 
  (full_cons1percent_plot + labs(subtitle = 'Upper and lower quantile removed') + full_cont1percent_plot + labs(subtitle = 'Upper and lower quantile removed'))

# Plot combined plots insted 

scenario4_5+scenario4_10+scenario4_quant

## Scenario 1 (liberal) ----
# select ts > 5 years

lpi_lib_cons5 <- lpi_lib_cons %>% 
  filter(treatment == 1 & ts_length > 5)

lpi_lib_cont5 <- lpi_lib_cont %>% 
  filter(treatment == 0 & ts_length > 5)

# create infiles

create_infile(lpi_lib_cons5, name = "lpi_lib_cons5",  start_col_name = 'X1970', end_col_name = 'X2016')

lib_cons5<-LPIMain(infile = "lpi_lib_cons5_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

create_infile(lpi_lib_cont5, name = "lpi_lib_cont5",  start_col_name = 'X1970', end_col_name = 'X2016')

lib_cont5<-LPIMain(infile = "lpi_lib_cont5_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

ggplot_lpi(lib_cons5, title = "Conservation", ylim=c(0.9, 4.7))+ggplot_lpi(lib_cont5, title ="Without conservation")

lib_cons5_plot <- ggplot_lpi(lib_cons5, title = "lib LPI - Conservation", ylim = c(0.9, 2.5))
lib_cont5_plot <- ggplot_lpi(lib_cont5,title ="Without conservation", ylim = c(0.7, 1.2))
lib_cons5_plot + lib_cont5_plot

# Using the sens_plot function

scenario1_5 <- sens(lib_cons5, lib_cont5, scenario = 'Scenario 1', sensitivity = '>5')

# Plot with <5 year excluded

(scen1_5<-plot_sens(scenario1_5, "Scenario 1", "Excluding time series < 5"))


# As above but restricted to ts > 10 

lpi_lib_cons10 <- lpi_lib_cons %>% 
  filter(treatment == 1 & ts_length > 10)

lpi_lib_cont10 <- lpi_lib_cont %>% 
  filter(treatment == 0 & ts_length > 10)
# create infiles

create_infile(lpi_lib_cons10, name = "lpi_lib_cons10",  start_col_name = 'X1970', end_col_name = 'X2016')

lib_cons10<-LPIMain(infile = "lpi_lib_cons10_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

create_infile(lpi_lib_cont10, name = "lpi_lib_cont10",  start_col_name = 'X1970', end_col_name = 'X2016')

lib_cont10<-LPIMain(infile = "lpi_lib_cont10_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

ggplot_lpi(lib_cons10, title = "Conservation", ylim=c(0.9, 2.5))+ggplot_lpi(lib_cont5, title ="Without conservation")

lib_cons10_plot <- ggplot_lpi(lib_cons10, ylim=c(0.9, 2.5))
lib_cont10_plot <- ggplot_lpi(lib_cont10, ylim = c(0.7, 1.2))
lib_cons10_plot + lib_cont10_plot

# plot

scenario1_10 <- sens(lib_cons10, lib_cont10, scenario = 'Scenario 1', sensitivity = '>10')

# Plot with <10 year excluded

(scen1_10 <- plot_sens(scenario1_10, "", "Excluding time series < 10"))


# lastly, excluding 1% quantile to test the sensitivity to the extremes

### Load lambda file (removing 1970) for the index you want to look at

lambda_cons <- read.csv("lpi_lib_cons_pops_Lambda.txt")
lambda_cont <- read.csv("lpi_lib_cont_pops_Lambda.txt")


### Remove column with species names

lambda2_cons <- lambda_cons[,-1]

lambda2_cont <- lambda_cont[,-1]


### Calculate the 95th or 99th quartile of the data set

q95_cons <- quantile(as.matrix(lambda2_cons), c(0.05, 0.95), na.rm=TRUE)
q99_cons <- quantile(as.matrix(lambda2_cons), c(0.01, 0.99), na.rm=TRUE)
hist(as.numeric(as.vector(as.matrix(lambda2_cons))), breaks=10000)
abline(v = q95_cons[1], col="red")
abline(v = q95_cons[2], col="red")
abline(v = q99_cons[1], col="pink")
abline(v = q99_cons[2], col="pink")

### Calculate the 95th or 99th quartile of the data set
q95_cont <- quantile(as.matrix(lambda2_cont), c(0.05, 0.95), na.rm=TRUE)
q99_cont <- quantile(as.matrix(lambda2_cont), c(0.01, 0.99), na.rm=TRUE)
hist(as.numeric(as.vector(as.matrix(lambda2_cont))), breaks=10000)
abline(v = q95_cont[1], col="red")
abline(v = q95_cont[2], col="red")
abline(v = q99_cont[1], col="pink")
abline(v = q99_cont[2], col="pink")

lower_cons <- q99_cons[1]
higher_cons <- q99_cons[2]

removed_cons <- as.matrix(lambda2_cons)


lower_cont <- q99_cont[1]
higher_cont <- q99_cont[2]

removed_cont <- as.matrix(lambda2_cont)

### Mark outliers with -9999

removed_cons[removed_cons < lower_cons | removed_cons > higher_cons] = -9999
range(removed_cons, na.rm =T)

removed_cont[removed_cont < lower_cont | removed_cont > higher_cont] = -9999
range(removed_cont, na.rm =T)

### Replaces the numbers with the species names as rownames

rownames(removed_cons) <- lambda_cons[, 1]

rownames(removed_cont) <- lambda_cont[, 1]

### Find species with outliers

indices_cons <- which(removed_cons == -9999, arr.ind=TRUE)
sp_to_be_removed_cons <- unique(rownames(indices_cons))
sp_to_be_removed_cons

indices_cont <- which(removed_cont == -9999, arr.ind=TRUE)
sp_to_be_removed_cont <- unique(rownames(indices_cont))
sp_to_be_removed_cont

### How many outlier values are there

sum(removed_cons == -9999, na.rm = T)

sum(removed_cont == -9999, na.rm = T)


#### Re-run the index calculation (print datapoint files) with the dataset cleared from outliers

lib_cons_nooutliers  = lpi_lib_cons %>% 
  filter(!Binomial %in% sp_to_be_removed_cons)

create_infile(lib_cons_nooutliers, name = "lib_cons_nooutliers", start_col_name = 'X1970', end_col_name = 'X2016')
lib_cons_index_nooutliers <- LPIMain("lib_cons_nooutliers_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2016)

lib_cont_nooutliers  = lpi_lib_cont %>% 
  filter(!Binomial %in% sp_to_be_removed_cont)

create_infile(lib_cont_nooutliers, name = "lib_cont_nooutliers", start_col_name = 'X1970', end_col_name = 'X2016')
lib_cont_index_nooutliers <- LPIMain("lib_cont_nooutliers_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2016)

lib_cons1percent_plot <- ggplot_lpi(lib_cons_index_nooutliers, ylim=c(0.9, 2.5))
lib_cont1percent_plot <- ggplot_lpi(lib_cont_index_nooutliers, ylim = c(0.7, 1.5))

(lib_cons5_plot + labs(subtitle = 'Time series > 5 year') + lib_cont5_plot + labs(subtitle = 'Time series > 5 year'))/ 
  (lib_cons10_plot + labs(subtitle = 'Time series > 10 year') + lib_cont10_plot + labs(subtitle = 'Time series > 10 year'))/ 
  (lib_cons1percent_plot + labs(subtitle = 'Upper and lower quantile removed') + lib_cont1percent_plot + labs(subtitle = 'Upper and lower quantile removed'))


# Plot full with outliers removed

scenario1_out <- sens(lib_cons_index_nooutliers, lib_cont_index_nooutliers, scenario = 'Scenario 1', sensitivity = '1% quantiles')

# Plot with 1% outliers excluded

(scen1_quant <- plot_sens(scenario1_out, "", "Excluding outliers"))

scen1_5 + scen1_10 + scen1_quant
## Scenario 2 (benchmark) ----
# select ts > 5 years

lpi_bench_cons5 <- lpi_bench_cons %>% 
  filter(treatment == 1 & ts_length > 5)

lpi_bench_cont5 <- lpi_bench_cont %>% 
  filter(treatment == 0 & ts_length > 5)

# create infiles

create_infile(lpi_bench_cons5, name = "lpi_bench_cons5",  start_col_name = 'X1970', end_col_name = 'X2016')

bench_cons5<-LPIMain(infile = "lpi_bench_cons5_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

create_infile(lpi_bench_cont5, name = "lpi_bench_cont5",  start_col_name = 'X1970', end_col_name = 'X2016')

bench_cont5<-LPIMain(infile = "lpi_bench_cont5_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

ggplot_lpi(bench_cons5, title = "Conservation", ylim=c(0.9, 4.7))+ggplot_lpi(bench_cont5, title ="Without conservation")

bench_cons5_plot <- ggplot_lpi(bench_cons5, title = "bench LPI - Conservation", ylim = c(0.9, 4.1))
bench_cont5_plot <- ggplot_lpi(bench_cont5,title ="Without conservation", ylim = c(0.6, 2.1))
bench_cons5_plot + bench_cont5_plot

# Using the sens_plot function

scenario2_5 <- sens(bench_cons5, bench_cont5, scenario = 'Scenario 2', sensitivity = '>5')

# Plot with <5 year excluded

(scen2_5<-plot_sens(scenario2_5, "Scenario 2", "Excluding time series < 5"))

# As above but restricted to ts > 10 

lpi_bench_cons10 <- lpi_bench_cons %>% 
  filter(treatment == 1 & ts_length > 10)

lpi_bench_cont10 <- lpi_bench_cont %>% 
  filter(treatment == 0 & ts_length > 10)

# create infiles

create_infile(lpi_bench_cons10, name = "lpi_bench_cons10",  start_col_name = 'X1970', end_col_name = 'X2016')

bench_cons10<-LPIMain(infile = "lpi_bench_cons10_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

create_infile(lpi_bench_cont10, name = "lpi_bench_cont10",  start_col_name = 'X1970', end_col_name = 'X2016')

bench_cont10<-LPIMain(infile = "lpi_bench_cont10_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

ggplot_lpi(bench_cons10, title = "Conservation", ylim=c(0.9, 2.5))+ggplot_lpi(bench_cont5, title ="Without conservation")

bench_cons10_plot <- ggplot_lpi(bench_cons10, ylim=c(0.9, 4.1))
bench_cont10_plot <- ggplot_lpi(bench_cont10, ylim = c(0.6, 2.6))
bench_cons10_plot + bench_cont10_plot

scenario2_10 <- sens(bench_cons10, bench_cont10, scenario = 'Scenario 2', sensitivity = '>10')

# Plot 

(scen2_10<-plot_sens(scenario2_10, "", "Excluding time series < 10"))


# lastly, excluding 1% quantile to test the sensitivity to the extremes

### Load lambda file (removing 1970) for the index you want to look at

lambda_cons <- read.csv("lpi_bench_cons_pops_Lambda.txt")
lambda_cont <- read.csv("lpi_bench_cont_pops_Lambda.txt")



### Remove column with species names

lambda2_cons <- lambda_cons[,-1]

lambda2_cont <- lambda_cont[,-1]


### Calculate the 95th or 99th quartile of the data set

q95_cons <- quantile(as.matrix(lambda2_cons), c(0.05, 0.95), na.rm=TRUE)
q99_cons <- quantile(as.matrix(lambda2_cons), c(0.01, 0.99), na.rm=TRUE)
hist(as.numeric(as.vector(as.matrix(lambda2_cons))), breaks=10000)
abline(v = q95_cons[1], col="red")
abline(v = q95_cons[2], col="red")
abline(v = q99_cons[1], col="pink")
abline(v = q99_cons[2], col="pink")

### Calculate the 95th or 99th quartile of the data set

q95_cont <- quantile(as.matrix(lambda2_cont), c(0.05, 0.95), na.rm=TRUE)
q99_cont <- quantile(as.matrix(lambda2_cont), c(0.01, 0.99), na.rm=TRUE)
hist(as.numeric(as.vector(as.matrix(lambda2_cont))), breaks=10000)
abline(v = q95_cont[1], col="red")
abline(v = q95_cont[2], col="red")
abline(v = q99_cont[1], col="pink")
abline(v = q99_cont[2], col="pink")

lower_cons <- q99_cons[1]
higher_cons <- q99_cons[2]

removed_cons <- as.matrix(lambda2_cons)


lower_cont <- q99_cont[1]
higher_cont <- q99_cont[2]

removed_cont <- as.matrix(lambda2_cont)

### Mark outliers with -9999

removed_cons[removed_cons < lower_cons | removed_cons > higher_cons] = -9999
range(removed_cons, na.rm =T)

removed_cont[removed_cont < lower_cont | removed_cont > higher_cont] = -9999
range(removed_cont, na.rm =T)

### Replaces the numbers with the species names as rownames

rownames(removed_cons) <- lambda_cons[, 1]

rownames(removed_cont) <- lambda_cont[, 1]

### Find species with outliers

indices_cons <- which(removed_cons == -9999, arr.ind=TRUE)
sp_to_be_removed_cons <- unique(rownames(indices_cons))
sp_to_be_removed_cons

indices_cont <- which(removed_cont == -9999, arr.ind=TRUE)
sp_to_be_removed_cont <- unique(rownames(indices_cont))
sp_to_be_removed_cont

### How many outlier values are there

sum(removed_cons == -9999, na.rm = T)

sum(removed_cont == -9999, na.rm = T)


#### Re-run the index calculation (print datapoint files) with the dataset cleared from outliers

bench_cons_nooutliers  = lpi_bench_cons %>% 
  filter(!Binomial %in% sp_to_be_removed_cons)

create_infile(bench_cons_nooutliers, name = "bench_cons_nooutliers", start_col_name = 'X1970', end_col_name = 'X2016')
bench_cons_index_nooutliers <- LPIMain("bench_cons_nooutliers_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2016)

bench_cont_nooutliers  = lpi_bench_cont %>% 
  filter(!Binomial %in% sp_to_be_removed_cont)

create_infile(bench_cont_nooutliers, name = "bench_cont_nooutliers", start_col_name = 'X1970', end_col_name = 'X2016')
bench_cont_index_nooutliers <- LPIMain("bench_cont_nooutliers_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2016)

bench_cons1percent_plot <- ggplot_lpi(bench_cons_index_nooutliers, ylim=c(0.9, 4.1))
bench_cont1percent_plot <- ggplot_lpi(bench_cont_index_nooutliers, ylim = c(0.6, 2.6))

(bench_cons5_plot + labs(subtitle = 'Time series > 5 year') + bench_cont5_plot + labs(subtitle = 'Time series > 5 year'))/ 
  (bench_cons10_plot + labs(subtitle = 'Time series > 10 year') + bench_cont10_plot + labs(subtitle = 'Time series > 10 year'))/ 
  (bench_cons1percent_plot + labs(subtitle = 'Upper and lower quantile removed') + bench_cont1percent_plot + labs(subtitle = 'Upper and lower quantile removed'))

# Using the sens_plot function

scenario2_quant <- sens(bench_cons_index_nooutliers, bench_cont_index_nooutliers, scenario = '', sensitivity = 'Excluding outliers')

# Plot with <5 year excluded

(scen2_quant<-plot_sens(scenario2_quant, "", "Excluding outliers"))

# plot sensitivity together

scen2_5+scen2_10+scen2_quant

## Scenario 3 (stringent) ----
# select ts > 5 years

lpi_string_cons5 <- lpi_string_cons %>% 
  filter(treatment == 1 & ts_length > 5)

lpi_string_cont5 <- lpi_string_cont %>% 
  filter(treatment == 0 & ts_length > 5)

# create infiles

create_infile(lpi_string_cons5, name = "lpi_string_cons5",  start_col_name = 'X1970', end_col_name = 'X2016')

string_cons5<-LPIMain(infile = "lpi_string_cons5_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

create_infile(lpi_string_cont5, name = "lpi_string_cont5",  start_col_name = 'X1970', end_col_name = 'X2016')

string_cont5<-LPIMain(infile = "lpi_string_cont5_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

ggplot_lpi(string_cons5, title = "Conservation", ylim=c(0.9, 4.7))+ggplot_lpi(string_cont5, title ="Without conservation")

string_cons5_plot <- ggplot_lpi(string_cons5, title = "string LPI - Conservation", ylim = c(0.9, 4.1))
string_cont5_plot <- ggplot_lpi(string_cont5,title ="Without conservation", ylim = c(0.6, 2.1))
string_cons5_plot + string_cont5_plot

# Using the sens_plot function

scenario3_5 <- sens(string_cons5, string_cont5, scenario = 'Scenario 3', sensitivity = '>5')

# Plot with <5 year excluded

(scen3_5<-plot_sens(scenario3_5, "Scenario 3", "Excluding time series < 5"))


# As above but restricted to ts > 10 

lpi_string_cons10 <- lpi_string_cons %>% 
  filter(treatment == 1 & ts_length > 10)

lpi_string_cont10 <- lpi_string_cont %>% 
  filter(treatment == 0 & ts_length > 10)
# create infiles

create_infile(lpi_string_cons10, name = "lpi_string_cons10",  start_col_name = 'X1970', end_col_name = 'X2016')

string_cons10<-LPIMain(infile = "lpi_string_cons10_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

create_infile(lpi_string_cont10, name = "lpi_string_cont10",  start_col_name = 'X1970', end_col_name = 'X2016')

string_cont10<-LPIMain(infile = "lpi_string_cont10_infile.txt", VERBOSE = FALSE, REF_YEAR = 1970)

ggplot_lpi(string_cons10, title = "Conservation", ylim=c(0.9, 2.5))+ggplot_lpi(string_cont5, title ="Without conservation")

string_cons10_plot <- ggplot_lpi(string_cons10, ylim=c(0.9, 4.1))
string_cont10_plot <- ggplot_lpi(string_cont10, ylim = c(0.6, 2.6))
string_cons10_plot + string_cont10_plot


scenario3_10 <- sens(string_cons10, string_cont10, scenario = 'Scenario 3', sensitivity = '>10')

# Plot 

(scen3_10<-plot_sens(scenario3_10, "", "Excluding time series < 10"))


# lastly, excluding 1% quantile to test the sensitivity to the extremes

### Load lambda file 
lambda_cons <- read.csv("lpi_string_cons_pops_Lambda.txt")
lambda_cont <- read.csv("lpi_string_cont_pops_Lambda.txt")



### Remove column with species names
lambda2_cons <- lambda_cons[,-1]

lambda2_cont <- lambda_cont[,-1]


### Calculate the 95th or 99th quartile of the data set
q95_cons <- quantile(as.matrix(lambda2_cons), c(0.05, 0.95), na.rm=TRUE)
q99_cons <- quantile(as.matrix(lambda2_cons), c(0.01, 0.99), na.rm=TRUE)
hist(as.numeric(as.vector(as.matrix(lambda2_cons))), breaks=10000)
abline(v = q95_cons[1], col="red")
abline(v = q95_cons[2], col="red")
abline(v = q99_cons[1], col="pink")
abline(v = q99_cons[2], col="pink")

### Calculate the 95th or 99th quartile of the data set
q95_cont <- quantile(as.matrix(lambda2_cont), c(0.05, 0.95), na.rm=TRUE)
q99_cont <- quantile(as.matrix(lambda2_cont), c(0.01, 0.99), na.rm=TRUE)
hist(as.numeric(as.vector(as.matrix(lambda2_cont))), breaks=10000)
abline(v = q95_cont[1], col="red")
abline(v = q95_cont[2], col="red")
abline(v = q99_cont[1], col="pink")
abline(v = q99_cont[2], col="pink")

lower_cons <- q99_cons[1]
higher_cons <- q99_cons[2]

removed_cons <- as.matrix(lambda2_cons)


lower_cont <- q99_cont[1]
higher_cont <- q99_cont[2]

removed_cont <- as.matrix(lambda2_cont)

### Mark outliers with -9999
removed_cons[removed_cons < lower_cons | removed_cons > higher_cons] = -9999
range(removed_cons, na.rm =T)

removed_cont[removed_cont < lower_cont | removed_cont > higher_cont] = -9999
range(removed_cont, na.rm =T)

### Replaces the numbers with the species names as rownames
rownames(removed_cons) <- lambda_cons[, 1]

rownames(removed_cont) <- lambda_cont[, 1]

### Find species with outliers
indices_cons <- which(removed_cons == -9999, arr.ind=TRUE)
sp_to_be_removed_cons <- unique(rownames(indices_cons))
sp_to_be_removed_cons

indices_cont <- which(removed_cont == -9999, arr.ind=TRUE)
sp_to_be_removed_cont <- unique(rownames(indices_cont))
sp_to_be_removed_cont

### How many outlier values are there
sum(removed_cons == -9999, na.rm = T)

sum(removed_cont == -9999, na.rm = T)


#### Re-run the index calculation (print datapoint files) with the dataset cleared from outliers
string_cons_nooutliers  = lpi_string_cons %>% 
  filter(!Binomial %in% sp_to_be_removed_cons)

create_infile(string_cons_nooutliers, name = "string_cons_nooutliers", start_col_name = 'X1970', end_col_name = 'X2016')
string_cons_index_nooutliers <- LPIMain("string_cons_nooutliers_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2016)

string_cont_nooutliers  = lpi_string_cont %>% 
  filter(!Binomial %in% sp_to_be_removed_cont)

create_infile(string_cont_nooutliers, name = "string_cont_nooutliers", start_col_name = 'X1970', end_col_name = 'X2016')
string_cont_index_nooutliers <- LPIMain("string_cont_nooutliers_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2016)

string_cons1percent_plot <- ggplot_lpi(string_cons_index_nooutliers, ylim=c(0.9, 4.1))
string_cont1percent_plot <- ggplot_lpi(string_cont_index_nooutliers, ylim = c(0.6, 2.6))

# Using the sens_plot function

scenario3_quant <- sens(string_cons_index_nooutliers, string_cont_index_nooutliers, scenario = '', sensitivity = 'Excluding outliers')

# Plot with <5 year excluded

(scen3_quant<-plot_sens(scenario3_quant, "", "Excluding outliers"))

# plot sensitivity together

scen3_5+scen3_10+scen3_quant


(string_cons5_plot + labs(subtitle = 'Time series > 5 year') + string_cont5_plot + labs(subtitle = 'Time series > 5 year'))/ 
  (string_cons10_plot + labs(subtitle = 'Time series > 10 year') + string_cont10_plot + labs(subtitle = 'Time series > 10 year'))/ 
  (string_cons1percent_plot + labs(subtitle = 'Upper and lower quantile removed') + string_cont1percent_plot + labs(subtitle = 'Upper and lower quantile removed'))


# Regression tests using Mcrae 2020 and Wauchope 2020 approach ----
# First, using Louise's approach where lambda values are summarized. Update - run infile and LPIMain for lpi_work instead of binding rows from individual
# conservation and control sample. Npops drops for whatever reason from 14448 pops in lpi_work8 to 14433 pops in row combined conservation + control sample      

lambda_test <- read.csv("lpi_full_cons_pops_PopLambda.txt") %>% 
  rename(ID = 1)


lambda_test1 <- read.csv("lpi_full_cont_pops_PopLambda.txt") %>% 
  rename(ID = 1)

lambda_full_lpi <- bind_rows(lambda_test, lambda_test1) # 14433 pops - 15 pops are removed using this method. Ask Louise why

# Add population data to the lambda values

lpi_info <- lpi_work8 %>% 
  select(everything(), - c(X1970:X2018)) %>% 
  select(everything(), -c('1950':'2019')) 

# Add location data to the info. Removed it earlier as it was duplicated during a sloppy join
loc_data <- lpi_all %>% 
  select(ID, Location)

lpi_info <- left_join(lpi_info, loc_data, by = "ID")

lambda_full <- left_join(lambda_full_lpi, lpi_info, by = "ID")

# Pivot lambda values to long format

lambda_full_long <- lambda_full %>% 
  pivot_longer(X1970:X2016,
               names_to = "year",
               values_to = "lambda") 

# Remove - 1 as these are NAs and index values of 1 starting in the index year

lambda_df <- lambda_full_long %>% 
  filter(lambda != -1 )%>% 
  filter(year != "X1970" & lambda != 1)

# Summarise lambdas and join to lambda_df

lambda_sums <- lambda_df %>% 
  group_by(ID) %>% 
  summarise(lambda_sum = sum(lambda)) %>% 
  ungroup()

lambda_df <- left_join(lambda_df, lambda_sums, by = "ID")

# Reduce so that each pop is presented by a single row

lambda_df_sum <- lambda_df %>% 
  select(everything(), -c(lambda, year)) %>% 
  distinct()

# Test in INLA as I don't remember the syntax for the other packages. Use default priors. Also, random effects might be a bit different here than 
# in Louise's paper so remember to check that (She is probably nesting species in class)

Louise_region <- inla(lambda_sum ~ 0 + ts_length + treatment +
                        f(Class, model = "iid") +
                        f(Region, model = "iid"),
                      family = "gaussian",
                      data = lambda_df_sum)

summary(Louise_region)

# Try similar specification using lme4 but random structure more similar to Louise's paper

library(lme4)

# Targeted vs not targeted

mixed.lmer <- lmer(lambda_sum ~ 0 + ts_length + Utilised * treatment + Class + (1|Family/Binomial) + (1|Location), data = lambda_df_sum)

summary(mixed.lmer)

# Level 2 conservation categories

mixed.lmer1 <- lmer(lambda_sum ~ 0 + Utilised + ts_length + land_water_protection + land_water_management + 
                      species_management + education_awareness + law_policy + incentives + external_capacity + research +  
                      Class + (1|Family/Binomial) + (1|Country), data = lambda_df_sum)


summary(mixed.lmer1)

# Using location instead of country

mixed.lmer2 <- lmer(lambda_sum ~ 0 + Utilised + ts_length + land_water_protection + land_water_management + 
                      species_management + education_awareness + law_policy + incentives + external_capacity + research +  
                      Class + (1|Family/Binomial) + (1|Location), data = lambda_df_sum)

summary(mixed.lmer2)

# Check model performance

library(performance)
library(see) # Might just have fucked up all my packages while trying to load the see package. Restart and check - Update, Fixed it. Crisis averted

compare_performance(mixed.lmer1, mixed.lmer2) # location is the better option

model_performance(mixed.lmer2)

check_model(mixed.lmer2) # Looks okay but not perfect
check_singularity(mixed.lmer2)

# Wauchope's method. Slightly tweaked as we do not have BA but more like CI instead.

# Remove X in the year variable and make sure it's numeric

lambda_df <- lambda_df %>% 
  mutate(year = str_remove(year, "X"))

lambda_df$year <- as.numeric(lambda_df$year)

# Specify model

mixed.ci <- lmer(lambda ~ 0 + year + treatment + Class + (1|ID) + (1|Country), data = lambda_df)

summary(mixed.ci)


## Plotting
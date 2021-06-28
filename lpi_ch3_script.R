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

lpi <- read_excel("C:/Users/seanj/OneDrive - University College London/Articles from Thesis/3. Assessing the effect of global conservation/Data/LPD_output_20201116.xlsx")

cons<- read_excel("C:/Users/seanj/OneDrive - University College London/Articles from Thesis/3. Assessing the effect of global conservation/Data/Conservation_LPD_worksheet25052021.xlsx")

# Check length of the different management types nand validate whether these can be recategorised into Salafsky (2008) categories

lpi$Management_type <- factor(lpi$Management_type)

list<-levels(lpi$Management_type)

list_man <- lpi %>% 
  group_by(Management_type) %>% 
  summarise(number_of_pops = n()) # Looks surprisingly manageable - 1025 different management types
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

# Remove populations without management info
 # Problems connecting

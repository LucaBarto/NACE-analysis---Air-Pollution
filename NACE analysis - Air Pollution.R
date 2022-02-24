if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(eurostat)) install.packages("eurostat", repos = "http://cran.us.r-project.org")
if(!require(fpp2)) install.packages("fpp2", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(xts)) install.packages("xts", repos = "http://cran.us.r-project.org")
if(!require(seastests)) install.packages("seastests", repos = "http://cran.us.r-project.org")
if(!require(tsbox)) install.packages("tsbox", repos = "http://cran.us.r-project.org")

library(dplyr)     #For data wrangling
library(lubridate) #For handling dates
library(ggplot2)   #For plotting of results
library(eurostat) # package used for querrying EUROSTAT
library(fpp2) # package used for forecasting
library(plotly) #interactive plotting
library(xts) # To handle time-based data
library(seastests) #Test for time series seasonality
library(tsbox) #convert time series stored as ts, xts, data.frame, data.table, tibble, zoo, tsibble, tibbletime or timeSeries to each other


# clear old variables from memory
rm(list=ls())


#Air emissions accounts by NACE Rev. 2 activity
dt_air_emission_original <- get_eurostat("env_ac_ainah_r2", stringsAsFactors = FALSE)

dt_air_emission <- dt_air_emission_original %>% filter(geo == 'IT') %>%
  filter(unit == 'T')

dt_air_emission <- dt_air_emission %>% mutate(year = year(time))

dt_air_emission <- dt_air_emission %>% select(airpol, nace_r2, year, values)

dt_air_emission <- dt_air_emission %>% group_by(year, nace_r2, airpol) %>% 
  mutate(air_emission_values = sum(values)) %>%
  ungroup()

dt_air_emission <- dt_air_emission %>% 
  select(year, nace_r2, airpol, air_emission_values) %>%
  distinct()


#Analyze air pollution for macro sector
air_pollution_nace_sector <- function(ds, nace_sector) {
  dt <- ds %>% filter(nace_r2 == nace_sector) %>% 
    group_by(year) %>% 
    mutate(value = sum(air_emission_values)) %>%
    ungroup()
  
  dt <- dt %>% select(year, value)
  
  p <- dt %>%
    ggplot( aes(year, value)) +
    geom_smooth(formula = y ~ x, method = "lm") +
    geom_line() +
    theme_bw()
  
  ggplotly(p)
}

##############################################################
##############Analysis all macro sectors######################
##############################################################

#Agriculture, forestry and fishing
air_pollution_nace_sector(dt_air_emission, 'A')

#Mining and quarrying
air_pollution_nace_sector(dt_air_emission, 'B')

#Manufacturing
air_pollution_nace_sector(dt_air_emission, 'C')

#Electricity, gas, steam and air conditioning supply
air_pollution_nace_sector(dt_air_emission, 'D')

#Water supply; sewerage, waste management and remediation activities
air_pollution_nace_sector(dt_air_emission, 'E')

#Construction
air_pollution_nace_sector(dt_air_emission, 'F')

#Wholesale and retail trade; repair of motor vehicles and motorcycles
air_pollution_nace_sector(dt_air_emission, 'G')

#Transportation and storage
air_pollution_nace_sector(dt_air_emission, 'H')

#Accommodation and food service activities
air_pollution_nace_sector(dt_air_emission, 'I')

#Information and communication
air_pollution_nace_sector(dt_air_emission, 'J')

#Financial and insurance activities
air_pollution_nace_sector(dt_air_emission, 'K')

#Real estate activities
air_pollution_nace_sector(dt_air_emission, 'L')

#Professional, scientific and technical activities
air_pollution_nace_sector(dt_air_emission, 'M')

#Administrative and support service activities
air_pollution_nace_sector(dt_air_emission, 'N')

#Public administration and defence; compulsory social security
air_pollution_nace_sector(dt_air_emission, 'O')

#Education
air_pollution_nace_sector(dt_air_emission, 'P')

#Human health and social work activities
air_pollution_nace_sector(dt_air_emission, 'Q')

#Arts, entertainment and recreation
air_pollution_nace_sector(dt_air_emission, 'R')

#Other service activities
air_pollution_nace_sector(dt_air_emission, 'S')

#Activities of households as employers; undifferentiated goods- and services-producing activities of households for own use
air_pollution_nace_sector(dt_air_emission, 'T')

#Activities of extraterritorial organisations and bodies
air_pollution_nace_sector(dt_air_emission, 'U')

#All NACE activities plus households
air_pollution_nace_sector(dt_air_emission, 'TOTAL_HH')


##############################################################
###########Analysis on sectors with increasing trend##########
##############################################################

#Europe data
dt_air_emission_eu <- dt_air_emission_original %>% 
  filter(geo == 'EU28') %>% 
  filter(unit == 'T')

dt_air_emission_eu <- dt_air_emission_eu %>% mutate(year = year(time))

dt_air_emission_eu <- dt_air_emission_eu %>% select(airpol, nace_r2, year, values)

dt_air_emission_eu <- dt_air_emission_eu %>% group_by(year, nace_r2, airpol) %>% 
  mutate(air_emission_values = sum(values)) %>%
  ungroup()

dt_air_emission_eu <- dt_air_emission_eu %>% 
  select(year, nace_r2, airpol, air_emission_values) %>%
  distinct()


#Detail analysis
air_pollution_detail_nace_sector <- function(ds, nace_sector) {
  dt <- ds %>% filter(nace_r2 == nace_sector)
  
  colnames(dt)<- c("Year","Sector", "Component", "Value")
  
  dt <- dt %>% select(Year, Component, Value)
  
  p <- dt %>%
    ggplot( aes( Year, Value, color=Component)) +
    geom_line() +
    theme_bw()
  
  ggplotly(p)
}

air_pollution_component_detail <- function(ds, component, nace_sector) {
  dt <- ds %>% filter(nace_r2 == nace_sector)
  
  colnames(dt)<- c("Year","Sector", "Component", "Value")
  
  dt <- dt %>% select(Year, Component, Value)
  
  dt <- dt %>% filter(Component == component)
  
  p <- dt %>%
    ggplot( aes( Year, Value)) +
    geom_smooth(formula = y ~ x, method = "lm") +
    geom_line() +
    theme_bw()
  
  ggplotly(p)
}

###############################################################################################################################
##Activities of households as employers; undifferentiated goods- and services-producing activities of households for own use###
###############################################################################################################################


#Activities of households as employers; undifferentiated goods- and services-producing activities of households for own use
air_pollution_detail_nace_sector(dt_air_emission, 'T')

air_pollution_component_detail(dt_air_emission, 'HFC_CO2E', 'T')

#Compare with Europe
air_pollution_detail_nace_sector(dt_air_emission_eu, 'T')
air_pollution_component_detail(dt_air_emission_eu, 'HFC_CO2E', 'T')


############################################################################
#############################Human health activities########################
############################################################################

#Human health activities
air_pollution_nace_sector(dt_air_emission, 'Q86')

#Residential care activities and social work activities without accommodation
air_pollution_nace_sector(dt_air_emission, 'Q87_Q88')

#Human health activities details
air_pollution_detail_nace_sector(dt_air_emission, 'Q86')
air_pollution_component_detail(dt_air_emission, 'GHG', 'Q86')
air_pollution_component_detail(dt_air_emission, 'CO2', 'Q86')

#Compare with Europe
air_pollution_detail_nace_sector(dt_air_emission_eu, 'Q86')
air_pollution_component_detail(dt_air_emission_eu, 'GHG', 'Q86')
air_pollution_component_detail(dt_air_emission_eu, 'CO2', 'Q86')


############################################################################
#########################Mining and quarrying###############################
############################################################################

#Mining and quarrying details
air_pollution_detail_nace_sector(dt_air_emission, 'B')
air_pollution_component_detail(dt_air_emission, 'GHG', 'B')
air_pollution_component_detail(dt_air_emission, 'CO2', 'B')

#Compare with Europe
air_pollution_detail_nace_sector(dt_air_emission_eu, 'B')
air_pollution_component_detail(dt_air_emission_eu, 'GHG', 'B')
air_pollution_component_detail(dt_air_emission_eu, 'CO2', 'B')


#######################################################################################
####Wholesale and retail trade; repair of motor vehicles and motorcycles details#######
#######################################################################################

#Wholesale and retail trade; repair of motor vehicles and motorcycles details
air_pollution_detail_nace_sector(dt_air_emission, 'G')
air_pollution_component_detail(dt_air_emission, 'HFC_CO2E', 'G')

#Compare with Europe
air_pollution_detail_nace_sector(dt_air_emission_eu, 'G')
air_pollution_component_detail(dt_air_emission_eu, 'GHG', 'G')
air_pollution_component_detail(dt_air_emission_eu, 'HFC_CO2E', 'G')

#Wholesale and retail trade and repair of motor vehicles and motorcycles details
air_pollution_detail_nace_sector(dt_air_emission, 'G45')
air_pollution_component_detail(dt_air_emission, 'GHG', 'G45')
air_pollution_component_detail(dt_air_emission, 'CO2', 'G45')

#Compare with Europe
air_pollution_detail_nace_sector(dt_air_emission_eu, 'G45')
air_pollution_component_detail(dt_air_emission_eu, 'GHG', 'G45')
air_pollution_component_detail(dt_air_emission_eu, 'CO2', 'G45')

#Wholesale trade, except of motor vehicles and motorcycles details
air_pollution_detail_nace_sector(dt_air_emission, 'G46')
air_pollution_component_detail(dt_air_emission, 'GHG', 'G46')
air_pollution_component_detail(dt_air_emission, 'HFC_CO2E', 'G46')

#Compare with Europe
air_pollution_detail_nace_sector(dt_air_emission_eu, 'G46')
air_pollution_component_detail(dt_air_emission_eu, 'GHG', 'G46')
air_pollution_component_detail(dt_air_emission_eu, 'CO2', 'G46')

#Retail trade, except of motor vehicles and motorcycles details
air_pollution_detail_nace_sector(dt_air_emission, 'G47')
air_pollution_component_detail(dt_air_emission, 'GHG', 'G47')
air_pollution_component_detail(dt_air_emission, 'CO2', 'G47')

#Compare with Europe
air_pollution_detail_nace_sector(dt_air_emission_eu, 'G47')
air_pollution_component_detail(dt_air_emission_eu, 'GHG', 'G47')
air_pollution_component_detail(dt_air_emission_eu, 'CO2', 'G47')


#########################################################################################################################
#################Causes of death - standardised death rate by NUTS 2 region of residence#################################
#########################################################################################################################
#http://appsso.eurostat.ec.europa.eu/nui/show.do?query=BOOKMARK_DS-417853_QID_-593EE5C5_UID_-3F171EB0&layout=SEX,L,X,0;GEO,L,Y,0;UNIT,L,Z,0;TIME,C,Z,1;AGE,L,Z,2;ICD10,L,Z,3;INDICATORS,C,Z,4;&zSelection=DS-417853AGE,TOTAL;DS-417853ICD10,A-R_V-Y;DS-417853UNIT,RT;DS-417853INDICATORS,OBS_FLAG;DS-417853TIME,2011;&rankName1=ICD10_1_2_-1_2&rankName2=TIME_1_0_-1_2&rankName3=UNIT_1_2_-1_2&rankName4=AGE_1_2_-1_2&rankName5=INDICATORS_1_2_-1_2&rankName6=SEX_1_2_0_0&rankName7=GEO_1_2_0_1&rStp=&cStp=&rDCh=&cDCh=&rDM=true&cDM=true&footnes=false&empty=false&wai=false&time_mode=ROLLING&time_most_recent=true&lang=EN&cfo=%23%23%23%2C%23%23%23.%23%23%23

dt_death_pneumonia_original <- get_eurostat("hlth_cd_asdr2", stringsAsFactors = FALSE)

head(dt_death_pneumonia_original)

dt_death_pneumonia_original %>% filter(geo == 'IT')

dt_death_pneumonia_it <- dt_death_pneumonia_original %>% filter(geo == 'IT')

dt_death_pneumonia_it <- dt_death_pneumonia_it %>% 
  filter(icd10 %in% c('J12-J18', 'J40-J47', 'J45_J46', 'J40-J44_J47', 'J_OTH'))

dt_death_pneumonia_it <- dt_death_pneumonia_it %>% mutate(year = year(time))

dt_death_pneumonia_it <- dt_death_pneumonia_it %>% select(sex, age, icd10, year, values)

sum(is.na(dt_death_pneumonia_it$values))

################################################################
######################Analyze by sex############################
################################################################

dt <- dt_death_pneumonia_it %>% filter(sex == 'F')

dt <- dt %>% select(year, values)

dt <- dt %>% group_by(year) %>% 
  mutate(values = sum(values)) %>%
  ungroup()

dt <- dt %>% select(year, values) %>% distinct()

p <- dt %>%
  ggplot( aes(year, values)) +
  geom_smooth(formula = y ~ x, method = "lm") +
  geom_line() +
  theme_bw()

ggplotly(p)

dt <- dt_death_pneumonia_it %>% filter(sex == 'M')

dt <- dt %>% select(year, values)

dt <- dt %>% group_by(year) %>% 
  mutate(values = sum(values)) %>%
  ungroup()

dt <- dt %>% select(year, values) %>% distinct()

p <- dt %>%
  ggplot( aes(year, values)) +
  geom_smooth(formula = y ~ x, method = "lm") +
  geom_line() +
  theme_bw()

ggplotly(p)

####Trend for women is increasing############
####Let's check for age
dt <- dt_death_pneumonia_it %>% filter(sex == 'F') %>% filter(age == 'Y_LT65')

dt <- dt %>% select(year, values)

dt <- dt %>% group_by(year) %>% 
  mutate(values = sum(values)) %>%
  ungroup()

dt <- dt %>% select(year, values) %>% distinct()

p <- dt %>%
  ggplot( aes(year, values)) +
  geom_smooth(formula = y ~ x, method = "lm") +
  geom_line() +
  theme_bw()

ggplotly(p)

dt <- dt_death_pneumonia_it %>% filter(sex == 'F') %>% filter(age == 'Y_GE65')

dt <- dt %>% select(year, values)

dt <- dt %>% group_by(year) %>% 
  mutate(values = sum(values)) %>%
  ungroup()

dt <- dt %>% select(year, values) %>% distinct()

p <- dt %>%
  ggplot( aes(year, values)) +
  geom_smooth(formula = y ~ x, method = "lm") +
  geom_line() +
  theme_bw()

ggplotly(p)

###For over 65 trend is increasing########
###Let's go deeper on type of pneumia#####
dt_women_over65 <- dt_death_pneumonia_it %>% 
  filter(sex == 'F') %>% 
  filter(age == 'Y_GE65')


####Pneumonia
dt <- dt_women_over65 %>% filter(icd10 == 'J12-J18')

dt <- dt %>% select(year, values)

dt <- dt %>% group_by(year) %>% 
  mutate(values = sum(values)) %>%
  ungroup()

dt <- dt %>% select(year, values) %>% distinct()

p <- dt %>%
  ggplot( aes(year, values)) +
  geom_smooth(formula = y ~ x, method = "lm") +
  geom_line() +
  theme_bw()

ggplotly(p)


####Chronic lower respiratory diseases
dt <- dt_women_over65 %>% filter(icd10 == 'J40-J47')

dt <- dt %>% select(year, values)

dt <- dt %>% group_by(year) %>% 
  mutate(values = sum(values)) %>%
  ungroup()

dt <- dt %>% select(year, values) %>% distinct()

p <- dt %>%
  ggplot( aes(year, values)) +
  geom_smooth(formula = y ~ x, method = "lm") +
  geom_line() +
  theme_bw()

ggplotly(p)

####Asthma and status asthmaticus
dt <- dt_women_over65 %>% filter(icd10 == 'J45_J46')

dt <- dt %>% select(year, values)

dt <- dt %>% group_by(year) %>% 
  mutate(values = sum(values)) %>%
  ungroup()

dt <- dt %>% select(year, values) %>% distinct()

p <- dt %>%
  ggplot( aes(year, values)) +
  geom_smooth(formula = y ~ x, method = "lm") +
  geom_line() +
  theme_bw()

ggplotly(p)


####Other lower respiratory diseases
dt <- dt_women_over65 %>% filter(icd10 == 'J40-J44_J47')

dt <- dt %>% select(year, values)

dt <- dt %>% group_by(year) %>% 
  mutate(values = sum(values)) %>%
  ungroup()

dt <- dt %>% select(year, values) %>% distinct()

p <- dt %>%
  ggplot( aes(year, values)) +
  geom_smooth(formula = y ~ x, method = "lm") +
  geom_line() +
  theme_bw()

ggplotly(p)


####Other diseases of the respiratory system (remainder of J00-J99)
dt <- dt_women_over65 %>% filter(icd10 == 'J_OTH')

dt <- dt %>% select(year, values)

dt <- dt %>% group_by(year) %>% 
  mutate(values = sum(values)) %>%
  ungroup()

dt <- dt %>% select(year, values) %>% distinct()

p <- dt %>%
  ggplot( aes(year, values)) +
  geom_smooth(formula = y ~ x, method = "lm") +
  geom_line() +
  theme_bw()

ggplotly(p)

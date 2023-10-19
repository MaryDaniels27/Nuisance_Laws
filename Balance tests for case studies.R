# Balance tests for case studies in nuisance law project #
# Mary Daniels
# 10/18/2023
# Locations studied: Charlotte and Raleigh, NC; Toledo, OH and Louisville, KY
# Unit of observation: census tract
# Data: ACS 2014 5-year estimates, ACS 2011 5-year estimates, HUD crosswalk files
# Toledo was treated in 2015 and Charlotte was treated in 2012

library(tidyverse)
library(readxl)
library(dplyr)

# HUD crosswalk files ####
# use 2012 for NC and 2015 for Toledo-Louisville
# The TRACT-ZIP files map census tracts to zip codes. To match those to cities, I use the Q1 2021
# ZIP-COUNTY file which includes the USPS preferred city (not included in older crosswalk files)
TRACT_ZIP_12 <- read_excel("C:\\Users\\maryd\\OneDrive\\UW Madison\\Research\\Nuisance Ordinances\\Data\\HUD crosswalk files\\TRACT_ZIP_032012.xlsx")
TRACT_ZIP_15 <- read_excel("C:\\Users\\maryd\\OneDrive\\UW Madison\\Research\\Nuisance Ordinances\\Data\\HUD crosswalk files\\TRACT_ZIP_032015.xlsx")
ZIP_COUNTY_21 <- read_excel("C:\\Users\\maryd\\OneDrive\\UW Madison\\Research\\Nuisance Ordinances\\Data\\HUD crosswalk files\\ZIP_COUNTY_032021.xlsx")

# ACS Data ####
# Note: the ACS did not have poverty tables for 2011 or 2010, so I use 2012 tables
poverty12 <- read_excel("C:\\Users\\maryd\\OneDrive\\UW Madison\\Research\\Nuisance Ordinances\\Data\\ACS\\NC-Toledo-Louisville\\Poverty12.xlsx")
poverty14 <- read_excel("C:\\Users\\maryd\\OneDrive\\UW Madison\\Research\\Nuisance Ordinances\\Data\\ACS\\NC-Toledo-Louisville\\Poverty14.xlsx")
housing11 <- read_excel("C:\\Users\\maryd\\OneDrive\\UW Madison\\Research\\Nuisance Ordinances\\Data\\ACS\\NC-Toledo-Louisville\\Housing11.xlsx")
housing14 <- read_excel("C:\\Users\\maryd\\OneDrive\\UW Madison\\Research\\Nuisance Ordinances\\Data\\ACS\\NC-Toledo-Louisville\\Housing14.xlsx")
households11 <- read_excel("C:\\Users\\maryd\\OneDrive\\UW Madison\\Research\\Nuisance Ordinances\\Data\\ACS\\NC-Toledo-Louisville\\Households11.xlsx")
households14 <- read_excel("C:\\Users\\maryd\\OneDrive\\UW Madison\\Research\\Nuisance Ordinances\\Data\\ACS\\NC-Toledo-Louisville\\Households14.xlsx")
income11 <- read_excel("C:\\Users\\maryd\\OneDrive\\UW Madison\\Research\\Nuisance Ordinances\\Data\\ACS\\NC-Toledo-Louisville\\Income11.xlsx")
income14 <- read_excel("C:\\Users\\maryd\\OneDrive\\UW Madison\\Research\\Nuisance Ordinances\\Data\\ACS\\NC-Toledo-Louisville\\Income14.xlsx")
rent11 <- read_excel("C:\\Users\\maryd\\OneDrive\\UW Madison\\Research\\Nuisance Ordinances\\Data\\ACS\\NC-Toledo-Louisville\\Rent11.xlsx")
rent14 <- read_excel("C:\\Users\\maryd\\OneDrive\\UW Madison\\Research\\Nuisance Ordinances\\Data\\ACS\\NC-Toledo-Louisville\\Rent14.xlsx")
demog11 <- read_excel("C:\\Users\\maryd\\OneDrive\\UW Madison\\Research\\Nuisance Ordinances\\Data\\ACS\\NC-Toledo-Louisville\\Demog11.xlsx")
demog14 <- read_excel("C:\\Users\\maryd\\OneDrive\\UW Madison\\Research\\Nuisance Ordinances\\Data\\ACS\\NC-Toledo-Louisville\\Demog14.xlsx")
employment11 <- read_excel("C:\\Users\\maryd\\OneDrive\\UW Madison\\Research\\Nuisance Ordinances\\Data\\ACS\\NC-Toledo-Louisville\\employment11.xlsx")
employment14 <- read_excel("C:\\Users\\maryd\\OneDrive\\UW Madison\\Research\\Nuisance Ordinances\\Data\\ACS\\NC-Toledo-Louisville\\employment14.xlsx")



# converting the necessary variables from characters to numeric. Note there are NAs because some census tracts
# do not have renters
demog11$BLACK <- as.numeric(demog11$BLACK)
demog11$ASIAN <- as.numeric(demog11$ASIAN)
demog11$HISPANIC <- as.numeric(demog11$HISPANIC)
demog11$WHITE <- as.numeric(demog11$WHITE)
demog11$YOUNG <- as.numeric(demog11$YOUNG)
demog11$HSDROP <- as.numeric(demog11$HSDROP)
demog11$HS <- as.numeric(demog11$HS)
demog11$BACH <- as.numeric(demog11$BACH)

demog14$BLACK <- as.numeric(demog14$BLACK)
demog14$ASIAN <- as.numeric(demog14$ASIAN)
demog14$HISPANIC <- as.numeric(demog14$HISPANIC)
demog14$WHITE <- as.numeric(demog14$WHITE)
demog14$YOUNG <- as.numeric(demog14$YOUNG)
demog14$HSDROP <- as.numeric(demog14$HSDROP)
demog14$HS <- as.numeric(demog14$HS)
demog14$BACH <- as.numeric(demog14$BACH)


# there are some census tracts where the median rent is 2,000+. For purpose of this analysis, I will recode these
# observations as 2000
rent11$RENT[rent11$RENT == "2,000+"] = 2000
rent14$RENT[rent14$RENT == "2,000+"] = 2000
rent11$RENT <- as.numeric(rent11$RENT)
rent14$RENT <- as.numeric(rent14$RENT)

income11$RMEDINC <- as.numeric(income11$RMEDINC)
income14$RMEDINC <- as.numeric(income14$RMEDINC)

households11$FEMALE <- as.numeric(households11$FEMALE)
households14$FEMALE <- as.numeric(households14$FEMALE)

housing11$VACANCY <- as.numeric(housing11$VACANCY)
housing11$GRBURDEN <- as.numeric(housing11$GRBURDEN)
housing14$VACANCY <- as.numeric(housing14$VACANCY)
housing14$GRBURDEN <- as.numeric(housing14$GRBURDEN)

poverty12$POVERTY <- as.numeric(poverty12$POVERTY)
poverty14$POVERTY <- as.numeric(poverty14$POVERTY)

employment11$UR <- as.numeric(employment11$UR)
employment14$UR <- as.numeric(employment14$UR)

# Combining Data Sets ####

# Columns: State, City, Census Tract, ACS variables

cities <- ZIP_COUNTY_21 %>%
  filter(USPS_ZIP_PREF_STATE %in% c("KY", "OH", "NC")) %>%
  filter(USPS_ZIP_PREF_CITY %in% c("LOUISVILLE", "TOLEDO", "CHARLOTTE", "RALEIGH"))

# deleting a zip code in Louisville, OH
cities <- cities[-which(cities$ZIP == "44641"), ]

# list of census tracts in the Charlotte and Raleigh, NC
NC_tracts <- unique(TRACT_ZIP_12$TRACT[TRACT_ZIP_12$ZIP %in% cities$ZIP[cities$USPS_ZIP_PREF_STATE == "NC"]])

# list of census tracts in Toledo, OH and Louisville, KY
OH_KY_tracts <- unique(TRACT_ZIP_15$TRACT[TRACT_ZIP_15$ZIP %in% cities$ZIP[cities$USPS_ZIP_PREF_STATE %in% c("OH", "KY")]])

# Creating the final data set
data <- as.data.frame(matrix(0, nrow = length(NC_tracts) + length(OH_KY_tracts), ncol = 20))
colnames(data) <- c("State", "City", "Census_Tract", "Year", "RHH", "WHITE", "BLACK", "HISPANIC", 
                    "ASIAN", "YOUNG", "FEMALE", "HSDROP", "HS", "BACH", "RMEDINC", "GRBURDEN",
                    "VACANCY", "RENT", "POVERTY", "UR")  # naming the columns

data$Census_Tract <- c(NC_tracts, OH_KY_tracts) # census tracts
data$Year <- c(rep(2011, length(NC_tracts)), rep(2014, length(OH_KY_tracts))) # year 

for(i in 1:nrow(data)){
  print(i)
  if(data$Year[i] == "2011"){  # fill in NC first
    # using the reported zip code in the HUD files, link each census tract to the state
    zip <- TRACT_ZIP_12$ZIP[TRACT_ZIP_12$TRACT == data$Census_Tract[i]] # the zip code tract i belongs to
    if(length(zip) == 1){
      data$State[i] = ZIP_COUNTY_21$USPS_ZIP_PREF_STATE[ZIP_COUNTY_21$ZIP == zip] # the state the zip code belongs to
      data$City[i] = ZIP_COUNTY_21$USPS_ZIP_PREF_CITY[ZIP_COUNTY_21$ZIP == zip] # the city the zip code belongs to 
    }
    if(length(zip) > 1){ 
      data$State[i] <- unique(ZIP_COUNTY_21$USPS_ZIP_PREF_STATE[ZIP_COUNTY_21$ZIP %in% zip]) # there's only one
      # state the zip codes could belong to, so just report the unique "list" of states
      
      # if the census tract spans multiple zip codes, identify the zip code belong in Charlotte
      # or Raleigh
      city <- ZIP_COUNTY_21$USPS_ZIP_PREF_CITY[ZIP_COUNTY_21$ZIP %in% zip]
      if("CHARLOTTE" %in% city){data$City[i] = "CHARLOTTE"}
      if("RALEIGH" %in% city){data$City[i] = "RALEIGH"}
      # p.s. I checked that there are no census tracts that span both Charlotte and Raleigh
    }
  }
  
  if(data$Year[i] == "2014"){
    zip <- TRACT_ZIP_12$ZIP[TRACT_ZIP_12$TRACT == data$Census_Tract[i]] # the zip code tract i belongs to
    if(length(zip) == 1){
      data$State[i] = ZIP_COUNTY_21$USPS_ZIP_PREF_STATE[ZIP_COUNTY_21$ZIP == zip] # the state the zip code belongs to
      data$City[i] = ZIP_COUNTY_21$USPS_ZIP_PREF_CITY[ZIP_COUNTY_21$ZIP == zip] # the city the zip code belongs to 
    }
    if(length(zip) > 1){ 
      data$State[i] <- unique(ZIP_COUNTY_21$USPS_ZIP_PREF_STATE[ZIP_COUNTY_21$ZIP %in% zip]) # there's only one
      # state the zip codes could belong to, so just report the unique "list" of states
      
      # if the census tract spans multiple zip codes, identify the zip code belong in Toledo or Louisville
      city <- ZIP_COUNTY_21$USPS_ZIP_PREF_CITY[ZIP_COUNTY_21$ZIP %in% zip]
      if("LOUISVILLE" %in% city){data$City[i] = "LOUISVILLE"}
      if("TOLEDO" %in% city){data$City[i] = "TOLEDO"}
  }
  }
}
    
for(i in 1:nrow(data)){
  print(i)    
  if(data$Year[i] == "2011"){  # fill in NC first
    # # filling in the ACS variables
    if(data$Census_Tract[i] %in% demog11$Census_Tract){
      data$RHH[i] = demog11$RHH[demog11$Census_Tract == data$Census_Tract[i]]
      data$WHITE[i] = demog11$WHITE[demog11$Census_Tract == data$Census_Tract[i]]
      data$BLACK[i] = demog11$BLACK[demog11$Census_Tract == data$Census_Tract[i]]
      data$HISPANIC[i] = demog11$HISPANIC[demog11$Census_Tract == data$Census_Tract[i]]
      data$ASIAN[i] = demog11$ASIAN[demog11$Census_Tract == data$Census_Tract[i]]
      data$YOUNG[i] = demog11$YOUNG[demog11$Census_Tract == data$Census_Tract[i]]
      data$HSDROP[i] = demog11$HSDROP[demog11$Census_Tract == data$Census_Tract[i]]
      data$HS[i] = demog11$HS[demog11$Census_Tract == data$Census_Tract[i]]
      data$BACH[i] = demog11$BACH[demog11$Census_Tract == data$Census_Tract[i]]
    }
    if(data$Census_Tract[i] %in% households11$Census_Tract){
      data$FEMALE[i] = households11$FEMALE[households11$Census_Tract == data$Census_Tract[i]]
    }
    
    if(data$Census_Tract[i] %in% income11$Census_Tract){
      data$RMEDINC[i] = income11$RMEDINC[income11$Census_Tract == data$Census_Tract[i]] 
    }
    if(data$Census_Tract[i] %in% housing11$Census_Tract){
      data$GRBURDEN[i] = housing11$GRBURDEN[housing11$Census_Tract == data$Census_Tract[i]]
      data$VACANCY[i] = housing11$VACANCY[housing11$Census_Tract == data$Census_Tract[i]]
    }
    if(data$Census_Tract[i] %in% rent11$Census_Tract){
      data$RENT[i] = rent11$RENT[rent11$Census_Tract == data$Census_Tract[i]]
    }
    if(data$Census_Tract[i] %in% poverty12$Census_Tract){
      data$POVERTY[i] = poverty12$POVERTY[poverty12$Census_Tract == data$Census_Tract[i]] 
    }
    if(data$Census_Tract[i] %in% employment11$Census_Tract){
      data$UR[i] = employment14$UR[employment11$Census_Tract == data$Census_Tract[i]]
    }
  }
  
  
  if(data$Year[i] == "2014"){  # fill in OH and KY
    # # filling in the ACS variables
    if(data$Census_Tract[i] %in% demog14$Census_Tract){
      data$RHH[i] = demog14$RHH[demog14$Census_Tract == data$Census_Tract[i]]
      data$WHITE[i] = demog14$WHITE[demog14$Census_Tract == data$Census_Tract[i]]
      data$BLACK[i] = demog14$BLACK[demog14$Census_Tract == data$Census_Tract[i]]
      data$HISPANIC[i] = demog14$HISPANIC[demog14$Census_Tract == data$Census_Tract[i]]
      data$ASIAN[i] = demog14$ASIAN[demog14$Census_Tract == data$Census_Tract[i]]
      data$YOUNG[i] = demog14$YOUNG[demog14$Census_Tract == data$Census_Tract[i]]
      data$HSDROP[i] = demog14$HSDROP[demog14$Census_Tract == data$Census_Tract[i]]
      data$HS[i] = demog14$HS[demog14$Census_Tract == data$Census_Tract[i]]
      data$BACH[i] = demog14$BACH[demog14$Census_Tract == data$Census_Tract[i]]
    }
    if(data$Census_Tract[i] %in% households14$Census_Tract){
      data$FEMALE[i] = households14$FEMALE[households14$Census_Tract == data$Census_Tract[i]]
    }
    
    if(data$Census_Tract[i] %in% income14$Census_Tract){
      data$RMEDINC[i] = income14$RMEDINC[income14$Census_Tract == data$Census_Tract[i]] 
    }
    if(data$Census_Tract[i] %in% housing14$Census_Tract){
      data$GRBURDEN[i] = housing14$GRBURDEN[housing14$Census_Tract == data$Census_Tract[i]]
      data$VACANCY[i] = housing14$VACANCY[housing14$Census_Tract == data$Census_Tract[i]]
    }
    if(data$Census_Tract[i] %in% rent14$Census_Tract){
      data$RENT[i] = rent14$RENT[rent14$Census_Tract == data$Census_Tract[i]]
    }
    if(data$Census_Tract[i] %in% poverty14$Census_Tract){
      data$POVERTY[i] = poverty14$POVERTY[poverty14$Census_Tract == data$Census_Tract[i]] 
    }
    if(data$Census_Tract[i] %in% employment14$Census_Tract){
      data$UR[i] = employment14$UR[employment14$Census_Tract == data$Census_Tract[i]]
    }
  }
}


# there are 27 census tracts that either a) do not have a renter population 
# or b) HUD identifies as belonging to one of the identified cities, but the ACS places them in a different
# county. I am going to go with the ACS and not count these census tracts as being part of our cities.
# All of the census tracts described above will be deleted from the data set.

data <- data[-which(data$RHH == 0), ]

# Calculating means and standard deviations ####
# income, gross rent burden and rent have some NA values that need to be removed when calculating
# the statistics
View(t(data %>%
  filter(State == "NC") %>%
  group_by(City) %>%
  summarise(avg_white = round(mean(WHITE), 2), sd_white = round(sd(WHITE), 2),
            avg_black = round(mean(BLACK), 2), sd_black = round(sd(BLACK), 2),
            avg_hisp = round(mean(HISPANIC), 2), sd_hisp = round(sd(HISPANIC), 2),
            avg_asian = round(mean(ASIAN), 2), sd_asian = round(sd(ASIAN), 2),
            avg_young = round(mean(YOUNG), 2), sd_young = round(sd(YOUNG), 2),
            avg_female = round(mean(FEMALE), 2), sd_female = round(sd(FEMALE), 2),
            avg_bach = round(mean(BACH), 2), sd_bach = round(sd(BACH), 2),
            avg_hs = round(mean(HS), 2), sd_hs = round(sd(HS), 2),
            avg_hsd = round(mean(HSDROP), 2), sd_hsd = round(sd(HSDROP), 2),
            avg_inc = round(mean(RMEDINC, na.rm = T), 2), sd_inc = round(sd(RMEDINC, na.rm = T), 2),
            avg_burden = round(mean(GRBURDEN, na.rm = T), 2), sd_burden = round(sd(GRBURDEN, na.rm = T), 2),
            avg_rhh = round(mean(RHH)), sd_rhh = round(sd(RHH)),
            avg_vac = round(mean(VACANCY), 2), sd_vac = round(sd(VACANCY), 2),
            avg_rent = round(mean(RENT, na.rm = T), 2), sd_rent = round(sd(RENT, na.rm = T), 2),
            avg_pov = round(mean(POVERTY), 2), sd_pov = round(sd(POVERTY), 2),
            avg_ur = round(mean(UR), 2), sd_ur = round(sd(UR), 2)
            )))

View(t(data %>%
         filter(State != "NC") %>%
         group_by(City) %>%
         summarise(avg_white = round(mean(WHITE), 2), sd_white = round(sd(WHITE), 2),
                   avg_black = round(mean(BLACK), 2), sd_black = round(sd(BLACK), 2),
                   avg_hisp = round(mean(HISPANIC), 2), sd_hisp = round(sd(HISPANIC), 2),
                   avg_asian = round(mean(ASIAN), 2), sd_asian = round(sd(ASIAN), 2),
                   avg_young = round(mean(YOUNG), 2), sd_young = round(sd(YOUNG), 2),
                   avg_female = round(mean(FEMALE), 2), sd_female = round(sd(FEMALE), 2),
                   avg_bach = round(mean(BACH), 2), sd_bach = round(sd(BACH), 2),
                   avg_hs = round(mean(HS), 2), sd_hs = round(sd(HS), 2),
                   avg_hsd = round(mean(HSDROP), 2), sd_hsd = round(sd(HSDROP), 2),
                   avg_inc = round(mean(RMEDINC, na.rm = T), 2), sd_inc = round(sd(RMEDINC, na.rm = T), 2),
                   avg_burden = round(mean(GRBURDEN, na.rm = T), 2), sd_burden = round(sd(GRBURDEN, na.rm = T), 2),
                   avg_rhh = round(mean(RHH)), sd_rhh = round(sd(RHH)),
                   avg_vac = round(mean(VACANCY), 2), sd_vac = round(sd(VACANCY), 2),
                   avg_rent = round(mean(RENT, na.rm = T), 2), sd_rent = round(sd(RENT, na.rm = T), 2),
                   avg_pov = round(mean(POVERTY), 2), sd_pov = round(sd(POVERTY), 2),
                   avg_ur = round(mean(UR), 2), sd_ur = round(sd(UR), 2)
         )))

# bootstrap p-values, NC ####
data$Treatment <- ifelse(data$City %in% c("CHARLOTTE", "TOLEDO"), 1, 0)  # adding a treatment indicator to the 
# main data set

# I will use the clustered wild bootstrap method to construct the p-values from the 
# test of the null hypothesis that the means in the treatment and control groups are equal.

# X_c = beta_0 + beta_1 Treat_c + epsilon_c

# 1. Perform restricted estimation where X_c is regressed on just a constant. Here, we are
# imposing the null hypothesis that beta_1 = 0. 

rm <- lm(UR ~ 1, data = filter(data, State == "NC"))

# storing the intercept and residuals
beta_0 <- rm$coefficients[1]
eps_hat <- rm$residuals

# 2. Creating a data set to contain the residuals and a treatment indicator.
bal.data <- as.data.frame(matrix(0, nrow = length(eps_hat), ncol = 5))
colnames(bal.data) <- c("X_c", "City", "Treatment_c", "weights", "eps_c")
bal.data$eps_c <- eps_hat  # adding the error term
bal.data$City <- data$City[data$State == "NC"] 
bal.data$Treatment_c <- ifelse(bal.data$City == "CHARLOTTE", 1, 0) # treatment indicator

# 3. Creating a data frame to storing the bootstrap draws of beta_1
boot.data <- as.data.frame(matrix(0, nrow = 10000, ncol = 1))
colnames(boot.data) <- "Beta_1"

# Now, I start the bootstrap iterations
for(i in 1:10000){
  print(i)  # to track iterations
  # 4. choose a set of weights for the residuals where all counties in the same state get
  # multiplied by the same weight. Since I have a small number of clusters, I will use 
  # the Webb distribution.
  # Use sample(x, size, replace = FALSE, prob = NULL) to generate random numbers with a specific
  # probability. 
  
  w_s <- sample(c(-sqrt(3/2), -sqrt(2/2), -sqrt(1/2), sqrt(1/2), sqrt(2/2), sqrt(3/2)), size = 2, replace = TRUE,
                prob = rep((1/6), 6))
  
  bal.data$weights[bal.data$City == "CHARLOTTE"] = w_s[1]
  bal.data$weights[bal.data$City == "RALEIGH"] = w_s[2]
  
  # 5. Generate values of X_c for iteration r
  bal.data$X_c <- beta_0 + bal.data$weights*bal.data$eps_c
  
  # 6. Using X_c and Treatment_c, run the unrestricted regression
  um_r <- lm(X_c ~ Treatment_c, data = bal.data)
  
  # 7. store the bootstrap beta_1 for iteration r
  boot.data$Beta_1[i] <-  um_r$coefficients[2]
  
  # Then repeat. 
  
}

# 8. Calculate the test statistic 
urm <- lm(UR ~ Treatment, data = filter(data, State == "NC"))
beta_1 <- urm$coefficients[2]
print(beta_1)

# 9. calculating the p-value
round(length(boot.data$Beta_1[abs(boot.data$Beta_1) > abs(beta_1)])/10000, 3)

# bootstrap p-values for variables with NAs, NC ####
# Variables with NAs: Income, gross rent burden, and rent
exclude <- data$Census_Tract[which(is.na(data$RMEDINC))]
exclude <- data$Census_Tract[which(is.na(data$RENT))]  # there's a separate list of tracts that need to 
# be excluded for rent

# 1. Perform restricted estimation where X_c is regressed on just a constant. Here, we are
# imposing the null hypothesis that beta_1 = 0. 

rm <- lm(RENT ~ 1, data = filter(data, State == "NC" & !(Census_Tract %in% exclude)))

# storing the intercept and residuals
beta_0 <- rm$coefficients[1]
eps_hat <- rm$residuals

# 2. Creating a data set to contain the residuals and a treatment indicator.
bal.data <- as.data.frame(matrix(0, nrow = length(eps_hat), ncol = 5))
colnames(bal.data) <- c("X_c", "City", "Treatment_c", "weights", "eps_c")
bal.data$eps_c <- eps_hat  # adding the error term
bal.data$City <- data$City[data$State == "NC" & !(data$Census_Tract %in% exclude)] 
bal.data$Treatment_c <- ifelse(bal.data$City == "CHARLOTTE", 1, 0) # treatment indicator

# 3. Creating a data frame to storing the bootstrap draws of beta_1
boot.data <- as.data.frame(matrix(0, nrow = 10000, ncol = 1))
colnames(boot.data) <- "Beta_1"

# Now, I start the bootstrap iterations
for(i in 1:10000){
  print(i)  # to track iterations
  # 4. choose a set of weights for the residuals where all counties in the same state get
  # multiplied by the same weight. Since I have a small number of clusters, I will use 
  # the Webb distribution.
  # Use sample(x, size, replace = FALSE, prob = NULL) to generate random numbers with a specific
  # probability. 
  
  w_s <- sample(c(-sqrt(3/2), -sqrt(2/2), -sqrt(1/2), sqrt(1/2), sqrt(2/2), sqrt(3/2)), size = 2, replace = TRUE,
                prob = rep((1/6), 6))
  
  bal.data$weights[bal.data$City == "CHARLOTTE"] = w_s[1]
  bal.data$weights[bal.data$City == "RALEIGH"] = w_s[2]
  
  # 5. Generate values of X_c for iteration r
  bal.data$X_c <- beta_0 + bal.data$weights*bal.data$eps_c
  
  # 6. Using X_c and Treatment_c, run the unrestricted regression
  um_r <- lm(X_c ~ Treatment_c, data = bal.data)
  
  # 7. store the bootstrap beta_1 for iteration r
  boot.data$Beta_1[i] <-  um_r$coefficients[2]
  
  # Then repeat. 
  
}

# 8. Calculate the test statistic 
urm <- lm(RENT ~ Treatment, data = filter(data, State == "NC" & !(Census_Tract %in% exclude)))
beta_1 <- urm$coefficients[2]
print(beta_1)

# 9. calculating the p-value
round(length(boot.data$Beta_1[abs(boot.data$Beta_1) > abs(beta_1)])/10000, 3)















# bootstrap p-values, OH ####

# I will use the clustered wild bootstrap method to construct the p-values from the 
# test of the null hypothesis that the means in the treatment and control groups are equal.

# X_c = beta_0 + beta_1 Treat_c + epsilon_c

# 1. Perform restricted estimation where X_c is regressed on just a constant. Here, we are
# imposing the null hypothesis that beta_1 = 0. 

rm <- lm(UR ~ 1, data = filter(data, State != "NC"))

# storing the intercept and residuals
beta_0 <- rm$coefficients[1]
eps_hat <- rm$residuals

# 2. Creating a data set to contain the residuals and a treatment indicator.
bal.data <- as.data.frame(matrix(0, nrow = length(eps_hat), ncol = 5))
colnames(bal.data) <- c("X_c", "City", "Treatment_c", "weights", "eps_c")
bal.data$eps_c <- eps_hat  # adding the error term
bal.data$City <- data$City[data$State != "NC"] 
bal.data$Treatment_c <- ifelse(bal.data$City == "TOLEDO", 1, 0) # treatment indicator

# 3. Creating a data frame to storing the bootstrap draws of beta_1
boot.data <- as.data.frame(matrix(0, nrow = 10000, ncol = 1))
colnames(boot.data) <- "Beta_1"

# Now, I start the bootstrap iterations
for(i in 1:10000){
  print(i)  # to track iterations
  # 4. choose a set of weights for the residuals where all counties in the same state get
  # multiplied by the same weight. Since I have a small number of clusters, I will use 
  # the Webb distribution.
  # Use sample(x, size, replace = FALSE, prob = NULL) to generate random numbers with a specific
  # probability. 
  
  w_s <- sample(c(-sqrt(3/2), -sqrt(2/2), -sqrt(1/2), sqrt(1/2), sqrt(2/2), sqrt(3/2)), size = 2, replace = TRUE,
                prob = rep((1/6), 6))
  
  bal.data$weights[bal.data$City == "TOLEDO"] = w_s[1]
  bal.data$weights[bal.data$City == "LOUISVILLE"] = w_s[2]
  
  # 5. Generate values of X_c for iteration r
  bal.data$X_c <- beta_0 + bal.data$weights*bal.data$eps_c
  
  # 6. Using X_c and Treatment_c, run the unrestricted regression
  um_r <- lm(X_c ~ Treatment_c, data = bal.data)
  
  # 7. store the bootstrap beta_1 for iteration r
  boot.data$Beta_1[i] <-  um_r$coefficients[2]
  
  # Then repeat. 
  
}

# 8. Calculate the test statistic 
urm <- lm(UR ~ Treatment, data = filter(data, State != "NC"))
beta_1 <- urm$coefficients[2]
print(beta_1)

# 9. calculating the p-value
round(length(boot.data$Beta_1[abs(boot.data$Beta_1) > abs(beta_1)])/10000, 3)

# bootstrap p-values for variables with NAs, OH ####
# Variables with NAs: Income, gross rent burden, and rent
exclude <- data$Census_Tract[which(is.na(data$RMEDINC))]
exclude <- data$Census_Tract[which(is.na(data$RENT))]  # there's a separate list of tracts that need to 
# be excluded for rent

# 1. Perform restricted estimation where X_c is regressed on just a constant. Here, we are
# imposing the null hypothesis that beta_1 = 0. 

rm <- lm(RENT ~ 1, data = filter(data, State != "NC" & !(Census_Tract %in% exclude)))

# storing the intercept and residuals
beta_0 <- rm$coefficients[1]
eps_hat <- rm$residuals

# 2. Creating a data set to contain the residuals and a treatment indicator.
bal.data <- as.data.frame(matrix(0, nrow = length(eps_hat), ncol = 5))
colnames(bal.data) <- c("X_c", "City", "Treatment_c", "weights", "eps_c")
bal.data$eps_c <- eps_hat  # adding the error term
bal.data$City <- data$City[data$State != "NC" & !(data$Census_Tract %in% exclude)] 
bal.data$Treatment_c <- ifelse(bal.data$City == "TOLEDO", 1, 0) # treatment indicator

# 3. Creating a data frame to storing the bootstrap draws of beta_1
boot.data <- as.data.frame(matrix(0, nrow = 10000, ncol = 1))
colnames(boot.data) <- "Beta_1"

# Now, I start the bootstrap iterations
for(i in 1:10000){
  print(i)  # to track iterations
  # 4. choose a set of weights for the residuals where all counties in the same state get
  # multiplied by the same weight. Since I have a small number of clusters, I will use 
  # the Webb distribution.
  # Use sample(x, size, replace = FALSE, prob = NULL) to generate random numbers with a specific
  # probability. 
  
  w_s <- sample(c(-sqrt(3/2), -sqrt(2/2), -sqrt(1/2), sqrt(1/2), sqrt(2/2), sqrt(3/2)), size = 2, replace = TRUE,
                prob = rep((1/6), 6))
  
  bal.data$weights[bal.data$City == "TOLEDO"] = w_s[1]
  bal.data$weights[bal.data$City == "LOUISVILLE"] = w_s[2]
  
  # 5. Generate values of X_c for iteration r
  bal.data$X_c <- beta_0 + bal.data$weights*bal.data$eps_c
  
  # 6. Using X_c and Treatment_c, run the unrestricted regression
  um_r <- lm(X_c ~ Treatment_c, data = bal.data)
  
  # 7. store the bootstrap beta_1 for iteration r
  boot.data$Beta_1[i] <-  um_r$coefficients[2]
  
  # Then repeat. 
  
}

# 8. Calculate the test statistic 
urm <- lm(RENT ~ Treatment, data = filter(data, State != "NC" & !(Census_Tract %in% exclude)))
beta_1 <- urm$coefficients[2]
print(beta_1)

# 9. calculating the p-value
round(length(boot.data$Beta_1[abs(boot.data$Beta_1) > abs(beta_1)])/10000, 3)














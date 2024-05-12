install.packages("readxl")
library(readxl)

install.packages("dplyr")
library(dplyr)

install.packages("tidyverse")
library(tidyverse)

install.packages("ggplot2")
library(ggplot2)

install.packages("zoo")
library(zoo)
setwd("/Users/alexander.devera25/Documents")

hhi_data <- read.csv("/Users/alexander.devera25/Downloads/HHI_index_2.csv")

education_stats <- read.csv("/Users/alexander.devera25/Downloads/education_states.csv")
FDI_Data <- read.csv("/Users/alexander.devera25/Documents/FDI_Data.csv")
patent_data <- read.csv("/Users/alexander.devera25/Documents/patent_data.csv")
grant_data <- read.csv("/Users/alexander.devera25/Documents/technological_grant_data.csv")
manufactuaring_data <- read.csv("/Users/alexander.devera25/Documents/manufactuaring_data.csv")
fuel_data <- read.csv("/Users/alexander.devera25/Documents/Fuel_data.csv")
resource_rents <- read.csv("/Users/alexander.devera25/Documents/resource_rents_data.csv")
manufactuaring_data <- plyr::rename(manufactuaring_data, c("Data.Source" = "Country"))
grant_data <- plyr::rename(grant_data, c("Data.Source" = "Country"))

#must decide to either merge everything into a single dataset or utilize education to directly assess economic diversification to directly assess FDI

education_stats_filter <- plyr ::rename(education_stats, c("X" = "Country"))
resource_rents <- plyr::rename(resource_rents, c("Data.Source" = "Country"))
FDI_Data <- plyr::rename(FDI_Data, c("Data.Source" = "Country"))
patent_data <- plyr::rename(patent_data, c("Data.Source" = "Country"))
fuel_data <- plyr::rename(fuel_data, c("Data.Source" = "Country"))
research_expenditure_data <- plyr::rename(research_expenditure_data, c("Country.Name" = "Country"))

education_stats_filter <- education_stats_filter |>
  dplyr::filter(education_stats_filter$Country == "Canada" | Country == "Saudi Arabia" | Country == "Canada" | Country == "United States" |     Country == "Oman" 
         | Country == "United Arab Emirates" | Country == "Korea, Rep." | Country == "Singapore" |
       Country == "Norway" |  Country == "Iran, Islamic Rep." | Country == "Qatar")


resource_rents_filter <- resource_rents |>
  dplyr::filter(Country == "Canada" | Country == "Saudi Arabia" | Country == "Canada" | Country == "United States" |     Country == "Oman" 
                | Country == "United Arab Emirates" | Country == "Korea, Rep." | Country == "Singapore" |
                  Country == "Norway" |  Country == "Iran, Islamic Rep." | Country == "Qatar")

fuel_data_filter <- fuel_data |>
  dplyr::filter(Country == "Canada" | Country == "Saudi Arabia" | Country == "Canada" | Country == "United States" |     Country == "Oman" 
                | Country == "United Arab Emirates" | Country == "Korea, Rep." | Country == "Singapore" |
                  Country == "Norway" |  Country == "Iran, Islamic Rep." | Country == "Qatar")

grant_data_filter <- grant_data |>
  dplyr::filter(Country == "Canada" | Country == "Saudi Arabia" | Country == "Canada" | Country == "United States" |     Country == "Oman" 
                | Country == "United Arab Emirates" | Country == "Korea, Rep." | Country == "Singapore" |
                  Country == "Norway" |  Country == "Iran, Islamic Rep." | Country == "Qatar")

fdi_data_filter <- FDI_Data |>
  dplyr::filter(Country == "Canada" | Country == "Saudi Arabia" | Country == "Canada" | Country == "United States" |     Country == "Oman" 
                | Country == "United Arab Emirates" | Country == "Korea, Rep." | Country == "Singapore" |
                  Country == "Norway" |  Country == "Iran, Islamic Rep." | Country == "Qatar")

manufactuaring_data_filter <- manufactuaring_data |>
  dplyr::filter(Country == "Canada" | Country == "Saudi Arabia" | Country == "Canada" | Country == "United States" |     Country == "Oman" 
                | Country == "United Arab Emirates" | Country == "Korea, Rep." | Country == "Singapore" |
                  Country == "Norway" |  Country == "Iran, Islamic Rep." | Country == "Qatar")


patent_data_filter <- patent_data |>
  dplyr::filter(Country == "Canada" | Country == "Saudi Arabia" | Country == "Canada" | Country == "United States" |     Country == "Oman" 
                | Country == "United Arab Emirates" | Country == "Korea, Rep." | Country == "Singapore" |
                  Country == "Norway" |  Country == "Iran, Islamic Rep." | Country == "Qatar")


gini_index <- read.csv("/Users/alexander.devera25/Downloads/gini_index.csv")
gdp_per_capita <- read.csv("/Users/alexander.devera25/Downloads/gdp_data.csv")
age_data <- read.csv("/Users/alexander.devera25/Downloads/age_data_2.csv")

years_full <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)
values_iran <- c(0.07427865, 0.08628414, 0.09048938,
                 0.09655194, 0.08369071, 0.08708321, 0.07514205, 0.07970087, 0.08425968, 0.08881849,
                 0.09337731, 0.10664621, 0.158323, 0.2099998,0.22164428, 0.20298728, 0.14240445, 0.12496231)

data_approx_1 <- approx(years_full, values_iran, xout = 2007:2009)
data_approx_2 <- approx(years_full, values_iran, xout = 2012)
print(data_approx_1)
print(data_approx_2)

years_full_2 <- c(1997, 1998, 1999,
                2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 
                2013, 2014, 2015, 2016, 2017)

values_iran_2 <- c(0.07153774045, 0.0705953867, 0.06639994388, 0.07427865, 0.08628414, 0.09048938,
                 0.09655194, 0.08369071, 0.08708321, 0.07514205, 0.07970087, 0.08425968, 0.08881849,
                 0.09337731, 0.10664621, 0.158323, 0.2099998,0.22164428, 0.20298728, 0.14240445, 0.12496231)

data_approx_3 <- na.approx(years_full_2, values_iran_2, xout = 1989:1996)
print(data_approx_3)

years_full_2 <- c(1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)
values_iran_2 <- c(0.07153774045, 0.0705953867, 0.06639994388, 0.07427865, 0.08628414, 0.09048938,
                   0.09655194, 0.08369071, 0.08708321, 0.07514205, 0.07970087, 0.08425968, 0.08881849,
                   0.09337731, 0.10664621, 0.158323, 0.2099998, 0.22164428, 0.20298728, 0.14240445, 0.12496231)


iran_years <- c(1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2010, 2011, 2013, 2014, 2015, 2016, 2017)
values_iran_2 <- c(0.07153774045, 0.0705953867, 0.06639994388, 0.07427865, 0.08628414, 0.09048938,
                   0.09655194, 0.08369071, 0.08708321, 0.07514205,
                   0.09337731, 0.10664621, 0.2099998, 0.22164428, 0.20298728, 0.14240445, 0.12496231)

model <- lm(values_iran_2 ~ years_full_2)
predict_iran <- predict(model, data.frame(years_full_2 = 2012))
print(predict_iran)

qatar_years <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2010, 2012, 2013, 2014, 2015, 2016, 2017)
qatar_values <- c(0.2996849269,0.3108523618,0.2748852572,0.2704717397,0.2299112072,0.2290205525,0.2307878835,
                  0.2265701601,0.2147820321,0.148814718,0.1449655793,0.1411337896,0.1426444004,0.1140966258,
                  0.09870989354,0.09351573474)

model_2 <- lm(qatar_values ~ qatar_years)
predict_qatar <- predict(model_2, data.frame(qatar_years = 2009))
predict_qatar_2 <- predict(model_2, data.frame(qatar_years = 2011))

print(predict_qatar)
print(predict_qatar_2)

uae_years <- c(2011,2012,2013,2014,2015,2016,2017)
uae_values <- c(1.62607,	1.30518,	1.44603,	1.53732,	1.7092,	1.73711,	1.63757)

model_3 <- lm(uae_values ~ uae_years)
predict_uae <- predict(model_3, data.frame(uae_years = 2000:2007))
predict_uae_2 <- predict(model_3, data.frame(uae_years = 2008:2010))
print(predict_uae)
print(predict_uae_2)

new_hhi_index <- read.csv("/Users/alexander.devera25/Downloads/new_hhi_index.csv")

research_expenditure_data <- read.csv("/Users/alexander.devera25/Downloads/research_data.csv")

research_expenditure_data_filter <- research_expenditure_data |>
  dplyr::filter(Country == "Canada" | Country == "Saudi Arabia" | Country == "Canada" | Country == "United States" | Country == "Oman" 
                | Country == "United Arab Emirates" | Country == "Korea, Rep." | Country == "Singapore" |
                  Country == "Norway" |  Country == "Iran, Islamic Rep." | Country == "Qatar")

canada_years <- c(2000, 2001, 2002, 2005, 2007, 2008, 2009, 2010, 
                  2011, 2012, 2013, 2014, 2015, 2016, 2017)

canada_values <- c(5.423319817,	4.953030109,	4.993080139,			
                   4.765880108,		4.766409874,	4.62612009,
                   4.840579987,	5.356369972,	5.262050152,	4.692587376,
                   4.589009762,	4.843589783,	4.739379883,	4.816420078,	4.959969997)

model_4 <- lm(canada_values ~ canada_years)

predict_canada <- predict(model_4, data.frame(canada_years = 2003:2004))
predict_canada_2 <- predict(model_4, data.frame(canada_years = 2006))

print(predict_canada)
print(predict_canada_2)

saudi_arabia_years <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2008)
saudi_arabia_values <- c(5.909830093,	7.718870163,	7.636889935,	7.105090141,
                         6.277460098,	5.430920124,	5.891170025,		5.13781023)

model_5 <- lm(saudi_arabia_values ~ saudi_arabia_years)

predict_saudi_arabia <- predict(model_5, data.frame(saudi_arabia_years = 2006))
predict_saudi_arabia_2 <- predict(model_5, data.frame(saudi_arabia_years = 2009:2017))

print(predict_saudi_arabia)
print(predict_saudi_arabia_2)

qatar_education_years <- c(2000, 2001, 2002, 2003, 2004, 2005, 2008, 2009, 2010, 2011,
                           2012, 2013, 2014, 2017)

qatar_education_values <- c(3.725980043,	3.200190067,	3.846560001,	3.373939991,	3.462589979,	
                            3.971879959,			4.22755003,	3.413150072,	4.539659977,	4.012239933,
                            3.50744009,	4.073840141,	3.605350018,			2.967459917)

model_6 <- lm(qatar_education_values ~ qatar_education_years)

predict_qatar_e <- predict(model_6, data.frame(qatar_education_years = 2006:2007))
predict_qatar_e_2 <- predict(model_6, data.frame(qatar_education_years = 2015:2016))

print(predict_qatar_e)
print(predict_qatar_e_2)

oman_years <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2009, 2013)
oman_values <- c(3.200239897,	4.020319939,	4.325069904,	3.896990061,
                 4.02835989,	3.517210007,	3.859339952,	4.187739849,
                 4.35383987)

model_7 <- lm(oman_values ~ oman_years)

predict_oman <- predict(model_7, data.frame(oman_years = 2007:2008))
predict_oman_2 <- predict(model_7, data.frame(oman_years = 2010:2012))
predict_oman_3 <- predict(model_7, data.frame(oman_years = 2014:2017))

print(predict_oman)
print(predict_oman_2)
print(predict_oman_3)


south_korea_years <- c(2009, 2010, 2015, 2016, 2017)
south_korea_values <- c(3.91358161,	3.52756691,	4.454237938,
                        4.333099842,	4.328239918)

model_8 <- lm(south_korea_values ~ south_korea_years)

predict_south_korea <- predict(model_8, data.frame(south_korea_years = 2000:2008))
predict_south_korea_2 <- predict(model_8, data.frame(south_korea_years = 2011:2014))

print(predict_south_korea)
print(predict_south_korea_2)


dataset <- read.csv("/Users/alexander.devera25/Downloads/the_dataset.csv")


canada_model <- lm(Canada_HHI ~ Canada_ed + Canada_high_tech_exports + Canada_fuel + Canada_age
                   + Canada_gdp, data = dataset)

iran_model <- lm(Iran_HHI ~ Iran_ed + Iran_high_tech_export + Iran_fuel + Iran_age + Iran__gdp, data = dataset)

south_korea_model <- lm(South.Korea_HHI ~ South.Korea_ed + 
                          South.Korea_high_tech_export + South.Korea_fuel + South.Korea_age +
                          South.Korea__gdp, data = dataset)

oman_model <- lm(Oman_HHI ~ Oman_ed + Oman_high_tech_export + Oman_fuel + Oman_age + Oman_gdp, data = dataset)

saudi_arabia_model <- lm(Saudi.Arabia_HHI ~ Saudi.Arabia_ed + Saudi.Arabia_high_tech_export + Saudi.Arabia_fuel 
                         + Saudi.Arabia_gdp + Saudi.Arabia_age, data = dataset)

qatar_model <- lm(Qatar_HHI ~ Qatar_ed + Qatar_high_tech_export + Qatar_fuel + Qatar_age + Qatar_gdp, data = dataset)

united_arab_emirates_model <- lm(United.Arab.Emirates_HHI ~ United.Arab.Emirates_ed + United.Arab.Emirates_high_tech_exports  +
                            United.Arab.Emirates_fuel + United.Arab.Emirates_age + United.Arab.Emirates_gdp, data = dataset)

united_states_model <- lm(United.States_HHI ~ United.States_ed + United.States_high_tech_exports + 
                            United.States_fuel + United.States_age + United.States_gdp, data = dataset)


norway_model <- lm(Norway_HHI ~ Norway_ed + Norway_high_tech_export + Norway_fuel + Norway_age 
                   + Norway_gdp, data = dataset)

singapore_model <- lm(Singapore_HHI ~ Singapore_ed + Singapore_high_tech_exports + Singapore_fuel +
                        Singapore_age + Singapore_gdp, data = dataset)

summary(singapore_model)
summary(canada_model)
summary(norway_model)
summary(united_states_model)
summary(qatar_model)
summary(saudi_arabia_model)
summary(oman_model)
summary(south_korea_model)
summary(iran_model)
summary(united_arab_emirates_model)


singapore_fdi <- lm(Singapore_FDI ~ Singapore_ed + Singapore_high_tech_exports + Singapore_fuel + 
                      Singapore_age + Singapore_gdp, data = dataset)
canada_fdi_model <- lm(Canada_FDI ~ Canada_ed + Canada_high_tech_exports + Canada_fuel + Canada_age
                       + Canada_gdp, data = dataset)

iran_fdi_model <- lm(Iran_FDI ~ Iran_ed + Iran_high_tech_export + Iran_fuel + Iran_age + Iran__gdp, data = dataset)

south_korea_fdi_model <- lm(South.Korea_FDI ~ South.Korea_ed + 
                              South.Korea_high_tech_export + South.Korea_fuel + South.Korea_age +
                              South.Korea__gdp, data = dataset)

oman_fdi_model <- lm(Oman_FDI ~ Oman_ed + Oman_high_tech_export + Oman_fuel + Oman_age + Oman_gdp, data = dataset)

saudi_arabia_fdi_model <- lm(Saudi.Arabia_FDI ~ Saudi.Arabia_ed + Saudi.Arabia_high_tech_export + Saudi.Arabia_fuel 
                             + Saudi.Arabia_gdp + Saudi.Arabia_age, data = dataset)

qatar_fdi_model <- lm(Qatar_FDI ~ Qatar_ed + Qatar_high_tech_export + Qatar_fuel + Qatar_age + Qatar_gdp, data = dataset)

united_arab_emirates_fdi_model <- lm(United.Arab.Emirates_FDI ~ United.Arab.Emirates_ed + United.Arab.Emirates_high_tech_exports  +
                                       United.Arab.Emirates_fuel + United.Arab.Emirates_age + United.Arab.Emirates_gdp, data = dataset)

united_states_fdi_model <- lm(United.States_FDI ~ United.States_ed + United.States_high_tech_exports + 
                                United.States_fuel + United.States_age + United.States_gdp, data = dataset)

norway_fdi_model <- lm(Norway_FDI ~ Norway_ed + Norway_high_tech_export + Norway_fuel + Norway_age 
                       + Norway_gdp, data = dataset)


summary(canada_fdi_model)
summary(iran_fdi_model)
summary(south_korea_fdi_model)
summary(oman_fdi_model)
summary(saudi_arabia_fdi_model)
summary(qatar_fdi_model)
summary(united_arab_emirates_fdi_model)
summary(united_states_fdi_model)
summary(norway_fdi_model)
summary(singapore_fdi)

singapore_bivariate <- lm(Singapore_FDI ~ Singapore_ed, data = dataset)
canada_bivariate <- lm(Canada_FDI ~ Canada_ed, data = dataset)
iran_bivariate <- lm(Iran_FDI ~ Iran_ed, data = dataset)
south_korea_bivariate <- lm(South.Korea_FDI ~ South.Korea_ed, data = dataset)
oman_bivariate <- lm(Oman_FDI ~ Oman_ed, data = dataset)
saudi_arabia_bivariate <- lm(Saudi.Arabia_FDI ~ Saudi.Arabia_ed, data = dataset)
qatar_bivariate <- lm(Qatar_FDI ~ Qatar_ed, data = dataset)
united_arab_emirates_bivariate <- lm(United.Arab.Emirates_FDI ~ United.Arab.Emirates_ed, data = dataset)
united_states_bivariate <- lm(United.States_FDI ~ United.States_ed, data = dataset)
norway_bivariate <- lm(Norway_FDI ~ Norway_ed, data = dataset)

summary(canada_bivariate)
summary(iran_bivariate)
summary(south_korea_bivariate)
summary(oman_bivariate)
summary(saudi_arabia_bivariate)
summary(qatar_bivariate)
summary(united_arab_emirates_bivariate)
summary(united_states_bivariate)
summary(norway_bivariate)
summary(singapore_bivariate)

canada_fuel <- lm(Canada_HHI ~ Canada_fuel + Canada_age + Canada_gdp, data = dataset)
iran_fuel <- lm(Iran_HHI ~ Iran_fuel + Iran_age + Iran__gdp, data = dataset)
south_korea_fuel <- lm(South.Korea_HHI ~ South.Korea_fuel + South.Korea_age + South.Korea__gdp, data = dataset)
oman_fuel <- lm(Oman_HHI ~ Oman_fuel + Oman_age + Oman_gdp, data = dataset)
saudi_arabia_fuel <- lm(Saudi.Arabia_HHI ~ Saudi.Arabia_fuel + Saudi.Arabia_age + Saudi.Arabia_gdp, data = dataset)
qatar_fuel <- lm(Qatar_HHI ~ Qatar_fuel + Qatar_age + Qatar_gdp, data = dataset)
united_arab_emirates_fuel <- lm(United.Arab.Emirates_HHI ~ United.Arab.Emirates_fuel + United.Arab.Emirates_age + United.Arab.Emirates_gdp, data = dataset)
united_states_fuel <- lm(United.States_HHI ~ United.States_fuel + United.States_age + United.States_gdp, data = dataset)
norway_fuel <- lm(Norway_HHI ~ Norway_fuel + Norway_age + Norway_gdp, data = dataset)
singapore_fuel <- lm(Singapore_HHI ~ Singapore_fuel + Singapore_age + Singapore_gdp, data = dataset)

summary(canada_fuel)
summary(iran_fuel)
summary(south_korea_fuel)
summary(oman_fuel)
summary(saudi_arabia_fuel)
summary(qatar_fuel)
summary(united_arab_emirates_fuel)
summary(united_states_fuel)
summary(norway_fuel)
summary(singapore_fuel)


singapore_bivariate_2 <- lm(Singapore_HHI ~ Singapore_ed, data = dataset)
canada_bivariate_2 <- lm(Canada_HHI ~ Canada_ed, data = dataset)
iran_bivariate_2 <- lm(Iran_HHI ~ Iran_ed, data = dataset)
south_korea_bivariate_2 <- lm(South.Korea_HHI ~ South.Korea_ed, data = dataset)
oman_bivariate_2 <- lm(Oman_HHI ~ Oman_ed, data = dataset)
saudi_arabia_bivariate_2 <- lm(Saudi.Arabia_HHI ~ Saudi.Arabia_ed, data = dataset)
qatar_bivariate_2 <- lm(Qatar_HHI ~ Qatar_ed, data = dataset)
united_arab_emirates_bivariate_2 <- lm(United.Arab.Emirates_HHI ~ United.Arab.Emirates_ed, data = dataset)
united_states_bivariate_2 <- lm(United.States_HHI ~ United.States_ed, data = dataset)
norway_bivariate_2 <- lm(Norway_HHI ~ Norway_ed, data = dataset)

summary(canada_bivariate_2)
summary(iran_bivariate_2)
summary(south_korea_bivariate_2)
summary(oman_bivariate_2)
summary(saudi_arabia_bivariate_2)
summary(qatar_bivariate_2)
summary(united_arab_emirates_bivariate_2)
summary(united_states_bivariate_2)
summary(norway_bivariate_2)
summary(singapore_bivariate_2)


canada_model_2 <- lm(Canada_HHI ~ Canada_ed + Canada_age
                   + Canada_gdp, data = dataset)

iran_model_2 <- lm(Iran_HHI ~ Iran_ed + Iran_age + Iran__gdp, data = dataset)

south_korea_model_2 <- lm(South.Korea_HHI ~ South.Korea_ed + 
                         South.Korea_age +
                          South.Korea__gdp, data = dataset)

oman_model_2 <- lm(Oman_HHI ~ Oman_ed + Oman_age + Oman_gdp, data = dataset)

saudi_arabia_model_2 <- lm(Saudi.Arabia_HHI ~ Saudi.Arabia_ed  
                         + Saudi.Arabia_gdp + Saudi.Arabia_age, data = dataset)

qatar_model_2 <- lm(Qatar_HHI ~ Qatar_ed + Qatar_age + Qatar_gdp, data = dataset)

united_arab_emirates_model_2 <- lm(United.Arab.Emirates_HHI ~ United.Arab.Emirates_ed + United.Arab.Emirates_age + United.Arab.Emirates_gdp, data = dataset)

united_states_model_2 <- lm(United.States_HHI ~ United.States_ed + United.States_age + United.States_gdp, data = dataset)


norway_model_2 <- lm(Norway_HHI ~ Norway_ed + Norway_age 
                   + Norway_gdp, data = dataset)

singapore_model_2 <- lm(Singapore_HHI ~ Singapore_ed +
                        Singapore_age + Singapore_gdp, data = dataset)

summary(singapore_model_2)
summary(canada_model_2)
summary(norway_model_2)
summary(united_states_model_2)
summary(qatar_model_2)
summary(saudi_arabia_model_2)
summary(oman_model_2)
summary(south_korea_model_2)
summary(iran_model_2)
summary(united_arab_emirates_model_2)

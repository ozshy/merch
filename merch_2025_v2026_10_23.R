#"How Merchants Get Paid"
#Redoing a 2021 unpublished paper
#Code is partly based on merch_210204.R and merch23.pdf (used 2019 Diary data)

# Packages used for this coding: 
library(dplyr)
#library(tidyr)# function "spread()" OUTDATED pivot_longer does not work
#library(plyr)# for "join" function
#library(formattable)# formats tables to allow trailing zeros (w/ numeric format)
#library(plotrix)# weighted histograms
#library(spatstat)# for weighted median
#library(lubridate) # extracts day of the week
library(ggplot2); theme_set(theme_bw())
library(scales)# for thousand comma in geom_text
library("xtable") #exporting to LaTeX
#library(Hmisc)# for cutting data into bins & weighted medians
#library(gtools)# for stars.pval function
library(energy)# for distance correlation dcor()

# Regression packages
#library(mfx) # logit regressions with marginal effects
library(stargazer) # for displaying multinomial coefficients. Does not work with mfx but works with linear regressions
#library(texreg) # for displaying multinomial coefficients. Works with mfx (unlike stargazer). Also displays multiple regression.
#library(huxtable)#displays multiple regressions as table => advantage, since the table can be edited in R => Problem: built-in to_latex output does not run in LaTeX in my experience. 

# Imputing missing obs packages
#library("missForest")#to impute NAs, preparation for random forest (not used because cannot impute based on selected variables (no var selection.)
#library(mice)# for imputations. => cannot impute a large number of NA?
#library(missRanger)# for imputations of bill based on "merch only

# ML packages
#library(rpart)
#library(rpart.plot)
#library(partykit)# modifies rpart tree plot
#library("randomForest")

#Begin: read data####
setwd("C:/Oz_local_workspace_2_merch")#work
#setwd("~/Research/SCPC_DCPC/2025_SDCPC")#home
#
dir()
# Read transaction dataset
trans2025_1.df = readRDS("dcpc-2025-tranlevel-public.rds")
dim(trans2025_1.df)
names(trans2025_1.df)
(sampled_num_trans_2025 = nrow(trans2025_1.df))

# Read individual dataset
indiv2025_1.df = readRDS("dcpc-2025-indlevel-public.rds")
dim(indiv2025_1.df)
names(indiv2025_1.df)
(sampled_num_resp_2025 = nrow(indiv2025_1.df))

# select needed variables from the indiv dataset
indiv2025_2.df = subset(indiv2025_1.df, select = c(id, ind_weight_all, age, income_hh, chk_t_m, pa035))

# merge the indiv with the trans datasets by id
# delete transactions not made in October
table(trans2025_1.df$date)
trans2025_2.df = subset(trans2025_1.df, date > "2025-09-30" & date < "2025-11-01")
table(trans2025_2.df$date)
#
merch_1.df = left_join(trans2025_2.df, indiv2025_2.df, by = "id")
dim(merch_1.df)# num of payments (transactions)
length(unique(merch_1.df$id))# num respondents
table(merch_1.df$date)

# Select variables
names(merch_1.df)
merch2.df = subset(merch_1.df, select = c(id, ind_weight_all, age, merch, pi, amnt, in_person, bill, income_hh))
# remove (some) NAs
dim(merch2.df)
sum(is.na(merch2.df$merch))
table(merch2.df$merch, useNA = "always")
merch3.df = subset(merch2.df, !is.na(merch))
dim(merch3.df)
table(merch3.df$pi, useNA = "always")
merch4.df = subset(merch3.df, !is.na(pi))
dim(merch4.df)
sum(is.na(merch3.df$id))
table(merch4.df$in_person, useNA = "always")
merch5.df = subset(merch4.df, !is.na(in_person))
table(merch5.df$in_person, useNA = "always")
table(merch5.df$bill, useNA = "always")
merch6.df = subset(merch5.df, !is.na(bill))
table(merch5.df$bill, useNA = "always")
sum(is.na(merch5.df$ind_weight_all))
merch6.df = subset(merch5.df, !is.na(ind_weight_all))
sum(is.na(merch6.df$ind_weight_all))
sum(is.na(merch6.df$amnt))
subset(merch6.df, amnt <= 0)# verify all amnt >0
sum(is.na(merch6.df$income_hh))
sum(is.na(merch6.df$age))

nrow(merch6.df)# num payments
length(unique(merch6.df$id))# num respondents

#digression (not related): Num payments per-respondent summary
temp.df = as.data.frame(merch6.df %>% count(id))
names(temp.df)
table(temp.df$n)
sum(temp.df$n)# num payments total
nrow(merch6.df)#verify
nrow(merch6.df)/length(unique(merch6.df$id))#avg num payment per resp
(31/3)*nrow(merch6.df)/length(unique(merch6.df$id))#above converted to monthly
# End digression

#End: read data####

#++++++++++++++

#Begin: Table A1: merchant definitions####

## Vector of 21 merchant names (Table 1)
(merch1_name = "Grocery stores, convenience stores without gas stations, pharmacies")
(merch2_name = "Gas stations and car charging stations")
(merch3_name = "Sit-down restaurants and bars")
(merch4_name = "Fast food restaurants, coffee shops, cafeterias, food trucks")
(merch5_name = "General merchandise stores, department stores, other stores, online shopping")
(merch6_name = "General services: hair dressers, auto repair, parking lots, laundry or dry cleaning, etc.")
(merch7_name = "Arts, entertainment, recreation")
(merch8_name = "Utilities not paid to the government: electricity, natural gas, water, sewer, trash, heating oil")
(merch9_name = "Taxis, airplanes, delivery")
(merch10_name = "Telephone, internet, cable or satellite TV, video or music streaming services, movie theaters")
(merch11_name = "Building contractors, plumbers, electricians, HVAC, etc.")
(merch12_name = "Professional services: legal, accounting, architectural services; veterinarians, photographers or photo
processers")
(merch13_name = "Hotels, motels, RV parks, campsites")
(merch14_name = "Rent for apartments, homes, or other buildings, real estate companies, property managers, etc.")
(merch15_name = "Mortgage companies, credit card companies, banks, insurance companies, stock brokers, IRA funds, mutual funds, credit unions, sending remittances")
(merch16_name = "Can be a gift or repayment to a family member, friend, or co-worker. Can be a payment to somebody who did a small job for you.")
(merch17_name = "Charitable or religious donations")
(merch18_name = "Hospital, doctor, dentist, nursing homes, etc.")
(merch19_name = "Government taxes or fees")
(merch20_name = "Schools, colleges, childcare centers")
(merch21_name = "Public transportation and tolls")
# Make names a vector
(merch_name.vec = c(merch1_name, merch2_name, merch3_name, merch4_name, merch5_name, merch6_name, merch7_name, merch8_name, merch9_name, merch10_name, merch11_name, merch12_name, merch13_name, merch14_name, merch15_name, merch16_name, merch17_name, merch18_name, merch19_name, merch20_name, merch21_name))
# Finanlizing merchant name table
merch_num.vec = 1:21
(merch_name.df = data.frame(merch_num.vec, merch_name.vec))
dim(merch_name.df)
# finalizing Table merchant names (Table 1)
# below, create matrix w\ 1 extra column to indicate number of digits for each row
#(digitm = matrix(c(rep(1,6+1), rep(1,6+1), rep(2,6+1) , rep(2,6+1) , rep(2,6+1) , rep(2,6+1), rep(1,6+1), rep(1,6+1), rep(1,6+1), rep(1,6+1), rep(1,6+1), rep(1,6+1), rep(1,6+1)), nrow = 13, ncol = 6+1, byrow = T))
#
#print(xtable(use.df, digits = digitm), include.rownames = F, hline.after = c(0,2,6,13))
print(xtable(merch_name.df), include.rownames = F, hline.after = c(0))

### Abbreviating Table 1's 21 merchant description (from Brian's slides)
# to be used in tables (see below further merch_abv_fig for figures)
(merch1_abv = "1. Grocery store")
(merch2_abv = "2. Gas/charging station")
(merch3_abv = "3. Restaurant/bar")
(merch4_abv = "4. Fast food/coffee shop")
(merch5_abv = "5. General merchandise store")
(merch6_abv = "6. General service")
(merch7_abv = "7. Art/entertainment")
(merch8_abv = "8. Non-government utility")
(merch9_abv = "9. Taxi/airplane/delivery")
(merch10_abv = "10. Phone/internet/cable")
(merch11_abv = "11. Contractor/plumber/ electrician")
(merch12_abv = "12. Professional service")
(merch13_abv = "13. Hotel/motel/campsite")
(merch14_abv = "14. Rent")
(merch15_abv = "15. Mortgage/insurance/credit card")
(merch16_abv = "16. Person-to-person")
(merch17_abv = "17. Charitable/religious donation")
(merch18_abv = "18. Hospital/doctor/dentist")
(merch19_abv = "19. Government taxes")
(merch20_abv = "20. School/college/childcare centers")
(merch21_abv = "21. Public transport/tolls")
# Make names a vector
(merch_abv.vec = c(merch1_abv, merch2_abv, merch3_abv, merch4_abv, merch5_abv, merch6_abv, merch7_abv, merch8_abv, merch9_abv, merch10_abv, merch11_abv, merch12_abv, merch13_abv, merch14_abv, merch15_abv, merch16_abv, merch17_abv, merch18_abv, merch19_abv, merch20_abv, merch21_abv))

# adding merch abv (shorter for figures) description
(merch1_abv_fig = "1. Grocery store")
(merch2_abv_fig = "2. Gas/charging station")
(merch3_abv_fig = "3. Restaurant/bar")
(merch4_abv_fig = "4. Fast food/coffee shop")
(merch5_abv_fig = "5. General merchandise")
(merch6_abv_fig = "6. General service")
(merch7_abv_fig = "7. Art/entertainment")
(merch8_abv_fig = "8. Non-gov't utility")
(merch9_abv_fig = "9. Taxi/airplane/delivery")
(merch10_abv_fig = "10. Phone/internet/cable")
(merch11_abv_fig = "11. Contractor")
(merch12_abv_fig = "12. Professional service")
(merch13_abv_fig = "13. Hotel/motel/campsite")
(merch14_abv_fig = "14. Rent")
(merch15_abv_fig = "15. Mortg/insur/credit c")
(merch16_abv_fig = "16. Person-to-person")
(merch17_abv_fig = "17. Charitable/religious")
(merch18_abv_fig = "18. Hospital/doctor/dentist")
(merch19_abv_fig = "19. Government taxes")
(merch20_abv_fig = "20. Education/childcare")
(merch21_abv_fig = "21. Public transport/tolls")
# Make names a vector
(merch_abv_fig.vec = c(merch1_abv_fig, merch2_abv_fig, merch3_abv_fig, merch4_abv_fig, merch5_abv_fig, merch6_abv_fig, merch7_abv_fig, merch8_abv_fig, merch9_abv_fig, merch10_abv_fig, merch11_abv_fig, merch12_abv_fig, merch13_abv_fig, merch14_abv_fig, merch15_abv_fig, merch16_abv_fig, merch17_abv_fig, merch18_abv_fig, merch19_abv_fig, merch20_abv_fig, merch21_abv_fig))

#End: Table A1 merchant definitions####

#++++++++++++++

#Begin: Table 1: Sample statistics####
merch7.df = merch6.df
names(merch7.df)
dim(merch7.df)# num payments
length(unique(merch7.df$id))# num resp

table(merch7.df$merch)
merch7_merch1.df = subset(merch7.df, merch==1)
merch7_merch2.df = subset(merch7.df, merch==2)
merch7_merch3.df = subset(merch7.df, merch==3)
merch7_merch4.df = subset(merch7.df, merch==4)
merch7_merch5.df = subset(merch7.df, merch==5)
merch7_merch6.df = subset(merch7.df, merch==6)
merch7_merch7.df = subset(merch7.df, merch==7)
merch7_merch8.df = subset(merch7.df, merch==8)
merch7_merch9.df = subset(merch7.df, merch==9)
merch7_merch10.df = subset(merch7.df, merch==10)
merch7_merch11.df = subset(merch7.df, merch==11)
merch7_merch12.df = subset(merch7.df, merch==12)
merch7_merch13.df = subset(merch7.df, merch==13)
merch7_merch14.df = subset(merch7.df, merch==14)
merch7_merch15.df = subset(merch7.df, merch==15)
merch7_merch16.df = subset(merch7.df, merch==16)
merch7_merch17.df = subset(merch7.df, merch==17)
merch7_merch18.df = subset(merch7.df, merch==18)
merch7_merch19.df = subset(merch7.df, merch==19)
merch7_merch20.df = subset(merch7.df, merch==20)
merch7_merch21.df = subset(merch7.df, merch==21)

# vector of Volume (obs) by merchant category
(merch_vol.vec = c(nrow(merch7_merch1.df), nrow(merch7_merch2.df), nrow(merch7_merch3.df), nrow(merch7_merch4.df), nrow(merch7_merch5.df), nrow(merch7_merch6.df), nrow(merch7_merch7.df), nrow(merch7_merch8.df), nrow(merch7_merch9.df), nrow(merch7_merch10.df), nrow(merch7_merch11.df), nrow(merch7_merch12.df), nrow(merch7_merch13.df), nrow(merch7_merch14.df), nrow(merch7_merch15.df), nrow(merch7_merch16.df), nrow(merch7_merch17.df), nrow(merch7_merch18.df), nrow(merch7_merch19.df), nrow(merch7_merch20.df), nrow(merch7_merch21.df)))

## vector of average-value by merchant
(merch_avg_val.vec = c(mean(merch7_merch1.df$amnt), mean(merch7_merch2.df$amnt), mean(merch7_merch3.df$amnt), mean(merch7_merch4.df$amnt), mean(merch7_merch5.df$amnt), mean(merch7_merch6.df$amnt), mean(merch7_merch7.df$amnt), mean(merch7_merch8.df$amnt), mean(merch7_merch9.df$amnt), mean(merch7_merch10.df$amnt), mean(merch7_merch11.df$amnt), mean(merch7_merch12.df$amnt), mean(merch7_merch13.df$amnt), mean(merch7_merch14.df$amnt), mean(merch7_merch15.df$amnt), mean(merch7_merch16.df$amnt), mean(merch7_merch17.df$amnt), mean(merch7_merch18.df$amnt), mean(merch7_merch19.df$amnt), mean(merch7_merch20.df$amnt), mean(merch7_merch21.df$amnt)))

## vector of median-value by merchant
(merch_med_val.vec = c(median(merch7_merch1.df$amnt), median(merch7_merch2.df$amnt), median(merch7_merch3.df$amnt), median(merch7_merch4.df$amnt), median(merch7_merch5.df$amnt), median(merch7_merch6.df$amnt), median(merch7_merch7.df$amnt), median(merch7_merch8.df$amnt), median(merch7_merch9.df$amnt), median(merch7_merch10.df$amnt), median(merch7_merch11.df$amnt), median(merch7_merch12.df$amnt), median(merch7_merch13.df$amnt), median(merch7_merch14.df$amnt), median(merch7_merch15.df$amnt), median(merch7_merch16.df$amnt), median(merch7_merch17.df$amnt), median(merch7_merch18.df$amnt), median(merch7_merch19.df$amnt), median(merch7_merch20.df$amnt), median(merch7_merch21.df$amnt)))

## vector of SD value by merchant
(merch_sd_val.vec = c(sd(merch7_merch1.df$amnt), sd(merch7_merch2.df$amnt), sd(merch7_merch3.df$amnt), sd(merch7_merch4.df$amnt), sd(merch7_merch5.df$amnt), sd(merch7_merch6.df$amnt), sd(merch7_merch7.df$amnt), sd(merch7_merch8.df$amnt), sd(merch7_merch9.df$amnt), sd(merch7_merch10.df$amnt), sd(merch7_merch11.df$amnt), sd(merch7_merch12.df$amnt), sd(merch7_merch13.df$amnt), sd(merch7_merch14.df$amnt), sd(merch7_merch15.df$amnt), sd(merch7_merch16.df$amnt), sd(merch7_merch17.df$amnt), sd(merch7_merch18.df$amnt), sd(merch7_merch19.df$amnt), sd(merch7_merch20.df$amnt), sd(merch7_merch21.df$amnt)))

## vector of min value by merchant
(merch_min_val.vec = c(min(merch7_merch1.df$amnt), min(merch7_merch2.df$amnt), min(merch7_merch3.df$amnt), min(merch7_merch4.df$amnt), min(merch7_merch5.df$amnt), min(merch7_merch6.df$amnt), min(merch7_merch7.df$amnt), min(merch7_merch8.df$amnt), min(merch7_merch9.df$amnt), min(merch7_merch10.df$amnt), min(merch7_merch11.df$amnt), min(merch7_merch12.df$amnt), min(merch7_merch13.df$amnt), min(merch7_merch14.df$amnt), min(merch7_merch15.df$amnt), min(merch7_merch16.df$amnt), min(merch7_merch17.df$amnt), min(merch7_merch18.df$amnt), min(merch7_merch19.df$amnt), min(merch7_merch20.df$amnt), min(merch7_merch21.df$amnt)))

## vector of max value by merchant
(merch_max_val.vec = c(max(merch7_merch1.df$amnt), max(merch7_merch2.df$amnt), max(merch7_merch3.df$amnt), max(merch7_merch4.df$amnt), max(merch7_merch5.df$amnt), max(merch7_merch6.df$amnt), max(merch7_merch7.df$amnt), max(merch7_merch8.df$amnt), max(merch7_merch9.df$amnt), max(merch7_merch10.df$amnt), max(merch7_merch11.df$amnt), max(merch7_merch12.df$amnt), max(merch7_merch13.df$amnt), max(merch7_merch14.df$amnt), max(merch7_merch15.df$amnt), max(merch7_merch16.df$amnt), max(merch7_merch17.df$amnt), max(merch7_merch18.df$amnt), max(merch7_merch19.df$amnt), max(merch7_merch20.df$amnt), max(merch7_merch21.df$amnt)))

## Median HH buyer income by merchant type
names(select(merch7.df, contains("income")))
#
(merch_med_income.vec = c(median(merch7_merch1.df$income_hh, na.rm = T), median(merch7_merch2.df$income_hh, na.rm = T), median(merch7_merch3.df$income_hh, na.rm = T), median(merch7_merch4.df$income_hh, na.rm = T), median(merch7_merch5.df$income_hh, na.rm = T), median(merch7_merch6.df$income_hh, na.rm = T), median(merch7_merch7.df$income_hh, na.rm = T), median(merch7_merch8.df$income_hh, na.rm = T), median(merch7_merch9.df$income_hh, na.rm = T), median(merch7_merch10.df$income_hh, na.rm = T), median(merch7_merch11.df$income_hh, na.rm = T), median(merch7_merch12.df$income_hh, na.rm = T), median(merch7_merch13.df$income_hh, na.rm = T), median(merch7_merch14.df$income_hh, na.rm = T), median(merch7_merch15.df$income_hh, na.rm = T), median(merch7_merch16.df$income_hh, na.rm = T), median(merch7_merch17.df$income_hh, na.rm = T), median(merch7_merch18.df$income_hh, na.rm = T), median(merch7_merch19.df$income_hh, na.rm = T), median(merch7_merch20.df$income_hh, na.rm = T), median(merch7_merch21.df$income_hh, na.rm = T)))

## SD buyer HH income by merchant type
(merch_sd_income.vec = c(sd(merch7_merch1.df$income_hh, na.rm = T), sd(merch7_merch2.df$income_hh, na.rm = T), sd(merch7_merch3.df$income_hh, na.rm = T), sd(merch7_merch4.df$income_hh, na.rm = T), sd(merch7_merch5.df$income_hh, na.rm = T), sd(merch7_merch6.df$income_hh, na.rm = T), sd(merch7_merch7.df$income_hh, na.rm = T), sd(merch7_merch8.df$income_hh, na.rm = T), sd(merch7_merch9.df$income_hh, na.rm = T), sd(merch7_merch10.df$income_hh, na.rm = T), sd(merch7_merch11.df$income_hh, na.rm = T), sd(merch7_merch12.df$income_hh, na.rm = T), sd(merch7_merch13.df$income_hh, na.rm = T), sd(merch7_merch14.df$income_hh, na.rm = T), sd(merch7_merch15.df$income_hh, na.rm = T), sd(merch7_merch16.df$income_hh, na.rm = T), sd(merch7_merch17.df$income_hh, na.rm = T), sd(merch7_merch18.df$income_hh, na.rm = T), sd(merch7_merch19.df$income_hh, na.rm = T), sd(merch7_merch20.df$income_hh, na.rm = T), sd(merch7_merch21.df$income_hh, na.rm = T)))

## Median age by merchant type
(merch_med_age.vec = c(median(merch7_merch1.df$age, na.rm = T), median(merch7_merch2.df$age, na.rm = T), median(merch7_merch3.df$age, na.rm = T), median(merch7_merch4.df$age, na.rm = T), median(merch7_merch5.df$age, na.rm = T), median(merch7_merch6.df$age, na.rm = T), median(merch7_merch7.df$age, na.rm = T), median(merch7_merch8.df$age, na.rm = T), median(merch7_merch9.df$age, na.rm = T), median(merch7_merch10.df$age, na.rm = T), median(merch7_merch11.df$age, na.rm = T), median(merch7_merch12.df$age, na.rm = T), median(merch7_merch13.df$age, na.rm = T), median(merch7_merch14.df$age, na.rm = T), median(merch7_merch15.df$age, na.rm = T), median(merch7_merch16.df$age, na.rm = T), median(merch7_merch17.df$age, na.rm = T), median(merch7_merch18.df$age, na.rm = T), median(merch7_merch19.df$age, na.rm = T), median(merch7_merch20.df$age, na.rm = T), median(merch7_merch21.df$age, na.rm = T)))

## SD age by merchant type
(merch_sd_age.vec = c(sd(merch7_merch1.df$age, na.rm = T), sd(merch7_merch2.df$age, na.rm = T), sd(merch7_merch3.df$age, na.rm = T), sd(merch7_merch4.df$age, na.rm = T), sd(merch7_merch5.df$age, na.rm = T), sd(merch7_merch6.df$age, na.rm = T), sd(merch7_merch7.df$age, na.rm = T), sd(merch7_merch8.df$age, na.rm = T), sd(merch7_merch9.df$age, na.rm = T), sd(merch7_merch10.df$age, na.rm = T), sd(merch7_merch11.df$age, na.rm = T), sd(merch7_merch12.df$age, na.rm = T), sd(merch7_merch13.df$age, na.rm = T), sd(merch7_merch14.df$age, na.rm = T), sd(merch7_merch15.df$age, na.rm = T), sd(merch7_merch16.df$age, na.rm = T), sd(merch7_merch17.df$age, na.rm = T), sd(merch7_merch18.df$age, na.rm = T), sd(merch7_merch19.df$age, na.rm = T), sd(merch7_merch20.df$age, na.rm = T), sd(merch7_merch21.df$age, na.rm = T)))

## number (obs) in-person by merchant type
(merch_vol_inperson.vec = c(nrow(subset(merch7_merch1.df, in_person==1)), nrow(subset(merch7_merch2.df, in_person==1)), nrow(subset(merch7_merch3.df, in_person==1)), nrow(subset(merch7_merch4.df, in_person==1)), nrow(subset(merch7_merch5.df, in_person==1)), nrow(subset(merch7_merch6.df, in_person==1)), nrow(subset(merch7_merch7.df, in_person==1)), nrow(subset(merch7_merch8.df, in_person==1)), nrow(subset(merch7_merch9.df, in_person==1)), nrow(subset(merch7_merch10.df, in_person==1)), nrow(subset(merch7_merch11.df, in_person==1)), nrow(subset(merch7_merch12.df, in_person==1)), nrow(subset(merch7_merch13.df, in_person==1)), nrow(subset(merch7_merch14.df, in_person==1)), nrow(subset(merch7_merch15.df, in_person==1)), nrow(subset(merch7_merch16.df, in_person==1)), nrow(subset(merch7_merch17.df, in_person==1)), nrow(subset(merch7_merch18.df, in_person==1)), nrow(subset(merch7_merch19.df, in_person==1)), nrow(subset(merch7_merch20.df, in_person==1)), nrow(subset(merch7_merch21.df, in_person==1))))

## fractions of in-person by merchant type
(merch_in_person_frac.vec =merch_vol_inperson.vec/merch_vol.vec)

# Make it data frame
#(stat1.df = data.frame(data.frame("Merchant"=merch_abv.vec, "Obs"=merch_vol.vec, "In_person" = 100*merch_in_person_frac.vec, "Min.val" = merch_min_val.vec, "Med.Val"=merch_med_val.vec, "Max.val" = merch_max_val.vec, "Med.Income"=merch_med_income.vec), "Med.age" = merch_med_age.vec))
#dim(stat1.df)# with median age

(stat1.df = data.frame(data.frame("Merchant"=merch_abv.vec, "Obs"=merch_vol.vec, "In_person" = 100*merch_in_person_frac.vec, "Min.Val" = merch_min_val.vec, "Med.Val"=merch_med_val.vec, "Max.Val" = merch_max_val.vec, "Med.Income"=merch_med_income.vec)))
dim(stat1.df)#without median age

# insert , for thousands
stat2.df = stat1.df
stat2.df$Med.Income = formatC(stat2.df$Med.Income, format = "f", big.mark = ",", digits = 0)
stat2.df$Obs = formatC(stat2.df$Obs, format = "f", big.mark = ",", digits = 0)
stat2.df$Max.Val = formatC(stat2.df$Max.Val, format = "f", big.mark = ",", digits = 2)
stat2.df

#digits_mat <- matrix(0, nrow=nrow(stat1.df), ncol = 9)#w/ age
digits_mat <- matrix(0, nrow=nrow(stat1.df), ncol = 8)#w/0 age
digits_mat[, 5:7] = 2

print(xtable(stat2.df, digits=digits_mat), include.rownames = F, hline.after = c(0:21))

#Info for Table 1:
nrow(merch7.df)# num payments
sum(stat1.df$Obs)# verify
length(unique(merch7.df$id))# num respondents

#End: Table 1: Sample Statistics####

#++++++++++++++++++++++

#Begin: Table 2: Share of payment methods by volume####
table(merch7.df$pi, useNA = "always")

# reclassify pi==0 (multiple), pi==8(money order), pi==11 (A2A) pi==14(income deduction) AS pi==13(other) 
#merch8.df = merch7.df %>%   mutate(pi = if_else(pi %in% c(0, 8, 11, 14), 13, pi))
#table(merch8.df$pi, useNA = "always")
# reclassify pi==0 (multiple) AS pi==13(other) only!
merch8.df = merch7.df %>% mutate(pi = if_else(pi %in% c(0), 13, pi))
table(merch8.df$pi, useNA = "always")

# rename PI 
merch9.df = merch8.df %>% mutate(pi = case_when(
  pi == 1 ~ "Cash",
  pi == 2 ~ "Check",
  pi == 3 ~ "Credit",
  pi == 4 ~ "Debit",
  pi == 5 ~ "Prepaid",
  pi == 6 ~ "BANP",
  pi == 7 ~ "OBBP",
  pi == 8 ~ "Order",# money order
  pi == 10 ~ "App",# mobile app
  pi == 11 ~ "A2A",
  pi == 13 ~ "Other",
  pi == 14 ~ "Income", #deduction
  TRUE ~ as.character(pi)
))
table(merch9.df$pi)
length(unique(merch9.df$pi))# num of distinct payment methods
# reorder as in the codebook
merch10.df = merch9.df
merch10.df$pi = factor(merch10.df$pi, levels = c("Cash", "Check", "Credit", "Debit", "Prepaid", "BANP", "OBBP", "Order", "App", "A2A", "Other", "Income"))
table(merch10.df$pi)
length(unique(merch10.df$pi))# verify num payment instruments
(merch_index.vec = 1:21) #merch==16, P2P to be deleted later

# cash vector by merch type
(cash_vol.vec = c(rep(NA, length(merch_index.vec))))
(cash_frac.vec = c(rep(NA, length(merch_index.vec))))
for (x in merch_index.vec) {
  cash_vol.vec[x] = nrow(subset(merch10.df, merch==x & pi == "Cash"))
  cash_frac.vec[x] = nrow(subset(merch10.df, merch==x & pi == "Cash"))/nrow(subset(merch10.df, merch==x))
}
cash_vol.vec#not needed, just for later verification
cash_frac.vec

# check vector by merch type
(check_vol.vec = c(rep(NA, length(merch_index.vec))))
(check_frac.vec = c(rep(NA, length(merch_index.vec))))
for (x in merch_index.vec) {
  check_vol.vec[x] = nrow(subset(merch10.df, merch==x & pi == "Check"))
  check_frac.vec[x] = nrow(subset(merch10.df, merch==x & pi == "Check"))/nrow(subset(merch10.df, merch==x))
}
check_vol.vec
check_frac.vec

# credit vector by merch type
(credit_vol.vec = c(rep(NA, length(merch_index.vec))))
(credit_frac.vec = c(rep(NA, length(merch_index.vec))))
for (x in merch_index.vec) {
  credit_vol.vec[x] = nrow(subset(merch10.df, merch==x & pi == "Credit"))
  credit_frac.vec[x] = nrow(subset(merch10.df, merch==x & pi == "Credit"))/nrow(subset(merch10.df, merch==x))
}
credit_vol.vec
credit_frac.vec

# debit vector by merch type
(debit_vol.vec = c(rep(NA, length(merch_index.vec))))
(debit_frac.vec = c(rep(NA, length(merch_index.vec))))
for (x in merch_index.vec) {
  debit_vol.vec[x] = nrow(subset(merch10.df, merch==x & pi == "Debit"))
  debit_frac.vec[x] = nrow(subset(merch10.df, merch==x & pi == "Debit"))/nrow(subset(merch10.df, merch==x))
}
debit_vol.vec
debit_frac.vec

# prepaid vector by merch type
(prepaid_vol.vec = c(rep(NA, length(merch_index.vec))))
(prepaid_frac.vec = c(rep(NA, length(merch_index.vec))))
for (x in merch_index.vec) {
  prepaid_vol.vec[x] = nrow(subset(merch10.df, merch==x & pi == "Prepaid"))
  prepaid_frac.vec[x] = nrow(subset(merch10.df, merch==x & pi == "Prepaid"))/nrow(subset(merch10.df, merch==x))
}
prepaid_vol.vec
prepaid_frac.vec

# BANP vector by merch type
(banp_vol.vec = c(rep(NA, length(merch_index.vec))))
(banp_frac.vec = c(rep(NA, length(merch_index.vec))))
for (x in merch_index.vec) {
  banp_vol.vec[x] = nrow(subset(merch10.df, merch==x & pi == "BANP"))
  banp_frac.vec[x] = nrow(subset(merch10.df, merch==x & pi == "BANP"))/nrow(subset(merch10.df, merch==x))
}
banp_vol.vec
banp_frac.vec

# OBBP vector by merch type
(obbp_vol.vec = c(rep(NA, length(merch_index.vec))))
(obbp_frac.vec = c(rep(NA, length(merch_index.vec))))
for (x in merch_index.vec) {
  obbp_vol.vec[x] = nrow(subset(merch10.df, merch==x & pi == "OBBP"))
  obbp_frac.vec[x] = nrow(subset(merch10.df, merch==x & pi == "OBBP"))/nrow(subset(merch10.df, merch==x))
}
obbp_vol.vec
obbp_frac.vec

# order vector by merch type
(order_vol.vec = c(rep(NA, length(merch_index.vec))))
(order_frac.vec = c(rep(NA, length(merch_index.vec))))
for (x in merch_index.vec) {
  order_vol.vec[x] = nrow(subset(merch10.df, merch==x & pi == "Order"))
  order_frac.vec[x] = nrow(subset(merch10.df, merch==x & pi == "Order"))/nrow(subset(merch10.df, merch==x))
}
order_vol.vec
order_frac.vec

# Mobile app vector by merch type
(app_vol.vec = c(rep(NA, length(merch_index.vec))))
(app_frac.vec = c(rep(NA, length(merch_index.vec))))
for (x in merch_index.vec) {
  app_vol.vec[x] = nrow(subset(merch10.df, merch==x & pi == "App"))
  app_frac.vec[x] = nrow(subset(merch10.df, merch==x & pi == "App"))/nrow(subset(merch10.df, merch==x))
}
app_vol.vec
app_frac.vec

# Account-to-account vector by merch type
(a2a_vol.vec = c(rep(NA, length(merch_index.vec))))
(a2a_frac.vec = c(rep(NA, length(merch_index.vec))))
for (x in merch_index.vec) {
  a2a_vol.vec[x] = nrow(subset(merch10.df, merch==x & pi == "A2A"))
  a2a_frac.vec[x] = nrow(subset(merch10.df, merch==x & pi == "A2A"))/nrow(subset(merch10.df, merch==x))
}
a2a_vol.vec
a2a_frac.vec

# other vector by merch type
(other_vol.vec = c(rep(NA, length(merch_index.vec))))
(other_frac.vec = c(rep(NA, length(merch_index.vec))))
for (x in merch_index.vec) {
  other_vol.vec[x] = nrow(subset(merch10.df, merch==x & pi == "Other"))
  other_frac.vec[x] = nrow(subset(merch10.df, merch==x & pi == "Other"))/nrow(subset(merch10.df, merch==x))
}
other_vol.vec
other_frac.vec

# Income deduction vector by merch type
(income_vol.vec = c(rep(NA, length(merch_index.vec))))
(income_frac.vec = c(rep(NA, length(merch_index.vec))))
for (x in merch_index.vec) {
  income_vol.vec[x] = nrow(subset(merch10.df, merch==x & pi == "Income"))
  income_frac.vec[x] = nrow(subset(merch10.df, merch==x & pi == "Income"))/nrow(subset(merch10.df, merch==x))
}
income_vol.vec
income_frac.vec

#verify the above sum up to 1
cash_frac.vec +check_frac.vec +credit_frac.vec +debit_frac.vec +prepaid_frac.vec +banp_frac.vec + obbp_frac.vec + order_frac.vec + app_frac.vec + a2a_frac.vec +other_frac.vec + income_frac.vec
#Verify total volume of payment sums up to nrow
(vol.vec = cash_vol.vec +check_vol.vec +credit_vol.vec +debit_vol.vec +prepaid_vol.vec +banp_vol.vec + obbp_vol.vec + order_vol.vec + app_vol.vec + a2a_vol.vec +other_vol.vec + income_vol.vec)
sum(vol.vec)
nrow(merch10.df)

# Make it a data frame
table(merch10.df$pi)
(method1.df = data.frame("Merchant" = merch_abv_fig.vec, "Cash" = 100*cash_frac.vec, "Check" = 100*check_frac.vec, "Order" = 100*order_frac.vec, "Credit" = 100*credit_frac.vec, "Debit" = 100*debit_frac.vec, "Prepaid" = 100*prepaid_frac.vec, "BANP" = 100*banp_frac.vec, "OBBP" = 100*obbp_frac.vec, "A2A" = 100*a2a_frac.vec, "App" = 100*app_frac.vec, "Income" = 100*income_frac.vec, "Other" = 100*other_frac.vec))

# Computing (but not adding) HHI for by merchant type [for fig 1]
dim(method1.df)
names(method1.df)
(hhi.vec = rep(0, length(merch_index.vec))) # initialize vector
for (x in merch_index.vec) { # HHI by merch type
  hhi.vec[x] =sum((method1.df[x, c(2:13) ])^2)# 12 payment methods
}
hhi.vec
#(method2.df = cbind(method1.df, hhi.vec))
#dim(method2.df)
#colnames(method2.df)
#method2.df = method2.df %>% rename(HHI = hhi.vec)

#delete row merchant 16 (P2P)
(method3.df = method1.df[-16, ])
dim(method3.df)

#Sort according to declining HHI# Not included in Table 2
#(method4.df = method3.df %>% arrange(desc(HHI)))

(digitm = matrix(c(0,0,1,1,1,1,1,1,1,1,1,1,1,1  ), nrow = 20, ncol = 13+1, byrow = T))
dim(digitm)
#
print(xtable(method3.df, digits = digitm), include.rownames = F, hline.after = c(0:20))

# Info for Table 2:
nrow(merch10.df)# num payments
length(unique(merch10.df$id))# num respondents

#End: Table 2: Share of payment methods by volume####

#++++++++++++++++++

#Begin: Table 3: HHI regressions####
# start with all 21 merchant (delete merch 16 later on)
names(stat1.df)
# merge with HHI
(hhi1.df = cbind(stat1.df, hhi.vec))
names(hhi1.df)
#del unneeded columns
hhi2.df = subset(hhi1.df, select = c("Merchant", "In_person", "Med.Val", "Med.Income", "hhi.vec"))
(hhi2.df = hhi2.df %>% rename(HHI = hhi.vec))#rename HHI

#add SD value, SD income, med age, sd age to data frame
names(hhi2.df)
hhi3.df = cbind(hhi2.df, merch_sd_val.vec, merch_sd_income.vec, merch_med_age.vec, merch_sd_age.vec)
names(hhi3.df)

# rename some column
hhi4.df = hhi3.df
names(hhi4.df)
hhi4.df = hhi4.df %>% rename(SD.Val = merch_sd_val.vec, SD.Income = merch_sd_income.vec, SD.Age = merch_sd_age.vec, Med.Age = merch_med_age.vec)
names(hhi4.df)

# delete P2P (merch 16)
dim(hhi4.df)
(hhi5.df = hhi4.df[-16, ])
str(hhi5.df)# verify all numerical and make merch a factor
hhi5.df$Merchant = factor(hhi5.df$Merchant,  levels = hhi5.df$Merchant)
levels(hhi5.df$Merchant)

# HHI regression model
hhi_model1  = HHI ~ Med.Val +SD.Val +In_person +Med.Income +SD.Income +Med.Age +SD.Age
#  
hhi1_reg = lm(hhi_model1, data = hhi5.df)
summary(hhi1_reg)
stargazer(hhi1_reg, no.space = TRUE)# LaTeX table code

#Discussion of table 3 (regression result)
# a $10k increase in buyers median HH income
names(hhi1_reg)
hhi1_reg$coefficients["Med.Income"]*10000# increase in HHI

# correlation with regression variables
cor(hhi5.df$HHI, hhi5.df$Med.Income)
cor(hhi5.df$HHI, hhi5.df$SD.Income)
cor(hhi5.df$HHI, hhi5.df$Med.Age)
cor(hhi5.df$HHI, hhi5.df$SD.Age)
cor(hhi5.df$HHI, hhi5.df$In_person)
  
#End: Table 3: HHI regressions####

#++++++++++++++++++

#Begin: Figure 1: HHI plot####
names(hhi5.df)
dim(hhi5.df)
hhi5.df
str(hhi5.df)

ggplot(hhi5.df, aes(x = reorder(Merchant, HHI), y = HHI)) +
  geom_col(fill = "cyan") + labs(x = "Merchant category", y = "Herfindahl-Hirschman concentration index (HHI)") + theme_minimal() + coord_flip() + scale_y_continuous(breaks = seq(0,5000,500)) +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) + geom_text(aes(label = comma(round(HHI,0))), hjust = 1.1, color = "black", size = 5) 

##Discussion of Figure 1 (extreme HHI: Hotel versus rent payments)
#
#Hotels concentration on card payments
round(method1.df[13, "Credit"] + method1.df[13, "Debit"] + method1.df[13, "Prepaid"],1)
#
#Taxi concentration on card payments
round(method1.df[9, "Credit"] + method1.df[9, "Debit"] + method1.df[9, "Prepaid"],1)
#
#Rent concentration on paper
round(method1.df[14, "Cash"] + method1.df[14, "Check"] + method1.df[14, "Order"],1)
#Rent concentration on card payments
round(method1.df[14, "Credit"] + method1.df[14, "Debit"] + method1.df[14, "Prepaid"],1)
#Rent concentration on bank accounts
round(method1.df[14, "BANP"] + method1.df[14, "OBBP"] + method1.df[14, "A2A"],1)
#Rent concentration on assorted methods
round(method1.df[14, "App"] + method1.df[14, "Income"] + method1.df[14, "Other"],1)

#End: Figure 1: HHI plot####

#++++++++++++++++++

#Begin: Table 4: Share of payments by dollar value####
names(merch10.df)

#Idenfifying a problem: merch==11 (contractor) has a payment for $180018 (see table 1) which is classified as "multiple PI" reclassfied as "other" (pi==13) in merch10.df
# explaining why 58% by value paid via "other" to contractors (merch==11):
# go back before combo payment methods (pi==0) was reclassified as "other" (pi==13)
table(merch7.df$pi)
names(merch7.df)
#focus on pi==0 and pi==13 to find out which ones have high dollar values
#
#check val of payments made by merch==11 and with pi==0 (combo)
subset(merch7.df, (merch==11 & pi==0))
#
#check val of payments made by merch==11 and with pi==13 (other)
subset(merch7.df, merch==11 & pi==13)
# => the combo high amount biases => check average amount for merch==11
summary(subset(merch7.df, merch==11)$amnt)# => highly skewed
# order these to find the second-highest amount for merch==11
subset(merch7.df, merch==11)$amnt[order(subset(merch7.df, merch==11)$amnt)]
# the 2nd-largest spending is 18k not 180k
# => This payment should be removed as an extreme outliar from Table 4!

# remove this outliar payment from merch10.df

m10.df = merch10.df
# locate this obs
m10.df[which.max(m10.df$amnt), ]
dim(m10.df)# before removal num of payments
m11.df = m10.df[-which.max(m10.df$amnt), ]
dim(m11.df)# after removal

# cash vector by merch type (dollar value)
(cash_val.vec = c(rep(NA, length(merch_index.vec))))
(cash_val_frac.vec = c(rep(NA, length(merch_index.vec))))
for (x in merch_index.vec) {
  cash_val.vec[x] = sum(subset(m11.df, merch==x & pi == "Cash")$amnt)
  cash_val_frac.vec[x] = sum(subset(m11.df, merch==x & pi == "Cash")$amnt)/sum(subset(m11.df, merch==x)$amnt)
}
cash_val.vec#not needed, just for later verification
cash_val_frac.vec

# check vector by merch type (dollar value)
(check_val.vec = c(rep(NA, length(merch_index.vec))))
(check_val_frac.vec = c(rep(NA, length(merch_index.vec))))
for (x in merch_index.vec) {
  check_val.vec[x] = sum(subset(m11.df, merch==x & pi == "Check")$amnt)
  check_val_frac.vec[x] = sum(subset(m11.df, merch==x & pi == "Check")$amnt)/sum(subset(m11.df, merch==x)$amnt)
}
check_val.vec#not needed, just for later verification
check_val_frac.vec

# Order vector by merch type (dollar value)
(order_val.vec = c(rep(NA, length(merch_index.vec))))
(order_val_frac.vec = c(rep(NA, length(merch_index.vec))))
for (x in merch_index.vec) {
  order_val.vec[x] = sum(subset(m11.df, merch==x & pi == "Order")$amnt)
  order_val_frac.vec[x] = sum(subset(m11.df, merch==x & pi == "Order")$amnt)/sum(subset(m11.df, merch==x)$amnt)
}
order_val.vec#not needed, just for later verification
order_val_frac.vec

# credit vector by merch type (dollar value)
(credit_val.vec = c(rep(NA, length(merch_index.vec))))
(credit_val_frac.vec = c(rep(NA, length(merch_index.vec))))
for (x in merch_index.vec) {
  credit_val.vec[x] = sum(subset(m11.df, merch==x & pi == "Credit")$amnt)
  credit_val_frac.vec[x] = sum(subset(m11.df, merch==x & pi == "Credit")$amnt)/sum(subset(m11.df, merch==x)$amnt)
}
credit_val.vec#not needed, just for later verification
credit_val_frac.vec

# debit vector by merch type (dollar value)
(debit_val.vec = c(rep(NA, length(merch_index.vec))))
(debit_val_frac.vec = c(rep(NA, length(merch_index.vec))))
for (x in merch_index.vec) {
  debit_val.vec[x] = sum(subset(m11.df, merch==x & pi == "Debit")$amnt)
  debit_val_frac.vec[x] = sum(subset(m11.df, merch==x & pi == "Debit")$amnt)/sum(subset(m11.df, merch==x)$amnt)
}
debit_val.vec#not needed, just for later verification
debit_val_frac.vec

# prepaid vector by merch type (dollar value)
(prepaid_val.vec = c(rep(NA, length(merch_index.vec))))
(prepaid_val_frac.vec = c(rep(NA, length(merch_index.vec))))
for (x in merch_index.vec) {
  prepaid_val.vec[x] = sum(subset(m11.df, merch==x & pi == "Prepaid")$amnt)
  prepaid_val_frac.vec[x] = sum(subset(m11.df, merch==x & pi == "Prepaid")$amnt)/sum(subset(m11.df, merch==x)$amnt)
}
prepaid_val.vec#not needed, just for later verification
prepaid_val_frac.vec

# banp vector by merch type (dollar value)
(banp_val.vec = c(rep(NA, length(merch_index.vec))))
(banp_val_frac.vec = c(rep(NA, length(merch_index.vec))))
for (x in merch_index.vec) {
  banp_val.vec[x] = sum(subset(m11.df, merch==x & pi == "BANP")$amnt)
  banp_val_frac.vec[x] = sum(subset(m11.df, merch==x & pi == "BANP")$amnt)/sum(subset(m11.df, merch==x)$amnt)
}
banp_val.vec#not needed, just for later verification
banp_val_frac.vec

# obbp vector by merch type (dollar value)
(obbp_val.vec = c(rep(NA, length(merch_index.vec))))
(obbp_val_frac.vec = c(rep(NA, length(merch_index.vec))))
for (x in merch_index.vec) {
  obbp_val.vec[x] = sum(subset(m11.df, merch==x & pi == "OBBP")$amnt)
  obbp_val_frac.vec[x] = sum(subset(m11.df, merch==x & pi == "OBBP")$amnt)/sum(subset(m11.df, merch==x)$amnt)
}
obbp_val.vec#not needed, just for later verification
obbp_val_frac.vec

# a2a vector by merch type (dollar value)
(a2a_val.vec = c(rep(NA, length(merch_index.vec))))
(a2a_val_frac.vec = c(rep(NA, length(merch_index.vec))))
for (x in merch_index.vec) {
  a2a_val.vec[x] = sum(subset(m11.df, merch==x & pi == "A2A")$amnt)
  a2a_val_frac.vec[x] = sum(subset(m11.df, merch==x & pi == "A2A")$amnt)/sum(subset(m11.df, merch==x)$amnt)
}
a2a_val.vec#not needed, just for later verification
a2a_val_frac.vec

# app vector by merch type (dollar value)
(app_val.vec = c(rep(NA, length(merch_index.vec))))
(app_val_frac.vec = c(rep(NA, length(merch_index.vec))))
for (x in merch_index.vec) {
  app_val.vec[x] = sum(subset(m11.df, merch==x & pi == "App")$amnt)
  app_val_frac.vec[x] = sum(subset(m11.df, merch==x & pi == "App")$amnt)/sum(subset(m11.df, merch==x)$amnt)
}
app_val.vec#not needed, just for later verification
app_val_frac.vec

# Income vector by merch type (dollar value)
(income_val.vec = c(rep(NA, length(merch_index.vec))))
(income_val_frac.vec = c(rep(NA, length(merch_index.vec))))
for (x in merch_index.vec) {
  income_val.vec[x] = sum(subset(m11.df, merch==x & pi == "Income")$amnt)
  income_val_frac.vec[x] = sum(subset(m11.df, merch==x & pi == "Income")$amnt)/sum(subset(m11.df, merch==x)$amnt)
}
income_val.vec#not needed, just for later verification
income_val_frac.vec

# other vector by merch type (dollar value)
(other_val.vec = c(rep(NA, length(merch_index.vec))))
(other_val_frac.vec = c(rep(NA, length(merch_index.vec))))
for (x in merch_index.vec) {
  other_val.vec[x] = sum(subset(m11.df, merch==x & pi == "Other")$amnt)
  other_val_frac.vec[x] = sum(subset(m11.df, merch==x & pi == "Other")$amnt)/sum(subset(m11.df, merch==x)$amnt)
}
other_val.vec#not needed, just for later verification
other_val_frac.vec

# verifying sum to 1 (for each merchant)
cash_val_frac.vec +check_val_frac.vec +order_val_frac.vec +credit_val_frac.vec +debit_val_frac.vec +prepaid_val_frac.vec +banp_val_frac.vec +obbp_val_frac.vec +a2a_val_frac.vec +app_val_frac.vec +income_val_frac.vec +other_val_frac.vec 

# Make it a data frame
(value1.df = data.frame("Merchant" = merch_abv_fig.vec, "Cash" = 100*cash_val_frac.vec, "Check" = 100*check_val_frac.vec, "Order" = 100*order_val_frac.vec, "Credit" = 100*credit_val_frac.vec, "Debit" = 100*debit_val_frac.vec, "Prepaid" = 100*prepaid_val_frac.vec, "BANP" = 100*banp_val_frac.vec, "OBBP" = 100*obbp_val_frac.vec, "A2A" = 100*a2a_val_frac.vec, "App" = 100*app_val_frac.vec, "Income" = 100*income_val_frac.vec, "Other" = 100*other_val_frac.vec))
dim(value1.df)

# verify sum up to 100% X 21 merchants
(sum(value1.df[, 2:13]))
# verify the values sum up to total amount
sum(m11.df$amnt)# total dollar amount
temp_value.df = data.frame(cash_val.vec, check_val.vec, order_val.vec, credit_val.vec, debit_val.vec, prepaid_val.vec, banp_val.vec, obbp_val.vec, a2a_val.vec, app_val.vec, income_val.vec, other_val.vec)
(sum(temp_value.df[, 1:12]))

# Computing (but not adding) HHI value for by merchant type [for fig 2]
dim(value1.df)
names(value1.df)
merch_index.vec
length(merch_index.vec)
(hhi_val.vec = rep(0, length(merch_index.vec))) # initialize vector
for (x in merch_index.vec) { # HHI value by merch type
  hhi_val.vec[x] =sum((value1.df[x, c(2:13) ])^2)# 12 payment methods
}
hhi_val.vec

# del merchant 16 (p2p)
value1.df
(value2.df = value1.df[-16, ])
# LaTeX table
(digitm = matrix(c(0,0,1,1,1,1,1,1,1,1,1,1,1,1  ), nrow = 20, ncol = 13+1, byrow = T))
dim(digitm)
#
print(xtable(value2.df, digits = digitm), include.rownames = F, hline.after = c(0:20))

# Info for Table 4:
nrow(m11.df)# num payments
length(unique(m11.df$id))# num respondents

#End: Table 4: Share of payments by dollar value####

#++++++++++++++++++

#Begin: Table 5: HHI by value and vol-val correlations####

# correlations between table 4 (value) and table 2 (volume): Pearson and spearman by merchant category
# recall table 2: 
method3.df# volume
dim(method3.df)
str(method3.df)
#
value2.df# value
dim(value2.df)
str(value2.df)

(corr_pearson.vec = rep(0, nrow(method3.df))) # initialize vector
(corr_distance.vec = rep(0, nrow(method3.df))) # initialize vector
for (x in 1:nrow(method3.df)) { # HHI value by merch type
  corr_pearson.vec[x] = cor(as.numeric(method3.df[x, 2:13]), as.numeric(value2.df[x, 2:13]), method = "pearson")
#corr_spearman.vec[x] = cor(as.numeric(method3.df[x, 2:13]), as.numeric(value2.df[x, 2:13]), method = "spearman")}
corr_distance.vec[x] = dcor(as.numeric(method3.df[x, 2:13]), as.numeric(value2.df[x, 2:13]))}
corr_pearson.vec
#corr_spearman.vec #Not good for non-monotonic correlations
corr_distance.vec

# making a data frame for table 5 (HHI value & correlation coeff)
# recall HHI volume
hhi.vec
# delete merch 16
hhi.vec[16]
(hhi2.vec = hhi.vec[-16])
# recall HHI value
hhi_val.vec
# delete merch 16
hhi_val.vec[16]
(hhi_val2.vec = hhi_val.vec[-16])

(corr1.df = data.frame("Merchant" = merch_abv_fig.vec[-16], "Pearson" = corr_pearson.vec, "Distance" = corr_distance.vec, "Volume" = hhi2.vec, "Value" = hhi_val2.vec, "Val-Vol" = hhi_val2.vec - hhi2.vec))

# convert to LaTeX
dim(corr1.df)
(digitm = matrix(c(0,2,2,2,0,0,0), nrow = 20, ncol = 6+1, byrow = T))
dim(digitm)
#
print(xtable(corr1.df, digits = digitm), include.rownames = F, hline.after = c(0))

#Examples provided below table 5 for merchant 1 (groceries)
method3.df[1, ]# volume
round(as.numeric(method3.df[1, 2:13]),1)
sum(round(as.numeric(method3.df[1, 2:13]),1))

value2.df[1, ]# value
round(as.numeric(value2.df[1, 2:13]),1)
sum(round(as.numeric(value2.df[1, 2:13]),1))

# discussion of low correlations in table 5 (Education merchant 20):
method3.df[19, 1:4]
round(sum(method3.df[19, 2:4]),1)# paper methods
#
value2.df[19, 1:4]
round(sum(value2.df[19, 2:4]),1)# paper methods
# card
method3.df[19, 5:7]
round(sum(method3.df[19, 5:7]),1)# card methods
#
value2.df[19, 5:7]
round(sum(value2.df[19, 5:7]),1)# card methods
# bank
method3.df[19, 8:10]
round(sum(method3.df[19, 8:10]),1)# bank methods
#
value2.df[19, 8:10]
round(sum(value2.df[19, 8:10]),1)# bank methods

# discussion of low correlations in table 5 (Donations merchant 17):
method3.df[16, 1:4]
round(sum(method3.df[16, 2:4]),1)# paper methods
#
value2.df[16, 1:4]
round(sum(value2.df[16, 2:4]),1)# paper methods
# card
method3.df[16, 5:7]
round(sum(method3.df[16, 5:7]),1)# card methods
#
value2.df[16, 5:7]
round(sum(value2.df[16, 5:7]),1)# card methods
# bank
method3.df[16, 8:10]
round(sum(method3.df[16, 8:10]),1)# bank methods
#
value2.df[16, 8:10]
round(sum(value2.df[16, 8:10]),1)# bank methods


# discussion of largest difference between HHIs (merchant 9) in table 5
method3.df[9, 5:7]# cards volume
round(sum(method3.df[9, 5:7]),1)# 
value2.df[9, 5:7]# cards value
round(sum(value2.df[9, 5:7]),1)# 
#
method3.df[9, 2:4]# paper volume
round(sum(method3.df[9, 2:4]),1)# 
value2.df[9, 2:4]# paper value
round(sum(value2.df[9, 2:4]),1)# 


#computing correlation between HHI vol and HHI val (discussion after table 5)
hhi2.vec# HHI by volume
length(hhi2.vec)
hhi_val2.vec# HHI by value
length(hhi_val2.vec)

(hhi_cor_pearson = round(cor(hhi2.vec, hhi_val2.vec, method = "pearson"),2))
(hhi_cor_distance = round(dcor(hhi2.vec, hhi_val2.vec),2))


#End: Table 5: HHI by value and vol-val correlations####

#++++++++++++++++++


#Begin: HHI tree (cancelled)####
# class(hhi5.df$HHI)
# set.seed(1955)# to be able to reproduce the rpart CV below
# tree1 = rpart(hhi_model1, data = hhi5.df, method = "anova", control = rpart.control(cp = 0.001))# Extremely-long tree first, then prune it
# #Below, plot a tree (Note: Longer than optimal, but needed for later prunning and redrawing). 
# prp(tree1, type = 3, box.palette = "auto", extra = 100, under = T, tweak = 1.0, varlen = 0, faclen = 0)#faclet=0 avoids abbreviations, tweak for char size
# #now search for optimal cp, rpart has cp table built in
# plotcp(tree1)# plot cp: 
# #names(tree1)
# tree1$cptable # List cp, number of splits and errors
# # Below, I choose cp to use for pruning. There are 2 methods:
# #Method 1: Use the plotcp and pick the highest relative error below the dashed line => may lead to a larger tree than needed.
# #
# #Method 2 (1-SE rule, preferred): Pick the lowest xerror in the cptable. Add the corresponding xstd. Then, pick the cp in the cptable with xerror < or = to this sum.
# #
# (cp.choice = tree1$cptable[13, "CP"]) # Corresponds to 9 splits (just for demonstration)
# prune1 = prune.rpart(tree1, cp=cp.choice)
# # plot prunned tree
# prp(prune1, type = 3, box.palette = "auto", legend.x=NA, legend.y=NA, extra = 100, under = T, varlen = 0, faclen = 0, Margin = 0.0, digits = -2, tweak = 0.9, cex = 1.0)
# #Note: Maximize the Plots window in RStudio before prp
# #faclen = 0 → no abbreviation (print full factor level names)
# #faclen = n → abbreviate factor levels to n characters tweak for char size
# #tweak also controls the fontsize (in addition or replacement for cex = 1.x, but it looks good this way)
# #varlen = -1 (default) → use full variable names
# #varlen = 0 → shortest unique abbreviation
# #varlen = n → truncate variable names to at most n characters
# # extra = 0 → show the predicted class only (default)
# #extra = 1 → show number of observations in the node
# #extra = 2 → show class probabilities
# #extra = 4 → show class percentages
# #extra = 5 → show class and probability
# #extra = 6 → show class, probability, and N
# #extra = 7 → show class, N, and percentage
# # For regression trees:  extra = 101 → show fitted value; extra = 104 → show fitted value and number of observations
# Information for Fig 3 (tree)

#End: HHI tree (cancelled)####

#++++++++++++++++++


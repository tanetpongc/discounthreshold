#This file will estimate utility choice model using logistic (multinomial if several brands are considered) regression

###################################
######## LOAD PACKAGE #############
###################################
library(data.table)
library(mlogit) 

###################################
########### LOAD DATA #############
###################################
df_choice <- fread("../gen/df_choice.csv")
df_choice[, bought := as.logical(choice)]
df_choice <- as.data.frame(df_choice)  # Convert to dataframe to fit with mlogit

df <- mlogit.data(df_choice, choice = "bought", shape = "long", chid.var = "chid", id = "cust", alt.var = "brand_alt")


###################################
############ ESTIMATE #############
###################################

#Random parameters are purely assumed for this simulated data
# Choice model where there is asymmetry of final price (gain VS loss) in convenience store
m_utilitychoice <- mlogit(bought ~ hyperregprice + hyperdiscount 
                          + convefinalpricegain + convefinalpriceloss + convefinalprice 
                          + superfinalprice + Nonfocal_intercept + loyal_nonfocal |-1, data = df, panel = TRUE, rpar = c(Nonfocal_intercept = "n",loyal_nonfocal = "n"), correlation = FALSE, R = 100, Halton = NA)
summary(m_utilitychoice)
#This file will return final aggregate data for each brand in each week at each format


###################################
######## LOAD PACKAGE #############
###################################
library(data.table)
library(ggplot2)
library(car) #function for testing coefficient equivalence
library(lmtest) #function for testing nonlinearity by likelihood ratio

###################################
########### LOAD DATA #############
###################################
df <- fread("../data/df.csv")

#Assuming we are interested in category z
df <- df[category == "z",]


#########################################################
########### OWN VARIABLE OPERATIONALIZATION #############
#########################################################

#Calculate (market-weighted average) brand variables
df[, finalspending := regprice - discount]
df[, regpriceperunit := regprice/quantity]
df[, finalpriceperunit := finalspending /quantity]
df[, discountperunit :=  discount/quantity]

# Calculate weekly sales by brand
weeklysalesbybrand <- df[, .(totalsales = sum(quantity), LL = .N), by = .(week_nr,holiday, brand, format)]
#LL can be calculated by uniqueN(product_id) to count distinct product id

# Merge df_complete with weeklysalesbybrand and calculate weights
df_weightcalc <- merge(df, weeklysalesbybrand, by = c("week_nr","holiday", "brand", "format"))
df_weightcalc[, wp := quantity/totalsales]

# Calculate weighted averages
df_weightcalc[, avgregpriceperunit := regpriceperunit * wp]
df_weightcalc[, avgfinalprice := finalpriceperunit * wp]
df_weightcalc[, avgdiscount := discountperunit * wp]

# Aggregate by week_nr, brand, and format
df_bybrand <- df_weightcalc[, .(totalvolume = sum(quantity), 
                                totalvalue = sum(finalspending), 
                                LL = mean(LL), 
                                avgregprice = sum(avgregpriceperunit), 
                                avgfinalprice = sum(avgfinalprice), 
                                avgdiscount = sum(avgdiscount)), by = .(week_nr,holiday, brand, format)]

rm(weeklysalesbybrand,df_weightcalc)


df_bybrand[, depth := avgdiscount/avgregprice]
df_bybrand[, nondepth := 1 - depth]

###########################################################
########### CROSS VARIABLE OPERATIONALIZATION #############
###########################################################

#We calculate (1) log-transformed (market-weighted average) competitor brand variables (2) log-transformed lag variable (3) Gaussian-copula correction term  (4) first difference of log-transformed variables for aggregate model estimation and (5) (Final price, Regular price, Discounts) Gain and loss with respect to what offered the week before

#Calculate (market-weighted average) related variables of competitors and lag variable
#We need for loop to construct competitor of each brand, specify further
distinctformat<-unique(as.factor(df_bybrand$format))
distinctbrand<-unique(as.factor(df_bybrand$brand))
distinctweek<-unique(as.factor(df_bybrand$week_nr))

nbrand<-length(distinctbrand)
nweek<-length(distinctweek)
nformat <- length(distinctformat)

#Set up function for calculate lag variable
lag_1 <- function(x, k = 1) head(c(rep(NA, k), x), length(x))

#Function for copula correction term  following Park and Gupta 2012 see more https://github.com/hannesdatta/marketingtools
make_copula <- function(x) {
  if (length(unique(x)) == 1) return(as.numeric(rep(NA, length(x))))
  return(ifelse(ecdf(x)(x) == 1, qnorm(1 - .0000001), qnorm(ecdf(x)(x))))
}


#We calculate (market-weighted average) competitors info for each brand

gen_comp_lag_format <- function(df_bybrand, distinctbrand, format, nbrand, nweek) {
  results_list <- list()
  
  for (i in 1:length(distinctbrand)) {
    # Creating a copy of the relevant subset of df_bybrand
    df_own = copy(df_bybrand[brand == distinctbrand[i] & format == format])
    df_competitor = df_bybrand[brand != distinctbrand[i] & format == format]
    
    # create copula of log variable to mitigate potential endogeneity by format and brand
    df_own[, `:=` (
      cop_logavgregprice = make_copula(log(avgregprice)),
      cop_logavgdiscount = make_copula(log(avgdiscount)),
      cop_logavgfinalprice = make_copula(log(avgfinalprice)),
      cop_lognondepth = make_copula(log(nondepth))
    )]
    
    
    # Calculate weekly sales by competing brand
    weeklysalesbycompetingbrand = df_competitor[, .(totalmarketvolume = sum(totalvolume)), by = .(week_nr)]
    # Calculate competitor weight and weighted averages
    df_competitorweightcalc = df_competitor[weeklysalesbycompetingbrand, on = "week_nr"]
    df_competitorweightcalc[, wp := totalvolume / totalmarketvolume]
    df_competitorweightcalc[, `:=` (
      avgcompLL = LL * wp,
      avgcompregprice = avgregprice * wp,
      avgcompfinalprice = avgfinalprice * wp,
      avgcompdiscount = avgdiscount * wp
    )]
    
    # Summarize competitor info
    df_competitorinfo = df_competitorweightcalc[, .(
      avgcompLL = sum(avgcompLL),
      avgcompregprice = sum(avgcompregprice),
      avgcompfinalprice = sum(avgcompfinalprice),
      avgcompdiscount = sum(avgcompdiscount)
    ), by = .(week_nr)]
    
    # Merge with own brand data
    df_own = df_own[df_competitorinfo, on = "week_nr"]
    
    # Add lagged variables
    df_own <- df_own[order(week_nr)] #Make sure that week_nr is ordered correctly before applying the lag
    lag_vars = c("totalvolume", "avgfinalprice", "avgregprice", "nondepth", "avgcompfinalprice")
    for (var in lag_vars) {
      df_own[, paste0(var, "1") := shift(get(var), 1, type = "lag"), by = .(brand, format)]
    }
    
    df_own <- na.omit(df_own)
    
    # Store the processed df_own in the results list
    results_list[[i]] <- df_own
  }
  
  # Combine all processed df_own into one data.table
  return(rbindlist(results_list, use.names = TRUE))
}

df_bybrand_formats <- gen_comp_lag_format(df_bybrand, distinctbrand, format, nbrand, nweek)

# calculate first difference for estimation
df_bybrand_formats[, `:=` (
  dlogtotalvolume = log(totalvolume) - log(totalvolume1),
  dlogavgfinalprice = log(avgfinalprice) - log(avgfinalprice1),
  dlogavgregprice = log(avgregprice) - log(avgregprice1),
  dlognondepth = log(nondepth) - log(nondepth1),
  dlogavgcompfinalprice = log(avgcompfinalprice) - log(avgcompfinalprice1)
)]

# calculate separate price/discount gain/loss for indicator function (simplified version of aggregate model)
df_bybrand_formats[, `:=` (
  dlogavgfinalprice_loss = fifelse(dlogavgfinalprice > 0, dlogavgfinalprice, 0),
  dlogavgfinalprice_gain = fifelse(dlogavgfinalprice < 0, dlogavgfinalprice, 0),
  dlogavgregprice_loss = fifelse(dlogavgregprice > 0, dlogavgregprice, 0),
  dlogavgregprice_gain = fifelse(dlogavgregprice < 0, dlogavgregprice, 0),
  dlognondepth_loss = fifelse(dlognondepth > 0, dlognondepth, 0),
  dlognondepth_gain = fifelse(dlognondepth < 0, dlognondepth, 0) 
)]

#############################################
########### Model Free Evidence #############
#############################################
#Plot model-free (of Brand B)
ggplot(df_bybrand_formats[brand == "B",], aes(x = dlognondepth, y = dlogtotalvolume, color = format)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title ="Effect of discounts change on sales of brand B", x = "(%) Change of (non) discount depth", y = "(%) Change of volume sold", color = "Format")

ggplot(df_bybrand_formats[brand == "B",], aes(x = dlogavgfinalprice, y = dlogtotalvolume, color = format)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title ="Effect of final prices change on sales of brand B", x = "(%) Change of final price", y = " (%) Change of volume sold", color = "Format")

ggplot(df_bybrand_formats[brand == "B",], aes(x = dlogavgregprice, y = dlogtotalvolume, color = format)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title ="Effect of regular prices change on sales of brand B", x = "(%) Change of regular price", y = " (%) Change of volume sold", color = "Format")


###############################################
########### Export Aggregate Data #############
###############################################
#export aggregate dataset
write.csv(df_bybrand_formats,'../gen/df_bybrand_aggregate.csv')


rm(list=ls())
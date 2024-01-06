#This file will return table of result indicating which model is appropriate (non/linear model with/without discounts)

###################################
######## LOAD PACKAGE #############
###################################
library(data.table)
library(car)
library(lmtest) #function for testing nonlinearity by likelihood ratio



##########################################################################################
########## FUNCTION: Test effectiveness between regular price and discounts ##############
##########################################################################################
#Create function for testing discount and reg price effectiveness across formats
test_discount_coef <- function(aggregate_df) {
  
  distinctbrands <- unique(aggregate_df$brand)
  distinctformats <- unique(aggregate_df$format)
  
  df_discount_coef_test <- data.table()  # Create an empty data.table
  
  for (format_val in distinctformats) {
    for (brand_val in distinctbrands) {
      subset_df <- aggregate_df[brand == brand_val & format == format_val]
      
      if (nrow(subset_df) > 0) {  # Check if there are data for this brand and format
        
        lm_discount <- lm(dlogtotalvolume ~ dlogavgregprice + dlognondepth + dlogavgcompfinalprice + 
                            log(totalvolume1) + log(avgregprice1) + log(nondepth1) + 
                            cop_logavgregprice + cop_lognondepth + holiday, 
                          data = subset_df)
        
        # Test coefficient between price before discount
        coeftest <- linearHypothesis(lm_discount, "dlogavgregprice - dlognondepth = 0")
        
        # Store test results in the data.table
        df_discount_coef_test <- rbind(df_discount_coef_test, list(
          format = format_val,
          brand = brand_val,
          Coef_Regprice = coef(summary(lm_discount))[["dlogavgregprice", "Estimate"]],
          Sd_Regprice = coef(summary(lm_discount))[["dlogavgregprice", "Std. Error"]],
          Coef_NonDiscount = coef(summary(lm_discount))[["dlognondepth", "Estimate"]],
          Sd_NonDiscount = coef(summary(lm_discount))[["dlognondepth", "Std. Error"]],
          p_coef_test = round(coeftest$`Pr(>F)`[2], 4),
          R2_of_model = round(summary(lm_discount)$r.squared, 4)
        ), fill = TRUE)
      }
    }
  }
  
  # Add the decision column based on p-value
  df_discount_coef_test[, pricedecision := fifelse(`p_coef_test` < 0.05, 'discount', 'finalprice')]
  
  return(df_discount_coef_test)
}

#####################################################
########## FUNCTION: Test NONLINEARITY ##############
#####################################################
#Create function for evidence of nonlinear term
test_nonlinearity_both <- function(aggregate_df, discount_coef_result, critical_p){
  
  # Merge on both brand and format
  df_merge_result <- merge(aggregate_df, discount_coef_result, by = c("brand", "format"))
  
  # Split data based on price decision for each brand-format combination
  df_finalprice <- df_merge_result[df_merge_result$pricedecision == 'finalprice', ]
  distinctbrandformat_finalprice <- unique(df_finalprice[, .(brand, format)])
  
  df_discount <- df_merge_result[df_merge_result$pricedecision == 'discount', ]
  distinctbrandformat_discount <- unique(df_discount[, .(brand, format)])
  
  # Initialize data frames for results
  df_finalprice_lr <- data.frame(matrix(NA, ncol = 5 , nrow = nrow(distinctbrandformat_finalprice)))  
  df_discount_lr <- data.frame(matrix(NA, ncol = 5 , nrow = nrow(distinctbrandformat_discount)))
  
  # Process final price data
  for (i in 1:nrow(distinctbrandformat_finalprice)) {
    brand_val <- distinctbrandformat_finalprice$brand[i]
    format_val <- distinctbrandformat_finalprice$format[i]
    
    lm_finalprice <- lm(dlogtotalvolume ~ dlogavgfinalprice + dlogavgcompfinalprice + log(totalvolume1) + log(avgfinalprice1) + cop_logavgfinalprice + holiday,
                        data = df_finalprice[brand == brand_val & format == format_val])
    
    lm_finalprice_price2 <- lm(dlogtotalvolume ~ dlogavgfinalprice + dlogavgcompfinalprice + log(totalvolume1) + log(avgfinalprice1) + cop_logavgfinalprice + holiday + I(dlogavgfinalprice^2),
                               data = df_finalprice[brand == brand_val & format == format_val])
    
    test_price <- lrtest(lm_finalprice, lm_finalprice_price2)
    
    df_finalprice_lr[i,] <- c(brand_val, format_val, round(test_price$`Pr(>Chisq)`[2],3), "NA", "NA")
  }
  
  # Process discount data
  for (i in 1:nrow(distinctbrandformat_discount)) {
    brand_val <- distinctbrandformat_discount$brand[i]
    format_val <- distinctbrandformat_discount$format[i]
    
    lm_discount <- lm(dlogtotalvolume ~ dlogavgregprice + dlognondepth + dlogavgcompfinalprice + log(totalvolume1) + log(avgregprice1) + log(nondepth1) + cop_logavgregprice + cop_lognondepth  + holiday,
                      data = df_discount[brand == brand_val & format == format_val])
    
    lm_discount_price2 <- lm(dlogtotalvolume ~ dlogavgregprice + dlognondepth + dlogavgcompfinalprice + log(totalvolume1) + log(avgregprice1) + log(nondepth1) + cop_logavgregprice + cop_lognondepth  + holiday + I(dlogavgregprice^2),
                             data = df_discount[brand == brand_val & format == format_val])
    
    lm_discount_discount2 <- lm(dlogtotalvolume ~ dlogavgregprice + dlognondepth + dlogavgcompfinalprice + log(totalvolume1) + log(avgregprice1) + log(nondepth1) + cop_logavgregprice + cop_lognondepth  + holiday + I(dlognondepth^2),
                                data = df_discount[brand == brand_val & format == format_val])
    
    lm_discount_pricediscount2 <- lm(dlogtotalvolume ~ dlogavgregprice + dlognondepth + dlogavgcompfinalprice + log(totalvolume1) + log(avgregprice1) + log(nondepth1) + cop_logavgregprice + cop_lognondepth  + holiday + I(dlogavgregprice^2) + I(dlognondepth^2),
                                     data = df_discount[brand == brand_val & format == format_val])
    
    test_price <- lrtest(lm_discount, lm_discount_price2)
    test_discount <- lrtest(lm_discount, lm_discount_discount2)
    test_price_discount <- lrtest(lm_discount, lm_discount_pricediscount2)
    
    df_discount_lr[i,] <- c(brand_val, format_val, round(test_price$`Pr(>Chisq)`[2],3), round(test_discount$`Pr(>Chisq)`[2],3), round(test_price_discount$`Pr(>Chisq)`[2],3))
  }
  
  # Combine results and return
  df_nonlinearity_test <- data.table(rbind(df_discount_lr, df_finalprice_lr))
  colnames(df_nonlinearity_test) <- c('brand', 'format', 'p-value-price^2', 'p-value-discount^2', 'p-value-priceanddiscount^2')
  
  # Give the decision in this nonlinearity step
  # Initialize default decision
  df_nonlinearity_test[, Decision := NA_character_]
  
  df_nonlinearity_test[`p-value-price^2` < critical_p & `p-value-discount^2` < critical_p & `p-value-priceanddiscount^2` < critical_p, Decision := "RegDisNonlinear (mod 6)"]
  df_nonlinearity_test[`p-value-price^2` >= critical_p & `p-value-discount^2` < critical_p, Decision := "DisNonlinear (mod 5)"]
  df_nonlinearity_test[`p-value-price^2` < critical_p & `p-value-discount^2` >= critical_p, Decision := "RegPriceNonlinear (mod 4)"]
  df_nonlinearity_test[`p-value-price^2` >= critical_p & `p-value-discount^2` >= critical_p & `p-value-priceanddiscount^2` < critical_p, Decision := "RegDisLinear (mod 3)"]
  df_nonlinearity_test[`p-value-price^2` < critical_p & `p-value-discount^2` == "NA" & `p-value-priceanddiscount^2` == "NA", Decision := "NonlinearFinal (mod 2)"]
  df_nonlinearity_test[`p-value-price^2` >= critical_p & `p-value-discount^2` == "NA" & `p-value-priceanddiscount^2` == "NA", Decision := "FinalLinear (mod 1)"]
  
  return(df_nonlinearity_test)
}

###################################
########## LOAD DATA ##############
###################################
df <- fread("../gen/df_bybrand_aggregate.csv")

#################################################################
########## EMPIRICAL TESTING ON OUR SIMULATED DATA ##############
#################################################################
# Test reg price and discount effectiveness
df_results <- test_discount_coef(df)

# Test nonlinearity
df_nonlinearresults <- test_nonlinearity_both(df, df_results, critical_p = 0.05)

################################################
########## EXPORT TESTING RESULTS ##############
################################################
write.csv(df_nonlinearresults,'../gen/empiricaltest_result.csv')



rm(list=ls())
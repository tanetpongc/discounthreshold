#This file will return final customer choice data for customer who shop at different format at different times


###################################
######## LOAD PACKAGE #############
###################################
library(data.table) #for set DT checking brand
library(mlogit) #for making choice dataset

#For our case, we chose one brand to study with choice model, we need to construct attribute based on brand
#Instead of treating different brand diferrent, we simply assume between focal brand (brand we are interested) and nonfocal brand

###################################
########### LOAD DATA #############
###################################
df <- fread("../data/df.csv")

brand_we_interested <- "D"

#############################################
#####CALCULATE STORE LEVEL DATA #############
#############################################
# Filter the data for brand "D" and then group by week_nr
brand_focal_week_data <- df[brand == brand_we_interested, .(formatvisit = uniqueN(format)), by = .(week_nr)]
weeks_with_allvisits <- brand_focal_week_data[formatvisit >= 3, week_nr]
df_store <- df[week_nr %in% weeks_with_allvisits]
rm(brand_focal_week_data)

df_store[, brand_focal := ifelse(brand == brand_we_interested, "Focal", "Nonfocal")]

#Generate (market-weighted average) brand_focal variables
df_store[, finalspending := regprice - discount]
df_store[, regpriceperunit := regprice/quantity]
df_store[, finalpriceperunit := finalspending /quantity]
df_store[, avgdiscountperunit :=  discount/quantity]

# Calculate weekly sales by brand_focal
weeklysalesbybrand <- df_store[, .(totalsales = sum(quantity), LL = .N), by = .(week_nr,holiday, brand_focal, format)]
#LL can be calculated by uniqueN(product_id) to count distinct product id

# Merge df_complete with weeklysalesbybrand and calculate weights
df_weightcalc <- merge(df_store, weeklysalesbybrand, by = c("week_nr","holiday", "brand_focal", "format"))
df_weightcalc[, wp := quantity/totalsales]

# Calculate weighted averages
df_weightcalc[, avgregpriceperunit := regpriceperunit * wp]
df_weightcalc[, avgfinalprice := finalpriceperunit * wp]
df_weightcalc[, avgavgdiscount := avgdiscountperunit * wp]

# Aggregate by week_nr, brand, and format
df_bybrand_focal <- df_weightcalc[, .(totalvolume = sum(quantity), 
                                totalvalue = sum(finalspending), 
                                LL = mean(LL), 
                                regprice = sum(avgregpriceperunit), 
                                finalprice = sum(avgfinalprice), 
                                discount = sum(avgavgdiscount)), by = .(week_nr,holiday, brand_focal, format)]

rm(weeklysalesbybrand,df_weightcalc)
#FOR SIMPLICITY WE SPLIT BETWEEN FOCAL BRAND AND NON-FOCAL BRAND, in the end we want detail of focal brand and non focal in the same row
weeklysalesbybrand_Focal <- df_bybrand_focal[df_bybrand_focal$brand_focal == "Focal",]
weeklysalesbybrand_Nonfocal <- df_bybrand_focal[df_bybrand_focal$brand_focal!= "Focal",]

# Rename columns to distinguish between Focal and Nonfocal
setnames(weeklysalesbybrand_Nonfocal, old = c("finalprice", "regprice", "discount"), 
         new = c("finalprice_Nonfocal", "regprice_Nonfocal", "discount_Nonfocal"))
setnames(weeklysalesbybrand_Focal, old = c("finalprice", "regprice", "discount"), 
         new = c("finalprice_Focal", "regprice_Focal", "discount_Focal"))

# Merge Focal and Nonfocal data
df_storeinfo <- merge(weeklysalesbybrand_Focal, weeklysalesbybrand_Nonfocal, by = c("week_nr", "format"))
df_storeinfo <- df_storeinfo[,c("week_nr","finalprice_Nonfocal","regprice_Nonfocal","discount_Nonfocal","finalprice_Focal","regprice_Focal","discount_Focal","format")]

# Create a function to streamline the creation of format-specific columns
create_format_specific_cols <- function(data, format_name) {
  cols <- c("finalprice", "regprice","discount")
  setnames(data, old = paste0(cols, "_Focal"), new = paste0(format_name, cols, "_Focal"))
  setnames(data, old = paste0(cols, "_Nonfocal"), new = paste0(format_name, cols, "_Nonfocal"))
  # Remove 'format' column
  data <- data[, !("format"), with = FALSE]
  return(data)
}

# Apply the function to each format and combine the results
storedf_hyper <- create_format_specific_cols(df_storeinfo[format == "hypermarket"], "hyper")
storedf_super <- create_format_specific_cols(df_storeinfo[format == "supermarket"], "super")
storedf_conve <- create_format_specific_cols(df_storeinfo[format == "convenience"], "conve")

# Combine all format data
# Assuming store_df, storedf_hyper, storedf_super, storedf_conve are your data frames
list_of_df_formats <- list(df_storeinfo, storedf_hyper, storedf_super, storedf_conve)

# Merge all data frames in the list based on 'vecka'
relevant_storedf <- Reduce(function(x, y) merge(x, y, by = c("week_nr"), all = TRUE), list_of_df_formats)
#we can check the avgregprice should be the same with format_avgregprice


# Find columns that match the patterns 'regprice_' or 'avgdiscount_'
finalprice_cols <- grep("finalprice_", names(relevant_storedf), value = TRUE)
regprice_cols <- grep("regprice_", names(relevant_storedf), value = TRUE)
discount_cols <- grep("discount_", names(relevant_storedf), value = TRUE)

# Combine these with the other column names
relevant_columns <- c("week_nr","format",finalprice_cols, regprice_cols,discount_cols)

# Subset the data.table with the relevant columns
relevant_storedf <- relevant_storedf[, ..relevant_columns]


# Optionally, remove unnecessary variables from environment
rm(list = c("df_bybrand_focal","weeklysalesbybrand_Nonfocal", "weeklysalesbybrand_Focal", "df_storeinfo", 
            "storedf_hyper", "storedf_super", "storedf_conve","list_of_df_formats"))



###################################################
########### MODEL FREE EVIDENCE STORE #############
###################################################
#Plot store offering
ggplot(relevant_storedf, aes(x = week_nr, y = regprice_Focal, color = format)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(title ="Regular Prices offered of brand D over time", x = "Week (th)", y = "Regular Price", color = "Format")

ggplot(relevant_storedf, aes(x = week_nr, y = discount_Focal, color = format)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(title ="Discounts offered of brand D over time", x = "Week (th)", y = "Regular Price", color = "Format")




#######################################################
########### CALCULATE CUSTOMER LEVEL DATA #############
#######################################################

df_hh <- df_store #We selected the dataset that we have complete store formats information
df_choice <- merge(df_hh,relevant_storedf, by=c("week_nr","format")) #we need to maintain the format so we can conlude the price that customer encounter at the format the bought
remove(list=c("df_hh", "df_store","relevant_storedf","df"))


genattribute_focalbrand <- function(df,brand_we_interested,lambda_IRP,lambda_loyalty) {
  
  # Remove duplicates
  df <- unique(df, by = c("week_nr", "cust"))
  
  # Define choices
  df[, choice_Focal := ifelse(brand == brand_we_interested, 1, 0)]
  df[, choice_Nonfocal := ifelse(brand != brand_we_interested, 1, 0)]
  
  # Define lag function
  lag_1 <- function(x) shift(x, 1, type = "lag")
  
  # Order the dataframe for lagged operations
  df <- df[order(cust, week_nr)]
  
  # Function to calculate IRP
  calc_irp <- function(col, lambda_IRP) {
    col1 <- lag_1(col)
    col2 <- lag_1(col1)
    irp1 <- lambda_IRP * col2 + (1 - lambda_IRP) * col1
    irp <- lambda_IRP * irp1 + (1 - lambda_IRP) * col1
    return(list(irp1 = irp1, irp = irp))
  }
  
  # Calculate IRP for price and avgdiscount for both Focal and Nonfocal
  df[, c("IR_finalprice1_Focal", "IR_finalprice_Focal") := calc_irp(regprice_Focal, lambda_IRP)]
  df[, c("IR_regprice1_Focal", "IR_regprice_Focal") := calc_irp(regprice_Focal, lambda_IRP)]
  df[, c("IR_discount1_Focal", "IR_discount_Focal") := calc_irp(discount_Focal, lambda_IRP)]
  df[, c("IR_finalprice1_Nonfocal", "IR_finalprice_Nonfocal") := calc_irp(regprice_Nonfocal, lambda_IRP)]
  df[, c("IR_regprice1_Nonfocal", "IR_regprice_Nonfocal") := calc_irp(regprice_Nonfocal, lambda_IRP)]
  df[, c("IR_discount1_Nonfocal", "IR_discount_Nonfocal") := calc_irp(discount_Nonfocal, lambda_IRP)]
  
  # Calculate gain/loss variables
  gain_loss <- function(gap, format = NULL) {
    if (!is.null(format)) {
      gap <- gap * (df$format == format)
    }
    gain <- pmax(gap, 0)
    loss <- pmax(-gap, 0)
    return(list(gain = gain, loss = loss))
  }
  
  df[, c("finalpricegap_Focal","regpricegap_Focal", "discountgap_Focal","finalpricegap_Nonfocal" ,"regpricegap_Nonfocal", "discountgap_Nonfocal") := 
       .((finalprice_Focal - IR_finalprice_Focal),
         (regprice_Focal - IR_regprice_Focal), 
         (discount_Focal - IR_discount_Focal),
         (finalprice_Nonfocal - IR_finalprice_Nonfocal),
         (regprice_Nonfocal - IR_regprice_Nonfocal), 
         (discount_Nonfocal - IR_discount_Nonfocal))]
  
  # Apply gain/loss function to each format and type
  formats <- c("hyper", "super", "conve")
  for (focal in c("Focal", "Nonfocal")) {
    for (format in formats) {
      df[, c(paste0(format, "finalpricegain_", focal), paste0(format, "finalpriceloss_", focal)) := 
           gain_loss(get(paste0("regpricegap_", focal)), format)]
      df[, c(paste0(format, "regpricegain_", focal), paste0(format, "regpriceloss_", focal)) := 
           gain_loss(get(paste0("regpricegap_", focal)), format)]
      df[, c(paste0(format, "discountgain_", focal), paste0(format, "discountloss_", focal)) := 
           gain_loss(get(paste0("discountgap_", focal)), format)]
    }
  }
  
  # Calculate loyalty variable
  lambda_loyalty = lambda_loyalty
  
  # Calculate lagged loyalty variables
  df[, loyal1_Focal := lag_1(choice_Focal)]
  df[, loyal1_Nonfocal := lag_1(choice_Nonfocal)]
  
  # Calculate loyalty variables
  df[, loyal_Focal := lambda_loyalty * loyal1_Focal + (1 - lambda_loyalty) * choice_Focal]
  df[, loyal_Nonfocal := lambda_loyalty * loyal1_Nonfocal + (1 - lambda_loyalty) * choice_Nonfocal]
  df[, loyal := fifelse(choice_Nonfocal == 0, loyal_Nonfocal, loyal_Focal)]
  
  # Calculate format share
  df[, `:=` (supermarket_shop = as.integer(format == "supermarket"),
             hypermarket_shop = as.integer(format == "hypermarket"),
             convenience_shop = as.integer(format == "convenience"),
             shoppingtime = 1)]
  df[, `:=` (TotalShoppingtime = cumsum(shift(shoppingtime, fill = 1)),
             supermarkettime = cumsum(shift(supermarket_shop, fill = 0)),
             hypermarkettime = cumsum(shift(hypermarket_shop, fill = 0)),
             conveniencetime = cumsum(shift(convenience_shop, fill = 0))
  ), by = cust]
  
  df[, `:=` (sup_share = supermarkettime / TotalShoppingtime,
             hyper_share = hypermarkettime / TotalShoppingtime,
             conve_share = conveniencetime / TotalShoppingtime)]
  
  # Remove first week used for constructing IRP and Loyalty and duplicates
  df <- na.omit(df)
  df <- unique(df, by = c("week_nr", "cust"))
  
  return(df)
}

df_choicemade <- genattribute_focalbrand(df_choice,brand_we_interested = "D",lambda_IRP = 0.90,lambda_loyalty= 0.89)


#############################################################
########### MODEL FREE CUSTOMER ENCOUNTER PRICE #############
#############################################################
df_choicemade[, cust := as.factor(cust)]
ggplot(df_choicemade[df_choicemade$cust %in% c("15","43"),], 
       aes(x = week_nr, y = regpricegap_Focal, color = format, shape = cust)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue", size = 1) + # Add this line for the horizontal line
  theme_minimal() +
  labs(title = "Regular Price Gap of brand D when Customer 15th and 43th shopped over time", 
       x = "Week (th)", 
       y = "Regular Price Gap", 
       color = "Format", 
       shape = "Customer_id")





###############################################################
########## WIDEN DATA FOR CHOICE ATTRIBUTE SETTING ############
###############################################################

choice_rearrange <- function(df_choicemade){
  
  df_choice_rearranged <-df_choicemade[,c("week_nr","cust","brand_focal",
                                          "hyperfinalprice_Focal","hyperfinalprice_Nonfocal","hyperregprice_Focal","hyperregprice_Nonfocal","hyperdiscount_Focal","hyperdiscount_Nonfocal",
                                          "superfinalprice_Focal","superfinalprice_Nonfocal","superregprice_Focal","superregprice_Nonfocal","superdiscount_Focal","superdiscount_Nonfocal",
                                          "convefinalprice_Focal","convefinalprice_Nonfocal","converegprice_Focal","converegprice_Nonfocal","convediscount_Focal","convediscount_Nonfocal",
                                          "hyperfinalpricegain_Focal","hyperfinalpricegain_Nonfocal","hyperfinalpriceloss_Focal","hyperfinalpriceloss_Nonfocal",
                                          "hyperregpricegain_Focal","hyperregpricegain_Nonfocal","hyperregpriceloss_Focal","hyperregpriceloss_Nonfocal",
                                          "hyperdiscountgain_Focal","hyperdiscountgain_Nonfocal","hyperdiscountloss_Focal","hyperdiscountloss_Nonfocal",
                                          "superfinalpricegain_Focal","superfinalpricegain_Nonfocal","superfinalpriceloss_Focal","superfinalpriceloss_Nonfocal",
                                          "superregpricegain_Focal","superregpricegain_Nonfocal","superregpriceloss_Focal","superregpriceloss_Nonfocal",
                                          "superdiscountgain_Focal","superdiscountgain_Nonfocal","superdiscountloss_Focal","superdiscountloss_Nonfocal",
                                          "convefinalpricegain_Focal","convefinalpricegain_Nonfocal","convefinalpriceloss_Focal","convefinalpriceloss_Nonfocal",
                                          "converegpricegain_Focal","converegpricegain_Nonfocal","converegpriceloss_Focal","converegpriceloss_Nonfocal",
                                          "convediscountgain_Focal","convediscountgain_Nonfocal","convediscountloss_Focal","convediscountloss_Nonfocal")]
  
  df_choice_tranformed <- dfidx(df_choice_rearranged, choice = "brand_focal", varying = 4:57, sep = "_")
  df_choice_tranformed$choice <- as.numeric(df_choice_tranformed$brand_focal)
  
  
  df_choicevariant_0 <- data.frame(df_choice_tranformed[,c("week_nr","cust","choice",                                                                              
                                                           "hyperfinalprice","hyperregprice","hyperdiscount","hyperfinalpricegain","hyperfinalpriceloss","hyperregpricegain","hyperregpriceloss","hyperdiscountgain","hyperdiscountloss",
                                                           "superfinalprice","superregprice","superdiscount","superfinalpricegain","superfinalpriceloss","superregpricegain","superregpriceloss","superdiscountgain","superdiscountloss",
                                                           "convefinalprice","converegprice","convediscount","convefinalpricegain","convefinalpriceloss","converegpricegain","converegpriceloss","convediscountgain","convediscountloss")])
  extract_index <- data.frame(df_choicevariant_0[,31])
  setnames(extract_index, new = c("chid","brand_alt"))
  df_choicevariant<- cbind(df_choicevariant_0,extract_index)
  
  
  df_choiceinvariant <- data.frame(df_choicemade[,c("week_nr","cust","brand_focal",
                                                    "loyal_Focal","loyal_Nonfocal","sup_share","hyper_share","conve_share")])
  
  df <- merge(df_choicevariant,df_choiceinvariant, by=c("week_nr","cust"))
  

    df$Nonfocal_intercept = ifelse(df$brand_alt == "Nonfocal", 1, 0)
    df$Focal_intercept = ifelse(df$brand_alt == "Focal", 1, 0)
    df$loyal_nonfocal = ifelse(df$brand_alt == "Nonfocal", df$loyal_Nonfocal, df$loyal_Focal)

  return(df)
}

df_choice <- choice_rearrange(df_choicemade)


#generate output
write.csv(df_choice,'../gen/df_choice.csv', row.names = TRUE)

rm(list=ls())


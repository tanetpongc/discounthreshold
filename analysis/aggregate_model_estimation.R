#This file give a function to estimate each proposed model (nonlinear and simplified version), run with simulated data for giving example
#The detail of each function is available in the appendix of the slides in this repository


####################################################
################## MODEL (I) #######################
####################################################
aggregate_model1<-function(df){
  m_1 <- lm(dlogtotalvolume ~ dlogavgfinalprice + dlogavgcompfinalprice + log(totalvolume1) + log(avgfinalprice1) + cop_logavgfinalprice + holiday,data=df)
  return(m_1)
}
###########################################################################

####################################################
################## MODEL (II) ######################
####################################################
aggregate_model2 <- function(df,alpha_l_price,gamma){
  gamma = gamma #Speed of transition
  LS_nonlinear <- function(parvec){
    F_P_L <- 1/(1+exp(-gamma*(df$dlogavgfinalprice)))
    ut <- df$dlogtotalvolume - (parvec[1] + (parvec[2]+ parvec[3]*F_P_L)*df$dlogavgfinalprice
                                + parvec[4]*df$dlogavgcompfinalprice
                                + parvec[5]*((log(df$totalvolume1))+parvec[6]*log(df$avgfinalprice1))
                                + parvec[7]*df$cop_logavgfinalprice + parvec[8]*df$holiday)
    logL <- sum (-0.5* log(parvec[9]) - 0.5*ut^2/parvec[9] )
    return(-logL)
  }
  

  lm_linear <- lm(dlogtotalvolume ~ dlogavgfinalprice + dlogavgcompfinalprice + log(totalvolume1) + log(avgfinalprice) + cop_logavgfinalprice + holiday,data=df)
  c_lm <- summary(lm_linear)$coefficients[1,1]
  alpha_0_lm <- summary(lm_linear)$coefficients[2,1]
  beta_compprice <- summary(lm_linear)$coefficients[3,1]
  phi_1_lm <- summary(lm_linear)$coefficients[4,1]
  phi_2_lm <- (summary(lm_linear)$coefficients[5,1])/(summary(lm_linear)$coefficients[4,1]) 
  beta_coplogfinalprice <- summary(lm_linear)$coefficients[6,1]
  beta_holiday <- summary(lm_linear)$coefficients[7,1]
  
  #print(paste("Linearmodel_price_coef:",summary(lm_linear)$coefficients[2,1],Linearmodel_price_se:",summary(lm_linear)$coefficients[2,2],...)
  parvec_1 <- c_lm
  parvec_2 <- alpha_0_lm
  parvec_3 <- alpha_l_price
  parvec_4 <- beta_compprice
  parvec_5 <- phi_1_lm
  parvec_6 <- phi_2_lm
  parvec_7 <- beta_coplogfinalprice
  parvec_8 <- beta_holiday
  parvec_9 <- summary(lm_linear)$sigma
  Start_v_LS <- c(parvec_1,parvec_2,parvec_3,parvec_4,parvec_5,parvec_6,parvec_7,parvec_8,parvec_9)
  
  est_nonlinear = optim(Start_v_LS,
                            fn = LS_nonlinear, # function to maximize
                            method = "BFGS",
                            control = list(fnscale = 1), # minimize the function
                            hessian = T # calculate Hessian matrix because we will need for confidence intervals
  )
  
  par_LS.est<-est_nonlinear$par
  
  OI<-solve(est_nonlinear$hessian)
  par_LS.se<-sqrt(diag(OI))
  
  Start_v_LS
  par_LS.est
  
  par_LS_name <- c("constant", "price_elas","price_elas_loss",
                   "compprice","correction_vol","correction_finalprice","copula_logfinalprice","holiday","sigma")
  par_LS <- data.table(cbind(par_LS_name,par_LS.est,par_LS.se))
  return(par_LS)
}

aggregate_model2_sim <-function(df){
  m_2_sim <- lm(dlogtotalvolume ~ dlogavgfinalprice_gain + dlogavgfinalprice_loss + dlogavgcompfinalprice + log(totalvolume1) + log(avgfinalprice1) + cop_logavgfinalprice + holiday,data=df)
  return(m_2_sim)
}
###########################################################################

#####################################################
################## MODEL (III) ######################
#####################################################
aggregate_model3 <-function(df){
  m_3 <- lm(dlogtotalvolume ~ dlogavgregprice + dlognondepth + dlogavgcompfinalprice + log(totalvolume1) + log(avgregprice1) + log(nondepth1) + cop_logavgregprice + cop_lognondepth + holiday,data=df)
  return(m_3)
}
###########################################################################

####################################################
################## MODEL (IV) ######################
####################################################
aggregate_model4 <- function(df,alpha_l_regprice,gamma){
  gamma = gamma #Speed of transition
  LS_nonlinear <- function(parvec){
    F_P_L <- 1/(1+exp(-gamma*(df$dlogavgregprice)))
    ut <- df$dlogtotalvolume - (parvec[1] + (parvec[2]+ parvec[3]*F_P_L)*df$dlogavgregprice
                                + parvec[4]*df$dlognondepth
                                + parvec[5]*df$dlogavgcompfinalprice
                                + parvec[6]*((log(df$totalvolume1))+parvec[7]*log(df$avgregprice1)+parvec[8]*log(df$nondepth1))
                                + parvec[9]*df$cop_logavgregprice + parvec[10]*df$cop_lognondepth + parvec[11]*df$holiday)
    logL <- sum (-0.5* log(parvec[12]) - 0.5*ut^2/parvec[12] )
    return(-logL)
  }
  
  
  lm_linear <- lm(dlogtotalvolume ~ dlogavgregprice + dlognondepth + dlogavgcompfinalprice + log(totalvolume1) + log(avgregprice1) + log(nondepth1) + cop_logavgregprice + cop_lognondepth + holiday,data=df)
  c_lm <- summary(lm_linear)$coefficients[1,1]
  alpha_0_p_lm <- summary(lm_linear)$coefficients[2,1]
  beta_nondepth <- summary(lm_linear)$coefficients[3,1]
  beta_compprice <- summary(lm_linear)$coefficients[4,1]
  phi_1_lm <- summary(lm_linear)$coefficients[5,1]
  phi_2_lm <- (summary(lm_linear)$coefficients[6,1])/(summary(lm_linear)$coefficients[5,1]) 
  phi_3_lm <- (summary(lm_linear)$coefficients[7,1])/(summary(lm_linear)$coefficients[5,1]) 
  beta_coplogregprice <- summary(lm_linear)$coefficients[8,1]
  beta_coplognondepth <- summary(lm_linear)$coefficients[9,1]
  beta_holiday <- summary(lm_linear)$coefficients[10,1]
  
  parvec_1 <- c_lm
  parvec_2 <- alpha_0_p_lm
  parvec_3 <- alpha_l_regprice
  parvec_4 <- beta_nondepth
  parvec_5 <- beta_compprice
  parvec_6 <- phi_1_lm
  parvec_7 <- phi_2_lm
  parvec_8 <- phi_3_lm
  parvec_9 <- beta_coplogregprice
  parvec_10 <- beta_coplognondepth
  parvec_11 <- beta_holiday
  parvec_12 <- summary(lm_linear)$sigma
  Start_v_LS <- c(parvec_1,parvec_2,parvec_3,parvec_4,parvec_5,parvec_6,parvec_7,parvec_8,parvec_9,parvec_10,parvec_11,parvec_12)
  
  est_nonlinear = optim(Start_v_LS,
                        fn = LS_nonlinear, # function to maximize
                        method = "BFGS",
                        control = list(fnscale = 1), # minimize the function
                        hessian = T # calculate Hessian matrix because we will need for confidence intervals
  )
  
  par_LS.est<-est_nonlinear$par
  
  OI<-solve(est_nonlinear$hessian)
  par_LS.se<-sqrt(diag(OI))
  
  Start_v_LS
  par_LS.est
  
  par_LS_name <- c("constant", "regprice_elas","regprice_elas_loss","nondepth_elas","compprice",
                   "correction_vol","correction_regprice","correction_nondepth","copula_logregprice","copula_lognondepth",
                   "holiday","sigma")
  par_LS <- data.table(cbind(par_LS_name,par_LS.est,par_LS.se))
  return(par_LS)
}
aggregate_model4_sim <-function(df){
  m_4_sim <- lm(dlogtotalvolume ~ dlogavgregprice_gain + dlogavgregprice_loss + dlognondepth + dlogavgcompfinalprice + log(totalvolume1) + log(avgregprice1) + log(nondepth1) + cop_logavgregprice + cop_lognondepth + holiday,data=df)
  return(m_4_sim)
}
###########################################################################


###################################################
################## MODEL (V) ######################
###################################################
aggregate_model5 <- function(df,alpha_l_nondepth,gamma){
  gamma = gamma #Speed of transition
  LS_nonlinear <- function(parvec){
    F_D_L <- 1/(1+exp(-gamma*(df$dlognondepth)))
    ut <- df$dlogtotalvolume - (parvec[1] + parvec[2]*df$dlogavgregprice
                                + (parvec[3]+ parvec[4]*F_D_L)*df$dlognondepth
                                + parvec[5]*df$dlogavgcompfinalprice
                                + parvec[6]*((log(df$totalvolume1))+parvec[7]*log(df$avgregprice1)+parvec[8]*log(df$nondepth1))
                                + parvec[9]*df$cop_logavgregprice + parvec[10]*df$cop_lognondepth + parvec[11]*df$holiday)
    logL <- sum (-0.5* log(parvec[12]) - 0.5*ut^2/parvec[12] )
    return(-logL)
  }
  
  
  lm_linear <- lm(dlogtotalvolume ~ dlogavgregprice + dlognondepth + dlogavgcompfinalprice + log(totalvolume1) + log(avgregprice1) + log(nondepth1) + cop_logavgregprice + cop_lognondepth + holiday,data=df)
  c_lm <- summary(lm_linear)$coefficients[1,1]
  beta_regprice <- summary(lm_linear)$coefficients[2,1]
  alpha_0_d_lm <- summary(lm_linear)$coefficients[3,1]
  beta_compprice <- summary(lm_linear)$coefficients[4,1]
  phi_1_lm <- summary(lm_linear)$coefficients[5,1]
  phi_2_lm <- (summary(lm_linear)$coefficients[6,1])/(summary(lm_linear)$coefficients[5,1]) 
  phi_3_lm <- (summary(lm_linear)$coefficients[7,1])/(summary(lm_linear)$coefficients[5,1]) 
  beta_coplogregprice <- summary(lm_linear)$coefficients[8,1]
  beta_coplognondepth <- summary(lm_linear)$coefficients[9,1]
  beta_holiday <- summary(lm_linear)$coefficients[10,1]
  
  parvec_1 <- c_lm
  parvec_2 <- beta_regprice
  parvec_3 <- alpha_0_d_lm 
  parvec_4 <- alpha_l_nondepth
  parvec_5 <- beta_compprice
  parvec_6 <- phi_1_lm
  parvec_7 <- phi_2_lm
  parvec_8 <- phi_3_lm
  parvec_9 <- beta_coplogregprice
  parvec_10 <- beta_coplognondepth
  parvec_11 <- beta_holiday
  parvec_12 <- summary(lm_linear)$sigma
  Start_v_LS <- c(parvec_1,parvec_2,parvec_3,parvec_4,parvec_5,parvec_6,parvec_7,parvec_8,parvec_9,parvec_10,parvec_11,parvec_12)
  
  est_nonlinear = optim(Start_v_LS,
                        fn = LS_nonlinear, # function to maximize
                        method = "BFGS",
                        control = list(fnscale = 1), # minimize the function
                        hessian = T # calculate Hessian matrix because we will need for confidence intervals
  )
  
  par_LS.est<-est_nonlinear$par
  
  OI<-solve(est_nonlinear$hessian)
  par_LS.se<-sqrt(diag(OI))
  
  Start_v_LS
  par_LS.est
  
  par_LS_name <- c("constant", "regprice_elas","nondepth_elas","nondepth_elas_loss","compprice",
                   "correction_vol","correction_regprice","correction_nondepth","copula_logregprice","copula_lognondepth",
                   "holiday","sigma")
  par_LS <- data.table(cbind(par_LS_name,par_LS.est,par_LS.se))
  return(par_LS)
}
aggregate_model5_sim <-function(df){
  m_5_sim <- lm(dlogtotalvolume ~ dlogavgregprice + dlognondepth_gain + dlognondepth_loss + dlogavgcompfinalprice + log(totalvolume1) + log(avgregprice1) + log(nondepth1) + cop_logavgregprice + cop_lognondepth + holiday,data=df)
  return(m_5_sim)
}
###########################################################################


####################################################
################## MODEL (VI) ######################
####################################################
aggregate_model6 <- function(df,alpha_l_regprice,alpha_l_nondepth,gamma){
  gamma = gamma #Speed of transition
  LS_nonlinear <- function(parvec){
    F_P_L <- 1/(1+exp(-gamma*(df$dlogavgregprice)))
    F_D_L <- 1/(1+exp(-gamma*(df$dlognondepth)))
    ut <- df$dlogtotalvolume - (parvec[1] + (parvec[2]+ parvec[3]*F_P_L)*df$dlogavgregprice
                                + (parvec[4]+ parvec[5]*F_D_L)*df$dlognondepth
                                + parvec[6]*df$dlogavgcompfinalprice
                                + parvec[7]*((log(df$totalvolume1))+parvec[8]*log(df$avgregprice1)+parvec[9]*log(df$nondepth1))
                                + parvec[10]*df$cop_logavgregprice + parvec[11]*df$cop_lognondepth + parvec[12]*df$holiday)
    logL <- sum (-0.5*log(parvec[13]) - 0.5*ut^2/parvec[13] )
    return(-logL)
  }
  
  
  lm_linear <- lm(dlogtotalvolume ~ dlogavgregprice + dlognondepth + dlogavgcompfinalprice + log(totalvolume1) + log(avgregprice1) + log(nondepth1) + cop_logavgregprice + cop_lognondepth + holiday,data=df)
  c_lm <- summary(lm_linear)$coefficients[1,1]
  alpha_0_p_lm <- summary(lm_linear)$coefficients[2,1]
  alpha_0_d_lm <- summary(lm_linear)$coefficients[3,1]
  beta_compprice <- summary(lm_linear)$coefficients[4,1]
  phi_1_lm <- summary(lm_linear)$coefficients[5,1]
  phi_2_lm <- (summary(lm_linear)$coefficients[6,1])/(summary(lm_linear)$coefficients[5,1]) 
  phi_3_lm <- (summary(lm_linear)$coefficients[7,1])/(summary(lm_linear)$coefficients[5,1]) 
  beta_coplogregprice <- summary(lm_linear)$coefficients[8,1]
  beta_coplognondepth <- summary(lm_linear)$coefficients[9,1]
  beta_holiday <- summary(lm_linear)$coefficients[10,1]
  
  parvec_1 <- c_lm
  parvec_2 <- alpha_0_p_lm
  parvec_3 <- alpha_l_regprice
  parvec_4 <- alpha_0_d_lm 
  parvec_5 <- alpha_l_nondepth
  parvec_6 <- beta_compprice
  parvec_7 <- phi_1_lm
  parvec_8 <- phi_2_lm
  parvec_9 <- phi_3_lm
  parvec_10 <- beta_coplogregprice
  parvec_11 <- beta_coplognondepth
  parvec_12 <- beta_holiday
  parvec_13 <- summary(lm_linear)$sigma
  Start_v_LS <- c(parvec_1,parvec_2,parvec_3,parvec_4,parvec_5,parvec_6,parvec_7,parvec_8,parvec_9,parvec_10,parvec_11,parvec_12,parvec_13)
  
  est_nonlinear = optim(Start_v_LS,
                        fn = LS_nonlinear, # function to maximize
                        method = "BFGS",
                        control = list(fnscale = 1), # minimize the function
                        hessian = T # calculate Hessian matrix because we will need for confidence intervals
  )
  
  par_LS.est<-est_nonlinear$par
  
  OI<-solve(est_nonlinear$hessian)
  par_LS.se<-sqrt(diag(OI))
  
  Start_v_LS
  par_LS.est
  
  par_LS_name <- c("constant","regprice_elas", "regprice_elas_loss","nondepth_elas","nondepth_elas_loss","compprice",
                   "correction_vol","correction_regprice","correction_nondepth","copula_logregprice","copula_lognondepth",
                   "holiday","sigma")
  par_LS <- data.table(cbind(par_LS_name,par_LS.est,par_LS.se))
  return(par_LS)
}
aggregate_model6_sim <-function(df){
  m_6_sim <- lm(dlogtotalvolume ~ dlogavgregprice_gain + dlogavgregprice_loss + dlognondepth_gain + dlognondepth_loss + dlogavgcompfinalprice + log(totalvolume1) + log(avgregprice1) + log(nondepth1) + cop_logavgregprice + cop_lognondepth + holiday,data=df)
  return(m_6_sim)
}
###########################################################################

######################################################################
################## MODEL EXAMPLE WITH THRESHOLD ######################
######################################################################

showcase_threshold_mod5 <-  function(df,alpha_g_regprice,alpha_l_regprice,threshold_l,threshold_g,gamma){
  gamma = gamma #Speed of transition
  LS_nonlinear <- function(parvec){
    F_P_G <- 1/(1+exp(gamma*(df$dlogavgregprice - parvec[4])))
    F_P_L <- 1/(1+exp(-gamma*(df$dlogavgregprice - parvec[6])))
    ut <- df$dlogtotalvolume - (parvec[1] + (parvec[2] + parvec[3]*F_P_G + parvec[5]*F_P_L)*df$dlogavgregprice
                                + parvec[7]*df$dlognondepth
                                + parvec[8]*df$dlogavgcompfinalprice
                                + parvec[9]*((log(df$totalvolume1))+parvec[10]*log(df$avgregprice1)+parvec[11]*log(df$nondepth1))
                                + parvec[12]*df$cop_logavgregprice + parvec[13]*df$cop_lognondepth + parvec[14]*df$holiday)
    logL <- sum (-0.5* log(parvec[15]) - 0.5*ut^2/parvec[15])
    return(-logL)
  }
  
  
  lm_linear <- lm(dlogtotalvolume ~ dlogavgregprice + dlognondepth + dlogavgcompfinalprice + log(totalvolume1) + log(avgregprice1) + log(nondepth1) + cop_logavgregprice + cop_lognondepth + holiday,data=df)
  c_lm <- summary(lm_linear)$coefficients[1,1]
  alpha_0_p_lm <- summary(lm_linear)$coefficients[2,1]
  beta_nondepth <- summary(lm_linear)$coefficients[3,1]
  beta_compprice <- summary(lm_linear)$coefficients[4,1]
  phi_1_lm <- summary(lm_linear)$coefficients[5,1]
  phi_2_lm <- (summary(lm_linear)$coefficients[6,1])/(summary(lm_linear)$coefficients[5,1]) 
  phi_3_lm <- (summary(lm_linear)$coefficients[7,1])/(summary(lm_linear)$coefficients[5,1]) 
  beta_coplogregprice <- summary(lm_linear)$coefficients[8,1]
  beta_coplognondepth <- summary(lm_linear)$coefficients[9,1]
  beta_holiday <- summary(lm_linear)$coefficients[10,1]
  
  parvec_1 <- c_lm
  parvec_2 <- alpha_0_p_lm
  parvec_3 <- alpha_g_regprice
  parvec_4 <- threshold_g
  parvec_5 <- alpha_l_regprice
  parvec_6 <- threshold_g
  parvec_7 <- beta_nondepth
  parvec_8 <- beta_compprice
  parvec_9 <- phi_1_lm
  parvec_10 <- phi_2_lm
  parvec_11 <- phi_3_lm
  parvec_12 <- beta_coplogregprice
  parvec_13 <- beta_coplognondepth
  parvec_14 <- beta_holiday
  parvec_15 <- summary(lm_linear)$sigma
  
  Start_v_LS <- c(parvec_1,parvec_2,parvec_3,parvec_4,parvec_5,parvec_6,parvec_7,parvec_8,parvec_9,parvec_10,parvec_11,parvec_12,parvec_13,parvec_14,parvec_15)
  
  est_nonlinear = optim(Start_v_LS,
                        fn = LS_nonlinear, # function to maximize
                        method = "BFGS",
                        control = list(fnscale = 1), # minimize the function
                        hessian = T # calculate Hessian matrix because we will need for confidence intervals
  )
  
  par_LS.est<-est_nonlinear$par
  
  OI<-solve(est_nonlinear$hessian)
  par_LS.se<-sqrt(diag(OI))
  
  Start_v_LS
  par_LS.est
  
  par_LS_name <- c("constant", "regprice_elas","regprice_elas_gain","thresholdregprice_gain","regprice_elas_loss","thresholdregprice_loss",
                   "nondepth_elas","compprice","correction_vol","correction_regprice","correction_nondepth",
                   "copula_logregprice","copula_lognondepth","holiday","sigma")
  par_LS <- data.table(cbind(par_LS_name,par_LS.est,par_LS.se))
  return(par_LS)
}
###########################################################################


####################################################################################
######## LOAD PACKAGE AND DATA AND ESTIMATE SIMULATED DATA FOR BRAND D #############
####################################################################################
library(data.table)
df <- fread("../gen/df_bybrand_aggregate.csv")

##For simplicity, we will use simplified version for nonlinear model

##########################################
########## Hypermarket Brand D ###########
##########################################

Est_D_hypermarket = aggregate_model3(df[brand == "D" & format == "hypermarket",])

#Plot for Brand D Hypermarket
dt_Est_D_hypermarket <- data.table(x = df[brand == "D" & format == "hypermarket",]$dlogavgregprice)

fit_Est_D_hypermarket <- coef(Est_D_hypermarket)["dlogavgregprice"]*dt_Est_D_hypermarket$x
dt_Est_D_hypermarket[,fit := fit_Est_D_hypermarket]

ggplot(dt_Est_D_hypermarket,aes(x = x, y = fit)) +
  geom_point() +  # Pastel color for points
  geom_line(color = "darkblue") +  # Pastel color for line
  theme_minimal() +
  labs(title = "Estimated Effect of Change in Regular Price of Brand D in Hypermarket", x = expression(Delta * " ln(Regular Price)"), y = expression(Delta * " ln(TotalVolumes)"))

##########################################
########## Convenience Brand D ###########
##########################################

Est_D_convenience = aggregate_model2_sim(df[brand == "D" & format == "convenience",]) #Positive coefficient but its just simulated data

#Plot for Brand D Convenience
dt_Est_D_convenience <- data.table(x_gain = df[brand == "D" & format == "convenience",]$dlogavgfinalprice_gain,x_loss = df[brand == "D" & format == "convenience",]$dlogavgfinalprice_loss, x = df[brand == "D" & format == "convenience",]$dlogavgfinalprice)
fit_Est_D_convenience <- coef(Est_D_convenience)["dlogavgfinalprice_gain"]*dt_Est_D_convenience$x_gain + coef(Est_D_convenience)["dlogavgfinalprice_loss"]*dt_Est_D_convenience$x_loss
dt_Est_D_convenience[,fit := fit_Est_D_convenience]

ggplot(dt_Est_D_convenience,aes(x = x, y = fit)) +
  geom_point() +  # Pastel color for points
  geom_line(color = "darkblue") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") + # Pastel color for line
  theme_minimal() +
  labs(title = "Estimated Effect of Change in Final Price of Brand D in Convenience Store", x = expression(Delta * " ln(Final Price)"), y = expression(Delta * " ln(TotalVolumes)"))

##########################################
########## Supermarket Brand D ###########
##########################################

Est_D_supermarket = aggregate_model1(df[brand == "D" & format == "supermarket",])

#Plot for Brand D Hypermarket
dt_Est_D_supermarket <- data.table(x = df[brand == "D" & format == "supermarket",]$dlogavgfinalprice)

fit_Est_D_supermarket <- coef(Est_D_supermarket)["dlogavgfinalprice"]*dt_Est_D_supermarket$x
dt_Est_D_supermarket[,fit := fit_Est_D_supermarket]

ggplot(dt_Est_D_supermarket,aes(x = x, y = fit)) +
  geom_point() +  # Pastel color for points
  geom_line(color = "darkblue") +  # Pastel color for line
  theme_minimal() +
  labs(title = "Estimated Effect of Change in Regular Price of Brand D in Supermarket", x = expression(Delta * " ln(Final Price)"), y = expression(Delta * " ln(TotalVolumes)"))


###################################################
########## EXAMPLE MODEL WITH THRESHOLD ###########
###################################################

Est_modelwiththreshold <- showcase_threshold_mod5(df[brand == "B" & format == "hypermarket",],alpha_g_regprice = 2,alpha_l_regprice = 1,threshold_l = -10,threshold_g = 0,gamma = 300)

# Plot
dt_Est_modelwiththreshold  <- data.table(x = seq(min(df[brand == "B" & format == "hypermarket",]$dlogavgregprice), max(df[brand == "B" & format == "hypermarket",]$dlogavgregprice), by = 0.01))

# Extracting values
regprice_elas <- as.numeric(Est_modelwiththreshold[par_LS_name == "regprice_elas", par_LS.est][1])
regprice_elas_gain <- as.numeric(Est_modelwiththreshold[par_LS_name == "regprice_elas_gain", par_LS.est][1])
regprice_elas_loss <- as.numeric(Est_modelwiththreshold[par_LS_name == "regprice_elas_loss", par_LS.est][1])
thresholdregprice_gain <- as.numeric(Est_modelwiththreshold[par_LS_name == "thresholdregprice_gain", par_LS.est][1])
thresholdregprice_loss <- as.numeric(Est_modelwiththreshold[par_LS_name == "thresholdregprice_loss", par_LS.est][1])

gamma = 300

fit_Est_modelwiththreshold <- (regprice_elas
                          + regprice_elas_gain * (1 + exp(gamma * (dt_Est_modelwiththreshold$x - thresholdregprice_gain)))^-1
                          + regprice_elas_loss * (1 + exp(-gamma * (dt_Est_modelwiththreshold$x - thresholdregprice_loss)))^-1) * dt_Est_modelwiththreshold$x

fit_transition_Est_modelwiththreshold <- (regprice_elas
                                     + regprice_elas_gain * (1 + exp(gamma * (dt_Est_modelwiththreshold$x - thresholdregprice_gain)))^-1
                                     + regprice_elas_loss * (1 + exp(-gamma * (dt_Est_modelwiththreshold$x - thresholdregprice_loss)))^-1)

dt_Est_modelwiththreshold[, `:=`(Nonlinearealasticity = fit_Est_modelwiththreshold,
                       Transitionfunction = fit_transition_Est_modelwiththreshold)]

ggplot(dt_Est_modelwiththreshold, aes(x = x, y = fit_Est_modelwiththreshold)) +
  geom_line(color = "darkblue") +  
  theme_minimal() +
  labs(title = "Nonlinear Price Elasticities for brand B in Hypermarket", 
       x = "Regular Price Gap", 
       y = "Sales Change")

ggplot(dt_Est_modelwiththreshold, aes(x = x, y = fit_transition_Est_modelwiththreshold)) +
  geom_line(color = "darkblue") +  
  theme_minimal() +
  labs(title = "Transition Function for the Three-Regime Quadratic Logistic Sales Model of brand B", 
       x = "Regular Price Gap", 
       y = "F(GAP)")


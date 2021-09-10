#################################################
## RUN DIAGNOSTIC ACCURACY SIMULATION STUDY
#################################################

#load needed libraries
library(rstan)
library(MASS)

#define progress bar for convenience
pb = txtProgressBar(min = 0, max = 500, initial = 0, style = 3)

#initialize empty dataframe to store results
Diag_results <- data.frame(matrix(0, nrow = 500, ncol = 14))
colnames(Diag_results) <- c("BD", "SI", "DS", "CD", "IM", "VC", "LA", "AT", "DM", "TS", 
                            "Population", paste("PostProb", seq(1, 3, 1), sep = ""))

#define the iterative 'for' loop of the simulation
for(i in 1:500) {
  
  ######################
  ## NORMAL CONDITION
  ######################
  
  #create data for stan
  stan_dat <- list(pop = 3, dim = 4, raw_x = round(mvrnorm(n = 4, mu = WS_nor_mns, Sigma = WS_nor_cov, empirical = TRUE), 0)[1, ], 
                   ref_mn = rbind(WS_nor_mns, WS_mci_mns, WS_alz_mns), 
                   ref_stdv = rbind(WS_nor_sds, WS_mci_sds, WS_alz_sds), 
                   cor_mat = list(WS_cor_mat, WS_cor_mat, WS_cor_mat), 
                   ref_N = c(100, 50, 40), eta = c(1, 1, 1), theta = c(0.714, 0.148, 0.138)) #base rates based on prevalence of MCI (https://doi.org/10.1212/WNL.0000000000004826) & AD (https://www.alz.org/media/documents/alzheimers-facts-and-figures.pdf) among 77 year olds
  
  #ensure no missing generations (was occasionally getting NAs for WAIS in normal condition before)
  while(anyNA(stan_dat$raw_x))
    stan_dat$raw_x <- round(mvrnorm(n = 4, mu = WS_nor_mns, Sigma = WS_nor_cov, empirical = TRUE), 0)[1, ]
  
  #confirm that scaled score is appropriate
  stan_dat$raw_x <- sapply(stan_dat$raw_x, function(x) ifelse(x < 1, 1, ifelse(x > 20, 20, x)))
  
  #record simulated scaled scores
  Diag_results[3*(i-1)+1, 1:4] <- stan_dat$raw_x
  
  #run the stan model for this observation using the reference samples
  stan_sample <- sampling(stan_model, data = stan_dat, seed = SEED)
  
  #update data
  stan_dat <- list(pop = 3, dim = 6, raw_x = round(mvrnorm(n = 6, mu = RB_nor_mns, Sigma = RB_nor_cov, empirical = TRUE), 0)[1, ], 
                   ref_mn = rbind(RB_nor_mns, RB_mci_mns, RB_alz_mns), 
                   ref_stdv = rbind(RB_nor_sds, RB_mci_sds, RB_alz_sds), 
                   cor_mat = list(RB_cor_mat, RB_cor_mat, RB_cor_mat), 
                   ref_N = c(90, 81, 138), eta = c(1, 1, 1), theta = summary(stan_sample, use_cache = FALSE, pars = "postProb")$summary[, 1]) #update prior probabilities with post-test probabilities
  
  #confirm that standard score is appropriate
  stan_dat$raw_x <- sapply(stan_dat$raw_x, function(x) ifelse(x < 40, 40, ifelse(x > 160, 160, x)))
  
  #record simulated standard score
  Diag_results[3*(i-1)+1, 5:10] <- stan_dat$raw_x
  
  #run the stan model for this observation usin the reference samples
  stan_sample <- sampling(stan_model, data = stan_dat, seed = SEED)
  
  #record update post-test probabilities
  Diag_results[3*(i-1)+1, 11:14] <- c(1, summary(stan_sample, use_cache = FALSE, pars = "postProb")$summary[, 1])
  
  ######################
  ## MCI CONDITION
  ######################
  
  #create data for stan
  stan_dat <- list(pop = 3, dim = 4, raw_x = round(mvrnorm(n = 4, mu = WS_mci_mns, Sigma = WS_mci_cov, empirical = TRUE), 0)[1, ], 
                   ref_mn = rbind(WS_nor_mns, WS_mci_mns, WS_alz_mns), 
                   ref_stdv = rbind(WS_nor_sds, WS_mci_sds, WS_alz_sds), 
                   cor_mat = list(WS_cor_mat, WS_cor_mat, WS_cor_mat), 
                   ref_N = c(100, 50, 40), eta = c(1, 1, 1), theta = c(0.714, 0.148, 0.138)) #base rates based on prevalence of MCI (https://doi.org/10.1212/WNL.0000000000004826) & AD (https://www.alz.org/media/documents/alzheimers-facts-and-figures.pdf) among 77 year olds
  
  #confirm that scaled score is appropriate
  stan_dat$raw_x <- sapply(stan_dat$raw_x, function(x) ifelse(x < 1, 1, ifelse(x > 20, 20, x)))
  
  #record simulated scaled scores
  Diag_results[3*(i-1)+2, 1:4] <- stan_dat$raw_x
  
  #run the stan model for this observation using the reference samples
  stan_sample <- sampling(stan_model, data = stan_dat, seed = SEED)
  
  #update data
  stan_dat <- list(pop = 3, dim = 6, raw_x = round(mvrnorm(n = 6, mu = RB_mci_mns, Sigma = RB_mci_cov, empirical = TRUE), 0)[1, ], 
                   ref_mn = rbind(RB_nor_mns, RB_mci_mns, RB_alz_mns), 
                   ref_stdv = rbind(RB_nor_sds, RB_mci_sds, RB_alz_sds), 
                   cor_mat = list(RB_cor_mat, RB_cor_mat, RB_cor_mat), 
                   ref_N = c(90, 81, 138), eta = c(1, 1, 1), theta = summary(stan_sample, use_cache = FALSE, pars = "postProb")$summary[, 1]) #update prior probabilities with post-test probabilities
  
  #confirm that standard score is appropriate
  stan_dat$raw_x <- sapply(stan_dat$raw_x, function(x) ifelse(x < 40, 40, ifelse(x > 160, 160, x)))
  
  #record simulated standard score
  Diag_results[3*(i-1)+2, 5:10] <- stan_dat$raw_x
  
  #run the stan model for this observation usin the reference samples
  stan_sample <- sampling(stan_model, data = stan_dat, seed = SEED)
  
  #record update post-test probabilities
  Diag_results[3*(i-1)+2, 11:14] <- c(2, summary(stan_sample, use_cache = FALSE, pars = "postProb")$summary[, 1])
  
  ######################
  ## ALZHEIMER'S CONDITION
  ######################
  
  #create data for stan
  stan_dat <- list(pop = 3, dim = 4, raw_x = round(mvrnorm(n = 4, mu = WS_alz_mns, Sigma = WS_alz_cov, empirical = TRUE), 0)[1, ], 
                   ref_mn = rbind(WS_nor_mns, WS_mci_mns, WS_alz_mns), 
                   ref_stdv = rbind(WS_nor_sds, WS_mci_sds, WS_alz_sds), 
                   cor_mat = list(WS_cor_mat, WS_cor_mat, WS_cor_mat), 
                   ref_N = c(100, 50, 40), eta = c(1, 1, 1), theta = c(0.714, 0.148, 0.138)) #base rates based on prevalence of MCI (https://doi.org/10.1212/WNL.0000000000004826) & AD (https://www.alz.org/media/documents/alzheimers-facts-and-figures.pdf) among 77 year olds
  
  #confirm that scaled score is appropriate
  stan_dat$raw_x <- sapply(stan_dat$raw_x, function(x) ifelse(x < 1, 1, ifelse(x > 20, 20, x)))
  
  #record simulated scaled scores
  Diag_results[3*(i-1)+3, 1:4] <- stan_dat$raw_x
  
  #run the stan model for this observation using the reference samples
  stan_sample <- sampling(stan_model, data = stan_dat, seed = SEED)
  
  #update data
  stan_dat <- list(pop = 3, dim = 6, raw_x = round(mvrnorm(n = 6, mu = RB_alz_mns, Sigma = RB_alz_cov, empirical = TRUE), 0)[1, ], 
                   ref_mn = rbind(RB_nor_mns, RB_mci_mns, RB_alz_mns), 
                   ref_stdv = rbind(RB_nor_sds, RB_mci_sds, RB_alz_sds), 
                   cor_mat = list(RB_cor_mat, RB_cor_mat, RB_cor_mat), 
                   ref_N = c(90, 81, 138), eta = c(1, 1, 1), theta = summary(stan_sample, use_cache = FALSE, pars = "postProb")$summary[, 1]) #update prior probabilities with post-test probabilities
  
  #confirm that standard score is appropriate
  stan_dat$raw_x <- sapply(stan_dat$raw_x, function(x) ifelse(x < 40, 40, ifelse(x > 160, 160, x)))
  
  #record simulated standard score
  Diag_results[3*(i-1)+3, 5:10] <- stan_dat$raw_x
  
  #run the stan model for this observation usin the reference samples
  stan_sample <- sampling(stan_model, data = stan_dat, seed = SEED)
  
  #record update post-test probabilities
  Diag_results[3*(i-1)+3, 11:14] <- c(3, summary(stan_sample, use_cache = FALSE, pars = "postProb")$summary[, 1])
  
  #update progress bar
  setTxtProgressBar(pb,i)
}

#keep environment tidy
rm(pb, i, stan_dat, stan_sample)

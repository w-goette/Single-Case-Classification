#################################################
## RUN PARAMETER RECOVERY SIMULATION STUDY
#################################################

#load needed libraries
library(rstan)
library(MASS)

#define progress bar for convenience
pb = txtProgressBar(min = 0, max = 1000, initial = 0, style = 3)

########################
## CONDITION 1
########################

#initialize empty dataframe to store results
C1_result <- data.frame(matrix(0, nrow = 1000, ncol = 8))
colnames(C1_result) <- c("SimScore",
                         "EstZscore",
                         "EstRaw",
                         "95%LB_EstZscore",
                         "95%LB_EstRaw",
                         "95%UB_EstZscore",
                         "95%UB_EstRaw",
                         "TrueZscore")

#create data for stan
stan_dat <- list(pop = 1, dim = 1, raw_x = 0, 
                 ref_mn = array(mean(C1_ref), c(1, 1)), 
                 ref_stdv = array(SD(C1_ref), c(1, 1)), 
                 cor_mat = list(matrix(1, ncol = 1, nrow = 1)), 
                 ref_N = array(length(C1_ref), 1), eta = array(1, 1), theta = array(1, 1))

#define iterative 'for' loop for simulation
for(i in 1:1000) {
  
  #simulate observation from the population
  stan_dat$raw_x <- round(array(rnorm(n = 1, mean = 50, sd = 10), 1), 0)
  
  #run the Stan model for this observation using the reference sample statistics
  stan_sample <- sampling(stan_model, data = stan_dat, seed = SEED)
  
  #put the results into the dataframe
  C1_result[i, ] <- c(stan_dat$raw_x, 
                      as.vector(summary(stan_sample, probs = c(0.025, 0.975), use_cache = FALSE, pars = c("Zscore", "rScore"))$summary[, c(1, 4:5)]), 
                      ((stan_dat$raw_x-50)/10))
  
  #update progress bar
  setTxtProgressBar(pb,i)
}

########################
## CONDITION 2
########################

#initialize empty dataframe to store results
C2_result <- data.frame(matrix(0, nrow = 1000, ncol = 8))
colnames(C2_result) <- c("SimScore",
                         "EstZscore",
                         "EstRaw",
                         "95%LB_EstZscore",
                         "95%LB_EstRaw",
                         "95%UB_EstZscore",
                         "95%UB_EstRaw",
                         "TrueZscore")

#create data for stan
stan_dat <- list(pop = 1, dim = 1, raw_x = 0, 
                 ref_mn = array(mean(C2_ref), c(1, 1)), 
                 ref_stdv = array(SD(C2_ref), c(1, 1)), 
                 cor_mat = list(matrix(1, ncol = 1, nrow = 1)), 
                 ref_N = array(length(C2_ref), 1), eta = array(1, 1), theta = array(1, 1))

#define iterative 'for' loop for simulation
for(i in 1:1000) {
  
  #simulate observation from the population
  stan_dat$raw_x <- round(array(rnorm(n = 1, mean = 50, sd = 10), 1), 0)
  
  #run the Stan model for this observation using the reference sample statistics
  stan_sample <- sampling(stan_model, data = stan_dat, seed = SEED)
  
  #put the results into the dataframe
  C2_result[i, ] <- c(stan_dat$raw_x, 
                      as.vector(summary(stan_sample, probs = c(0.025, 0.975), use_cache = FALSE, pars = c("Zscore", "rScore"))$summary[, c(1, 4:5)]), 
                      ((stan_dat$raw_x-50)/10))
  
  #update progress bar
  setTxtProgressBar(pb,i)
}

########################
## CONDITION 3
########################

#initialize empty dataframe to store results
C3_result <- data.frame(matrix(0, nrow = 1000, ncol = 8))
colnames(C3_result) <- c("SimScore",
                         "EstZscore",
                         "EstRaw",
                         "95%LB_EstZscore",
                         "95%LB_EstRaw",
                         "95%UB_EstZscore",
                         "95%UB_EstRaw",
                         "TrueZscore")

#create data for stan
stan_dat <- list(pop = 1, dim = 1, raw_x = 0, 
                 ref_mn = array(mean(C3_ref), c(1, 1)), 
                 ref_stdv = array(SD(C3_ref), c(1, 1)), 
                 cor_mat = list(matrix(1, ncol = 1, nrow = 1)), 
                 ref_N = array(length(C3_ref), 1), eta = array(1, 1), theta = array(1, 1))

#define iterative 'for' loop for simulation
for(i in 1:1000) {
  
  #simulate observation from the population
  stan_dat$raw_x <- round(array(rnorm(n = 1, mean = 50, sd = 10), 1), 0)
  
  #run the Stan model for this observation using the reference sample statistics
  stan_sample <- sampling(stan_model, data = stan_dat, seed = SEED)
  
  #put the results into the dataframe
  C3_result[i, ] <- c(stan_dat$raw_x, 
                      as.vector(summary(stan_sample, probs = c(0.025, 0.975), use_cache = FALSE, pars = c("Zscore", "rScore"))$summary[, c(1, 4:5)]), 
                      ((stan_dat$raw_x-50)/10))
  
  #update progress bar
  setTxtProgressBar(pb,i)
}

########################
## CONDITION 4
########################

#define an empty dataframe for results
C4_result <- data.frame(matrix(0, nrow = 1000, ncol = 48))
colnames(C4_result) <- c(paste("SimScore", seq(1, 6, 1)),
                         paste("EstZscore", seq(1, 6, 1)),
                         paste("EstRaw", seq(1, 6, 1)),
                         paste("95%LB_EstZscore", seq(1, 6, 1)),
                         paste("95%LB_EstRaw", seq(1, 6, 1)),
                         paste("95%UB_EstZscore", seq(1, 6, 1)),
                         paste("95%UB_EstRaw", seq(1, 6, 1)),
                         paste("TrueZscore", seq(1, 6, 1)))

#create data for stan
stan_dat <- list(pop = 1, dim = 6, raw_x = 0, 
                 ref_mn = array(colMeans(C4_ref), c(1, 6)), 
                 ref_stdv = array(apply(C4_ref, 2, SD), c(1, 6)), 
                 cor_mat = list(cor(C4_ref)), 
                 ref_N = array(nrow(C4_ref), 1), eta = array(1, 1), theta = array(1, 1))


#define iterative 'for' loop for simulation
for(i in 1:1000) {
  
  #simulate observation from the population
  stan_dat$raw_x <- round(mvrnorm(n = 1, mu = rep(50, 6), Sigma = C4_covmat, empirical = FALSE), 0)
  
  #run the Stan model for this observation using the reference sample statistics
  stan_sample <- sampling(stan_model, data = stan_dat, seed = SEED)
  
  #put the results into the dataframe
  C4_result[i, ] <- c(stan_dat$raw_x, 
                      as.vector(summary(stan_sample, probs = c(0.025, 0.975), use_cache = FALSE, pars = c("Zscore", "rScore"))$summary[, c(1, 4:5)]), 
                      ((stan_dat$raw_x-rep(50, 6))/10))
  
  #update progress bar
  setTxtProgressBar(pb,i)
}

########################
## CONDITION 5
########################

#define an empty dataframe for results
C5_result <- data.frame(matrix(0, nrow = 1000, ncol = 48))
colnames(C5_result) <- c(paste("SimScore", seq(1, 6, 1)),
                         paste("EstZscore", seq(1, 6, 1)),
                         paste("EstRaw", seq(1, 6, 1)),
                         paste("95%LB_EstZscore", seq(1, 6, 1)),
                         paste("95%LB_EstRaw", seq(1, 6, 1)),
                         paste("95%UB_EstZscore", seq(1, 6, 1)),
                         paste("95%UB_EstRaw", seq(1, 6, 1)),
                         paste("TrueZscore", seq(1, 6, 1)))

#create data for stan
stan_dat <- list(pop = 1, dim = 6, raw_x = 0, 
                 ref_mn = array(colMeans(C5_ref), c(1, 6)), 
                 ref_stdv = array(apply(C5_ref, 2, SD), c(1, 6)), 
                 cor_mat = list(cor(C5_ref)), 
                 ref_N = array(nrow(C5_ref), 1), eta = array(1, 1), theta = array(1, 1))


#define iterative 'for' loop for simulation
for(i in 1:1000) {
  
  #simulate observation from the population
  stan_dat$raw_x <- round(mvrnorm(n = 1, mu = rep(50, 6), Sigma = C5_covmat, empirical = FALSE), 0)
  
  #run the Stan model for this observation using the reference sample statistics
  stan_sample <- sampling(stan_model, data = stan_dat, seed = SEED)
  
  #put the results into the dataframe
  C5_result[i, ] <- c(stan_dat$raw_x, 
                      as.vector(summary(stan_sample, probs = c(0.025, 0.975), use_cache = FALSE, pars = c("Zscore", "rScore"))$summary[, c(1, 4:5)]), 
                      ((stan_dat$raw_x-rep(50, 6))/10))
  
  #update progress bar
  setTxtProgressBar(pb,i)
}

########################
## CONDITION 6
########################

#define an empty dataframe for results
C6_result <- data.frame(matrix(0, nrow = 1000, ncol = 48))
colnames(C6_result) <- c(paste("SimScore", seq(1, 6, 1)),
                         paste("EstZscore", seq(1, 6, 1)),
                         paste("EstRaw", seq(1, 6, 1)),
                         paste("95%LB_EstZscore", seq(1, 6, 1)),
                         paste("95%LB_EstRaw", seq(1, 6, 1)),
                         paste("95%UB_EstZscore", seq(1, 6, 1)),
                         paste("95%UB_EstRaw", seq(1, 6, 1)),
                         paste("TrueZscore", seq(1, 6, 1)))

#create data for stan
stan_dat <- list(pop = 1, dim = 6, raw_x = 0, 
                 ref_mn = array(colMeans(C6_ref), c(1, 6)), 
                 ref_stdv = array(apply(C6_ref, 2, SD), c(1, 6)), 
                 cor_mat = list(cor(C6_ref)), 
                 ref_N = array(nrow(C6_ref), 1), eta = array(1, 1), theta = array(1, 1))


#define iterative 'for' loop for simulation
for(i in 1:1000) {
  
  #simulate observation from the population
  stan_dat$raw_x <- round(mvrnorm(n = 1, mu = rep(50, 6), Sigma = C6_covmat, empirical = FALSE), 0)
  
  #run the Stan model for this observation using the reference sample statistics
  stan_sample <- sampling(stan_model, data = stan_dat, seed = SEED)
  
  #put the results into the dataframe
  C6_result[i, ] <- c(stan_dat$raw_x, 
                      as.vector(summary(stan_sample, probs = c(0.025, 0.975), use_cache = FALSE, pars = c("Zscore", "rScore"))$summary[, c(1, 4:5)]), 
                      ((stan_dat$raw_x-rep(50, 6))/10))
  
  #update progress bar
  setTxtProgressBar(pb,i)
}

#keep environment tidy
rm(pb, i, stan_dat, stan_sample)

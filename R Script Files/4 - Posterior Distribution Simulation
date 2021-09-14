#################################################
## POSTERIOR DISCREPANCY SIMULATION ANALYSIS
#################################################

#load needed library
library(MASS)
library(rstan)
library(bayesplot)
library(ggplot2)

########################
## SETUP CONDITION
########################

#enter WAIS subtest data from the WAIS manual
WS_cor_mat_full <- matrix(c(1.00, 0.56, 0.52, 0.60, 0.36, 0.56, 0.35, 0.57, 0.40, 0.43,
                            0.56, 1.00, 0.45, 0.55, 0.71, 0.46, 0.12, 0.35, 0.63, 0.39,
                            0.52, 0.45, 1.00, 0.45, 0.50, 0.60, 0.28, 0.36, 0.46, 0.44,
                            0.60, 0.55, 0.45, 1.00, 0.49, 0.53, 0.27, 0.45, 0.45, 0.49,
                            0.36, 0.71, 0.50, 0.49, 1.00, 0.47, 0.15, 0.17, 0.70, 0.42,
                            0.56, 0.46, 0.60, 0.53, 0.47, 1.00, 0.29, 0.37, 0.61, 0.40,
                            0.35, 0.12, 0.28, 0.27, 0.15, 0.29, 1.00, 0.17, 0.20, 0.61,
                            0.57, 0.35, 0.36, 0.45, 0.17, 0.37, 0.17, 1.00, 0.27, 0.27,
                            0.40, 0.63, 0.46, 0.45, 0.70, 0.61, 0.20, 0.27, 1.00, 0.40,
                            0.43, 0.39, 0.44, 0.49, 0.42, 0.40, 0.61, 0.27, 0.40, 1.00),
                          nrow = 10, ncol = 10)

WS_mns <- c(9.9, 10.1, 9.8, 9.6, 9.9, 10.0, 9.5, 9.7, 9.7, 9.9)
WS_sds <- c(3.1, 2.9, 3.2, 3.0, 2.9, 2.8, 2.8, 3.3, 3.1, 2.9)

#convert correlation matrix to covariance
WS_cov <- diag(WS_sds) %*% WS_cor_mat_full %*% t(diag(WS_sds))
rownames(WS_cov) <- colnames(WS_cov) <- c("BD", "SI", "DS", "MR", "VC", "AR", "SS", "VP", "IN", "CD")

#initialize a single model fit using the WAIS data
stan_dat <- list(pop = 1, dim = 10, raw_x = round(mvrnorm(n = 10, mu = WS_mns, Sigma = WS_cov, empirical = TRUE), 0)[1, ], 
                 ref_mn = array(WS_mns, c(1, 10)), 
                 ref_stdv = array(WS_sds, c(1, 10)), 
                 cor_mat = list(WS_cor_mat_full), 
                 ref_N = array(100, 1), eta = array(1, 1), theta = array(1, 1))

#run sampler to get posterior distribution
stan_sample <- sampling(stan_model, data = stan_dat, seed = SEED) #2000 iterations, 1000 = warmup

########################
## RAW SCORE ANALYSES
########################

#extract posterior distribution of raw scores
post_raw <- as.data.frame(extract(stan_sample, pars = "rScore"))

#visualize scores
ppc_intervals(stan_dat$raw_x, as.matrix(round(post_raw)), prob = 0.50, prob_outer = 0.95, size = 1.5, fatten = 3.5) +
  scale_x_continuous(
    labels = c("BD", "SI", "DS", "MR", "VC", "AR", "SS", "VP", "IN", "CD"),
    breaks = 1:10
  ) +
  scale_y_continuous(
    labels = 3:17,
    breaks = 3:17
  ) +
  xlab("Subtest") +
  ylab("Scaled Score") +
  ggtitle("Visualization of Subtest Performance Relative to Population") +
  bayesplot:::scale_color_ppc_dist(labels = c("Observed", "Population")) + 
  bayesplot:::scale_fill_ppc_dist(labels = c("Observed", "Population"))

ppc_stat(stan_dat$raw_x, as.matrix(post_raw), stat = "mean") +
  ggtitle("Visualization of Average of Subtests to Average of Population")

#compute percentiles from sum of posterior predicted subtest scores (10 point intervals)
round(ecdf(rowSums(post_raw))(seq(10, 190, 10))*100, 1)

#compute summary statistics of difference for DS < AR
mean(post_raw[post_raw[, 3]<post_raw[, 6], 3]-post_raw[post_raw[, 3]<post_raw[, 6], 6])
sd(post_raw[post_raw[, 3]<post_raw[, 6], 3]-post_raw[post_raw[, 3]<post_raw[, 6], 6])
median(post_raw[post_raw[, 3]<post_raw[, 6], 3]-post_raw[post_raw[, 3]<post_raw[, 6], 6])
round(ecdf(round(post_raw[, 3])-round(post_raw[, 6]))(seq(-18, -1, 1))*100, 1)

#compute summary statistics of difference for DS > AR
mean(post_raw[post_raw[, 3]>post_raw[, 6], 3]-post_raw[post_raw[, 3]>post_raw[, 6], 6])
sd(post_raw[post_raw[, 3]>post_raw[, 6], 3]-post_raw[post_raw[, 3]>post_raw[, 6], 6])
median(post_raw[post_raw[, 3]>post_raw[, 6], 3]-post_raw[post_raw[, 3]>post_raw[, 6], 6])
round(mean(round(post_raw[, 3])-round(post_raw[, 6]) >= 1)*100, 1)
round(mean(round(post_raw[, 3])-round(post_raw[, 6]) >= 2)*100, 1)
round(mean(round(post_raw[, 3])-round(post_raw[, 6]) >= 3)*100, 1)
round(mean(round(post_raw[, 3])-round(post_raw[, 6]) >= 4)*100, 1)
round(mean(round(post_raw[, 3])-round(post_raw[, 6]) >= 5)*100, 1)
round(mean(round(post_raw[, 3])-round(post_raw[, 6]) >= 6)*100, 1)
round(mean(round(post_raw[, 3])-round(post_raw[, 6]) >= 7)*100, 1)
round(mean(round(post_raw[, 3])-round(post_raw[, 6]) >= 8)*100, 1)
round(mean(round(post_raw[, 3])-round(post_raw[, 6]) >= 9)*100, 1)
round(mean(round(post_raw[, 3])-round(post_raw[, 6]) >= 10)*100, 1)
round(mean(round(post_raw[, 3])-round(post_raw[, 6]) >= 11)*100, 1)
round(mean(round(post_raw[, 3])-round(post_raw[, 6]) >= 12)*100, 1)
round(mean(round(post_raw[, 3])-round(post_raw[, 6]) >= 13)*100, 1)
round(mean(round(post_raw[, 3])-round(post_raw[, 6]) >= 14)*100, 1)
round(mean(round(post_raw[, 3])-round(post_raw[, 6]) >= 15)*100, 1)
round(mean(round(post_raw[, 3])-round(post_raw[, 6]) >= 16)*100, 1)
round(mean(round(post_raw[, 3])-round(post_raw[, 6]) >= 17)*100, 1)
round(mean(round(post_raw[, 3])-round(post_raw[, 6]) >= 18)*100, 1)

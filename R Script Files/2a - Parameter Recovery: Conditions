#################################################
## PARAMETER RECOVERY SIMULATION CONDITIONS
#################################################

#load needed libraries
library(MASS)

########################
## CONDITION 1 - UNIVARIATE POPULATION (RECOVERY)
########################

#Mu = 50, Sigma = 10, N = 1000
C1_ref <- round(rnorm(n = 1000, mean = 50, sd = 10), 0)

########################
## CONDITION 2 - UNIVARIATE POPULATION (RECOVERY)
########################

#Mu = 50, Sigma = 10, N = 100
C2_ref <- round(rnorm(n = 100, mean = 50, sd = 10), 0)

########################
## CONDITION 3 - UNIVARIATE POPULATION (RECOVERY)
########################

#Mu = 50, Sigma = 10, N = 50
C3_ref <- round(rnorm(n = 50, mean = 50, sd = 10), 0)

########################
## CONDITION 4 - MULTIVARIATE POPULATION (RECOVERY)
########################

#generate noisy 2-factor model for six simulated tests
factors_major <- rnorm(6, 0.60, 0.05)

#generate factor loadings for the minor factors
factors_minor <- matrix(0, ncol = 50, nrow = 6)

for(i in 1:50) {
  y <- 0.80^(i - 1)
  factors_minor[, i] <- rnorm(6, 0, y)
}

#ensure that the minor factors account for about 10% of the overall item variance
for(i in 1:6) {
  factors_minor[i, ] <- sqrt(factors_minor[i, ]^2 / sum(factors_minor[i, ]^2) * 0.1)
}

#combine into a final factor loading matrix for Conditions 1-5
factors <- matrix(0, ncol = 51, nrow = 6)
factors[1:3, 1] <- factors_major[1:3]
factors[4:6, 2] <- factors_major[4:6]

factors[1, 2:51] <- factors_minor[1, ]
factors[2, 2:51] <- factors_minor[2, ]
factors[3, 2:51] <- factors_minor[3, ]

factors[4, c(1, 3:51)] <- factors_minor[4, ]
factors[5, c(1, 3:51)] <- factors_minor[5, ]
factors[6, c(1, 3:51)] <- factors_minor[6, ]

#generate a factor intercorrelation matrix
phi <- vec2symMat(rep(0.25, 1275), diag = FALSE)

#compute the correlation matrix
cor_mat <- factors %*% phi %*% t(factors)
diag(cor_mat) <- rep(1, 6)
rownames(cor_mat) <- colnames(cor_mat) <- paste("V", seq(1, 6, 1), sep = "")

#convert correlation matrix into covariance matrix
cov_mat <- diag(rep(10, 6)) %*% cor_mat %*% t(diag(rep(10, 6)))
C4_covmat <- cov_mat

#Mu = 50, Sigma = 10, N = 1000
C4_ref <- round(mvrnorm(n = 1000, mu = rep(50, 6), Sigma = C4_covmat, empirical = FALSE), 0)

########################
## CONDITION 5 - MULTIVARIATE POPULATION (RECOVERY)
########################

#generate noisy 2-factor model for six simulated tests
factors_major <- rnorm(6, 0.60, 0.05)

#generate factor loadings for the minor factors
factors_minor <- matrix(0, ncol = 50, nrow = 6)

for(i in 1:50) {
  y <- 0.80^(i - 1)
  factors_minor[, i] <- rnorm(6, 0, y)
}

#ensure that the minor factors account for about 10% of the overall item variance
for(i in 1:6) {
  factors_minor[i, ] <- sqrt(factors_minor[i, ]^2 / sum(factors_minor[i, ]^2) * 0.1)
}

#combine into a final factor loading matrix for Conditions 1-5
factors <- matrix(0, ncol = 51, nrow = 6)
factors[1:3, 1] <- factors_major[1:3]
factors[4:6, 2] <- factors_major[4:6]

factors[1, 2:51] <- factors_minor[1, ]
factors[2, 2:51] <- factors_minor[2, ]
factors[3, 2:51] <- factors_minor[3, ]

factors[4, c(1, 3:51)] <- factors_minor[4, ]
factors[5, c(1, 3:51)] <- factors_minor[5, ]
factors[6, c(1, 3:51)] <- factors_minor[6, ]

#generate a factor intercorrelation matrix
phi <- vec2symMat(rep(0.25, 1275), diag = FALSE)

#compute the correlation matrix
cor_mat <- factors %*% phi %*% t(factors)
diag(cor_mat) <- rep(1, 6)
rownames(cor_mat) <- colnames(cor_mat) <- paste("V", seq(1, 6, 1), sep = "")

#convert correlation matrix into covariance matrix
cov_mat <- diag(rep(10, 6)) %*% cor_mat %*% t(diag(rep(10, 6)))
C5_covmat <- cov_mat

#Mu = 50, Sigma = 10, N = 100
C5_ref <- round(mvrnorm(n = 100, mu = rep(50, 6), Sigma = C5_covmat, empirical = FALSE), 0)

########################
## CONDITION 6 - MULTIVARIATE POPULATION (RECOVERY)
########################

#generate noisy 2-factor model for six simulated tests
factors_major <- rnorm(6, 0.60, 0.05)

#generate factor loadings for the minor factors
factors_minor <- matrix(0, ncol = 50, nrow = 6)

for(i in 1:50) {
  y <- 0.80^(i - 1)
  factors_minor[, i] <- rnorm(6, 0, y)
}

#ensure that the minor factors account for about 10% of the overall item variance
for(i in 1:6) {
  factors_minor[i, ] <- sqrt(factors_minor[i, ]^2 / sum(factors_minor[i, ]^2) * 0.1)
}

#combine into a final factor loading matrix for Conditions 1-5
factors <- matrix(0, ncol = 51, nrow = 6)
factors[1:3, 1] <- factors_major[1:3]
factors[4:6, 2] <- factors_major[4:6]

factors[1, 2:51] <- factors_minor[1, ]
factors[2, 2:51] <- factors_minor[2, ]
factors[3, 2:51] <- factors_minor[3, ]

factors[4, c(1, 3:51)] <- factors_minor[4, ]
factors[5, c(1, 3:51)] <- factors_minor[5, ]
factors[6, c(1, 3:51)] <- factors_minor[6, ]

#generate a factor intercorrelation matrix
phi <- vec2symMat(rep(0.25, 1275), diag = FALSE)

#compute the correlation matrix
cor_mat <- factors %*% phi %*% t(factors)
diag(cor_mat) <- rep(1, 6)
rownames(cor_mat) <- colnames(cor_mat) <- paste("V", seq(1, 6, 1), sep = "")

#convert correlation matrix into covariance matrix
cov_mat <- diag(rep(10, 6)) %*% cor_mat %*% t(diag(rep(10, 6)))
C6_covmat <- cov_mat

#Mu = 50, Sigma = 10, N = 50
C6_ref <- round(mvrnorm(n = 50, mu = rep(50, 6), Sigma = C6_covmat, empirical = FALSE), 0)

#keep dataset tidy
rm(factors, factors_minor, factors_major, cor_mat, cov_mat, phi, i, y)

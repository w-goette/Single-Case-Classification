#################################################
## DIAGNOSTIC ACCURACY SIMULATION CONDITIONS
#################################################

#load needed libraries
library(MASS)

########################
## RBANS MANUAL DATA
########################

#input summary data from the RBANS manual
RB_cor_mat <- matrix(c(1.00, 0.07, 0.42, 0.54, 0.52, 0.73,
                       0.07, 1.00, 0.25, 0.25, 0.21, 0.52,
                       0.42, 0.25, 1.00, 0.33, 0.40, 0.71,
                       0.54, 0.25, 0.33, 1.00, 0.40, 0.70,
                       0.52, 0.21, 0.40, 0.40, 1.00, 0.74,
                       0.73, 0.52, 0.71, 0.70, 0.74, 1.00),
                     nrow = 6, ncol = 6)

RB_nor_mns <- rep(100, 6)
RB_nor_sds <- rep(15, 6)
RB_mci_mns <- c(83.0, 87.8, 89.7, 96.4, 73.4, 81.8) #reported by Karantzoulis et al., 2013 - https://doi.org/10.1093/arclin/act057
RB_mci_sds <- c(14.1, 17.7, 11.6, 14.6, 16.5, 11.1) #Ibid
RB_alz_mns <- c(59.8, 75.3, 67.7, 71.2, 50.7, 58.4)
RB_alz_sds <- c(16.3, 20.4, 16.7, 17.5, 12.5, 12.0)

#convert correlation matrix to covariance matrix for simulation of multivariate normal data
RB_nor_cov <- diag(RB_nor_sds) %*% RB_cor_mat %*% t(diag(RB_nor_sds))
rownames(RB_nor_cov) <- colnames(RB_nor_cov) <- c("IM", "VC", "LA", "AT", "DM", "TS")

RB_mci_cov <- diag(RB_mci_sds) %*% RB_cor_mat %*% t(diag(RB_mci_sds))
rownames(RB_mci_cov) <- colnames(RB_mci_cov) <- c("IM", "VC", "LA", "AT", "DM", "TS")

RB_alz_cov <- diag(RB_alz_sds) %*% RB_cor_mat %*% t(diag(RB_alz_sds))
rownames(RB_alz_cov) <- colnames(RB_alz_cov) <- c("IM", "VC", "LA", "AT", "DM", "TS")

########################
## WAIS MANUAL DATA
########################

#input summary data from the WAIS manual (Table A.11)
WS_cor_mat <- matrix(c(1.00, 0.49, 0.48, 0.46,
                       0.49, 1.00, 0.51, 0.54,
                       0.48, 0.51, 1.00, 0.53,
                       0.46, 0.54, 0.53, 1.00),
                     nrow = 4, ncol = 4)

WS_nor_mns <- c(10.4, 10.3, 10.4, 9.8)
WS_nor_sds <- c(2.8, 2.8, 3.1, 3.3)
WS_mci_mns <- c(8.9, 9.9, 9.5, 9.0)
WS_mci_sds <- c(2.5, 2.6, 3.6, 2.7)
WS_alz_mns <- c(7.8, 7.5, 7.3, 6.1)
WS_alz_sds <- c(3.8, 3.7, 3.3, 3.5)

#convert correlation matrix to covariance matrix for simulation of multivariate normal data
WS_nor_cov <- diag(WS_nor_sds) %*% WS_cor_mat %*% t(diag(WS_nor_sds))
rownames(WS_nor_cov) <- colnames(WS_nor_cov) <- c("BD", "SI", "DS", "CD")

WS_mci_cov <- diag(WS_mci_sds) %*% WS_cor_mat %*% t(diag(WS_mci_sds))
rownames(WS_mci_cov) <- colnames(WS_mci_cov) <- c("BD", "SI", "DS", "CD")

WS_alz_cov <- diag(WS_alz_sds) %*% WS_cor_mat %*% t(diag(WS_alz_sds))
rownames(WS_alz_cov) <- colnames(WS_alz_cov) <- c("BD", "SI", "DS", "CD")

#################################################
## DIAGNOSTIC ACCURACY RESULTS ANALYSIS
#################################################

#load needed libraries
library(MASS)
library(nnet)
library(randomForest)

#get diagnostic accuracy from highest post-test probability
Diag_Acc <- mean(max.col(Diag_results[, 12:14]) == Diag_results$Population)

#create reference data for linear discriminant function analysis
RefSample <- round(cbind(rbind(matrix(mvrnorm(n = 100, mu = WS_nor_mns, Sigma = WS_nor_cov, empirical = TRUE), ncol = 4, nrow = 100, byrow = TRUE),
                               matrix(mvrnorm(n = 50, mu = WS_mci_mns, Sigma = WS_mci_cov, empirical = TRUE), ncol = 4, nrow = 50, byrow = TRUE),
                               matrix(mvrnorm(n = 40, mu = WS_alz_mns, Sigma = WS_alz_cov, empirical = TRUE), ncol = 4, nrow = 40, byrow = TRUE)),
                         rbind(matrix(mvrnorm(n = 100, mu = RB_nor_mns, Sigma = RB_nor_cov, empirical = TRUE), ncol = 6, nrow = 100, byrow = TRUE),
                               matrix(mvrnorm(n = 50, mu = RB_mci_mns, Sigma = RB_mci_cov, empirical = TRUE), ncol = 6, nrow = 50, byrow = TRUE),
                               matrix(mvrnorm(n = 40, mu = RB_alz_mns, Sigma = RB_alz_cov, empirical = TRUE), ncol = 6, nrow = 40, byrow = TRUE))), 0)

RefSample <- cbind(RefSample, c(rep(1, 100), rep(2, 50), rep(3, 40)))

colnames(RefSample) <- c("BD", "SI", "DS", "CD", "IM", "VC", "LA", "AT", "DM", "TS", "Class")

#train linear discriminant function analysis
LDA <- lda(Class ~., data = as.data.frame(RefSample), prior = c(0.714, 0.148, 0.138))
QDA <- qda(Class ~., data = as.data.frame(RefSample), prior = c(0.714, 0.148, 0.138))

#predict membership from trained LDA functions
LDA_pred <- predict(LDA, Diag_results[, 1:10])$`class`
QDA_pred <- predict(QDA, Diag_results[, 1:10])$`class`

#get LDA classification accuracy
LDA_pred_acc <- mean(LDA_pred == Diag_results$Population)
QDA_pred_acc <- mean(QDA_pred == Diag_results$Population)

#run multinomial regression analysis
MultiFit <- multinom(Class ~., data = as.data.frame(RefSample))

#make predictions from multinomial regression
MultiFit_pred <- predict(MultiFit, Diag_results[, 1:10])

#get multinomial prediction accuracy
MultiFit_pred_acc <- mean(MultiFit_pred == Diag_results$Population)

#run random forest classification methods
RanForest <- randomForest(as.factor(Class) ~. , data = as.data.frame(RefSample), classwt = c(0.714, 0.148, 0.138))

#make predictions from random forest
RanForest_pred <- predict(RanForest, Diag_results[, 1:10])

#get random forest accuracy
RanForest_pred_acc <- mean(RanForest_pred == Diag_results$Population)

#reorganize results into single object
Diag_Acc <- list("Bayesian" = Diag_Acc,
                 "LDA" = LDA_pred_acc,
                 "QDA" = QDA_pred_acc,
                 "Multinomial" = MultiFit_pred_acc,
                 "RandomForest" = RanForest_pred_acc)

#view classification tables
table(max.col(Diag_results[, 12:14]), Diag_results$Population)
table(LDA_pred, Diag_results$Population)
table(QDA_pred, Diag_results$Population)
table(MultiFit_pred, Diag_results$Population)
table(RanForest_pred, Diag_results$Population)

#keep environment tidy
rm(LDA, QDA, MultiFit, LDA_pred_acc, QDA_pred_acc, MultiFit_pred_acc, RanForest_pred_acc)

#run CMH tests
DiagTables <- array(c(as.vector(table(LDA_pred, Diag_results$Population)), 
                      as.vector(table(QDA_pred, Diag_results$Population)), 
                      as.vector(table(MultiFit_pred, Diag_results$Population)), 
                      as.vector(table(RanForest_pred, Diag_results$Population)),
                      as.vector(table(max.col(Diag_results[, 12:14]), Diag_results$Population))), dim = c(3, 3, 5))

Omnibus.CMH <- mantelhaen.test(DiagTables, alternative = "two.sided")
CMH.Bayes_LDA <- mantelhaen.test(DiagTables[, , c(1, 5)], alternative = "two.sided")
CMH.Bayes_QDA <- mantelhaen.test(DiagTables[, , c(2, 5)], alternative = "two.sided")
CMH.Bayes_Mul <- mantelhaen.test(DiagTables[, , c(3, 5)], alternative = "two.sided")
CMH.Bayes_RnF <- mantelhaen.test(DiagTables[, , c(4, 5)], alternative = "two.sided")

p.adjust(c(CMH.Bayes_LDA$p.value,
           CMH.Bayes_QDA$p.value,
           CMH.Bayes_Mul$p.value,
           CMH.Bayes_RnF$p.value), method = "bonferroni")

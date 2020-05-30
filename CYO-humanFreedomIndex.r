#=========================================
# CAPSTONE PROJECT CYO
#  Human Freedom Index
# Stefan Reinhard 2020-05-30
#=========================================


#=========================================
# 1.  Load libraries
#=========================================

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
library("tidyverse")
library("caret")
library("data.table")
library("ggcorrplot")

#=========================================
# 2.  Load datset, create df and tidy up
#=========================================

# Human freedom dataset:
# https://object.cato.org/sites/cato.org/files/human-freedom-index-files/human-freedom-index-2019.csv
# https://www.kaggle.com/gsutters/the-human-freedom-index


#Fetch data and put to dataframe from github
download.file("https://raw.githubusercontent.com/stefanreinhard/PH125.9x-CYO/master/2019.csv", "2019.csv")
# alternative from CATO:
#download.file("https://object.cato.org/sites/cato.org/files/human-freedom-index-files/human-freedom-index-2019.csv", "2019.csv")
data_bulk <- read.csv("2019.csv",stringsAsFactors=FALSE, strip.white = TRUE)

#convert chr to num
str(data_bulk)
for(i in seq(from=5, to=ncol(data_bulk), by=1)){
  data_bulk[, i]  <- as.numeric(data_bulk[, i])
}
rm(i)
head(data_bulk)

#shrink dataset to variables of interest and exclude na
data <- data_bulk[, c(1,2,3,4,5,11,24,28,34,46,54,60,61,70,80,86,99,118,119)] %>% drop_na()

#to analyize we use the 2016 and 2017 dataset
data2016 <- data %>% filter(year ==2016)
data2017 <- data %>% filter(year ==2017)


#=========================================
# 3. Exploratory Data Analysis
#=========================================

#the common EDA
glimpse(data2017)
str(data2017)
summary(data2017)
head(data2017, n=10)

#index distribution
hist(data2017$hf_score, freq=TRUE, xlab="Index score", ylab="Count", main="Human Freedom Index 2017", col = c("gray"))
#score distribution pf
ggplot(data = data2017, aes(x = hf_score, y = pf_score)) + geom_point() + geom_smooth(method = "glm", se = TRUE) + ggtitle("pf over hf")
#score distribution ef
ggplot(data = data2017, aes(x = hf_score, y = ef_score)) + geom_point() + geom_smooth(method = "glm", se = TRUE) + ggtitle("ef over hf")

#correlation pf
data_cor <- data2017[, c(6,7,8,9,10,11,12)]
ggcorrplot(cor(data_cor), title = "correlation pf")

#correlation ef
data_cor <- data2017[, c(14,15,16,17,18)]
ggcorrplot(cor(data_cor), title = "correlation ef")

#correlation ef, pf
data_cor <- data2017[, c(13,19)]
ggcorrplot(cor(data_cor), title = "correlation ef and pf")

#correlation ef, pf, hf
data_cor <- data2017[, c(5,13,19)]
cor(data_cor)
ggcorrplot(cor(data_cor), title = "correlation ef, pf and hf")

#correlation all 2017 
data_cor <- data2017[, c(5,13,19,6,7,8,9,10,11,12,14,15,16,17,18)]
ggcorrplot(cor(data_cor), title = "correlation indicators 2017")

#correlation all 2016 very similar
data_cor <- data2016[, c(5,13,19,6,7,8,9,10,11,12,14,15,16,17,18)]
ggcorrplot(cor(data_cor), title = "correlation indicators 2016")

#=========================================
# 4. Data analysis
#=========================================

#Model 1, average over all variables
model1 <- data2017 %>% mutate(pred_score = (pf_rol + pf_ss 
                                  + pf_movement + pf_religion + pf_association + pf_expression +pf_identity
                                  + ef_government + ef_legal_gender + ef_money + ef_trade + ef_regulation)
                                  /11)
#rsme model 1
rsme_model1 <- RMSE(model1$hf_score, model1$pred_score)
rsme_model1
rmse_results <- tibble(method = "average all", RMSE = rsme_model1)

#Model 2, variables weighting according the HFI report
hf_weighting <- c(0.125, 0.125, 0.05, 0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1, 0.1, 0.1)
model2 <- data2017 %>% mutate(pred_score = pf_rol*hf_weighting[1] + pf_ss*hf_weighting[2] 
                                  + pf_movement*hf_weighting[3] + pf_religion*hf_weighting[4] + pf_association*hf_weighting[5] + pf_expression*hf_weighting[6] + pf_identity*hf_weighting[7]
                                  + ef_government*hf_weighting[8] + ef_legal_gender*hf_weighting[9] + ef_money*hf_weighting[10] + ef_trade*hf_weighting[11] + ef_regulation*hf_weighting[12]
                                  ) 
#rsme model 2
rsme_model2 <- RMSE(model2$hf_score, model2$pred_score)
rsme_model2
rmse_results <- add_row(rmse_results, method = "weighted HFI", RMSE = rsme_model2) 

#show top results (hf_score > 8.5) of model 2 
model2 %>%
  filter(hf_score >= 8.5) %>%
  select(hf_score, countries, hf_score, pred_score) %>% arrange(desc(hf_score))


#find best data partition size
set.seed(2017, sample.kind='Rounding')
train_index <- createDataPartition(data2017$hf_score, times=1, p=0.1, list=FALSE)
train <- data2017[train_index,]
test <- data2017[-train_index,]
fit <- glm(hf_score ~ pf_rol + pf_ss + pf_movement + pf_religion + pf_association + pf_expression +pf_identity + ef_government + ef_legal_gender + ef_money + ef_trade + ef_regulation, data = train)
test <- test %>% mutate(pred_score = predict.glm(fit, newdata=test))
RMSE(test$hf_score, test$pred_score)

#calculate the rmses therof
psize <- seq(from=.30, to=.90, by=.01)
rmses <- sapply(psize, function(p){
  train_index <- createDataPartition(data2017$hf_score, times=1, p=p, list=FALSE)
  train <- data2017[train_index,]
  test <- data2017[-train_index,]
  fit <- glm(hf_score ~ pf_rol + pf_ss + pf_movement + pf_religion + pf_association + pf_expression +pf_identity + ef_government + ef_legal_gender + ef_money + ef_trade + ef_regulation, data = train)
  test <- test %>% mutate(pred_score = predict.glm(fit, newdata=test))
  RMSE(test$hf_score, test$pred_score)
})

#accuracy in probabilities, best rmse
plot(psize, rmses)
min(rmses)
psize[which.min(rmses)]


#data partition with p=0.90
set.seed(2017, sample.kind='Rounding')

train_index <- createDataPartition(data2017$hf_score, times=1, p=0.90, list=FALSE)
train <- data2017[train_index,]
test <- data2017[-train_index,]

#fit model
fit <- glm(hf_score ~ pf_rol + pf_ss + pf_movement + pf_religion + pf_association + pf_expression +pf_identity + ef_government + ef_legal_gender + ef_money + ef_trade + ef_regulation, data = train)

#predicted scores to a new data frame
results <- test %>% mutate(pred_score = predict.glm(fit, newdata = test))
#rsme
RMSE(results$hf_score, results$pred_score)

#predicted scores over actual scores
ggplot(data = results, aes(hf_score, pred_score)) +  geom_point() + geom_smooth(method = "glm", se = TRUE) + ggtitle("predicted scores over hf scores 2017")

#coefficients fitted model
fit$coefficients

#Model 3, with coefficients from fitted model
model3 <- data2017 %>% mutate(pred_score = (pf_rol*fit$coefficients[2] + pf_ss*fit$coefficients[3] + pf_movement*fit$coefficients[4] + pf_religion*fit$coefficients[5] + pf_association*fit$coefficients[6] + pf_expression*fit$coefficients[7] + pf_identity*fit$coefficients[8] + ef_government*fit$coefficients[9] + ef_legal_gender*fit$coefficients[10] + ef_money*fit$coefficients[11] + ef_trade*fit$coefficients[12] + ef_regulation*fit$coefficients[13] + fit$coefficients[1] ))

#rmse model 3
rsme_model3 <- RMSE(model3$hf_score, model3$pred_score)
rsme_model3
rmse_results <- add_row(rmse_results, method = "mutated coefficients 2017", RMSE = rsme_model3) 

#updated results
model3 %>% filter(hf_score >= 8.5) %>% select(countries, hf_score, pred_score) %>% arrange(desc(hf_score))

##Full data set partition
train_index <- createDataPartition(data$hf_score, times=1, p=0.90, list=FALSE)
train <- data[train_index,]
test <- data[-train_index,]

#fit model
fit_full <- glm(hf_score ~ pf_rol + pf_ss + pf_movement + pf_religion + pf_association + pf_expression +pf_identity + ef_government + ef_legal_gender + ef_money + ef_trade + ef_regulation, data = train)

#predicted scores to a new data frame
results_full <- test %>% mutate(pred_score = predict.glm(fit_full, newdata=test))

#predicted scores over actual scores
ggplot(data = results_full, aes(hf_score, pred_score)) +  geom_point() + geom_smooth(method = "glm", se = TRUE) + ggtitle("predicted scores over hf scores")

#rmse fit model
RMSE(results_full$hf_score, results_full$pred_score)

# print coefficients of fitted model
fit_full$coefficients

p_weighting <- c(fit_full$coefficients[2],fit_full$coefficients[3],fit_full$coefficients[4],fit_full$coefficients[5],fit_full$coefficients[6],fit_full$coefficients[7],fit_full$coefficients[8],fit_full$coefficients[9],fit_full$coefficients[10],fit_full$coefficients[11],fit_full$coefficients[12],fit_full$coefficients[13])

#Final model with fitted coefficients from all data 
model4 <- data2017 %>% mutate(pred_score = (pf_rol*fit_full$coefficients[2] + pf_ss*fit_full$coefficients[3] + pf_movement*fit_full$coefficients[4] + pf_religion*fit_full$coefficients[5] + pf_association*fit_full$coefficients[6] + pf_expression*fit_full$coefficients[7] + pf_identity*fit_full$coefficients[8] 
                                                + ef_government*fit_full$coefficients[9] + ef_legal_gender*fit_full$coefficients[10] + ef_money*fit_full$coefficients[11] + ef_trade*fit_full$coefficients[12] + ef_regulation*fit_full$coefficients[13] +fit_full$coefficients[1])
                                                )
#rmse model 4
rsme_model4 <- RMSE(model4$hf_score, model4$pred_score)
rsme_model4
rmse_results <- add_row(rmse_results, method = "mutated coefficients all years", RMSE = rsme_model4) 


#all rmse
rmse_results

#tidy up final model, create ranking
final_model <- model4
final_model$hf_rank <- NA
final_model <- arrange(final_model, desc(hf_score)) %>%  mutate(hf_rank = 1:nrow(final_model))
final_model <- arrange(final_model, desc(pred_score)) %>%  mutate(p_rank = 1:nrow(final_model))
final_model <-  final_model%>% mutate(rank_diff = hf_rank-p_rank)
final_model %>% filter(year == "2017", rank_diff != 0)

#correlation weighting 
cor(p_weighting,hf_weighting, method="pearson")


#calculate weight differences
weights <- rbind(hf_weighting, p_weighting)
weights <- as.data.frame(t(as.matrix(weights)),
                         stringsAsFactors = FALSE)
weights <- weights %>% rownames_to_column() %>% mutate(diff_weighting =  p_weighting  -hf_weighting)  %>% 
  column_to_rownames()
weights <- tibble::rownames_to_column(weights, "variables")

#plot weight differences
weights %>%  ggplot(aes( x = p_weighting,y= variables, color = "predicted")) + geom_point() + ggtitle("variables over weighting predicted")  + geom_point(aes(size = diff_weighting)) + geom_point(aes( x = hf_weighting, colour = "hf index"))

#calculate add measurements 
weights$diff_weighting[1]
str(weights)

data_cor <- data2017[, c(5,13,19)]
ggcorrplot(cor(data_cor), title = "correlation ef, pf and hf")

cor_weights <- cor(weights[2:3])
ggcorrplot(cor_weights)

#display difference models
final_model %>% select(hf_score, pred_score) %>% gather(key=Type, value=Value) %>%  ggplot(aes(x=Value,fill=Type)) + geom_histogram(bins = 30)

#plot weight desnsity
final_model %>% select(hf_score, pred_score) %>% gather(key=Type, value=Value) %>%  ggplot(aes(x=Value,fill=Type)) + geom_density(alpha = 0.5)

#Freedom index
hist(final_model$hf_score, freq=TRUE, 
     main="Human Freedom Index 2017", xlab="hf_score", ylab="Count")
#Freedom index predicted
hist(final_model$pred_score, freq=TRUE,  
     main="Predicted Human Freedom Index 2017", xlab="pred_score", ylab="Count")
#correlation pred score and hf score
ggplot(data = final_model, aes(x = hf_score, y = pred_score)) + geom_point() + geom_smooth(method = "glm", se = TRUE)

#calculate measurements to understand the changes 
final_model %>% filter(year == "2017",  rank_diff < -2)  %>% nrow()
final_model %>% filter(year == "2017", rank_diff > 2) %>% nrow() 
final_model %>% filter(year == "2017", rank_diff == 0 ) %>% nrow() 
final_model %>% filter(year == "2017", rank_diff == -1 | rank_diff == 1 ) %>% nrow() 
final_model %>% filter(year == "2017", rank_diff > -1 | rank_diff < 1 ) %>% nrow() 
head (final_model %>% filter(year == "2017",  rank_diff < -2) %>% select("countries","region", "rank_diff") %>% arrange(rank_diff))
head (final_model %>% filter(year == "2017", rank_diff >2) %>% select("countries","region", "rank_diff") %>% arrange(desc(rank_diff)))
final_model %>% filter(year == "2017", rank_diff >2) %>% select("countries","region", "rank_diff") %>% arrange(desc(rank_diff))
final_model %>% filter(year == "2017", rank_diff >2) %>%  ggplot( aes(x = hf_rank, y = p_rank)) + geom_point() + geom_smooth(method = "glm", se = TRUE)

# Regions with biggest change
final_model %>% filter(year == "2017",  rank_diff < -2 | rank_diff > 2 )  %>%  ggplot(aes(x = rank_diff, y = countries, color = region)) + geom_point() + ggtitle("Rank difference between HF score and computed score") + xlab("rank different >2")
final_model %>% filter(year == "2017",  rank_diff < -2 ) %>% group_by(region) %>% summarise(count = n()) %>% arrange(count)
final_model %>% filter(year == "2017",  rank_diff > 2 ) %>% group_by(region) %>% summarise(count = n()) %>% arrange(desc(count))

# save project/session for Rmd
save.image()


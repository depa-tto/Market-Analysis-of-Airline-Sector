library(dplyr)
library(readr)
library(tidyverse)
library(ggcorrplot)
library(factoextra)
library(ggplot2)
library(ggrepel)
library(formattable)
library(plyr)  
library(caret)
library(gridExtra) 
library(rsample)
library(e1071) 
library(GGally)
library(data.table)
library(DT)
library(tidyr)
library(corrplot)
library(rms)
library(MASS)
library(e1071)
library(ROCR)
library(gplots)
library(pROC)
library(ggpubr)
library(rpart)
library(rpart.plot)
library(jtools)
library(gplots)
library(margins)
library(rcompanion)

# data
raw_data <- read_csv("https://raw.githubusercontent.com/depa-tto/Statistical-Learning-Module-Antonio-De-Patto/refs/heads/main/airline_satisfaction.csv")
middle_data <- raw_data[apply(raw_data, MARGIN = 1, FUN = function(rows) all(rows!=0)),] %>% drop_na()
data <- middle_data %>% dplyr::select(satisfaction : `Flight Distance`) %>% 
  dplyr::rename(satisfaction = satisfaction, gender = Gender, customer_type = `Customer Type`, age = Age,
         travel_type = `Type of Travel`, class = Class, distance = `Flight Distance`)

data$customer_type <- car::recode(data$customer_type, "'disloyal Customer' =  'Disloyale Customer'; 'Loyal Customer' = 'Loyal Customer'")


# descriptive analysis

p1 <- ggplot(data, aes(x = satisfaction)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = after_stat(count), 
            label = paste0(round(prop.table(after_stat(count)), 4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 5) + theme_classic() + scale_fill_manual(values = c("#FC4E07", "#1B80BF"))


p2 <- ggplot(data, aes(x = gender)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = after_stat(count), 
            label = paste0(round(prop.table(after_stat(count)),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 4) + theme_classic() + scale_fill_manual(values = c("#FC4E07", "#1B80BF"))

p3 <- ggplot(data, aes(x = customer_type)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = after_stat(count), 
            label = paste0(round(prop.table(after_stat(count)),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 4) + theme_classic() + scale_fill_manual(values = c("#FC4E07", "#1B80BF"))


p4 <- ggplot(data, aes(x = travel_type)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = after_stat(count), 
            label = paste0(round(prop.table(after_stat(count)),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 4) + theme_classic() + scale_fill_manual(values = c("#FC4E07", "#1B80BF"))

p5 <- ggplot(data, aes(x = class)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = after_stat(count), 
            label = paste0(round(prop.table(after_stat(count)),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 4)+ theme_classic() + scale_fill_manual(values = c("#FC4E07", "#1B80BF"))

p1
p2
p3
p4
p5

grid.arrange(p2, p3, p4, p5, ncol=2)


# variabili numeriche

p6 <- ggplot(data = data, aes(age, color = satisfaction))+
  geom_freqpoly(binwidth = 5, linewidth = 1) +
  scale_color_manual(values = c("#FC4E07", "#1B80BF")) + theme_classic()

p7 <- ggplot(data = data, aes(distance, color = satisfaction))+
  geom_freqpoly(binwidth = 5, linewidth = 1) +
  scale_color_manual(values = c("#FC4E07", "#1B80BF")) + theme_classic()

grid.arrange(p6, p7, ncol=2)

mean(data$age)

qqnorm(data$age) 
ggqqplot(data$age)
ggdensity(data, x = "age", fill = "lightgray", title = "Age") +
  stat_overlay_normal_density(color = "#FC4E07", linetype = "dashed")


qqnorm(data$distance)
ggqqplot(data$distance)
ggdensity(data, x = "distance", fill = "lightgray", title = "Flight Distance") +
  stat_overlay_normal_density(color = "#FC4E07", linetype = "dashed")

# outliers

boxplot(data$age, col = "#1B80BF", main = 'Age')
quartiles <- quantile(data$age, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data$age)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

final_data <- subset(data, data$age > Lower & data$age < Upper)


dim(final_data)
boxplot(final_data$age, col = "#1B80BF", main = 'Age')


boxplot(final_data$distance, col = "#1B80BF", main = 'Distance with outliers')
quartiles <- quantile(final_data$distance, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(final_data$distance)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

final_data <- subset(final_data, final_data$distance > Lower & final_data$distance < Upper)

dim(data)
dim(final_data)
boxplot(final_data$distance, col = "#1B80BF", main = 'Distance without outliers')



qqnorm(final_data$age)
ggqqplot(final_data$age)
ggdensity(final_data, x = "age", fill = "lightgray", title = "Age") +
  stat_overlay_normal_density(color = "#FC4E07", linetype = "dashed")


qqnorm(final_data$distance)
ggqqplot(final_data$distance)
ggdensity(final_data, x = "distance", fill = "lightgray", title = "Flight Distance") +
  stat_overlay_normal_density(color = "#FC4E07", linetype = "dashed")


final_data$class <- car::recode(final_data$class, "'Business' =  'Business'; 'Eco' = 'Eco'; 'Eco Plus' = 'Eco'")


# descriptive analysis

p8 <- ggplot(final_data, aes(x = satisfaction)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = after_stat(count), 
                label = paste0(round(prop.table(after_stat(count)), 4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 5) + theme_classic() + scale_fill_manual(values = c("#FC4E07", "#1B80BF"))


p9 <- ggplot(final_data, aes(x = gender)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = after_stat(count), 
                label = paste0(round(prop.table(after_stat(count)),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 4) + theme_classic() + scale_fill_manual(values = c("#FC4E07", "#1B80BF"))

p10 <- ggplot(final_data, aes(x = customer_type)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = after_stat(count), 
                label = paste0(round(prop.table(after_stat(count)),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 4) + theme_classic() + scale_fill_manual(values = c("#FC4E07", "#1B80BF"))


p11 <- ggplot(final_data, aes(x = travel_type)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = after_stat(count), 
                label = paste0(round(prop.table(after_stat(count)),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 4) + theme_classic() + scale_fill_manual(values = c("#FC4E07", "#1B80BF"))

p12 <- ggplot(final_data, aes(x = class)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = after_stat(count), 
                label = paste0(round(prop.table(after_stat(count)),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 4)+ theme_classic() + scale_fill_manual(values = c("#FC4E07", "#1B80BF"))

p8
p9
p10
p11
p12

grid.arrange(p9, p10, p11, p12, ncol=2)


# variabili numeriche

p13 <- ggplot(data = final_data, aes(age, color = satisfaction))+
  geom_freqpoly(binwidth = 5, linewidth = 1) +
  scale_color_manual(values = c("#FC4E07", "#1B80BF")) + theme_classic()

p14 <- ggplot(data = final_data, aes(distance, color = satisfaction))+
  geom_freqpoly(binwidth = 5, linewidth = 1) +
  scale_color_manual(values = c("#FC4E07", "#1B80BF")) + theme_classic()

grid.arrange(p13, p14, ncol=2)

mean(final_data$age)

qqnorm(final_data$age) 
ggqqplot(final_data$age)
ggdensity(final_data, x = "age", fill = "lightgray", title = "Age") +
  stat_overlay_normal_density(color = "#FC4E07", linetype = "dashed")


qqnorm(final_data$distance)
ggqqplot(final_data$distance)
ggdensity(final_data, x = "distance", fill = "lightgray", title = "Flight Distance") +
  stat_overlay_normal_density(color = "#FC4E07", linetype = "dashed")


# correlation

cor(final_data$age, final_data$distance)

chisq.test(final_data$satisfaction, final_data$gender)
cramerV(table(final_data$satisfaction, final_data$gender))

chisq.test(final_data$satisfaction, final_data$customer_type)
cramerV(table(final_data$satisfaction, final_data$customer_type))

chisq.test(final_data$satisfaction, final_data$travel_type)
cramerV(table(final_data$satisfaction, final_data$travel_type))


chisq.test(final_data$satisfaction, final_data$class)
cramerV(table(final_data$satisfaction, final_data$class))


chisq.test(final_data$gender, final_data$customer_type)
cramerV(table(final_data$gender, final_data$customer_type))


chisq.test(final_data$gender, final_data$travel_type)
cramerV(table(final_data$gender, final_data$travel_type))


chisq.test(final_data$gender, final_data$class)
cramerV(table(final_data$gender, final_data$class))


chisq.test(final_data$customer_type, final_data$travel_type)
cramerV(table(final_data$customer_type, final_data$travel_type))


chisq.test(final_data$customer_type, final_data$class)
cramerV(table(final_data$customer_type, final_data$class))



chisq.test(final_data$travel_type, final_data$class)
cramerV(table(final_data$travel_type, final_data$class))


frequency_data <- final_data %>% dplyr::count(travel_type, class)

ggplot(frequency_data, aes(x = travel_type, y = class, size = n)) +
  geom_point(color = "#1B80BF", alpha = 0.7, shape = 16) +
  geom_text(aes(label = n), size = 3, color = "black", vjust = 1.5) +  
  scale_size_continuous(range = c(5, 25)) +
  labs(x = "Travel type", y = "Class") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12, color = "#333333"),
        axis.title = element_text(size = 14, face = "bold", color = "#333333"),
        panel.grid.major = element_line(color = "#DDDDDD"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 16, hjust = 0.5, color = "#333333"),
        plot.caption = element_blank(),
        legend.position = "none")


# training and test

set.seed(123)
split_train_test <- createDataPartition(final_data$satisfaction, times = 1, p = 0.8, list = FALSE)
dtrain <- final_data[split_train_test,]
dtest <-  final_data[-split_train_test,]


dim(final_data)
dim(dtrain)
dim(dtest)


# full logistic model

dtrain$satisfaction[dtrain$satisfaction == 'satisfied'] <- 1
dtrain$satisfaction[dtrain$satisfaction == 'dissatisfied'] <- 0

dtest$satisfaction[dtest$satisfaction == 'satisfied'] <- 1
dtest$satisfaction[dtest$satisfaction == 'dissatisfied'] <- 0


full_model <- glm(as.numeric(satisfaction) ~ ., data = dtrain, family = binomial(link = 'logit'))
summary(full_model)
export_summs(full_model, model.names = 'Full model', error_format = "({p.value})",
             error_pos = 'right',  scale = TRUE, robust = TRUE)


# full logistic model

full_model_1 <- glm(as.numeric(satisfaction) ~ gender + customer_type + log(age) + travel_type + class + distance, data = dtrain, family = binomial(link = 'logit'))
summary(full_model_1)
export_summs(full_model_1, model.names = 'Full model with log(age)', error_format = "({p.value})",
             error_pos = 'right',  scale = TRUE, robust = TRUE)


# logistic regression with stepwise regression

lr_fit <- glm(as.numeric(satisfaction) ~ ., data = dtrain, family = binomial(link = 'logit')) %>%
  stepAIC(direction = "both", trace = T)

summary(lr_fit)
export_summs(lr_fit, model.names = 'Forward and backward stepwise regression model', error_format = "({p.value})",
             error_pos = 'right',  scale = TRUE, robust = TRUE)

# effetti margiali stepwise

logit_scalar <- mean(dlogis(predict(lr_fit, type = "link")))
effetti_marginali_logit <- logit_scalar * coef(lr_fit)
effetti_marginali_logit 
logit_margins <- margins::margins(lr_fit)
logit_margins
x <- margins::margins_summary(lr_fit)
y <- round(x[-1], 4)
z <- cbind(x[1], y)

formattable(as.data.frame(z), list('AME' = color_tile("transparent", "lightblue"),
                                    'p'= color_tile("transparent", "lightblue")))

lr_prob1 <- predict.glm(lr_fit, newdata = dtest, type = "response")
lr_pred1 <- ifelse(lr_prob1 > 0.5,"satisfied", "dissatisfied")
table(Predicted = lr_pred1, Actual = dtest$satisfaction)

# accuracy

lr_prob2 <- predict.glm(lr_fit, newdata = dtrain, type = "response")
lr_pred2 <- ifelse(lr_prob2 > 0.5,"satisfied", "dissatisfied")
lr_tab1 <- table(Predicted = lr_pred2, Actual = dtrain$satisfaction)
lr_tab2 <- table(Predicted = lr_pred1, Actual = dtest$satisfaction)

dtrain$satisfaction[dtrain$satisfaction == '1'] <- 'satisfied'
dtrain$satisfaction[dtrain$satisfaction == '0'] <- 'dissatisfied'

dtest$satisfaction[dtest$satisfaction == '1'] <- 'satisfied'
dtest$satisfaction[dtest$satisfaction == '0'] <- 'dissatisfied'

# train
confusionMatrix(as.factor(lr_pred2), as.factor(dtrain$satisfaction), positive = "satisfied")

cm <- confusionMatrix(as.factor(lr_pred2), as.factor(dtrain$satisfaction), positive = "satisfied")
cm_matrix <- as.matrix(cm$table)
fourfoldplot(cm_matrix, color = c("#FC4E07", "#1B80BF"), conf.level = 0, margin = 1, main = "Confusion Matrix")



# test
confusionMatrix(as.factor(lr_pred1), as.factor(dtest$satisfaction), positive = "satisfied")

cm1 <- confusionMatrix(as.factor(lr_pred1), as.factor(dtest$satisfaction), positive = "satisfied")

cm1_matrix <- as.matrix(cm1$table)
fourfoldplot(cm1_matrix, color = c("#FC4E07", "#1B80BF"), conf.level = 0, margin = 1, main = "Confusion Matrix")


lr_prob2 <- predict.glm(lr_fit, dtest, type = "response")
test_roc = roc(dtest$satisfaction ~ lr_prob2, plot = TRUE, print.auc = TRUE)
as.numeric(test_roc$auc)


coords(test_roc, "best", ret = c("threshold", "specificity", "sensitivity"))
formattable(coords(test_roc, "best", ret = c("threshold", "specificity", "sensitivity")))


# linear discriminant analysis

lda.fit = lda(satisfaction ~ gender + customer_type + scale(age) + travel_type + class + scale(distance), data = dtrain)

lda_coeff <- lda.fit$scaling
lda_coeff
colnames(lda_coeff) <- c("Coefficients of LDA")
formattable(as.data.frame(lda_coeff), list('Coefficients of LDA' = color_tile("transparent", "lightblue")))

plot(lda.fit, col = c("#1B80BF"))
lda.prediction = predict(lda.fit, dtest)$class
table(lda.prediction, dtest$satisfaction)

confusionMatrix(as.factor(lda.prediction), as.factor(dtest$satisfaction), positive = "satisfied")
cm2 <- confusionMatrix(as.factor(lda.prediction), as.factor(dtest$satisfaction), positive = "satisfied")
cm2_matrix <- as.matrix(cm2$table)
fourfoldplot(cm2_matrix, color = c("#FC4E07", "#1B80BF"), conf.level = 0, margin = 1, main = "Confusion Matrix")

mean(lda.prediction == dtest$satisfaction)

final_data$satisfaction <- as.factor(final_data$satisfaction)
colors <- c("#FC4E07", "#1B80BF")
plot(final_data[, -1], col = colors[final_data$satisfaction])


# trees 

tree1 <- rpart(satisfaction ~ ., data = dtrain, 
               control = rpart.control(cp = 0.003, minsplit = 10, maxsurrogate = 10))
printcp(tree1)
formattable(as.data.frame(printcp(tree1)), list('CP' = color_tile("transparent", "lightblue"),
                                               'rel error' = color_tile("transparent", "lightblue"),
                                               'xerror' = color_tile("transparent", "lightblue")))

rpart.plot(tree1, digits = 2, under = TRUE, tweak = 1.5, branch = 0.5)
           
tree.pred = predict(tree1, dtest, type = "class")
table(tree.pred, dtest$satisfaction)

confusionMatrix(as.factor(tree.pred), as.factor(dtest$satisfaction), positive = "satisfied")
cm3 <- confusionMatrix(as.factor(tree.pred), as.factor(dtest$satisfaction), positive = "satisfied")
cm3_matrix <- as.matrix(cm3$table)
fourfoldplot(cm3_matrix, color = c("#FC4E07", "#1B80BF"), conf.level = 0, margin = 1, main = "Confusion Matrix")


tree2 <- rpart(satisfaction ~ ., data = dtrain)
printcp(tree2)
rpart.plot(tree2)
tree3 <- prune(tree2,cp=0.03)
rpart.plot(tree3)


# k-fold cross validation 

ctrlspecs <- trainControl(method = "cv", number = 10, savePredictions = "all", classProbs = TRUE)

cv_model <- train(satisfaction ~ gender + customer_type + scale(age) + travel_type + class + scale(distance), data = dtrain, method="glm", 
                family=binomial, trControl = ctrlspecs)

print(cv_model)
summary(cv_model)

predictions <- predict(cv_model, newdata = dtest)

confusionMatrix(as.factor(predictions), as.factor(dtest$satisfaction), positive = "satisfied")
cm4 <- confusionMatrix(as.factor(predictions), as.factor(dtest$satisfaction), positive = "satisfied")
cm4_matrix <- as.matrix(cm4$table)
fourfoldplot(cm4_matrix, color = c("#FC4E07", "#1B80BF"), conf.level = 0, margin = 1, main = "Confusion Matrix")





















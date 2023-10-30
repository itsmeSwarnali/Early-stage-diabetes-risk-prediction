library(readr)
library(tidyverse)
library(cowplot)
library(colorspace)
data <- read.csv("diabetes_data_upload.csv")


data_eda <- data
names(data)
str(data)
attach(data)

p1 <- ggplot(data_eda, aes(x=class , y=Age))+  geom_violin(aes(fill=class))+  geom_boxplot()+  theme_classic()+theme(legend.position = "none")+scale_fill_brewer(palette = "Set1")
p1
p2 <- ggplot(data_eda, aes(x=Gender))+  geom_bar(aes(fill= class), position= "stack")+  theme_classic()+  theme(legend.position = "none")+scale_fill_brewer(palette = "Set1")

p2
p3 <- ggplot(data_eda, aes(x=Polyuria))+  geom_bar(aes(fill= class), position= "stack")+  theme_classic()+  theme(legend.position = "none")+scale_fill_brewer(palette = "Set1")
p3

p4 <- ggplot(data_eda, aes(x=Polydipsia))+geom_bar(aes(fill= class), position= "stack")+  theme_classic()+  theme(legend.position = "none")+scale_fill_brewer(palette = "Set1")
p4

p5 <- ggplot(data_eda, aes(x=sudden.weight.loss))+geom_bar(aes(fill= class), position= "stack")+  theme_classic()+  theme(legend.position = "none")+scale_fill_brewer(palette = "Set1")
p5
p6 <- ggplot(data_eda, aes(x=weakness))+geom_bar(aes(fill= class), position= "stack")+  theme_classic()+  theme(legend.position = "none")+scale_fill_brewer(palette = "Set1")
p6
p7 <- ggplot(data_eda, aes(x=Polyphagia))+geom_bar(aes(fill= class), position= "stack")+  theme_classic()+  theme(legend.position = "none")+scale_fill_brewer(palette = "Set1")
p7
p8 <- ggplot(data_eda, aes(x=Genital.thrush))+geom_bar(aes(fill= class), position= "stack")+  theme_classic()+  theme(legend.position = "none")+scale_fill_brewer(palette = "Set1")
p8
p9 <- ggplot(data_eda, aes(x=visual.blurring))+geom_bar(aes(fill= class), position= "stack")+  theme_classic()+  theme(legend.position = "none")+scale_fill_brewer(palette = "Set1")
p9
p10 <- ggplot(data_eda, aes(x=Itching))+geom_bar(aes(fill= class), position= "stack")+  theme_classic()+  theme(legend.position = "none")+scale_fill_brewer(palette = "Set1")
p10
p11 <- ggplot(data_eda, aes(x=Irritability))+geom_bar(aes(fill= class), position= "stack")+  theme_classic()+  theme(legend.position = "none")+scale_fill_brewer(palette = "Set1")
p11
p12 <- ggplot(data_eda, aes(x=delayed.healing))+geom_bar(aes(fill= class), position= "stack")+  theme_classic()+  theme(legend.position = "none")+scale_fill_brewer(palette = "Set1")
p12
p13 <- ggplot(data_eda, aes(x=partial.paresis))+geom_bar(aes(fill= class), position= "stack")+  theme_classic()+  theme(legend.position = "none")+scale_fill_brewer(palette = "Set1")
p13
p14 <- ggplot(data_eda, aes(x=muscle.stiffness))+geom_bar(aes(fill= class), position= "stack")+  theme_classic()+  theme(legend.position = "none")+scale_fill_brewer(palette = "Set1")
p14
p15 <- ggplot(data_eda, aes(x=Alopecia))+geom_bar(aes(fill= class), position= "stack")+  theme_classic()+  theme(legend.position = "none")+scale_fill_brewer(palette = "Set1")
p15
p16 <- ggplot(data_eda, aes(x=Obesity))+geom_bar(aes(fill= class), position= "stack")+  theme_classic()+  theme(legend.position = "none")+scale_fill_brewer(palette = "Set1")
p16
p17 <- ggplot(data_eda, aes(x=class))+geom_bar(aes(fill= class), position= "dodge")+  theme_classic()+  geom_label(stat = "count", aes(label = ..count..))+scale_fill_brewer(palette = "Set1")
p17

plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17)


data_corr <- data %>% mutate_if(is.character, as.factor) %>% mutate_if(is.factor, as.numeric)

library(GGally)
knitr::kable(cor(data_corr))
ggcorr(data_corr, method = c("everything", "pearson"),nbreaks = 4,
       label=TRUE, label_round =2 ,legend.size = 5, vjust=0, hjust=0.8)+
  theme(legend.position = "bottom")+
  theme(title = element_text(size=7))



## Clustering

## Optimal number of clusters
library(factoextra)
library(NbClust)
library(cluster)


# {x- mean(x)}/sd(x) scaling or mean, standard deviation scaling
data_clust <- data_corr %>% select(-c(class, Age, Gender)) %>% scale()

# Get Distance
distance <- get_dist(data_clust)
distance
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"),
          lab_size = 1)


# K-Means clustering
KmeansCluster <- NbClust(data_clust, distance = "euclidean",
                         min.nc = 2, max.nc = 10, method = "kmeans")
KmeansCluster$All.index
view(KmeansCluster$Best.nc)


# Elbow method
plot_elbow <- fviz_nbclust(data_clust, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method")+
  theme(title=element_text(face="bold", size=20),
        legend.text = element_text(face="bold", size=15))

plot_elbow
# Silhouette method
plot_silhouette <- fviz_nbclust(data_clust, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")+
  theme(title=element_text(face="bold", size=20),
        legend.text = element_text(face="bold", size=15))

plot_silhouette


################# Final k=3
k_clust <- kmeans(data_clust, centers = 3, nstart = 25)

fviz_cluster(k_clust, data= data_clust, labelsize = 8)+theme_classic()
?fviz_cluster


k_clust$cluster

Rdata_k2 <- data.frame(cbind(data_clust, k_clust$cluster))
names(data_k2)[15] <- "Cluster"
# Clusrer Centers
knitr::kable(k_clust$centers)
# Cluster Size
k_clust$size
#### Class with Cluster
knitr::kable(table(data$class, k_clust$cluster))

# age with Clusrer
data %>%select(Age) %>%  mutate(cluster= k_clust$cluster) %>% 
  group_by(cluster)  %>%  summarise_all("mean")




library(Rtsne)
names(data_corr)
data_tsne <- data_corr %>% select(-c(class,Age,Gender)) %>%   mutate(ID=row_number()) 
names(data_tsne)
names(data_k2)
data_meta <- data_k2 %>% select(Cluster) %>% mutate(
  ID=row_number(),
  Cluster= as.factor(Cluster))
tSNE_fit <- data_tsne %>%
  select(where(is.numeric)) %>%
  column_to_rownames("ID") %>%
  scale() %>% 
  Rtsne(check_duplicates = FALSE)

tSNE_df <- tSNE_fit$Y %>% 
  as.data.frame() %>%
  rename(tSNE1="V1",
         tSNE2="V2") %>%
  mutate(ID=row_number())

tSNE_df <- tSNE_df %>%
  inner_join(data_meta, by="ID")

names(tSNE_df)
tSNE_df$Cluster <- as.factor(tSNE_df$Cluster)
tSNE_df %>%
  ggplot(aes(x = tSNE1, 
             y = tSNE2,
             color = Cluster,
             shape = Cluster))+
  geom_point(size = 5 )+
  theme_classic()+
  theme(title=element_text(face="bold", size=20),
        legend.text = element_text(face="bold", size=15))

# Model
library(caret)
library(tidyverse)

# Data 
data_model <- data_corr %>% select(-class) %>% scale()
data_k2 <- data.frame(cbind(data_model, k_clust$cluster))
data_k2 <- data.frame(cbi)
names(data_k2)[17] <- "Cluster"
names(data_k2)
data_k2$Cluster <- as.factor(data_k2$Cluster)

######################### Creating training and testing data########################
ind_train = createDataPartition(data_k2$Cluster, p = 0.7, list = FALSE)
train <- data_k2[ind_train,]
test <- data_k2[-ind_train,]
ctrl <- trainControl(method="cv",number = 10, savePredictions = TRUE)

# Random forest
model_RF <- train(Cluster ~ . , data = train, importance = TRUE, method='rf')

predicted_RF_train <- predict(model_RF, train)
knitr::kable(table(predicted_RF_train, train$Cluster),caption = "Confusion Matrix for training dataset")
train_cm_RF <- confusionMatrix(train$Cluster, predicted_RF_train)

predicted_RF_test <- predict(model_RF, test)
knitr:: kable(table(predicted_RF_test, test$Cluster), caption = "Confusion matrix for testing file")
test_cm_RF<-confusionMatrix(test$Cluster, predicted_RF_test)

knitr::kable(data.frame(train_cm_RF$overall),caption = "Overall Accuracy Training dataset")
knitr::kable(data.frame(test_cm_RF$overall), caption = "Overall Accuracy Testing dataset")

# Decision tree
model_DT <- train(Cluster ~ . , data = train, method='rpart2' )

predicted_RF_train <- predict(model_RF, train)
knitr::kable(table(predicted_RF_train, train$Cluster),caption = "Confusion Matrix for training dataset")
train_cm_RF <- confusionMatrix(train$Cluster, predicted_RF_train)

predicted_RF_test <- predict(model_RF, test)
knitr:: kable(table(predicted_RF_test, test$Cluster), caption = "Confusion matrix for testing file")
test_cm_RF<-confusionMatrix(test$Cluster, predicted_RF_test)

knitr::kable(data.frame(train_cm_RF$overall),caption = "Overall Accuracy Training dataset")
knitr::kable(data.frame(test_cm_RF$overall), caption = "Overall Accuracy Testing dataset")

# Support Vector Machine (Radial Kurnel)

library(kernlab)      # SVM methodology
library(e1071)        # SVM methodology

model_SVM <- train(Cluster~., data=train, method='lssvmRadial')

predicted_SVM_train <- predict(model_SVM, train)
knitr::kable(table(predicted_SVM_train, train$Cluster),caption = "Confusion Matrix for training dataset")
train_cm_SVM<-confusionMatrix(predicted_SVM_train, train$Cluster)

predicted_SVM_test <- predict(model_SVM, test)
knitr:: kable(table(predicted_SVM_test, test$Cluster), caption = "Confusion matrix for testing file")
test_cm_SVM<-confusionMatrix(predicted_SVM_test, test$Cluster)

knitr::kable(data.frame(train_cm_SVM$overall),caption = "Overall Accuracy training dataset")
knitr::kable(data.frame(test_cm_SVM$overall), caption = "Overall Accuracy testing dataset")

# Multi-Layer Perception
model_MLP <- train(Cluster~., data=train, method='mlp')

predicted_MLP_train <- predict(model_MLP, train)
knitr::kable(table(predicted_MLP_train, train$Cluster),caption = "Confusion Matrix for training dataset")
train_cm_MLP<-confusionMatrix(predicted_MLP_train, train$Cluster)

predicted_MLP_test <- predict(model_MLP, test)
knitr:: kable(table(predicted_MLP_test, test$Cluster), caption = "Confusion matrix for testing file")
test_cm_MLP<-confusionMatrix(predicted_MLP_test, test$Cluster)

knitr::kable(data.frame(train_cm_MLP$overall),caption = "Overall Accuracy training dataset")
knitr::kable(data.frame(test_cm_MLP$overall), caption = "Overall Accuracy testing dataset")


# KNN
model_KNN <- train(Cluster ~ ., data = train, method = "knn", preProcess = c("center","scale"), tuneLength = 20)


predicted_KNN_train <- predict(model_KNN, train)
knitr::kable(table(predicted_KNN_train, train$Cluster),caption = "Confusion Matrix for training dataset")
train_cm_KNN<-confusionMatrix(predicted_KNN_train, train$Cluster)

predicted_KNN_test <- predict(model_KNN, test)
knitr:: kable(table(predicted_KNN_test, test$Cluster), caption = "Confusion matrix for testing file")
test_cm_KNN<-confusionMatrix(predicted_KNN_test, test$Cluster)

knitr::kable(data.frame(train_cm_KNN$overall),caption = "Overall Accuracy training dataset")
knitr::kable(data.frame(test_cm_KNN$overall), caption = "Overall Accuracy testing dataset")









# Cluster data Accuracy

Cluster_model_accuracy_kappa <- read_excel("New folder/Cluster model accuracy kappa.xlsx")

Cluster_model_accuracy_kappa %>% 
  ggplot(aes(x=Measure, y=Value))+
  geom_bar(aes(fill=Model), stat="identity", position="dodge", width=0.9)+
  theme_classic()+
  coord_flip()+
  labs(y="value (%)")+
  theme(legend.position = "bottom",
        title = element_text(face="bold", size=20),
        text = element_text(size = 15))+
  scale_fill_brewer(palette = "Set3")


## Full Data Accuracy
Accuracy_Full_Data <- read_excel("New folder/Accuracy Full Data.xlsx")
a1 <- Accuracy_Full_Data[1:5,] %>% 
  ggplot(aes(x=Measure, y= Value, fill = Model))+
  geom_bar(aes(fill=Model), stat="identity", position="dodge", width=0.9)+
  theme_classic()+
  coord_flip()+
  labs(y="value (%)", x="")+
  theme(legend.position = "bottom",
        title = element_text(face="bold", size=20),
        text = element_text(size = 15),
        axis.text = element_text(face="bold")
  )+
  scale_fill_brewer(palette = "Set3")

a2 <- Accuracy_Full_Data[16:20, ] %>% 
  ggplot(aes(x=Measure, y= Value, fill = Model))+
  geom_bar(aes(fill=Model), stat="identity", position="dodge", width=0.9)+
  theme_classic()+
  coord_flip()+
  labs(y="value", x="")+
  theme(legend.position = "bottom",
        title = element_text(face="bold", size=20),
        text = element_text(size = 15),
        axis.text = element_text(face="bold")
  )+
  scale_fill_brewer(palette = "Set3")

plot_grid(a1,a2)

Accuracy_Full_Data[c(6:15,21:25),] %>% 
  ggplot(aes(x=Measure, y= Value, fill = Model))+
  geom_bar(aes(fill=Model), stat="identity", position="dodge", width=0.9)+
  theme_classic()+
  coord_flip()+
  labs(y="value", x="")+
  theme(legend.position = "bottom",
        title = element_text(face="bold", size=20),
        text = element_text(size = 15),
        axis.text = element_text(face="bold")
  )+
  scale_fill_brewer(palette = "Set3")

# XGboost feature Importance

# XGBOOST 
suppressPackageStartupMessages({
  library(xgboost);
  library(shapr );
  library(Matrix);
  library(SHAPforxgboost)})


sparse_matrix <- sparse.model.matrix(Cluster ~ .-1, data = data_k2)
output_vector = data_k2$Cluster
xg_df<- xgb.DMatrix(data = sparse_matrix, label = output_vector )

df_x <- as.matrix(data_k2[,-17])
df_y <- as.matrix(data_k2[ , 17])

param_list <- list(objective = "reg:squarederror",
                   eta = 0.02,
                   max_depth = 10,
                   gamma = 0.01,
                   subsample = 0.95
)
xgb_model <- xgboost::xgboost(data = df_x, 
                              label = df_y, 
                              params = param_list, nrounds = 10,
                              verbose = FALSE, nthread = parallel::detectCores() - 2,
                              early_stopping_rounds = 8)

# compute feature importance matrix

importance_matrix = xgb.importance(feature_names = colnames(df_x),
                                   model = xgb_model)
knitr::kable(data.frame(importance_matrix), caption = "Variable Importance with XGboost")

# XGBoost Importance plot
xgb.plot.importance(importance_matrix = importance_matrix)
# Shap
shap_values <- shap.values(xgb_model, xg_df)
xgb.ggplot.shap.summary(df_x, model = xgb_model)
shap_values$mean_shap_score

# To prepare the long-format data:
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = df_x)
# **SHAP summary plot**
shap.plot.summary(shap_long)


# Dependence Plot for top 10 features
fig_list <- lapply(names(shap_values$mean_shap_score)[1:10], 
                   shap.plot.dependence, data_long = shap_long)
gridExtra::grid.arrange(grobs = fig_list, ncol = 5)










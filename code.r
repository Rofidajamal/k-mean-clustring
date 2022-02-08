
#You are required to apply k-mean clustering on the dataset. You have to go through these steps.
#• Import dataset
# You need to understand this dataset and know what it needs to be clustered.
# Extract your features from the data.
# Make estimation about what you need to cluster and what these data tells you how samples do you have etc.
# Pre-processing
# Your dataset needs to be ready for the training step. So you need to filter your necessary information.
# Normalizing your data is necessary in this step. (if you have picked up a data that is already normalized then you will get this step points)
# Training the model
# What is your suitable k according to your problem?
# Evaluate your model
# What are your clusters and their centres?
# What is your error function? Manhattan distance or Euclidian distance
# Plot your clusters
# Justify what is the meaning of these clusters according to your problem?

data <-read.csv("Mall_Customers.csv")

library(tidyverse)
library("dplyr")

dataset <- data %>% select_if(is.numeric)
dataset <- dataset[,3:4]
names(dataset)[1] <- "AnnualIncome"
names(dataset)[2]<- "SpendingScores"

rescale_df <- dataset %>%
  mutate(
         Annual_scal = scale(AnnualIncome),
         score_scal = scale(SpendingScores)) %>%
  select(-c(AnnualIncome, SpendingScores))

kmean_withinss <- function(k) {
    cluster <- kmeans(rescale_df, k)
    return (cluster$tot.withinss)
}

max_k = 20
wss <- sapply(2:max_k,kmean_withinss)

elbow <- data.frame(2:max_k,wss)

ggplot(elbow,aes(x=X2.max_k,y=wss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks=seq(1,20,by=1))

library("animation")
set.seed(2345)
library(animation)
kmeans.ani(rescale_df,5)

pc_clusters <- kmeans(rescale_df,5)

pc_clusters$cluster
pc_clusters$centers


Eculidean <- function(x,y){
 result = sqrt (sum((x-y)^2))
 return (result)
}

error = Eculidean(rescale_df[,1],rescale_df[,2])
print ("The error =")
print(error)






  

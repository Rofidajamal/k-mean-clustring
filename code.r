#Rofida gamal 20198036
#Amira Hamdy 20198013
#Diaa Ahmed refaat 20198050

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






  

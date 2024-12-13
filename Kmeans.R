library(dplyr)
library(tidyverse)
library(readr)
library(ggcorrplot)
library(factoextra)
library(ggplot2)
library(ggrepel)
library(formattable)
library(cluster)
library(gridExtra) 

# data
raw_data <- read_csv("https://raw.githubusercontent.com/depa-tto/Market-Analysis-of-Airline-Sector/refs/heads/main/Dataset/Airline_review.csv")

middle_data <- raw_data %>% dplyr::select(`Airline Name`, Overall_Rating, Verified, `Seat Comfort` : `Value For Money`) %>%
  dplyr::filter(Verified == 'TRUE')
  

final_data <- middle_data[apply(middle_data, MARGIN = 1, FUN = function(rows) all(rows!=0)),] %>% drop_na() %>% 
  dplyr::select(`Airline Name`, `Seat Comfort` : `Value For Money`) %>% 
  group_by(`Airline Name`) %>% dplyr::filter(length(`Airline Name`) > 24) %>% 
  dplyr::summarise(
    seat = mean(`Seat Comfort`, na.rm = TRUE),
    cabin = mean(`Cabin Staff Service`, na.rm = TRUE),
    food = mean(`Food & Beverages`, na.rm = TRUE),
    ground = mean(`Ground Service`, na.rm = TRUE),
    entertainment = mean(`Inflight Entertainment`, na.rm = TRUE),
    wifi = mean(`Wifi & Connectivity`, na.rm = TRUE),
    value = mean(`Value For Money`, na.rm = TRUE)
  )

mean <- colMeans(final_data[,-1])
sigma <- apply(final_data[,-1], 2, sd)
descriptive <- round(cbind(mean, sigma),2)
colnames(descriptive) <-c ("Mean", "Sigma")
descriptive

formattable(as.data.frame(descriptive), caption = "Fig. 2.1: mean and standard deviation of each variable",
            align = c ("r", "r", "r"), list('Mean' = color_tile("transparent", "lightblue"))) 


# k-means
###########################################################################################################


final_data.stand <- scale(final_data[,-1]) 
set.seed(123)
k.means.fit <- kmeans(final_data.stand, 3) 
str(k.means.fit)
table(k.means.fit$cluster)

# how many clusters ?

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1) * sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(final_data.stand, nc=6) 

# silhouette
calculate_silhouette_values <- function(final_data, num_clusters) {
  set.seed(123)
  kmeans_fit <- kmeans(final_data, num_clusters)
  
  silhouette_values <- silhouette(kmeans_fit$cluster, dist(final_data))
  
  return(silhouette_values)
}

silhouette_values_2 <- calculate_silhouette_values(final_data.stand, 2)
silhouette_values_3 <- calculate_silhouette_values(final_data.stand, 3)
silhouette_values_4 <- calculate_silhouette_values(final_data.stand, 4)
silhouette_values_5 <- calculate_silhouette_values(final_data.stand, 5)
silhouette_values_6 <- calculate_silhouette_values(final_data.stand, 6)

print(silhouette_values_3)
print(silhouette_values_4)
print(silhouette_values_5)

plot(silhouette_values_2, main = 'Silhouette with 2 clusters', col = c("#E7B800", "#2E9FDF"))
plot(silhouette_values_3, main = 'Fig. 2.3: silhouette with 3 clusters', col = c("#E7B800", "#2E9FDF","#FC4E07"))
plot(silhouette_values_4, main = 'Fig. 2.4: silhouette with 4 clusters', col = c("#E7B800", "#2E9FDF","#FC4E07", "#008080"))


clusplot(final_data.stand, k.means.fit$cluster, 
         main = '2D representation of the cluster solution',
         color = TRUE, shade = FALSE,
         labels = 2, lines = 0, col.clus = c("#2E9FDF", "#E7B800", "#FC4E07"),
         col.p = "black",
         cex = 1,
         lwd = 2.5)


final_data$`Airline Name`


# go through all the plots
for(k in 2:6){
  k.m.fit <- kmeans(final_data.stand, k) # k = 3
  clusplot(final_data.stand, k.m.fit$cluster, 
           main=sprintf("2D representation of the Cluster solution\n k = %d",k),
           color=TRUE, shade=TRUE,
           labels=2, lines=0)
}

x <- k.means.fit$cluster
final_data$cluster <- x

summary(final_data %>% filter(cluster == 1))
summary(final_data %>% filter(cluster == 2))
summary(final_data %>% filter(cluster == 3))

fit <- kmeans(final_data[, 2: 8], 3)
fit

x <- aggregate(final_data[, 2: 8], by=list(fit$cluster),FUN=mean)
colnames(x) <-c ("Groups", "Seat", "Cabin","Food", "Ground", "Inflight", "Wifi", "Value")


formattable(as.data.frame(x), caption = "Fig. 2.4: means of every variables in the cluster",
            align = c ("r", "r", "r"), list('Cabin' = color_tile("transparent", "lightblue"),
                                            'Seat' = color_tile("transparent", "lightblue"),
                                            'Food' = color_tile("transparent", "lightblue"),
                                            'Ground' = color_tile("transparent", "lightblue"),
                                            'Inflight' = color_tile("transparent", "lightblue"),
                                            'Wifi' = color_tile("transparent", "lightblue"),
                                            'Value' = color_tile("transparent", "lightblue")))


first_cluster <- final_data %>% filter(cluster == 1)
second_cluster <- final_data %>% filter(cluster == 2)
third_cluster <- final_data %>% filter(cluster == 3)

colnames(second_cluster) <-c ("Airline Name", "Seat", "Cabin","Food", "Ground", "Inflight", "Wifi", "Value", "Cluster")
formattable(as.data.frame(second_cluster), list('Cabin' = color_tile("transparent", "lightblue"),
                                                'Seat' = color_tile("transparent", "lightblue"),
                                                'Food' = color_tile("transparent", "lightblue"),
                                                'Ground' = color_tile("transparent", "lightblue"),
                                                'Inflight' = color_tile("transparent", "lightblue"),
                                                'Wifi' = color_tile("transparent", "lightblue"),
                                                'Value' = color_tile("transparent", "lightblue")))
































































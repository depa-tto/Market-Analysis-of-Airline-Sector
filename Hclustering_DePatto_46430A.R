library(dplyr)
library(tidyverse)
library(readr)
library(ggcorrplot)
library(factoextra)
library(ggplot2)
library(ggrepel)
library(formattable)
library(cluster)

# data
raw_data <- read_csv("https://raw.githubusercontent.com/depa-tto/Statistical-Learning-Module/refs/heads/main/Airline_review.csv")

middle_data <- raw_data %>% dplyr::select(`Airline Name`, Overall_Rating, Verified, `Seat Comfort` : `Value For Money`) %>%
  filter(Verified == 'TRUE')


final_data <- middle_data[apply(middle_data, MARGIN = 1, FUN = function(rows) all(rows!=0)),] %>% drop_na() %>% 
  dplyr::select(`Airline Name`, `Seat Comfort` : `Value For Money`) %>% 
  group_by(`Airline Name`) %>% filter(length(`Airline Name`) > 24) %>% 
  summarise(
    seat = mean(`Seat Comfort`, na.rm = TRUE),
    cabin = mean(`Cabin Staff Service`, na.rm = TRUE),
    food = mean(`Food & Beverages`, na.rm = TRUE),
    ground = mean(`Ground Service`, na.rm = TRUE),
    inflight = mean(`Inflight Entertainment`, na.rm = TRUE),
    wifi = mean(`Wifi & Connectivity`, na.rm = TRUE),
    value = mean(`Value For Money`, na.rm = TRUE)
  )


# hierarchical clustering
###########################################################################################################
rownames(final_data) <- final_data$`Airline Name`

ds <- dist(scale(final_data[, -1]))

# average linkage
h1 <- hclust(ds, method = "average")
h1$labels <- final_data$`Airline Name`

fviz_dend(h1, k = 3, cex = 0.5, k_colors = c("#2E9FDF", "#FC4E07", "#E7B800"),
          color_labels_by_k = T, horiz = F, rect = T, show_labels = T, main = "Average linkage")

# single linkage
h2 <- hclust(ds, method = "single")
h2$labels <- final_data$`Airline Name`

fviz_dend(h2, k = 3, cex = 0.5, k_colors = c("#2E9FDF", "#FC4E07", "#E7B800"),
          color_labels_by_k = T, horiz = F, rect = T, show_labels = T, main = "Single linkage")

# complete linkage
h3 <- hclust(ds, method = "complete")
h3$labels <- final_data$`Airline Name`

fviz_dend(h3, k = 3, cex = 0.5, k_colors = c("#2E9FDF", "#E7B800", "#FC4E07"),
          color_labels_by_k = T, horiz = F, rect = T, show_labels = T, main = "Complete linkage")


# ward linkage
h4 <- hclust(ds, method = "ward.D2")
h4$labels <- final_data$`Airline Name`

fviz_dend(h4, k = 3, cex = 0.5, k_colors = c("#2E9FDF", "#E7B800", "#FC4E07"),
          color_labels_by_k = T, horiz = F, rect = T, show_labels = T, main = "Ward linkage")

average <- cutree(h1, k = 3)
complete <- cutree(h3, k = 3)
ward <- cutree(h4, k = 3)
table(average, complete)
table(average, ward)
table(complete, ward)

h4cluster <- cutree(h4, k = 3)
h4cluster
cluster_colors <- c("#FC4E07", "#E7B800", "#2E9FDF") 
plot(final_data, col = cluster_colors[h4cluster], main = "Ward linkage")
table(h4cluster)




# explore solution
final_data <- final_data[,-1]

medie <- aggregate(final_data, list(h4cluster), mean)
medie 

colnames(medie) <-c ("Groups", "Seat", "Cabin","Food", "Ground", "Inflight", "Wifi", "Value")
formattable(as.data.frame(medie), caption = "Fig. 2.10: means of every variables in the Ward cluster",
            align = c ("r", "r", "r"), list('Cabin' = color_tile("transparent", "lightblue"),
                                            'Seat' = color_tile("transparent", "lightblue"),
                                            'Food' = color_tile("transparent", "lightblue"),
                                            'Ground' = color_tile("transparent", "lightblue"),
                                            'Inflight' = color_tile("transparent", "lightblue"),
                                            'Wifi' = color_tile("transparent", "lightblue"),
                                            'Value' = color_tile("transparent", "lightblue")))



# standardized variable
mediez <- aggregate(scale(final_data), list(h4cluster), mean)
mediez


colnames(mediez) <-c ("Groups", "Seat", "Cabin","Food", "Ground", "Inflight", "Wifi", "Value")
formattable(as.data.frame(mediez), caption = "Fig. 2.10: means of every stardardized variables in the Ward cluster",
            align = c ("r", "r", "r"), list('Cabin' = color_tile("transparent", "lightblue"),
                                            'Seat' = color_tile("transparent", "lightblue"),
                                            'Food' = color_tile("transparent", "lightblue"),
                                            'Ground' = color_tile("transparent", "lightblue"),
                                            'Inflight' = color_tile("transparent", "lightblue"),
                                            'Wifi' = color_tile("transparent", "lightblue"),
                                            'Value' = color_tile("transparent", "lightblue")))






#Hierarchical Cluster Analysis

remove(list = ls())
setwd("~/OneDrive - University of New Haven/Spring 2022/BANL 6420-Unsupervised Machine Learning/Week 3 2.7.2022")
options(digits = 3, scipen = 9999)

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(cluster))
suppressPackageStartupMessages(library(factoextra)) 
suppressPackageStartupMessages(library(NbClust))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(randomForest))


############################################################################## #
# Use hierarchical cluster analysis to obtain a working clustering of the 
# cnbc states data

cnbc = read.csv("cnbc_data.csv", header = TRUE)
head(cnbc, 3)

      cnbc_df = cnbc %>% dplyr::select(-OVERALL, -X)
      cnbc_df = cnbc_df %>% column_to_rownames("State")
      head(cnbc_df,3)



      #How many clusters? ==================      
      NbClust(cnbc_df, method = "complete", index = 'hartigan')$Best.nc
      fviz_nbclust(cnbc_df, pam)
      
      #looking for the smallest bend on the knee. the smallest is closer from 4 to 5


      
      (d <- dist(cnbc_df))  # note the default method is euclidian distance
      (h <- hclust(d))     
      

# Visualize the tree =====================
plot(h)
fviz_dend(h, k = 5)

install.packages('igraph') #install the packages

fviz_dend(h, k = 5, repel = TRUE,  type = "phylogenic")
fviz_dend(h, k = 5,  repel = TRUE, type = "circular")

#deprecated is not used anymore 

# Cut the tree ==============================
grp = cutree(h, k = 3)
grp
rownames(cnbc_df)[grp == 1]
fviz_dend(h, k = 3, repel = TRUE,  type = "phylogenic")        

fviz_cluster(list(data = cnbc_df, cluster = grp), repel = T)

# ==============================================  #
# Use hierarchical cluster analysis to segment the car market - by brand.
# Use the data set mtcars.
# Use the MTCARS data set 
data(mtcars)
head(mtcars)


#Using Hierarchical Clustering

(d <- dist(mtcars))  # note the default method is euclidian distance
(h <- hclust(d))     

plot(h)

#draws rectangles around the branches of a dendogram highlighting the corresponding clusters.
rect.hclust(h, k = 4, border = 'red')
rect.hclust(h, k = 6, border = 'blue')

fviz_dend(h)

fviz_dend(h, k = 6, k_colors = c("red", "black", "blue"), rect = TRUE) + 
  labs(title = "Motor Cars", caption = "Andreas X")+
  theme_classic()

(cn4 <- cutree(h, k = 4))
(cn6 <- cutree(h, k = 6))
table(cn4)
table(cn6)

length(cn4)
nrow(mtcars)

mtcars$cn4 =  cn4

fviz_cluster(list(data = mtcars, cluster = cn4), repel = TRUE, 
             ggtheme = theme_minimal())

mtcars %>% rownames_to_column("Brand") %>% 
  dplyr::select(Brand, cn4) %>% filter(cn4 == 3)


mtcars %>% group_by(cn4) %>% summarize_all( list(average = mean, stdev= sd))
mtcars %>%  group_by(cn4) %>% summarize_all( list(average = mean))
mtcars  %>%  group_by(cn4) %>% dplyr::summarize_all( list(stdev= sd))

mtcars  %>%  group_by(cn4) %>% count()

# Cut the tree at k = 4 and create a "phylogenic" type dendogram for k = 4.
fviz_dend(h, k = 4, k_colors = c("red", "green", "blue"), repel = TRUE, type = "phylogenic")+ 
  labs(title = "Motor Cars", caption = "Andreas X")+
  theme_classic()
fviz_dend(h, k = 4, k_colors = c("red", "green", "blue"), repel = TRUE, type = "circular")

View(nfd)

#Installing Package#
library(factoextra)  #Mengaplikasikan algoritma clustering kmeans
library(cluster) #Mengaplikasikan algoritma clustering
library(ggplot2) #Memvisualisasikan data yang dimiliki
library(dplyr) #Manipulasi data
library(broom) #Manipulasi data, statistical objects into tibbles
library(ggdendro) #Memvisualisasikan dendogram
library(readxl) #Membaca file excel
library(fmsb) #Memvisualisasikan radar chart
library(NbClust) #Optimum number of Cluster
library(RColorBrewer) #Visualisasi Warna di R
library(gridExtra)

#Import Data#

library(readxl)
Final_Data_Set_Set_1_ <- read_excel("C:/Users/Medina Janneta El/Downloads/Final Data Set Set (1).xlsx")
View(Final_Data_Set_Set_1_)

str(Final_Data_Set_Set_1_)

Final_Data_Set_Set_1_$LotFrontage<- as.numeric(Final_Data_Set_Set_1_$LotFrontage)

library(dplyr)
nfd <- Final_Data_Set_Set_1_ %>% na.omit()

str(nfd)

library(dplyr)
library(tidyr)
#replace missing values in each numeric column with median value of column
nfd <- Final_Data_Set_Set_1_ %>% mutate(across(where(is.numeric),~replace_na(.,median(.,na.rm=TRUE))))


#Exploring Data Analysis#
hist(nfd$LotArea, lwd=3, main="Lot Area", xlab = "data 1(1)", col = "pink")
hist(nfd$GarageArea, lwd=3, main="Garage Area", xlab = "data 1(2)", col = "lightblue")
hist(nfd$TotalSFFlr, lwd=3, main="Total FlrSF", xlab = "data 1(3)", col = "lightgreen")
hist(nfd$SalePrice, lwd=3, main="Sale Price", xlab = "data 1(4)", col = "purple")
hist(nfd$LotFrontage, lwd=3, main="Lot Frontage", xlab = "data 1(4)", col = "yellow")

boxplot(nfd$LotFrontage)

pairs(nfd)


#Clustering

library(readxl)
buat_clustering <- read_excel("C:/Users/Medina Janneta El/Downloads/buat clustering.xlsx")
View(buat_clustering)

buat_clustering <- as.data.frame(buat_clustering)

nc <- NbClust(buat_clustering, distance="euclidean", min.nc=2, max.nc=10, method="kmeans")

set.seed(123)
km_out = kmeans(buat_clustering, 3)

fviz_cluster(km_out, data = buat_clustering)

res <- cbind(buat_clustering, km_out$cluster)

res

new <- aggregate(res[,-ncol(res)], list(res[,ncol(res)]), mean)
new

# Set graphic colors
library(RColorBrewer)
coul <- brewer.pal(4, "RdBu")
colors_border <- coul
library(scales)
colors_in <- alpha(coul, 0.3)

# If you remove the 2 first lines, the function compute the max and min of each variable with the available data:
radarchart( new[,-1], axistype=0 , maxmin=F,
            #custom polygon
            pcol=colors_border, pfcol=colors_in, plwd=4, plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
            #custom labels
            vlcex=0.8 
)

# Add a legend
  legend(x=0.7, y=1, legend = new$Group.1, bty = "n", pch=20, col=colors_in, text.col = "grey", cex=1.2, pt.cex=3)
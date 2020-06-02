source("http://blogs.5eanalytics.com/RCode/PCA_functions.R")
library(FactoMineR)
library(factoextra)

airpotcancel= read.csv(file.choose())
airportoperation=read.csv(file.choose())
# merger
names(airpotcancel)
names(airportoperation)
names(airpotcancel)[1:2] = c("airport","year")
airport <- merge( airportoperation, airpotcancel, by = c("airport", "year"))

summary(airport)
str(airport)
#take out non-numarical columns
clean_airport = airport[,c(-1,-2,-3,-4)]
summary(clean_airport)
is.na(clean_airport) #anything missing?
final_airport = na.omit(clean_airport) #take out missing values
summary(final_airport)

PCA_result = PCA(final_airport)
summary(PCA_result)
fviz_screeplot(PCA_result, ncp=15)
communality(PCA_result)
display_pc(PCA_result)

A=round(PCA_result$var$coord[,1],4)
B=round(PCA_result$var$coord[,2],4)
C=round(PCA_result$var$coord[,3],4)
V_D=round(as.matrix(final_airport)%*%as.matrix(PCA_result$var$coord[,1 :3]),4)
round(cor(V_D[,1:3]),4)

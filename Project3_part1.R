###############################################################
# Reading Data in R
###############################################################
install.packages("readxl")
library(readxl)
# Read data
S2Data<- read_excel("StockData.xlsx")
head(StockData,5) # Displays firs 5 rows of data

# Extract only numerical variables for the analysis
SData = StockData[,2:14]

# Check for missing values. Rows with missing vals should be removed
colSums(is.na(SData))

# Keep only complete cases
SData = SData[complete.cases(SData),]
head(SData,5) # Displays firs 5 rows of data

# Visualize variables to check variances
dev.off()
boxplot(SData, main="Distributions of Vars in Stock Data")

# Compute numerical summaries for each varable
library(tableone)
SDataNumSum = CreateTableOne(data= SData,includeNA = FALSE)
print(SDataNumSum) 

# Check original bi-variate scatterplots to see if obs (Stock) can be grouped 
pairs(SData,main="Stock", pch=19)

# Standardize data
SData = as.data.frame(scale(SData))

# Check transformation
SDataNumSum = CreateTableOne(data= SData,includeNA = FALSE)
print(SDataNumSum) 

library(FactoMineR)
# Perform PCA analysis on standardized data
PCA.Res = PCA(SData, graph = FALSE,ncp = 13,scale.unit = TRUE)

# Getting the eigenvectors/loadings 
sweep(PCA.Res$var$coord,2,sqrt(PCA.Res$eig[1:ncol(PCA.Res$var$coord),1]),FUN="/")

# See what information the PCA.Res object 
PCA.Res 
str(PCA.Res)

# Summary of PCA analysis
summary(PCA.Res)

# Alternative way of Computing loadings
PCA.Res1  = prcomp(SData,scale. = TRUE)
round(PCA.Res1$rotation,2) 

# See what information the PCA.Res object 
PCA.Res 
str(PCA.Res)

# Summary of PCA analysis
summary(PCA.Res)

library("factoextra")
# Scree plot to visualize variance explained by each principal component
fviz_eig(PCA.Res, addlabels = TRUE, ylim = c(0, 70))

# Variable contribution to each PC
C1 = PCA.Res$var$contrib
round(C1,1) 

# Quality of representation: Contribution of Vars to first 2 PCs
fviz_pca_var(PCA.Res, col.var = "cos2",
             gradient.cols =  c("black","#00AFBB", "#E7B800"),
             repel = TRUE, # Avoid text overlapping (slow if many points),
)+ labs(title ="Correlation Plot", x = "PC1", y = "PC2")

# Alternative way to see the contributions of each var to each PC
library("corrplot")
corrplot(PCA.Res$var$contrib, is.corr=FALSE, col= c("#C994C7","#DF65B0","#E7298A"))




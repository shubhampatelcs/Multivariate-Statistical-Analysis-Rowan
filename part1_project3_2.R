# Read data
DData = read.xlsx(xlsxFile= 'StockData.xlsx',colNames = TRUE, rowNames = TRUE)
head(DData,5) # Displays firs 5 rows of data

# Compute Correlation Matrix 
R = as.matrix(R)

# Correlations Between the vars in the study
library("corrplot")
corrplot(R, is.corr=TRUE, col= c("#C994C7","#DF65B0","#E7298A"),tl.col="black")

# Test to find the appropriate number of factors: 
pvals = c()
for(k in 1:5)
  pvals[k] = factanal(covmat = R,n.obs = 252,factors = k)$PVAL

pvals

# Varimax rotation to the resulting model
FA.Res4 = fa(R, nfactors = 3, rotate = "varimax", fm = "ml")
L4 = FA.Res4$loadings[,1:3]
PSI4 = diag(FA.Res4$uniquenesses)
# Error Matrix 
EPS4 = R - L4%*%t(L4) - PSI4
round(EPS4,3)
# Communalities: row sum of squares
H4 = FA.Res4$communality

# Visualize factors
fa.diagram(FA.Res4, cut = 0.32, simple = F, main = "Decathlon Data")







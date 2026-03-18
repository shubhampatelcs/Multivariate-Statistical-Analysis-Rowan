#read the data 
S2Data = read.xlsx(xlsxFile= 'StockData.xlsx')
head(S2Data,5) # Displays firs 5 rows of data

#keep only complete cases
S2Data=S2Data[complete.cases(S2Data),]
head(S2Data,5)#Display first 5 rows of data

# Extract only numerical variables for the analysis
S2Data = S2Data[,2:14]

#visualize variables to check variances
boxplot(S2Data,main="Distribution of vars in Stock Data")

#compute numerical summaries for each variable 
library(tableone)
S2DataNumSum=CreateTableOne(data=S2Data,includeNA = FALSE)
print(S2DataNumSum)

#standardize data
S2Data=as.data.frame(scale(S2Data))

#computer correlation matrix
R=cor(S2Data)

#correlations between the vars in the study
library("corrplot")
corrplot(R,is.corr = TRUE,tl.col = "black")

#compute the loading and variable specific/unique Matrices
library(psych)

#Using Pcs
Eig.Res=eigen(R)

# L Matrix for m=3, the minus is so it matches the ones in the book
L = round(Eig.Res$vectors[,1:3]%*% diag(Eig.Res$values[1:3]^.5),3)
PSI = diag(diag(R - L%*%t(L)))
# Error Matrix 
EPS = R - L%*%t(L) - PSI  

# Communalities: row sum of squares
H = round(rowSums(L^2),3)

# Using the mle method
FA.Res = fa(S2Data, nfactors = 3, rotate = "none", fm = "ml")
L1 = round(FA.Res$loadings[,1:3],3)
PSI1 = diag(FA.Res$uniquenesses)

# Error Matrix 
EPS1 = R - L1%*%t(L1) - PSI1 

# Communalities: row sum of squares
H1 = FA.Res$communality

# Comparison of Rotations
# install.packages("GPArotation") # needed for the Quartimax Roation
library(GPArotation)
#FA.Res.None = fa(S2Data, nfactors = 3, rotate = "none", fm = "ml")
FA.Res.Varimax = fa(S2Data, nfactors = 3, rotate = "varimax", fm = "ml")
#FA.Res.Quartimax = fa(S2Data, nfactors = 3, rotate = "quartimax", fm = "ml")

par(mfrow = c(1, 3))
# The argument cut = 0.3 specifies that any loading less than 0.3 (in absolute value) 
# will not be drawn, simple = F draws all the loadings (not just the largest 
# loading per variable).

#fa.diagram(FA.Res.None, cut = 0.3, simple = F, main = "No Rotation")
fa.diagram(FA.Res.Varimax, cut = 0.3, simple = F, main = "Varimax Rotation")
#fa.diagram(FA.Res.Quartimax, cut = 0.3, simple = F, main = "Quartimax Rotation")


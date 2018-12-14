library(data.table)
library(VGAM)

###Load data
data = data.table(read.csv("./data/budget_deficit_data.csv"))

#Count columns
ncol(data)

#Count records
nrow(data)

head(data)

deficit_data = subset(data, select=c("DEFICIT_M", "BoP", "GOV_DEBT", "IMF_FINANCING", "TAX_REVENUE"))
#Scaling data
deficit_data$BoP <- deficit_data$BoP*(-1)
deficit_data$TAX_REVENUE <- deficit_data$TAX_REVENUE/1000
#Set left bound for Tobit model
LOWER_BOUND <- max(deficit_data$DEFICIT_M)

model = vglm(DEFICIT_M ~ BoP + GOV_DEBT + IMF_FINANCING + TAX_REVENUE, tobit(Lower = LOWER_BOUND), data = deficit_data)
summary(model)

#Count marginal effect
xValues = c(1, colMeans(deficit_data)[2:ncol(deficit_data)])
allPar = coef(model, logSigma = FALSE)
beta = allPar[!names(allPar) %in% c("sigma")]
sigma = allPar["sigma"]
xBeta = crossprod(xValues, beta)
zLeft = (-LOWER_BOUND + xBeta)/sigma
## MARGINAL EFFECT ON DENSITY
beta[!names(beta) %in% c("(Intercept)")] * (pnorm(zLeft))
## MARGINAL EFFECT ON PROBABILITY
- beta[!names(beta) %in% c("(Intercept)")] / sigma * (dnorm(zLeft))

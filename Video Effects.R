rm(list=ls(all=TRUE))
library(plm)

fp_sales <- read.csv('D:/Study material/Sem 3/Causal Analytics and AB testing/Datasets/FP Analysis Data.csv')
cp_sales <- read.csv('D:/Study material/Sem 3/Causal Analytics and AB testing/Datasets/CP Analysis Data.csv')

### Randomization test for Product Prices ###
treat_mean = mean(subset(fp_sales, Vid == 1)$ProdPrice)
cont_mean = mean(subset(fp_sales, Vid == 0)$ProdPrice)
treat_sd = sd(subset(fp_sales, Vid == 1)$ProdPrice)
cont_sd = sd(subset(fp_sales, Vid == 0)$ProdPrice)

# There is no substantial difference between the mean
# and SD of price between the treatment and control groups

### T-stat for difference of means ###
t = (20.71 - 20.41)/sqrt((5.48^2/58)+(4.79^2/239))
t

# There is no statistical difference in means between treatment
# and control group.
# Sample has passed the randomization test

### Testing the difference in mean sales
### between the treatment and control group
diff_in_means = mean(subset(fp_sales, VidWk == 1)$Sales) - mean(subset(fp_sales, VidWk == 0)$Sales)
diff_in_means

# This value implies that the effect of a product video on sales
# is approximately 28, which isn't the true effect as this value
# is biased. It doesn't capture the effect of other promotions
# that have also been intermittetly applied

model1 <- plm(Sales ~ VidWk + PriceDiscWk + EmailWk + CatalogWk + HomePgWk + CatPgWk + factor(ProdID) + factor(Wk),
               data = fp_sales, model = 'pooling', index = c('ProdID', "Wk"))
summary(model1)

# Here, the effect of video in a particular week is 16.71.
# It is less due to the fact that other promotional variables
# have also been included in the model, and it is a more
# accurate estimate of the effect of product videos on sales.

model2 <- plm(CpSales ~ VidWk + FpPriceDiscWk + FpEmailWk + FpCatalogWk + FpHomePgWk + FpCatPgWk +
                CpPriceDiscWk + CpEmailWk + CpCatalogWk + CpHomePgWk + CpCatPgWk + factor(CpordID) + factor(Wk),
              data = cp_sales, model = 'pooling', index = c('CpordID', "Wk"))
summary(model2)

# The effect seems to be even greater for coordinating products
# This could be down to the fact that the videos are drawing
# more attention to the coordinating products as compared to
# the focal products. Also, coordinating products tend to be 
# more expensive (big ticket items)

## Testing the interaction between video and various promotions for Focal Products ## 
model3 <- plm(Sales ~ VidWk + PriceDiscWk + EmailWk + CatalogWk + HomePgWk + CatPgWk + I(VidWk*PriceDiscWk) +
                factor(ProdID) + factor(Wk), index=c("ProdID", "Wk"), data = fp_sales, model = 'pooling')
summary(model3)

model4 <- plm(Sales ~ VidWk + PriceDiscWk + EmailWk + CatalogWk + HomePgWk + CatPgWk + I(VidWk*EmailWk) +
                factor(ProdID) + factor(Wk), index=c("ProdID", "Wk"), data = fp_sales, model = 'pooling')
summary(model4)

model5 <- plm(Sales ~ VidWk + PriceDiscWk + EmailWk + CatalogWk + HomePgWk + CatPgWk + I(VidWk*CatalogWk) +
                factor(ProdID) + factor(Wk), index=c("ProdID", "Wk"), data = fp_sales, model = 'pooling')
summary(model5)

model6 <- plm(Sales ~ VidWk + PriceDiscWk + EmailWk + CatalogWk + HomePgWk + CatPgWk + I(VidWk*HomePgWk) +
                factor(ProdID) + factor(Wk), index=c("ProdID", "Wk"), data = fp_sales, model = 'pooling')
summary(model6)

model7 <- plm(Sales ~ VidWk + PriceDiscWk + EmailWk + CatalogWk + HomePgWk + CatPgWk + I(VidWk*CatPgWk) +
                factor(ProdID) + factor(Wk), index=c("ProdID", "Wk"), data = fp_sales, model = 'pooling')
summary(model7)

## Testing the interaction between video and various promotions for Coordinating Products ##
model8 <- plm(CpSales ~ VidWk + FpPriceDiscWk + FpEmailWk + FpCatalogWk + FpHomePgWk + FpCatPgWk + CpPriceDiscWk +
                CpEmailWk + CpCatalogWk + CpHomePgWk + CpCatPgWk + I(VidWk*CpPriceDiscWk) + factor(CpordID) + factor(Wk),
              data = cp_sales, model = 'pooling', index = c('CpordID', "Wk"))
summary(model8)

model9 <- plm(CpSales ~ VidWk + FpPriceDiscWk + FpEmailWk + FpCatalogWk + FpHomePgWk + FpCatPgWk + CpPriceDiscWk +
                CpEmailWk + CpCatalogWk + CpHomePgWk + CpCatPgWk + I(VidWk*CpEmailWk) + factor(CpordID) + factor(Wk),
              data = cp_sales, model = 'pooling', index = c('CpordID', "Wk"))
summary(model9)

model10 <- plm(CpSales ~ VidWk + FpPriceDiscWk + FpEmailWk + FpCatalogWk + FpHomePgWk + FpCatPgWk + CpPriceDiscWk +
                CpEmailWk + CpCatalogWk + CpHomePgWk + CpCatPgWk + I(VidWk*CpCatalogWk) + factor(CpordID) + factor(Wk),
              data = cp_sales, model = 'pooling', index = c('CpordID', "Wk"))
summary(model10)

model11 <- plm(CpSales ~ VidWk + FpPriceDiscWk + FpEmailWk + FpCatalogWk + FpHomePgWk + FpCatPgWk + CpPriceDiscWk +
                 CpEmailWk + CpCatalogWk + CpHomePgWk + CpCatPgWk + I(VidWk*CpHomePgWk) + factor(CpordID) + factor(Wk),
               data = cp_sales, model = 'pooling', index = c('CpordID', "Wk"))
summary(model11)

model12 <- plm(CpSales ~ VidWk + FpPriceDiscWk + FpEmailWk + FpCatalogWk + FpHomePgWk + FpCatPgWk + CpPriceDiscWk +
                 CpEmailWk + CpCatalogWk + CpHomePgWk + CpCatPgWk + I(VidWk*CpCatPgWk) + factor(CpordID) + factor(Wk),
               data = cp_sales, model = 'pooling', index = c('CpordID', "Wk"))
summary(model12)
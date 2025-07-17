#### POISSON GLM FOR REAL
### To Do
## move the "MaineDMR_Trawl_Survey_eDNA_Catch_Data.csv" to the github project folder
#Edit this script to conduct the first plot and Poisson regression on this data
# make a fake dataset
dat <-read.csv("MaineDMR_Trawl_Survey_eDNA_Catch_Data.csv", header=TRUE)
#check the dataframe, make sure numbers are numeric, factors are factors
summary(at)

# Fix any dataframe formating issues

# check the dataframe again
summary(dat)

# plot the data just to see things
plot(eDNA_count~Trawl_count, col=test_dat$Species, data=dat)

# Fit Poisson generalized linear models
model_00 <- glm(eDNA_count ~ 1, data=dat, family = "poisson") #no independent variables
model_01 <- glm(eDNA_count ~ Trawl_count, data=dat, family = "poisson") #just trawl count
model_02 <- glm(eDNA_count ~ Trawl_count+Species, data=dat, family = "poisson") #additive trawl count and species
model_03 <- glm(eDNA_count ~ Trawl_count*Species, data=dat, family = "poisson") #multiplicative trawl count * species

# Explore model fits and independent variables signifance
summary(test_model_00)
summary(test_model_01)
summary(test_model_02)
summary(test_model_03)

# Add the model prediction to the plot
abline(test_model_00, col="green", lwd=2)
abline(test_model_01, col="purple", lwd=2)

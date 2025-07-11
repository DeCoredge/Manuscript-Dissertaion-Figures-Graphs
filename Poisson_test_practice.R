#### POISSON GLM
# make a fake dataset
test_dat <- data.frame(eDNA_count = c(50,1505,23852,148,3058,2485,120,1373,8,9234), Trawl_count = c(38,835,175,305,23,487,139,597,23,59),
                       Species = c(rep("Silver_hake", 5), rep("Winter_flounder",5)), Primer = "12S_MiFish", Region=3, BodyType ="slimy")
#check the dataframe, make sure numbers are numeric, factors are factors
summary(test_dat)

# Fix any dataframe formating issues
test_dat$Species <- as.factor(test_dat$Species)
test_dat$Primer <- as.factor(test_dat$Primer)
test_dat$Region <- as.factor(test_dat$Region)
test_dat$BodyType <- as.factor(test_dat$BodyType)

# check the dataframe again
summary(test_dat)

# plot the data just to see things
plot(eDNA_count~Trawl_count, col=test_dat$Species, data=test_dat)

# Fit Poisson generalized linear models
test_model_00 <- glm(eDNA_count ~ 1, data=test_dat, family = "poisson")
test_model_01 <- glm(eDNA_count ~ Trawl_count, data=test_dat, family = "poisson")

# Explore model fits and independent variables signifance
summary(test_model_00)
summary(test_model_01)

# Add the model prediction to the plot
abline(test_model_00, col="green", lwd=2)
abline(test_model_01, col="purple", lwd=2)

## Check the MiFish_Abundance_Poisson, it doesn't look right
rm(list = ls())

#Perform a Poisson Regression GLM using the abundance proportion of Fish species to their read counts in csv.
edna_data<- read.csv("MiFish_Abundance_Poisson.csv", header = TRUE)
edna_data$Abundance <- edna_data$Abundance # quick fix, eventually want to get actual values
edna_data$MiFish <- edna_data$MiFish # quick fix, eventually want to get actual values
edna_data$Leray <- edna_data$Leray # quick fix, eventually want to get actual values

#Replace N/A in csv dataset table.
edna_data<- replace(edna_data, is.na(edna_data), "")

#Fix any dataframe formating issues
edna_data$Species <- as.factor(edna_data$Species)

#Fit data into Poisson Regression GLMs.
edna_MiFish_model <- glm(Abundance ~ 1, data= edna_data, family = "poisson")
edna_MiFish_model_02 <- glm(Abundance ~ MiFish, data= edna_data,
                            family = "poisson")

#View GLMs & Explore model fits and independent variables signifance
summary(edna_MiFish_model)
summary(edna_MiFish_model_02)

# Add the model prediction to the plot
plot(edna_data$MiFish ~ edna_data$Abundance)
abline(edna_MiFish_model, col="green", lwd=2)
abline(edna_MiFish_model_02, col="purple", lwd=2)

# Add the model prediction to the plot
plot(log10(edna_data$MiFish) ~ log10(edna_data$Abundance))
abline(a=0, b=1) # one to one line


#Perform a Poisson Regression GLM using the abundance proportion of all Invert. species to their read counts in csv.
edna_data_02<- read.csv("Leray_Abundance_Poisson.csv", header = TRUE)
edna_data_02$Abundance <- edna_data_02$Abundance
edna_data_02$Leray <- edna_data_02$Leray


#Replace N/A in csv dataset table.
edna_data_02<- replace(edna_data_02, is.na(edna_data_02), "")

#Fix any dataframe formating issues
edna_data_02$Species <- as.factor(edna_data_02$Species)

#Fit data into Poisson Regression GLMs.
edna_Leray_model <- glm(Abundance ~ 1, data= edna_data_02, family = "poisson")
edna_Leray_model_02 <- glm(Abundance ~ Leray, data= edna_data_02,
                           family = "poisson")

#View GLMs & Explore model fits and independent variables signifance
summary(edna_Leray_model)
summary(edna_Leray_model_02)

# Add the model prediction to the plot
plot(edna_data_02$Leray ~ edna_data_02$Abundance)
abline(edna_Leray_model, col="green", lwd=2)
abline(edna_Leray_model_02, col="purple", lwd=2)

# Add the model prediction to the plot
plot(log10(edna_data_02$Leray) ~ log10(edna_data_02$Abundance))
abline(a=0, b=1) # one to one line


#Perform a Poisson Regression GLM using the abundance proportion of cephalopod species to their read counts in csv.
edna_data_03<- read.csv("Ceph18s_Abundance_Poisson.csv", header = TRUE)
edna_data_03$Abundance <- edna_data_03$Abundance
edna_data_03$Ceph18s <- edna_data_03$Ceph18s

#Replace N/A in csv dataset table.
edna_data_03<- replace(edna_data_03, is.na(edna_data_03), "")

#Fix any dataframe formating issues
edna_data_03$Species <- as.factor(edna_data_03$Species)

#Fit data into Poisson Regression GLMs.
edna_Ceph18s_model <- glm(Abundance ~ 1, data= edna_data_03, family = "poisson")
edna_Ceph18s_model_02 <- glm(Abundance ~ Ceph18s, data= edna_data_03,
                             family = "poisson")

#View GLMs & Explore model fits and independent variables signifance
summary(edna_Ceph18s_model)
summary(edna_Ceph18s_model_02)

# Add the model prediction to the plot
plot(edna_data_03$Ceph18s ~ edna_data_03$Abundance)
abline(edna_Ceph18s_model, col="green", lwd=2)
abline(edna_Ceph18s_model_02, col="purple", lwd=2)

# Add the model prediction to the plot
plot(log10(edna_data_03$Ceph18s) ~ log10(edna_data_03$Abundance))
abline(a=0, b=1) # one to one line

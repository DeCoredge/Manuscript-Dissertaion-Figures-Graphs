rm(list = ls())
setwd("C:/Users/DeCorey Bolton Jr/Documents/GitHub/Manuscript-Dissertaion-Figures-Graphs")

#Perform a Poisson Regression GLM using the abundance proportion of Fish species to their read counts in csv.
edna_data<- read.csv("MiFish_Abundance_Poisson.csv", header = TRUE)
edna_data$Abundance <- edna_data$Abundance 
edna_data$MiFish <- edna_data$MiFish
edna_data$Leray <- edna_data$Leray

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
plot(edna_data$MiFish ~ edna_data$Abundance, xlab = "Total Abundance",
     ylab = "MiFish Read Count")
abline(edna_MiFish_model, col="green", lwd=2)
abline(edna_MiFish_model_02, col="purple", lwd=2)

# Add the model prediction to the plot
plot(log10(edna_data$MiFish) ~ log10(edna_data$Abundance),xlab = "Total Abundance",
     ylab = "MiFish Read Count" )
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
plot(edna_data_02$Leray ~ edna_data_02$Abundance, xlab = "Total Abundance",
     ylab = "Leray Read Count")
abline(edna_Leray_model, col="green", lwd=2)
abline(edna_Leray_model_02, col="purple", lwd=2)

# Add the model prediction to the plot
plot(log10(edna_data_02$Leray) ~ log10(edna_data_02$Abundance), xlab = "Total Abundance",
     ylab = "Leray Read Count")
abline(a=0, b=1) # one to one line

#Perform a Multiplicative factor log-Poisson Regression GLM using the abundance proportion of all Invert. species to their read counts, and to their body type in csv.
edna_data_02<- read.csv("Leray_Abundance_Poisson.csv", header = TRUE)
edna_data_02$Abundance <- edna_data_02$Abundance
edna_data_02$Leray <- edna_data_02$Leray
edna_data_02$Body_Type <-edna_data_02$Body_Type

#Replace N/A in csv dataset table.
edna_data_02<- replace(edna_data_02, is.na(edna_data_02), "")

#Fix any dataframe formating issues
edna_data_02$Species <- as.factor(edna_data_02$Species)
edna_data_02$Body_Type <- as.factor(edna_data_02$Body_Type)

#Fit data into Poisson Regression GLMs.
edna_Leray_model <- glm(Abundance ~ 1, data= edna_data_02, family = "poisson")
edna_Leray_model_02 <- glm(Abundance ~ Leray*Body_Type, data= edna_data_02,
                           family = "poisson")

#View GLMs & Explore model fits and independent variables signifance
summary(edna_Leray_model)
summary(edna_Leray_model_02)

# Add the model prediction to the plot
plot(edna_data_02$Leray ~ edna_data_02$Abundance, xlab = "Total Abundance",
     ylab = "Multiplicative Leray Read Count" )
abline(edna_Leray_model, col="green", lwd=2)
abline(edna_Leray_model_02, col="purple", lwd=2)

# Add the model prediction to the plot
plot(log10(edna_data_02$Leray) ~ log10(edna_data_02$Abundance), xlab = "Total Abundance",
     ylab = "Multiplicative Leray Read Count")
abline(a=0, b=1) # one to one line

#Perform a Additive factor log-Poisson Regression GLM using the abundance proportion of all Invert. species to their read counts, and to their body type in csv.
edna_data_02<- read.csv("Leray_Abundance_Poisson.csv", header = TRUE)
edna_data_02$Abundance <- edna_data_02$Abundance
edna_data_02$Leray <- edna_data_02$Leray
edna_data_02$Body_Type <-edna_data_02$Body_Type

#Replace N/A in csv dataset table.
edna_data_02<- replace(edna_data_02, is.na(edna_data_02), "")

#Fix any dataframe formating issues
edna_data_02$Species <- as.factor(edna_data_02$Species)
edna_data_02$Body_Type <- as.factor(edna_data_02$Body_Type)

#Fit data into Poisson Regression GLMs.
edna_Leray_model <- glm(Abundance ~ 1, data= edna_data_02, family = "poisson")
edna_Leray_model_02 <- glm(Abundance ~ Leray+Body_Type, data= edna_data_02,
                           family = "poisson")

#View GLMs & Explore model fits and independent variables signifance
summary(edna_Leray_model)
summary(edna_Leray_model_02)

# Add the model prediction to the plot
plot(edna_data_02$Leray ~ edna_data_02$Abundance, xlab = "Total Abundance",
     ylab = "Additive Leray Read Count")
abline(edna_Leray_model, col="green", lwd=2)
abline(edna_Leray_model_02, col="purple", lwd=2)

# Add the model prediction to the plot
plot(log10(edna_data_02$Leray) ~ log10(edna_data_02$Abundance),  xlab = "Total Abundance",
     ylab = "Additive Leray Read Count")
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
plot(edna_data_03$Ceph18s ~ edna_data_03$Abundance, xlab = "Total Abundance",
     ylab = "Ceph18S Read Count")
abline(edna_Ceph18s_model, col="green", lwd=2)
abline(edna_Ceph18s_model_02, col="purple", lwd=2)

# Add the model prediction to the plot
plot(log10(edna_data_03$Ceph18s) ~ log10(edna_data_03$Abundance), xlab = "Total Abundance",
     ylab = "Ceph18S Read Count")
abline(a=0, b=1) # one to one line

#Perform a Poisson Regression GLM using the abundance of eDNA collected from different located metaprobes to their in csv.
edna_data<- read.csv("Metaprobe_eDNA_Location.csv", header = TRUE)
edna_data$Metaprobes <- edna_data$Metaprobes
edna_data$Location <- edna_data$Location 
edna_data$Quantity <- edna_data$Quantity

#Replace N/A in csv dataset table.
edna_data<- replace(edna_data, is.na(edna_data), "")

#Fix any dataframe formating issues
edna_data$Metaprobes <- as.factor(edna_data$Metaprobes)
edna_data$Location <- as.factor(edna_data$Location)
edna_data$Quantity   <- as.numeric(edna_data$Quantity)

#Fit data into Poisson Regression GLMs.
edna_Metaprobe_model <- glm(Quantity ~ 1, data= edna_data, family = "poisson")
edna_Metaprobe_model_02 <- glm(Quantity ~ Location, data= edna_data,
                            family = "poisson")

#View GLMs & Explore model fits and independent variables signifance
summary(edna_Metaprobe_model)
summary(edna_Metaprobe_model_02)

#'Location' is a factor (categorical), a boxplot is more appropriate than a scatter plot.Using a lighter color so points show up, Darker border for contrast, and have outline FALSE to Hide default outliers so they aren't doubled

plot(Quantity ~ Location, data = edna_data, 
     main = "eDNA Quantity by Location", ylab = "Abundance", xlab = "Location",
     col = "green", border = "purple", outline = FALSE)

#Abline doesn't work directly with GLMs like this because GLMs are non-linear.
# To see if Location is significant:

anova(edna_Metaprobe_model, edna_Metaprobe_model_02, test = "Chisq")

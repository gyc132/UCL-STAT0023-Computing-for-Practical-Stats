##########################################################################
#     Exploratory Data Analysis     
##########################################################################

# Load the ggplot2 library for data visualization
library(ggplot2)

# Input the data from the AnemiaData.csv file
anemia_data <- read.csv("AnemiaData.csv")

# Separate the data into training data (with non-missing Haemoglobin values) 
# and testing data (with missing Haemoglobin values)
train <- anemia_data[anemia_data$Haemoglobin != -1, ]
test <- anemia_data[anemia_data$Haemoglobin == -1, ]

# Display the first few rows of the anemia_data dataframe
head(anemia_data)

###USED IN REPORT###
## distribution of Haemoglobin which make us choose linear regression
hist(train$Haemoglobin,
     main = "Figure 1: Histogram of Haemoglobin",
     xlab = "Haemoglobin",
     ylab = "Frequency",
     breaks = 30)
dev.copy(png,"Haemoglobin_distribution.png",width=8*72,height=6*72)
dev.off()


continuous_variables <- c("Age", "HHSize", "HHUnder5s", 
                          "TotalChildren", "WealthScore", "AgricArea")
discrete_variables <- c("RecentBirth", "CleanWater", "TreatedWater", 
                           "Electricity", "Toilet", "BikeScootCar", "AnimCart",
                           "AgricLandOwn", "Cows", "Horses", "Goats", "Sheep", 
                           "Chickens", "Rural", "Province","Pregnant", "Education",
                           "Region", "Ethnicity")


# Continuous variables plot
par(mfrow = c(2, 3))  # Set up the layout for plots: 2 rows, 3 columns

# Function to plot continuous variables 
plot_continuous <- function(variable_name){
  # Extract columns
  x <- train[[variable_name]]
  y <- train$Haemoglobin
  # Plot the scatter plot
  plot(x, y, main = paste("Scatter Plot of Haemoglobin by", variable_name),
       xlab = variable_name, ylab = "Haemoglobin")
  # Add linear fit
  abline(lm(y ~ x), col = "red")
}

# Plot each scatter plot for continuous variables
for (variable_name in continuous_variables) {
  plot_continuous(variable_name)
}

# Discrete variables boxplot
plot_discrete <- function(variable_name){
  # Extract columns
  x <- train[[variable_name]]
  y <- train$Haemoglobin
  # Plot the box plot
  boxplot(y ~ x, main = paste("Box Plot of Haemoglobin by", variable_name),
          xlab = variable_name, ylab = "Haemoglobin")
}
par(mfrow = c(1, 1)) 
# Plot box plots for each discrete variable
plot_discrete("RecentBirth")
plot_discrete("CleanWater")
plot_discrete("TreatedWater")
plot_discrete("Electricity")
plot_discrete("Toilet")
plot_discrete("BikeScootCar")
plot_discrete("AnimCart")
plot_discrete("AgricLandOwn")
plot_discrete("Cows")
plot_discrete("Horses")
plot_discrete("Goats")
plot_discrete("Sheep")
plot_discrete("Chickens")
plot_discrete("Rural")
plot_discrete("Province")
plot_discrete("Pregnant")
plot_discrete("Education")
plot_discrete("Region")
plot_discrete("Ethnicity")

###USED IN REPORT###
#boxplot for correlation between HHUnder5s and RecentBirth
ggplot(train, aes(x = RecentBirth, y = HHUnder5s)) +
  geom_boxplot()  +
  labs(x = "RecentBirth", y = "HHUnder5s") +
  ggtitle("Figure 2: Boxplot of HHUnder5s by RecentBirth")
dev.copy(png,"HHUnder5s_RecentBirth.png",width=8*72,height=6*72)
dev.off()

# function to create boxplot for interaction between two discrete variables
plot_interaction_two_discrete <- function(x_variable, y_variable, figure) {
  # Create the interaction variable
  interaction_var <- interaction(train[[x_variable]], train[[y_variable]])
  
  # Add the interaction variable to the data frame
  train$interaction_var <- interaction_var
  
  # Use ggplot to plot the boxplot
  ggplot(data = train, aes(x = interaction_var, y = Haemoglobin)) +
    geom_boxplot() +
    labs(x = paste(x_variable, "*", y_variable), y = "Haemoglobin") +
    ggtitle(paste('Figure ',figure,': ', "Interaction between", x_variable, "and", y_variable))
}


###USED IN REPORT###
# boxplot for interaction between Pregnant and RecentBirth
plot_interaction_two_discrete("Pregnant", "RecentBirth", 3)
dev.copy(png,"Pregnant_RecentBirth_interaction.png",width=8*72,height=6*72)
dev.off()

###USED IN REPORT###
# boxplot for interaction between Ethnicity and Pregnant
plot_interaction_two_discrete("Ethnicity","Pregnant", 4)
dev.copy(png,"Ethnicity_Pregnant_interaction.png",width=8*72,height=6*72)
dev.off()

###USED IN REPORT###
# boxplot for interaction between Electricity and Toilet
plot_interaction_two_discrete("Electricity","Toilet", 5)
dev.copy(png,"Electricity_Toilt_interaction.png",width=8*72,height=6*72)
dev.off()

###USED IN REPORT###
#CHierarchical clustering for Province

NumVars <- c('Age','WealthScore','AgricArea','TotalChildren','HHUnder5s','HHSize')

# Current height for cutting the tree
curr_h <- 4.8

# Compute summaries (mean and standard deviation) of numeric variables by Province
Summaries <- aggregate(train[,NumVars],
                       by=list(train$Province),
                       FUN=function(x) {c(Mean=mean(x), SD=sd(x))}) 

# Extract Province names
a <- Summaries[,1]

# Standardize the summaries
Summaries <- scale(Summaries[,-1])

# Compute distances between Province groups
Distances <- dist(Summaries)

# Perform hierarchical clustering
ClusTree <- hclust(Distances, method="complete")

# Plot the dendrogram
plot(ClusTree, xlab="Province group", ylab="Separation", main='Figure 6: Cluster Dendrogram')
abline(h=curr_h, col="red", lty=2)
dev.copy(png,"clustering_Province.png",width=8*72,height=6*72)
dev.off()

# Assign new group labels based on the dendrogram cut height
rownames(Summaries) <- a
NewGroups <- paste("ProvGrp", cutree(ClusTree, h=curr_h), sep="")

# Create a dataframe containing Province names and corresponding group labels
group_data <- data.frame(Province = rownames(Summaries), ProvGroup = NewGroups)

# Merge the group data into the original dataset, matching based on Province names
train <- merge(train, group_data, by = "Province", all.x = TRUE)

# Check the merged dataset
head(train)

# Tabulate the distribution of Province groups
table(rownames(Summaries), NewGroups)

# Boxplot
ggplot(train, aes(x = ProvGroup, y = Haemoglobin)) +
  geom_boxplot()  +
  labs(x = "ProvGroup", y = "Haemoglobin") +
  ggtitle("Boxplot of Haemoglobin by ProvGroup")




#PCA for HHSize, HHUnder5s, WealthScore, AgricArea, and TotalChildren

pc <- prcomp(train[,c('TotalChildren','HHUnder5s','HHSize','AgricArea','WealthScore')], scale. = TRUE)

# Summarize the PCA results
summary(pc)

par(mfrow=c(1,3))

# Biplot showing PC1 vs PC2
biplot(pc, col=c("skyblue3","darkred"), xlabs = rownames(train), cex=c(0.75,1), lwd=2, xlim = c(-0.08, 0.04))

# Biplot showing PC1 vs PC3
biplot(pc, col=c("skyblue3","darkred"), choices = c(1,3), xlabs = rownames(train), cex=c(0.75,1), lwd=2, xlim = c(-0.08, 0.04))

# Biplot showing PC3 vs PC2
biplot(pc, col=c("skyblue3","darkred"), choices = c(3,2), xlabs = rownames(train), cex=c(0.75,1), lwd=2, xlim = c(-0.16, 0.06))

par(mfrow = c(1, 1))
# Plot showing the variances of the Principal Components
plot(pc, main="Variances of PCs for Haemoglobin data")

# Extract PC components and add them to the dataset
pc_components <- predict(pc)[, 1:3]
colnames(pc_components) <- c("PC1", "PC2", "PC3")
train <- cbind(train, pc_components)



###USED IN REPORT###
#scatter plot for interaction between HHUnder5s and TreatedWater
ggplot(train, mapping = aes(x=HHUnder5s, y = Haemoglobin, color = TreatedWater)) +
  stat_smooth(method = "lm", se = FALSE) +
  geom_point(size = 1, alpha = 1) +
  labs(x = "HHUnder5s",
       y = "Haemoglobin") +
  ggtitle("Figure 7: Interaction between HHUnder5s and TreatedWater") +
  theme(plot.title = element_text(hjust = 0.5))
dev.copy(png,"HHUnder5s_TreatedWater_interaction.png",width=8*72,height=6*72)
dev.off()

###USED IN REPORT###
# boxplot for interaction between HHEducation and Rural
plot_interaction_two_discrete("HHEducation","Rural", 8)
dev.copy(png,"HHUnder5s_ Rural_interaction.png",width=8*72,height=6*72)
dev.off()


##########################################################################
#     Model Building
##########################################################################

# First linear model with Province and other variables
model1 <- lm(formula = Haemoglobin ~ Province + RecentBirth * Pregnant  + 
               WealthScore  + Sheep + TotalChildren  + Age + 
               Ethnicity * Pregnant + Electricity*Toilet + 
               HHUnder5s * TreatedWater + Rural * HHEducation,  data = train)

# Print summary and AIC of model1
summary(model1)
AIC(model1)

# Set up the layout for plots: 2 rows, 2 columns
par(mfrow = c(2, 2))

# Plot diagnostics for model1
plot(model1, which = c(1, 2, 3, 4))

# Save the plot as a PNG file
dev.copy(png, "model1_plot.png", width = 8 * 72, height = 6 * 72)
dev.off()

# Linear model with Region, and interaction between Region and Ethnicity and other variables 
model2 <- lm(formula = Haemoglobin ~ Region * Ethnicity + RecentBirth * Pregnant + 
               WealthScore  + Sheep + TotalChildren  + Age + 
               Ethnicity * Pregnant + Electricity * Toilet + 
               HHUnder5s * TreatedWater + Rural * HHEducation,  data = train)

# Print summary and AIC of model2
summary(model2)
AIC(model2)

# Linear model with clustering Province, and interaction between clustering Province and Ethnicity and other variables 
model3 <- lm(formula = Haemoglobin ~ ProvGroup * Ethnicity + RecentBirth * Pregnant + 
               WealthScore  + Sheep + TotalChildren  + Age + 
               Ethnicity * Pregnant + Electricity * Toilet + 
               HHUnder5s * TreatedWater + Rural * HHEducation,  data = train)

# Print summary and AIC of model3
summary(model3)
AIC(model3)

# Move Toilet and Electricity * Toilet in model1
model4 <- lm(formula = Haemoglobin ~ Province + RecentBirth * Pregnant  + 
               WealthScore  + Sheep + TotalChildren + Age + 
               Ethnicity * Pregnant + Electricity + 
               HHUnder5s * TreatedWater + Rural * HHEducation,  data = train)

# Print summary, AIC, and compare with model1
summary(model4)
AIC(model4)
anova(model1, model4)

# Move RecentBirth * Pregnant in model4
model5 <- lm(formula = Haemoglobin ~ Province +
               WealthScore  + Sheep + TotalChildren + Age + 
               Ethnicity * Pregnant + Electricity + 
               HHUnder5s * TreatedWater + Rural * HHEducation,  data = train)

# Print summary, AIC, and compare with model4
summary(model5)
AIC(model5)
anova(model4, model5)

# Remove RecentBirth * Pregnant in model6
model6 <- lm(formula = Haemoglobin ~ Province + RecentBirth * Pregnant  + 
               TotalChildren + 
               Ethnicity * Pregnant + Electricity + 
               HHUnder5s * TreatedWater + Rural * HHEducation,  data = train)

# Plot diagnostics for model6
plot(model6, which = c(1, 2, 3, 4))

# Save the plot as a PNG file
dev.copy(png, "model6_plot.png", width = 8 * 72, height = 6 * 72)
dev.off()

#fitted plot
par(mfrow=c(1,1))
fitted_values = fitted(model5)
plot(x=fitted_values,y= train$Haemoglobin,xlab='Fitted values',ylab = 'Observed values', 
     main = "Figure 11: Scatter plot of Observed values against Fitted values", abline(a=0,b=1,col='red'), 
     col = rgb(0, 0, 0, alpha = 0.3), pch = 16)
dev.copy(png, "fitted_plot.png", width = 8 * 72, height = 6 * 72)
dev.off()

##########################################################################
#     PREDICTION   
##########################################################################

# do the prediction
output <- predict(model6, newdata = test, se.fit = TRUE)
se <- output$se.fit
var <- se ^ 2

# Calculate residual variance
residual_variance <- summary(model6)$sigma^2 

# Calculate total prediction error variance
total_pred_error_variance <- residual_variance + var

# Calculate standard deviation of prediction error
sd_pred_error <- sqrt(total_pred_error_variance)

# Create output data frame and write output data to fil
output_data <- data.frame(ID = test$ID, Prediction = output$fit, 
                          Standard_Error = sd_pred_error)
write.table(output_data, "ICA2_Group89_pred.dat", sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE)
##########################################################################


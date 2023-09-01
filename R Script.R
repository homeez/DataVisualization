
pkgs <- c("ggplot2", "dplyr", 
          "tidyr", "mosaicData",
          "carData", "VIM", "scales",
          "treemapify", "gapminder", 
          "ggmap", "choroplethr", 
          "choroplethrMaps", "CGPfunctions",
          "ggcorrplot", "visreg", "gcookbook",
          "forcats", "survival", "survminer",
          "ggalluvial", "ggridges", 
          "GGally", "superheat", 
          "waterfalls", "factoextra",
          "networkD3", "ggthemes", 
          "hrbrthemes", 
          "ggpol", "ggbeeswarm")
install.packages(pkgs)
install.packages("ggrepel")

library(dplyr)
library(ggplot2)


# Loading up the Dataset into
df <- read.csv('C:/Users/USER/Documents/UoD/Info Viz/AssessmentCrimeData.csv')
head(df, 5)
df

# DATA CLEANING/MANIPULATION

# checking the proportion of missing values for each variable
# after checks, zero missing values detected 
pctmiss <- colSums(is.na(df))/nrow(df)
round(pctmiss, 2)

attach(df)
# Creating the Population Density column

Pop_density = round((df$Population/(df$Land.Area.in.Hectares * 10000)), 3)
Pop_density

# Using regex to extract the needed characters from the string variable Name
Name. = gsub("0.*$", "", Name)


df = cbind(df, Name., Pop_density)

#perform label encoding on variable cegorical variables
df$Name_Code <- as.numeric(factor(df$Name.))



# Creating a new column representing population density as High and Low
df <- mutate(df, Density_Level = ifelse(df$Pop_density > mean(Pop_density),"High","Low"))
df

# Attaching the variables to the dataframe fro ease of calling
attach(df)





PART 1


# the properties of the dataset
str(df)

cbind(summary(df))





crime_data <- data.frame(log10(Anti.Social.Behaviour/Land.Area.in.Hectares),
                         log10(Burglary/Land.Area.in.Hectares),
                         log10(Robbery/Land.Area.in.Hectares),
                         log10(Vehicle.Crimes/Land.Area.in.Hectares),
                         log10(Violent.Crimes/Land.Area.in.Hectares), 
                         log10(Shoplifting/Land.Area.in.Hectares),
                         log10(Criminal.Damage...Arson/Land.Area.in.Hectares), 
                         log10(Other.Theft/Land.Area.in.Hectares),
                         log10(Drugs/Land.Area.in.Hectares),
                         main = log10(Other.Crimes/Land.Area.in.Hectares),
                         log10(Bike.Theft/Land.Area.in.Hectares),
                         log10(Possession.of.Weapons/Land.Area.in.Hectares),
                         log10(Public.Order/Land.Area.in.Hectares),
                         log10(Theft.From.the.Person/Land.Area.in.Hectares))

colnames(crime_data)<-c("ASB","Burglary","Robbery","Vehicle.Crimes","Violent.Crimes","Shoplifting","Arson","Other.Theft","Drugs","Bike.Theft","Other.Crimes","Weapons","Public.Order","Theft.From.the.Person")
install.packages("tidyr")
library(tidyr)
crime_data_long <- gather(crime_data, key = "crime", value = "Frequency")

# Create the boxplot
ggplot(crime_data_long, aes(x = crime, y = Frequency)) + 
  geom_boxplot(color = "red") +
  labs(title = "",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
df
Name.

# Plot Population by region and color by group
library("ggpubr")
ggboxplot(df, x = "Name.", y = "Pop_density", 
          color = "Name.",
          ylab = "Population_density", xlab = "Region")

part3
library(treemapify)
library(gridExtra)

# QQPlot of Land Area
ggplot(pow_model, aes(sample = Land.Area.in.Hectares)) +
  geom_qq(color = "red") +
  geom_abline() +
  xlab("Theoretical Quantiles") +
  ylab("Land Area")



# Ploting the population density bar
plotdata <- df %>%
  count(Density_Level)


# plot the bars in ascending order
ggplot(plotdata,
       aes(x = reorder(Density_Level, n),
           y = n)) +
  geom_bar(stat = "identity") + 
  labs(x = "Population Density Level",
       y = "Frequency",
       title = "Population Density Count") +
  coord_flip()


# The tree map
plotdata <- df %>%
  count(Name.),
ggplot(plotdata,
       aes(fill = Name.,
           area = n,
           label = Name.)) +
  geom_treemap() +
  geom_treemap_text(colour = "black",
                    place = "centre")
labs(title = "Treemap of Records according to Region") +
theme(legend.position = "none"),
# Distribution of Population

ggplot(df, aes(x= log10(Population)))+
  geom_histogram(color="pink", fill="red") +
xlab("Population") + ylab("Count") + ggtitle("Population Distribution")




attach(df)

#finding the Models
model1 <- lm(log10(Anti.Social.Behaviour/Land.Area.in.Hectares)~log10(Population/Land.Area.in.Hectares))
model2 <- lm(log10(Burglary/Land.Area.in.Hectares)~log10(Population/Land.Area.in.Hectares))
model3 <- lm(log10(Robbery/Land.Area.in.Hectares)~log10(Population/Land.Area.in.Hectares))
model4 <- lm(log10(Vehicle.Crimes/Land.Area.in.Hectares)~log10(Population/Land.Area.in.Hectares))
model5 <- lm(log10(Violent.Crimes/Land.Area.in.Hectares)~log10(Population/Land.Area.in.Hectares))
model6 <- lm(log10(Shoplifting/Land.Area.in.Hectares)~log10(Population/Land.Area.in.Hectares))
model7 <- lm(log10(Criminal.Damage...Arson/Land.Area.in.Hectares)~log10(Population/Land.Area.in.Hectares))
model8 <- lm(log10(Other.Theft/Land.Area.in.Hectares)~log10(Population/Land.Area.in.Hectares))
model9 <- lm(log10(Drugs/Land.Area.in.Hectares)~log10(Population/Land.Area.in.Hectares))
model10 <- lm(log10(Bike.Theft/Land.Area.in.Hectares)~log10(Population/Land.Area.in.Hectares))
model11 <- lm(log10(Other.Crimes/Land.Area.in.Hectares)~log10(Population/Land.Area.in.Hectares))
model12 <- lm(log10(Possession.of.Weapons/Land.Area.in.Hectares)~log10(Population/Land.Area.in.Hectares))
model13 <- lm(log10(Public.Order/Land.Area.in.Hectares)~log10(Population/Land.Area.in.Hectares))
model14 <- lm(log10(Theft.From.the.Person/Land.Area.in.Hectares)~log10(Population/Land.Area.in.Hectares))

# Finding the Constant Variance
par(mfrow = c(1,4))
plot(log10(Population/Land.Area.in.Hectares), log10(Anti.Social.Behaviour/Land.Area.in.Hectares), col="red", xlab = "Population" , ylab = "Anti Social Behavior")
abline(model1, col = 'black', lwd = 2)
plot(log10(Population/Land.Area.in.Hectares), log10(Burglary/Land.Area.in.Hectares), col="red", xlab = "Population" , ylab = "Burglary")
abline(model1, col = 'black', lwd = 2)
plot(log10(Population/Land.Area.in.Hectares), log10(Robbery/Land.Area.in.Hectares), col="red", xlab = "Population" , ylab = "Robbery")
abline(model1, col = 'black', lwd = 2)
plot(log10(Population/Land.Area.in.Hectares), log10(Violent.Crimes/Land.Area.in.Hectares), col="red", xlab = "Population" , ylab = "Violent Crime")
abline(model1, col = 'black', lwd = 2)
plot(log10(Population/Land.Area.in.Hectares), log10(Vehicle.Crimes/Land.Area.in.Hectares), col="red", xlab = "Population" , ylab = "Vehicle Crime")
abline(model1, col = 'black', lwd = 2)
plot(log10(Population/Land.Area.in.Hectares), log10(Shoplifting/Land.Area.in.Hectares), col="red", xlab = "Population" , ylab = "Shoplifting")
abline(model1, col = 'black', lwd = 2)
plot(log10(Population/Land.Area.in.Hectares), log10(Criminal.Damage...Arson/Land.Area.in.Hectares), col="red", xlab = "Population" , ylab = "Arson")
abline(model1, col = 'black', lwd = 2)
plot(log10(Population/Land.Area.in.Hectares), log10(Other.Theft/Land.Area.in.Hectares), col="red", xlab = "Population" , ylab = "Other Theft")
abline(model1, col = 'black', lwd = 2)
plot(log10(Population/Land.Area.in.Hectares), log10(Drugs/Land.Area.in.Hectares), col="red", xlab = "Population" , ylab = "Drugs")
abline(model1, col = 'black', lwd = 2)
plot(log10(Population/Land.Area.in.Hectares), log10(Bike.Theft/Land.Area.in.Hectares), col="red", xlab = "Population" , ylab = "Bike Theft")
abline(model1, col = 'black', lwd = 2)
plot(log10(Population/Land.Area.in.Hectares), log10(Other.Crimes/Land.Area.in.Hectares), col="red", xlab = "Population" , ylab = "Other Crimes")
abline(model1, col = 'black', lwd = 2)
plot(log10(Population/Land.Area.in.Hectares), log10(Possession.of.Weapons/Land.Area.in.Hectares), col="red", xlab = "Population" , ylab = "Weapon")
abline(model1, col = 'black', lwd = 2)
plot(log10(Population/Land.Area.in.Hectares), log10(Public.Order/Land.Area.in.Hectares), col="red", xlab = "Population" , ylab = "Public Order")
abline(model1, col = 'black', lwd = 2)
plot(log10(Population/Land.Area.in.Hectares), log10(Theft.From.the.Person/Land.Area.in.Hectares), col="red", xlab = "Population" , ylab = "Theft from Person")
abline(model1, col = 'black', lwd = 2)


#Finding Dependency
par(mfrow = c(1,3))
plot(residuals(model1)[-length(residuals(model1))],residuals(model1)[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon) [i+1]), col = 'red', pch = 14,cex = 0.2, main = "Anti-Social Behaviour", cex.lab =1)
abline(h = 0, col = "black")
plot(residuals(model2)[-length(residuals(model2))],residuals(model2)[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon) [i+1]), col = 'red', pch = 14,cex = 0.2, main = "Burglary", cex.lab =1)
abline(h = 0, col = 'black')
plot(residuals(model3)[-length(residuals(model3))],residuals(model3)[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon) [i+1]), col = 'red', pch = 14,cex = 0.2, main = "Robbery", cex.lab =1)
abline(h = 0, col = "black")
plot(residuals(model4)[-length(residuals(model4))],residuals(model4)[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon) [i+1]), col = 'red', pch = 14,cex = 0.2, main = "Vehicle Crimes", cex.lab =1)
abline(h = 0, col = "black")
plot(residuals(model5)[-length(residuals(model5))],residuals(model5)[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon) [i+1]), col = 'red', pch = 14,cex = 0.2, main = "Violent Crimes", cex.lab =1)
abline(h = 0, col = "black")
plot(residuals(model6)[-length(residuals(model6))],residuals(model6)[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon) [i+1]), col = 'red',  pch = 14,cex = 0.2, main = "Shoplifting", cex.lab =1)
abline(h = 0, col = "black")
plot(residuals(model7)[-length(residuals(model7))],residuals(model7)[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon) [i+1]), col = 'red', pch = 14,cex = 0.2, main = "Criminal Damage & Arson", cex.lab =1)
abline(h = 0, col = "black")
plot(residuals(model8)[-length(residuals(model8))],residuals(model8)[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon) [i+1]), col = 'red',pch = 14,cex = 0.2, main = "Other Thefts", cex.lab =1)
abline(h = 0, col = "black")
plot(residuals(model9)[-length(residuals(model9))],residuals(model9)[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon) [i+1]), col = 'red', pch = 14,cex = 0.2, main = "Drugs", cex.lab =1)
abline(h = 0, col = "black")
plot(residuals(model10)[-length(residuals(model10))],residuals(model10)[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon) [i+1]), col = 'red', pch = 14,cex = 0.2, main = "Bike Theft", cex.lab =1)
abline(h = 0, col = "black")
plot(residuals(model11)[-length(residuals(model11))],residuals(model11)[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon) [i+1]), col = 'red', pch = 14,cex = 0.2, main = "Other Crimes", cex.lab =1)
abline(h = 0, col = "black")
plot(residuals(model12)[-length(residuals(model12))],residuals(model12)[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon) [i+1]), col = 'red', pch = 14,cex = 0.2, main = "Possession of Weapons", cex.lab =1)
abline(h = 0, col = "black")
plot(residuals(model13)[-length(residuals(model13))],residuals(model13)[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon) [i+1]), col = 'red', pch = 14,cex = 0.2, main = "Public Order", cex.lab =1)
abline(h = 0, col = "black")
plot(residuals(model14)[-length(residuals(model14))],residuals(model14)[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon) [i+1]), col = 'red', pch = 14,cex = 0.2, main = "Theft from the Person", cex.lab =1)
abline(h = 0, col = "black")


#Visualise for Normality
par(mfrow = c(1,3))
hist(model1$residuals, col="red", border=3, main = "Behaviour Residuals", xlab = "Residuals", cex.lab = 1.5)
hist(model2$residuals, col="red", border=3, main = "Burglary", xlab = "Residuals", cex.lab = 1.5)
hist(model3$residuals, col="red", border=3,main = "Robbery", xlab = "Residuals", cex.lab = 1.5)
hist(model4$residuals, col="red", border=3,main = "Vehicle Crimes", xlab = "Residuals", cex.lab = 1.5)
hist(model5$residuals, col="red", border=3,main = "Violent Crime", xlab = "Residuals", cex.lab = 1.5)
hist(model6$residuals, col="red", border=3,main = "Shoplifting", xlab = "Residuals", cex.lab = 1.5)
hist(model7$residuals, col="red", border=3,main = "Arson", xlab = "Residuals", cex.lab = 1.5)
hist(model8$residuals, col="red", border=3,main = "Other.Theft", xlab = "Residuals", cex.lab = 1.5)
hist(model9$residuals, col="red", border=3,main = "Drugs", xlab = "Residuals", cex.lab = 1.5)
hist(model10$residuals, col="red", border=3,main = "Bike.Theft", xlab = "Residuals", cex.lab = 1.5)
hist(model11$residuals, col="red", border=3,main = "Other.Crimes", xlab = "Residuals", cex.lab = 1.5)
hist(model12$residuals, col="red", border=3,main = "Weapons", xlab = "Residuals", cex.lab = 1.5)
hist(model13$residuals, col="red", border=3,main = "Public.Order", xlab = "Residuals", cex.lab = 1.5)
hist(model14$residuals, col="red", border=3,main = "Theft.From.the.Person", xlab = "Residuals", cex.lab = 1.5)



# Residual plot

residuals = data.frame(model1$residuals, model2$residuals, model3$residuals, model4$residuals, model5$residuals, model6$residuals, model7$residuals, model8$residuals, model9$residuals, model10$residuals, model11$residuals, model12$residuals, model3$residuals, model14$residuals)
colnames(residuals) = c("ASB", "Burglary", "Robbery", "Vehicle crimes", "Violent Crimes", "Shoplifting", "Criminal Danage", "Other Theft", "Drugs", "Bike Theft", "Other Theft", "Weapons", "Public Order", "TFoP")
plot(residuals, col = "red", cex = 0.2)


#Scatter Plot of Residuals
grid.arrange(
ggplot(data = df, aes(x = log10(Population/Land.Area.in.Hectares), y =log10(Anti.Social.Behaviour/Land.Area.in.Hectares))) +
  geom_smooth(method = "lm", se = FALSE) +
  #Add some line Segment
  geom_segment(aes(xend = log10(Population/Land.Area.in.Hectares), yend = model1$fitted.values), alpha = .3) +
  #Add points
  geom_point(aes(color = abs(model1$residuals), size = abs(model1$residuals)))+
  scale_color_continuous(low  = "white", high = "red") +
  theme_bw() +
  labs(title = "Anti Social Behaviour"),
ggplot(data = df, aes(x = log10(Population/Land.Area.in.Hectares), y =log10(Burglary/Land.Area.in.Hectares))) +
  geom_smooth(method = "lm", se = FALSE) +
  #Add some line Segment
  geom_segment(aes(xend = log10(Population/Land.Area.in.Hectares), yend = model2$fitted.values), alpha = .3) +
  #Add points
  geom_point(aes(color = abs(model2$residuals), size = abs(model2$residuals)))+
  scale_color_continuous(low  = "white", high = "red") +
  theme_bw() +
  labs(title = "Burglary"),
ggplot(data = df, aes(x = log10(Population/Land.Area.in.Hectares), y =log10(Robbery/Land.Area.in.Hectares))) +
  geom_smooth(method = "lm", se = FALSE) +
  #Add some line Segment
  geom_segment(aes(xend = log10(Population/Land.Area.in.Hectares), yend = model3$fitted.values), alpha = .3) +
  #Add points
  geom_point(aes(color = abs(model3$residuals), size = abs(model3$residuals)))+
  scale_color_continuous(low  = "white", high = "red") +
  theme_bw() +
  labs(title = "Robbery"),
ggplot(data = df, aes(x = log10(Population/Land.Area.in.Hectares), y =log10(Vehicle.Crimes/Land.Area.in.Hectares))) +
  geom_smooth(method = "lm", se = FALSE) +
  #Add some line Segment
  geom_segment(aes(xend = log10(Population/Land.Area.in.Hectares), yend = model4$fitted.values), alpha = .3) +
  #Add points
  geom_point(aes(color = abs(model4$residuals), size = abs(model4$residuals)))+
  scale_color_continuous(low  = "white", high = "red") +
  theme_bw() +
  labs(title = "Vehicle.Crimes"),
ggplot(data = df, aes(x = log10(Population/Land.Area.in.Hectares), y =log10(Violent.Crimes/Land.Area.in.Hectares))) +
  geom_smooth(method = "lm", se = FALSE) +
  #Add some line Segment
  geom_segment(aes(xend = log10(Population/Land.Area.in.Hectares), yend = model5$fitted.values), alpha = .3) +
  #Add points
  geom_point(aes(color = abs(model5$residuals), size = abs(model5$residuals)))+
  scale_color_continuous(low  = "white", high = "red") +
  theme_bw() +
  labs(title = "Violent.Crimes"),
ggplot(data = df, aes(x = log10(Population/Land.Area.in.Hectares), y =log10(Shoplifting/Land.Area.in.Hectares))) +
  geom_smooth(method = "lm", se = FALSE) +
  #Add some line Segment
  geom_segment(aes(xend = log10(Population/Land.Area.in.Hectares), yend = model6$fitted.values), alpha = .3) +
  #Add points
  geom_point(aes(color = abs(model6$residuals), size = abs(model6$residuals)))+
  scale_color_continuous(low  = "white", high = "red") +
  theme_bw() +
  labs(title = "Shoplifting"), nrow = 2, ncol =3 )

grid.arrange(
ggplot(data = df, aes(x = log10(Population/Land.Area.in.Hectares), y =log10(Criminal.Damage...Arson/Land.Area.in.Hectares))) +
  geom_smooth(method = "lm", se = FALSE) +
  #Add some line Segment
  geom_segment(aes(xend = log10(Population/Land.Area.in.Hectares), yend = model7$fitted.values), alpha = .3) +
  #Add points
  geom_point(aes(color = abs(model7$residuals), size = abs(model7$residuals)))+
  scale_color_continuous(low  = "white", high = "red") +
  theme_bw() +
  labs(title = "Criminal.Damage...Arson"),
ggplot(data = df, aes(x = log10(Population/Land.Area.in.Hectares), y =log10(Other.Theft/Land.Area.in.Hectares))) +
  geom_smooth(method = "lm", se = FALSE) +
  #Add some line Segment
  geom_segment(aes(xend = log10(Population/Land.Area.in.Hectares), yend = model8$fitted.values), alpha = .3) +
  #Add points
  geom_point(aes(color = abs(model8$residuals), size = abs(model8$residuals)))+
  scale_color_continuous(low  = "white", high = "red") +
  theme_bw() +
  labs(title = "Other.Theft"),
ggplot(data = df, aes(x = log10(Population/Land.Area.in.Hectares), y =log10(Drugs/Land.Area.in.Hectares))) +
  geom_smooth(method = "lm", se = FALSE) +
  #Add some line Segment
  geom_segment(aes(xend = log10(Population/Land.Area.in.Hectares), yend = model9$fitted.values), alpha = .3) +
  #Add points
  geom_point(aes(color = abs(model9$residuals), size = abs(model9$residuals)))+
  scale_color_continuous(low  = "white", high = "red") +
  theme_bw() +
  labs(title = "Drugs"),
ggplot(data = df, aes(x = log10(Population/Land.Area.in.Hectares), y =log10(Bike.Theft/Land.Area.in.Hectares))) +
  geom_smooth(method = "lm", se = FALSE) +
  #Add some line Segment
  geom_segment(aes(xend = log10(Population/Land.Area.in.Hectares), yend = model10$fitted.values), alpha = .3) +
  #Add points
  geom_point(aes(color = abs(model10$residuals), size = abs(model10$residuals)))+
  scale_color_continuous(low  = "white", high = "red") +
  theme_bw() +
  labs(title = "Drugs"),
ggplot(data = df, aes(x = log10(Population/Land.Area.in.Hectares), y =log10(Other.Crimes/Land.Area.in.Hectares))) +
  geom_smooth(method = "lm", se = FALSE) +
  #Add some line Segment
  geom_segment(aes(xend = log10(Population/Land.Area.in.Hectares), yend = model11$fitted.values), alpha = .3) +
  #Add points
  geom_point(aes(color = abs(model11$residuals), size = abs(model11$residuals)))+
  scale_color_continuous(low  = "white", high = "red") +
  theme_bw() +
  labs(title = "Bike.Theft"),
ggplot(data = df, aes(x = log10(Population/Land.Area.in.Hectares), y =log10(Possession.of.Weapons/Land.Area.in.Hectares))) +
  geom_smooth(method = "lm", se = FALSE) +
  #Add some line Segment
  geom_segment(aes(xend = log10(Population/Land.Area.in.Hectares), yend = model12$fitted.values), alpha = .3) +
  #Add points
  geom_point(aes(color = abs(model12$residuals), size = abs(model12$residuals)))+
  scale_color_continuous(low  = "white", high = "red") +
  theme_bw() +
  labs(title = "Other.Crimes"),
ggplot(data = df, aes(x = log10(Population/Land.Area.in.Hectares), y =log10(Public.Order/Land.Area.in.Hectares))) +
  geom_smooth(method = "lm", se = FALSE) +
  #Add some line Segment
  geom_segment(aes(xend = log10(Population/Land.Area.in.Hectares), yend = model13$fitted.values), alpha = .3) +
  #Add points
  geom_point(aes(color = abs(model13$residuals), size = abs(model13$residuals)))+
  scale_color_continuous(low  = "white", high = "red") +
  theme_bw() +
  labs(title = "Public.Order"),
ggplot(data = df, aes(x = log10(Population/Land.Area.in.Hectares), y =log10(Theft.From.the.Person/Land.Area.in.Hectares))) +
  geom_smooth(method = "lm", se = FALSE) +
  #Add some line Segment
  geom_segment(aes(xend = log10(Population/Land.Area.in.Hectares), yend = model14$fitted.values), alpha = .5) +
  #Add points
  geom_point(aes(color = abs(model14$residuals), size = abs(model14$residuals)))+
  scale_color_continuous(low  = "white", high = "red") +
  theme_bw() +
  labs(title = "Theft.From.the.Person"), nrow = 2, ncol =4)


R_1
R_2
R_3
R_4
R_5
R_6
R_7
R_8
R_9
R_10
R_11
R_12
R_13
R_14




install.packages("vctrs")


# HeatMAP
library(ggplot2)
library(ggcorrplot)

# calulate the correlations
htmap <- cor(residuals, use="complete.obs")
round(htmap,2)
ggcorrplot(htmap,
           hc.order = TRUE,
           type = "full",
           lab = TRUE) +
  scale_fill_gradient(low = "white",
                      high = "red",
                      guide = "colorbar")



library(ggplot2)
library(ggcorrplot)

# calulate the correlations
r <- cor(residuals, use="complete.obs")
round(r,2)
ggcorrplot(r,
           hc.order = TRUE,
           type = "upper",
           lab = TRUE)

# Clustering

# Assuming your data is stored in a data frame called "my_data"
library(ggplot2)
library(cluster)

# Elbow Method - WCSS plot
wcss <- vector(length = 10)
for (k in 1:10) {
  kmeans_result <- kmeans(residuals, centers = k)
  wcss[k] <- kmeans_result$tot.withinss
}

# Create a data frame for the WCSS values
elbow_data <- data.frame(k = 1:10, WCSS = wcss)

# Create WCSS plot using ggplot2
elbow_plot <- ggplot(elbow_data, aes(x = k, y = WCSS)) +
  geom_point(size = 7, shape = 19, color = "red") +
  geom_line(color = "red") +
  labs(x = "Number of Clusters (k)", y = "WCSS",
       title = "Elbow Plot") +
  theme_minimal()
elbow_plot 

# Perform k-means clustering
# Add cluster labels to the original data
k <- 4  # number of clusters
kmeans_result <- kmeans(residuals, centers = k)

data$cluster <- as.factor(kmeans_result$cluster)

library(factoextra)
set.seed(1234)
final <- kmeans(data$cluster, 4, nstart = 25)
fviz_cluster(final, data = residuals)




# Dendogram
library(ggdendro)
library(ggplotlyExtra)
library(plotly)
model <- hclust(dist(residuals), "ave")
dhc <- as.dendrogram(model)
data <- dendro_data(dhc, type = "rectangle")
p <- ggplot(segment(data)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend), color = "red") + 
  scale_y_reverse(expand = c(0.2, 0)) + ggtitle("The Dendogram")
p

library(gridExtra)
grid.arrange(
  elbow_plot, 
  fviz_cluster(final, data = residuals),
  p, nrow = 1, ncol = 3)


crop1=ggplot(data=residuals) +
  geom_boxplot(fill='red')
crop1












PART 2B SCATTERPLOTS AND HEATMAP OF RESIDUALS
residuals = data.frame(cbind(asm_res, burg_res, rob_res, sl_res, ot_res, po_res, pow_res, drg_res))
attach(residuals)


# calulate the correlations
r <- cor(residuals, use="complete.obs")
round(r,2)
library(ggcorrplot)
ggcorrplot(r,
           hc.order = TRUE,
           lab = TRUE)


PART 2C HEIRACHICAL CLUSTRING AND DENDOGRAM
# Calculate the distance matrix between residuals
dist_matrix <- dist(residuals)

# Apply hierarchical clustering
hc <- hclust(dist_matrix)
hc

# Plot the dendrogram
plot(hc, main = "Dendrogram of Residuals")

library(plotly)
library(ggplot2)
library(ggdendro)

model <- hclust(dist(residuals), "ave")
dhc <- as.dendrogram(model)

data <- dendro_data(dhc, type = "rectangle")
p <- ggplot(segment(data)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
  coord_flip() + 
  scale_y_reverse(expand = c(0, 5))

ggplotly(p)










































UNDERSTANDING THE REGIONAL LAND AREA
# finding the range
range(df$Land.Area.in.Hectares)

# finding the breaks
breaks = seq(0, 17000, by = 5000) 
breaks

#Classify the Quantity according to the half-unit-length sub-intervals with cut
LandArea.cut = cut(df$Land.Area.in.Hectares, breaks, right = FALSE)

#frequency distribution of the quantity of goods ordered for
LandArea.freq = table(LandArea.cut)
rbind(LandArea.freq)

#Finding the Cumulative frequency distribution of the Regional Land Area
LandArea.cumfreq = cumsum(LandArea.freq)
rbind(LandArea.cumfreq)

#Checking for the mean regional Total Land Area
Regional_Mean_Area = rbind(round(sort(tapply(df$Land.Area.in.Hectares, df$Name., mean),
                                      decreasing = TRUE),2))
Regional_Mean_Area



GEOMAPPING
library(sf)
# Loading the shapefile
sfile <- st_read(
  "C:/Users/USER/Documents/UoD/Info Viz/shapefiles2/Lower_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.shx")

# Loading the dataset
regions<- rio::import('C:/Users/USER/Documents/UoD/Info Viz/AssessmentCrimeData.csv')

# Rename column where names is "Sepal.Length"
names(regions)[names(regions) == "LSOA"] <- "LSOA11CD"
names(regions)[names(regions) == "Name"] <- "LSOA11NM"
#regions = fortify(aoi_boundary)


# merge two data frames by ID
shp_main <- merge(sfile,regions,by="LSOA11CD")

shp_main$"Population"<- regions$Population/regions$`Land Area in Hectares`
shp_main$"ASB"<- log10(regions$`Anti-Social Behaviour`/regions$`Land Area in Hectares`)
shp_main$"Burglary"<- log10(regions$Burglary/regions$`Land Area in Hectares`)
shp_main$"Robbery"<- log10(regions$Robbery/regions$`Land Area in Hectares`)
shp_main$"Vehicle Crimes"<- log10(regions$`Vehicle Crimes`/regions$`Land Area in Hectares`)
shp_main$"Violent Crimes"<- log10(regions$`Violent Crimes`/regions$`Land Area in Hectares`)
shp_main$"Shoftlifting"<- log10(regions$Shoplifting/regions$`Land Area in Hectares`)
shp_main$"Arson"<- log10(regions$`Criminal Damage & Arson`/regions$`Land Area in Hectares`)
shp_main$"Other Theft"<- log10(regions$`Other Theft`/regions$`Land Area in Hectares`)
shp_main$"Drugs"<- log10(regions$Drugs/regions$`Land Area in Hectares`)
shp_main$"Other Crimes"<- log10(regions$`Other Crimes`/regions$`Land Area in Hectares`)
shp_main$"Bike Theft"<- log10(regions$`Bike Theft`/regions$`Land Area in Hectares`)
shp_main$"Weapons"<- log10(regions$`Possession of Weapons`/regions$`Land Area in Hectares`)
shp_main$"Public Order"<- log10(regions$`Public Order`/regions$`Land Area in Hectares`)
shp_main$"Theft From"<- log10(regions$`Theft From the Person`/regions$`Land Area in Hectares`)

library(tmap)



library(ggplot2)
grid.arrange(
  ggplot() + geom_sf(data = shp_main, aes(fill = !!sym("Population"))) + scale_fill_gradient(low = "white", high = "red") + theme_void(),
  ggplot() + geom_sf(data = shp_main, aes(fill = !!sym("ASB"))) + scale_fill_gradient(low = "white", high = "red") + theme_void(),
  ggplot() + geom_sf(data = shp_main, aes(fill = !!sym("Burglary"))) + scale_fill_gradient(low = "white", high = "red") + theme_void(),
  ggplot() + geom_sf(data = shp_main, aes(fill = !!sym("Robbery"))) + scale_fill_gradient(low = "white", high = "red") + theme_void(),
  ggplot() + geom_sf(data = shp_main, aes(fill = !!sym("Shoftlifting"))) + scale_fill_gradient(low = "white", high = "red") + theme_void(), nrow = 1, ncol = 5)

grid.arrange(
  ggplot() + geom_sf(data = shp_main, aes(fill = !!sym("Violent Crimes"))) + scale_fill_gradient(low = "white", high = "red") + theme_void(),
  ggplot() + geom_sf(data = shp_main, aes(fill = !!sym("Arson"))) + scale_fill_gradient(low = "white", high = "red") + theme_void(),
  ggplot() + geom_sf(data = shp_main, aes(fill = !!sym("Other Theft"))) + scale_fill_gradient(low = "white", high = "red") + theme_void(),
  ggplot() + geom_sf(data = shp_main, aes(fill = !!sym("Drugs"))) + scale_fill_gradient(low = "white", high = "red") + theme_void(),
  ggplot() + geom_sf(data = shp_main, aes(fill = !!sym("Other Crimes"))) + scale_fill_gradient(low = "white", high = "red") + theme_void(), nrow = 1, ncol = 5)

grid.arrange(
  ggplot() + geom_sf(data = shp_main, aes(fill = !!sym("Bike Theft"))) + scale_fill_gradient(low = "white", high = "red") + theme_void(),
  ggplot() + geom_sf(data = shp_main, aes(fill = !!sym("Weapons"))) + scale_fill_gradient(low = "white", high = "red") + theme_void(),
  ggplot() + geom_sf(data = shp_main, aes(fill = !!sym("Public Order"))) + scale_fill_gradient(low = "white", high = "red") + theme_void(),
  ggplot() + geom_sf(data = shp_main, aes(fill = !!sym("Theft From"))) + scale_fill_gradient(low = "white", high = "red") + theme_void(),
  ggplot() + geom_sf(data = shp_main, aes(fill = !!sym("Vehicle Crimes"))) + scale_fill_gradient(low = "white", high = "red") + theme_void(), nrow = 1, ncol = 5)































# Check for constant variance using a plot of residuals vs fitted values
plot_resid_vs_fitted <- ggplot(model, aes(fitted.values, resid)) +
  geom_point() +
  xlab("Fitted Values") +
  ylab("Residuals") +
  ggtitle("Residuals vs Fitted Values")

# Check for independence using a plot of residuals vs order of observations
plot_resid_vs_order <- ggplot(model, aes(seq_along(resid), resid)) +
  geom_point() +
  xlab("Order of Observations") +
  ylab("Residuals") +
  ggtitle("Residuals vs Order of Observations")

# Check for normality using a histogram and a QQ plot of residuals
plot_resid_hist <- ggplot(model, aes(resid)) +
  geom_histogram(binwidth = 0.5) +
  xlab("Residuals") +
  ylab("Frequency") +
  ggtitle("Histogram of Residuals")

plot_resid_qq <- ggplot(model, aes(sample = resid)) +
  geom_qq() +
  geom_abline() +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  ggtitle("QQ Plot of Residuals")

# Display the visualizations
plot_resid_vs_fitted
plot_resid_vs_order
plot_resid_hist
plot_resid_qq



summary(df)
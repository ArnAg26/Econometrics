# install.packages("e1071")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("stargazer")

library(ggplot2)
library(dplyr)
library(e1071)
library(stargazer)

# Load datasets
df_gwl = read.csv("C:/Users/MANAS/Desktop/eco1/data/Groundwater Level/7063/NDAP_REPORT_7063.csv")
df_sdp <- read.csv("C:/Users/MANAS/Desktop/eco1/data/sdp.csv")
df_gini <- read.csv("C:/Users/MANAS/Desktop/eco1/data/gini.csv")

df_save_address = "C:/Users/MANAS/Desktop/eco1/data/group_18.csv"

# part (1)

# We have picked Ground Water Level as our Environmental Quality Measure (EQM).
# It has 'level' as an Environmental Qaulity Indicator (EQI)


# part (2)

# The dataset associated with our chosen EQM, consists of locations upto a granularity of districts. 
# However each district has records of GWL for several months in a given year, hence we take the average of all the months in a year.

df_gwl<-na.omit(df_gwl)


names(df_gwl)[11]<-"ground_water_level";
names(df_gwl)[3]<-"State_code"
names(df_gwl)[5]<-"District_code"

df <- df_gwl %>% group_by(District, Yearcode) %>% summarize(Ground_water_level=mean(ground_water_level),State=first(State),Country=first(Country),State_code=first(State_code),District_code=first(District_code))

df$ID<-df$District_code*10000+df$Yearcode

# drop irrelevant columns
drop = c("District_code", "State_code", "Country")
df = df[, !(names(df) %in% drop)]

View(df)


# part (3)

# In the dataset from DBIE portal, we are given Net State Domestic Product (NSDP or SDP) for each state. We assume each district from a given state to have the same SDP.

df$State <- apply(df["State"], 2, tolower)
df_sdp$State <- apply(df_sdp["State"], 2, tolower)

df <- merge(df, df_sdp, by=c("State", "Yearcode"))

View(df)


# part (4)

# Since the dataset for Gini index was terribly given in a PDF, we have copied it manually to a CSV file and reading it using `read.csv` method in R.


# We only keep the district name and it's gini index value, since that is the only attribute of our interest

colnames(df_gini)[9] <- "gini"
keep = c("District", "gini")
df_gini = df_gini[, (names(df_gini) %in% keep)]

df$District <- apply(df["District"], 2, tolower)
df_gini$District <- apply(df_gini["District"], 2, tolower)


df <- merge(df, df_gini, by="District")

df<-na.omit(df)

View(df)

# Save dataframe
write.csv(df, df_save_address, row.names=FALSE)


# Part (5)
df<-read.csv("C:/Users/ARNAV/Documents/R/EconData/data/group_18.csv")
df$SDP <- as.numeric(df$SDP)
df$gini <- as.numeric(df$gini)
df$Yearcode <- as.numeric(df$Yearcode)

df<-na.omit(df)


# tables, histogram, box-plot, shape of the distribution, skew

summary(df$District)
summary(df$State)
summary(df$Yearcode)
summary(df$Ground_water_level)
summary(df$SDP)
summary(df$gini)

stargazer(df, type = "text", title = "Summary of statistics of final dataset")


# Histograms

hist(df$Yearcode, main = "Histogram - Yearcode")

hist(df$Ground_water_level, main = "Histogram - Ground Water Level")

hist(df$SDP, main = "Histogram - SDP")

hist(df$gini, main = "Histogram - gini")


# Box plots and outliers

boxplot(df$Yearcode, main = "BoxPlot - Yearcode")

boxplot(df$Ground_water_level, main = "BoxPlot - Ground Water Level")
boxplot(df$Ground_water_level, outline = FALSE, main = "BoxPlot - Ground water level (without outliers)")

boxplot(df$SDP, main = "BoxPlot - SDP")

boxplot(df$gini, main = "BoxPlot - gini")


# display outliers and remove them from our dataset
outliers = boxplot(df$Yearcode)$out
print(outliers)
df <- df[!df %in% outliers]

outliers = boxplot(df$Ground_water_level)$out
print(outliers)
df <- df[!df %in% outliers]

outliers = boxplot(df$SDP)$out
print(outliers)
df <- df[!df %in% outliers]

outliers = boxplot(df$gini)$out
print(outliers)
df <- df[!df %in% outliers]


skewness(df$Yearcode)
skewness(df$Ground_water_level)
skewness(df$SDP)
skewness(df$gini)


# Shapes
plot(density(df$Yearcode), main = "Density plot - Year")

plot(density(df$Ground_water_level), main = "Density plot - Ground Water Level")

plot(density(df$SDP), main = "Density plot - SDP")

plot(density(df$gini), main = "Density plot - gini")

# ###############################


ggplot(df, aes(x = Ground_water_level)) +
  geom_histogram(fill = "white", colour = "black",bins=100) +
  facet_grid(Yearcode ~ .)

ggplot(data = df, aes(x = Yearcode, y = Ground_water_level)) +
  geom_boxplot()+stat_summary(fun.y="mean",color="red",shape=1)

skewness(df$Ground_water_level)


df$Yearcode <-as.factor(df$Yearcode)

df[df == '-'] <- NA

N<-log(df$SDP)
ggplot(df, aes(x = N)) +
  geom_histogram(fill = "white", colour = "black",bins=20,pad=0.1,stat="count") +
  facet_grid(Yearcode ~ .)

ggplot(data = df, aes(x = Yearcode, y = N)) +
  geom_boxplot()+stat_summary(fun.y="mean",color="red",shape=1)

df<-df[!is.na(df$gini),]
df$gini<-as.numeric(df$gini)

ggplot(df, aes(x = gini)) +
  geom_histogram(fill = "white", colour = "black",bins=100) +
  facet_grid(Yearcode ~ .)


df$Yearcode <-as.factor(df$Yearcode)
ggplot(data = df, aes(x = Yearcode, y = gini)) +
  geom_boxplot()+stat_summary(fun.y="mean",color="red",shape=1)

# ##############################

# Part (6)

SDP = df$SDP
GWL = df$Ground_water_level


model_1 <- lm(GWL~SDP)

summary(model_1)




# Part (7)

# We know u_hat = y - y_hat, so y_hat = y - u_hat
u_hat = resid(model_1)
y_hat = GWL - u_hat


plot(SDP, GWL, xlab="SDP", ylab="Ground water level", main="SDP vs GWL")


plot(SDP, u_hat, xlab="SDP", ylab="residuals ", main="SDP vs Residuals")


plot(GWL, y_hat, xlab="True value", ylab="Predicted value", main="Predicted vs True values")



# Part (8)

sum_residuals = sum(u_hat)

print(sum_residuals)

# We can see the sum of residuals is extremely small that is very close to 0. Hence we can say that the sum of residuals is 0. 
# It is not exactly zero due to numerical precision errors in computing the co-efficients and getting the residuals.

hist(u_hat, main = "Histogram - u_hat")


# Part (9)

SDP = df$SDP
GWL = df$Ground_water_level


gini = df$gini

SDP_2 = SDP ^ 2
SDP_3 = SDP ^ 3

model_2 <- lm(GWL~SDP + SDP_2 + SDP_3 + gini)

summary(model_2)
nrow(df)



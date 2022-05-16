# ----- House price prediction using R ----------
# https://www.kaggle.com/ysthehurricane/house-price-prediction-using-r-programming
# --- kaggle.com


library(ggplot2) 
# --------------------- Import dataset ----------------------------------
# setwd("~/Hema_Documents/SG Skills Future/Data_Samples_Lithan")    --- set working directory 

data <- read.csv(file='HousingPriceData.csv')

# ----- Data Exploration -------------------------------
head(data)

tail(data)

nrow(data)   # no of rows in data 4600
ncol(data)   # no of columns  18

print(paste("Number of records: ",nrow(data)))
print(paste("Number of features: ",ncol(data)))

summary(data)
colnames(data)    # columns names

unique(data$city)  # Unique cities

# ---------------8 Features selected for prediction ---------------
maindf <- data[,c("price","bedrooms","sqft_living","floors",
                  "sqft_lot", "condition", "view" , "yr_built")]


head(maindf)

# ---- Checking Null values -------------
nrow(maindf)    # total 4600 rows
is.na(maindf)   # 4475 rows not null
sum(is.na(maindf))

# ----- Figure out house age ----------
maindf$oldbuilt <- as.integer(format(Sys.Date(),"%Y")) - maindf$yr_built
# maindf has 4489 rows x 9 cols  price bedrooms sqft_living floors sqft_lot condition view yr_built oldbuilt
maindf       # --- has 4489 rows   


drops <- c("yr_built")

names(maindf) %in% drops

maindf = maindf[,!(names(maindf) %in% drops)]
maindf    # --- yr_built column removed

# ---- Plot Correlation matrix ---------------
cor(maindf)
typeof(cor(maindf))    # -- double ----


install.packages("ggcorrplot")
library(ggcorrplot)

corr <- data[,(round(cor(maindf), 1))]    # --- rounding all column values
typeof(corr)     # -- double 
#--------Plot ------
ggcorrplot(corr,
           type = "lower",
           lab = TRUE,
           lab_size = 5,
           colors = c("tomato2","white","green"),
           title = "Correlogram of Housing Dataset",
           ggtheme = theme_bw)


# ----- Scatterplot matrix -------------------------
pairs(~bedrooms + sqft_living + floors + condition, data = maindf,
      main = "Scatterplot Matrix")


# ----------- Plot boxplot for checking outliers ------------------

par(mfrow=c(2, 3))  # divide graph area in 2 rows 3 columns
boxplot(maindf$bedrooms, main="Bedrooms")
boxplot(maindf$sqft_living, main="sqft_living")
boxplot(maindf$floors, main="floors")
boxplot(maindf$condition, main="condition")
boxplot(maindf$view, main="view")
boxplot(maindf$oldbuilt, main="oldbuilt")

# -------- Plot scatterplots  ------------------

theme_set(theme_bw()) 
g <- ggplot(maindf, aes(bedrooms, floors))
g + geom_count(col="tomato3", show.legend=F) +
  labs(y="floors", 
       x="bedrooms", 
       title="Bedrooms vs Floors")


# ------ scatterplot 2 --------------------
plot(x = maindf$sqft_living, y = maindf$sqft_lot,
     xlab = "sqft_living",
     ylab = "sqft_lot",
     xlim = c(0, 3000), 
     ylim = c(0, 20000),
     main = "sqft_living vs sqft_lot"
)

# -------- Plot density plot to check normality ----------
install.packages("e1071")
library(e1071)

par(mfrow=c(2, 3)) 

plot(density(maindf$bedrooms), main="Density Plot: Bedrooms", ylab="Frequency",
     sub=paste("Skewness:", round(e1071::skewness(maindf$bedrooms), 2)))  
polygon(density(maindf$bedrooms), col="green")

plot(density(maindf$sqft_living), main="Density Plot: sqft_living", ylab="Frequency",
     sub=paste("Skewness:", round(e1071::skewness(maindf$sqft_living), 2)))  
polygon(density(maindf$sqft_living), col="orange")

plot(density(maindf$sqft_lot), main="Density Plot: sqft_lot", ylab="Frequency",
     sub=paste("Skewness:", round(e1071::skewness(maindf$sqft_lot), 2)))  
polygon(density(maindf$sqft_lot), col="green")

plot(density(maindf$condition), main="Density Plot: condition", ylab="Frequency",
     sub=paste("Skewness:", round(e1071::skewness(maindf$condition), 2)))  
polygon(density(maindf$condition), col="orange")

plot(density(maindf$floors), main="Density Plot: floors", ylab="Frequency",
     sub=paste("Skewness:", round(e1071::skewness(maindf$floors), 2)))  
polygon(density(maindf$floors), col="green")

plot(density(maindf$oldbuilt), main="Density Plot: oldbuilt", ylab="Frequency",
     sub=paste("Skewness:", round(e1071::skewness(maindf$oldbuilt), 2)))  
polygon(density(maindf$oldbuilt), col="orange")


#----------Plot univariate linear regression between sqft_living and price ------

ggplot(maindf,aes(y=price,x=sqft_living)) +
  geom_point() + 
  xlim(0, 9000) +
  ylim(0, 5000000) +
  geom_smooth(formula = y ~ x,method="lm")

# -----  Multi univariate linear regression ---------
linearmodel = lm(price~bedrooms + sqft_living + floors + sqft_lot + condition + view + oldbuilt,
                 data = maindf)
summary(linearmodel)

canvas size of visual -- decide how big it has to be

data <- read.csv(file='.../ /data.csv')
head(data)

columns which are highly affecting the cost of house

18 cols ---? 7 cols are selected which counts for price of house

Linear Reg works on '
  No independant variables cannot be co-related'

linermodel <- lm(price~bedrooms+sqft_living+floors +sqft_lot)

F - statistics  p-value
how your model is performing

Accuracy can be manipulated...tricy way

reducing errors from 50% to 25%  --- your model is good
Focus on errors instaed of focusing on accuracy

AS occuracy increases errors


installed.packages("name_of_package")   --> to install package.skeleton()



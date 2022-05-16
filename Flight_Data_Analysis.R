# Map 1-based optional input ports to variables 
# ----- Flight Data --------------
install.packages("gridExtra")

csv_data <- read.csv(file = 'Flight Delays Data.csv')
csv_data 

# dat <- maml.mapInputPort(1)   # class: data.frame 

#Code 3: Activity 7 View ArrDelay Distribution 
# Display ArrDelay data range 
range(csv_data$ArrDelay) 

## Load libraries and set plot properties 
library(ggplot2) 
library(gridExtra) 
options(repr.plot.width=6, repr.plot.height=3) 
## Calculate binwidth for histogram 
rg = range(csv_data[,'ArrDelay']) 
bw = (rg[2] - rg[1])/30 

## Histogram for ArrDelay 
p1 = ggplot(csv_data, aes(x=ArrDelay)) +  
  geom_histogram(binwidth=bw, fill = "red") + 
  labs(x = "ArrDelay", y = "Count of flights", title = "Histogram of ArrDelay") + 
  theme_bw(); 

## Box plot for ArrDelay 
p2 = ggplot(csv_data, aes(x = factor(0), y = ArrDelay)) +  
  geom_boxplot(color = "blue") + 
  ggtitle ("Boxplot of ArrDelay") + 
  theme_bw(); 

## Arrange the Histogram and Box plot on same row 
grid.arrange(p1,p2,nrow=1) 


#Code 3: Activity 8 Use Histograms to Compare Numeric Columns 
## Function to plot conditioned histograms 

arrdel15.hist = function(x) { 
  library(ggplot2) 
  library(gridExtra) 
  
  options(repr.plot.width=6, repr.plot.height=3) 
  
  ## Compute the bin width 
  rg = range(csv_data[,x]) 
  bw = (rg[2] - rg[1])/30 
  
  ## Define the title 
  title <- paste("Histogram of", x, "conditioned on ArrDel15") 
  
  ## Create the histogram 
  ggplot(dat, aes_string(x)) + 
    geom_histogram(aes(y = ..count..), binwidth = bw, fill = "red") + 
    facet_grid(. ~ ArrDel15) + 
    ggtitle(title) + 
    ylab("Count of ArrDelay") + 
    theme_bw() 
} 

## Create histograms for specified features. 
plot.cols2 = c("DepDelay", 
               "CRSArrTime", 
               "CRSDepTime", 
               "DayofMonth", 
               "DayOfWeek", 
               "Month") 

lapply(plot.cols2, arrdel15.hist) 

#Code 4: Activity 9 Use Scatter Plots to Compare Numeric Columns 
## Scatter plot using color to differentiate points 
scatter.delay = function(x){ 
  
  library(ggplot2) 
  library(gridExtra) 
  
  options(repr.plot.width=6, repr.plot.height=3) 
  
  title = paste("ArrDelay vs.", x, 'with color by ArrDel15') 
  ggplot(dat, aes(x, 'ArrDelay')) + 
    geom_point(aes(color = factor(ArrDel15))) + 
    ggtitle(title) + 
    theme_bw() 
} 

## Define columns for making scatter plots 
plot.cols3 = c("DepDelay", 
               "CRSArrTime", 
               "CRSDepTime", 
               "DayofMonth", 
               "DayOfWeek", 
               "Month") 

lapply(plot.cols3, scatter.delay) 

# Sample operation 
#data.set = rbind(dat); 
#head(dat) 
# You'll see this output in the R Device port. 
# It'll have your stdout, stderr and PNG graphics device(s). 
#plot(data.set); 

# Select data.frame to be sent to the output Dataset port 
maml.mapOutputPort("dat"); 


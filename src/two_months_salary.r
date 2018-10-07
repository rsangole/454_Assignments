# two_months_salary.r

# Sample R program to begin work on the Two Month's Salary case study

# to run the entire source code program type:  source("two_months_salary.r")  

# read in data from comma-delimited text file
diamonds <- read.csv("two_months_salary.csv")

cat("\n","----- Initial Structure of diamonds data frame -----","\n")
# examine the structure of the initial data frame
print(str(diamonds))

# we can create a new channel factor called internet as a binary indicator   
# the ifelse() function is a good way to do this type of variable definition
diamonds$internet <- ifelse((diamonds$channel == "Internet"),2,1)
diamonds$internet <- factor(diamonds$internet,levels=c(1,2),labels=c("NO","YES"))

cat("\n","----- Checking the Definition of the internet factor -----","\n")
# check the definition of the internet factor
print(table(diamonds$channel,diamonds$internet))
 
# we might want to transform the response variable price using a log function
diamonds$logprice <- log(diamonds$price)

cat("\n","----- Revised Structure of diamonds data frame -----","\n")
# we check the structure of the diamonds data frame again
print(str(diamonds))
                                                                      
# install the lattice graphics package prior to using the library() function

library(lattice) # required for the xyplot() function

# let's prepare a graphical summary of the diamonds data
# we note that price and carat are numeric variables with a strong relationship
# also cut and channel are factor variables related to price
# showing the relationship between price and carat, while conditioning
# on cut and channel provides a convenient view of the diamonds data
# in addition, we jitter to show all points in the data frame

xyplot(jitter(price) ~ jitter(carat) | channel + cut, 
       data = diamonds,
        aspect = 1, 
        layout = c(3, 2),
        strip=function(...) strip.default(..., style=1),
        xlab = "Size or Weight of Diamond (carats)", 
        ylab = "Price")
        
# to output this plot as a pdf file we use the pdf() function and close with dev.off()

pdf("plot_diamonds_lattice.pdf",width=11,height=8.5)
xyplot(jitter(price) ~ jitter(carat) | channel + cut, 
       data = diamonds,
        aspect = 1, 
        layout = c(3, 2),
        strip=function(...) strip.default(..., style=1),
        xlab = "Size or Weight of Diamond (carats)", 
        ylab = "Price")
 dev.off()
 
# having looked at the data, we are ready to begin our modeling work
# we can use the lm() function for linear regression
# predicting price from combinations of explanatory variables
# we can explore variable transformations
 
# for a generalized linear model we might try using the glm() function 
# to predict channel, internet (YES or NO), from price or other variables
# for logistic regression, we would use glm() with family="binomial"

# the modeling methods we employ should make sense for the case study problem
 



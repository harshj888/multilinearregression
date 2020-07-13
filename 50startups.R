Startups <- read.csv("D:/STUDY/Excelr Assignment/Assignment 5 - Multi Linear Regression/50_Startups.csv")
View(Startups)
class(Startups)
library(plyr)
Startups$State <- revalue(Startups$State,
                          c("New York"="0", "California"="1", "Florida"="2")) 
Startups <- as.data.frame(Startups)
attach(Startups) # Basically to avoid reference of Data Set name(Startups) in this report.
Startups$State <- as.numeric(Startups$State)

sd(Profit)
sd(R.D.Spend)
sd(Administration)
sd(Marketing.Spend)
sd(State)

var(Profit)
var(R.D.Spend)
var(Administration)
var(Marketing.Spend)
var(State)

skewness(Profit)
skewness(R.D.Spend)
skewness(Administration)
skewness(Marketing.Spend)
skewness(Startups$State)

kurtosis(Profit)
kurtosis(R.D.Spend)
kurtosis(Administration)
kurtosis(Marketing.Spend)
kurtosis(Startups$State)

hist(Profit)
hist(R.D.Spend)
hist(Administration)
hist(Marketing.Spend)
hist(Startups$State)

summary(Startups)

plot(R.D.Spend, Profit)
plot(Administration, Profit)
plot(Marketing.Spend, Profit)
plot(State, Profit)
#Find the correlation between Output (Profit) & inputs (R.D Spend, Administration, Marketing, State) - SCATTER DIAGRAM
pairs(Startups)

cor(Startups)

Model.Startups <- lm(Profit~RD_Spend+Administration+Marketing_Spend+State)
summary(Model.Startups)

MLQ1 <- lm(Profit ~ R.D.Spend + Administration + Marketing.Spend)
summary(MLQ1)

### Scatter plot matrix with Correlations inserted in graph
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  
  if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
}
pairs(Startups, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")

MLQ1a <- lm(Profit ~ Administration+Marketing.Spend)
summary(MLQ1a)

influence.measures(MLQ1)

FINALmODEL50 <- lm(Profit~R.D.Spend+Marketing.Spend)

summary(FINALmODEL50)

plot(FINALmODEL50)

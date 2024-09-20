# install.packages("readxl")
library(readxl)

data <- read_excel("C:/R/coursework/EPPS6356/test1/_site/HPI_country_data.xlsx")

colnames(data)


gdp <- data$`GDP per capita ($)`
life_expectancy <- data$`Life Expectancy (years)`
population <- data$`Population (thousands)`
wellbeing <- data$`Ladder of life (Wellbeing) (0-10)`
hpi <- data$HPI


x <- gdp[1:50] 
y1 <- wellbeing[1:50]
y2 <- life_expectancy[1:50]


par(mfrow = c(3, 2))

par(las=1, mar=c(4, 4, 2, 4), cex=.7)
plot.new()
plot.window(range(x, na.rm = TRUE), range(c(y1, y2), na.rm = TRUE))
lines(x, y1, col="blue")
lines(x, y2, col="red")  
points(x, y1, pch=16, cex=2, col="blue")  
points(x, y2, pch=21, bg="white", cex=2, col="red") 
par(col="gray50", fg="gray50", col.axis="gray50")
axis(1, at=seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out=5))  
axis(2, at=seq(min(y1, na.rm = TRUE), max(y1, na.rm = TRUE), length.out=5))  
axis(4, at=seq(min(y2, na.rm = TRUE), max(y2, na.rm = TRUE), length.out=5))  
box(bty="u")  
mtext("GDP per capita", side=1, line=2, cex=0.8)  
mtext("Wellbeing", side=2, line=2, las=0, cex=0.8)  
mtext("Life Expectancy", side=4, line=2, las=0, cex=0.8)  
text(gdp[5], wellbeing[5], data$Country[5], col="blue")  
par(mar=c(5.1, 4.1, 4.1, 2.1), col="black", fg="black", col.axis="black")


gdp_clean <- gdp[!is.na(gdp)]
par(mar=c(4.5, 4.1, 3.1, 0))
hist(gdp_clean, breaks=20, col="gray80", freq=FALSE)  
lines(density(gdp_clean, na.rm=TRUE), lwd=2)
par(mar=c(5.1, 4.1, 4.1, 2.1))

par(mar=c(2, 3.1, 2, 2.1)) 
barplot(gdp[1:50], names.arg = data$Country[1:50], las=2, col=gray(seq(0.1, 0.5, length=50)), main="GDP of Countries")
par(mar=c(5.1, 4.1, 4.1, 2.1))  


par(mar=c(3, 4.1, 2, 0))
boxplot(gdp ~ population, data = data[1:50, ], col="white", main="Boxplot of GDP by Population")
mtext("Population", side=1, line=2.5, cex=0.8)


x <- gdp[1:50]
y <- life_expectancy[1:50]
z <- wellbeing[1:50]
par(mar=c(0, 0.5, 0, 0), lwd=0.5)
persp(x, y, outer(x, y, function(x, y) x * y / max(x, na.rm=TRUE)), theta = 30, phi = 30, expand = 0.5)
par(mar=c(5.1, 4.1, 4.1, 2.1), lwd=1)

par(mar=c(0, 2, 1, 2), xpd=FALSE, cex=0.5)
population_data <- population[1:50]
names(population_data) <- data$Country[1:50]
pie(population_data, col = gray(seq(0.3, 1.0, length=50)), main="Population Distribution of Countries")


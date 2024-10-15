
library(readxl)

file_path <- "C:/R/coursework/EPPS6356/test1/_site/unemployment rate annual.xlsx"
df <- read_excel(file_path)


data_2022 <- df$`2022`

#histogram
jpeg("unemployment_rate_histogram_2022.jpg")
hist(data_2022, breaks=10, main="Unemployment Rate Histogram for 2022", 
     xlab="Unemployment Rate", col="blue", border="black")
dev.off()


data_2021 <- df$`2021`
industry_names <- df$Industry

#Vertical_barchart

pdf("unemployment_rate_Vertical_barchart_2021.pdf", width = 8, height = 6)
barplot(height = data_2021, names.arg = industry_names, las=2, col="lightblue", 
        main="Unemployment Rate by Industry in 2021", xlab="Industry", ylab="Unemployment Rate", 
        cex.names=0.7, border="black", horiz=FALSE)
dev.off() 

#horizontal_barchart
svg("unemployment_rate_horizontal_barchart_2022.svg", width = 8, height = 6)
barplot(height = data_2022, names.arg = industry_names, las=2, col="lightgreen", 
        main="Unemployment Rate by Industry in 2022", xlab="Unemployment Rate", ylab="Industry", 
        cex.names=0.7, border="black", horiz=TRUE)
dev.off() 

#Piechart
jpeg("unemployment_rate_piechart_2021.jpg", width = 800, height = 600)
pie(data_2021, labels = industry_names, main="Unemployment Rate by Industry in 2021", 
    col=rainbow(length(data_2021)), cex=0.8)
dev.off()

#Boxplot
jpeg("unemployment_rate_boxplot_2022.jpg", width = 800, height = 600)
boxplot(data_2022, main="Boxplot of Unemployment Rate in 2022", 
        ylab="Unemployment Rate", col="lightblue", border="black")
dev.off()


#scatterplot
jpeg("unemployment_rate_scatterplot_2022.jpg", width = 800, height = 600)
plot(data_2022, type="p", pch=16, col="blue", 
     main="Scatterplot of Unemployment Rate by Industry in 2022",
     xlab="Industry Index", ylab="Unemployment Rate")

text(x=1:length(data_2022), y=data_2022, labels=industry_names, pos=4, cex=0.7)
dev.off()


library(ggplot2)


#Histogram ggplot2
jpeg("unemployment_rate_histogram_ggplot2_2022.jpg", width = 800, height = 600)
ggplot(df, aes(x = data_2022)) +
  geom_histogram(binwidth = 1, fill="blue", color="black", alpha=0.7) +
  labs(title = "Unemployment Rate Histogram for 2022",
       x = "Unemployment Rate", y = "Frequency") +
  theme_minimal()
dev.off()


#horizontal_barchart ggplot2
jpeg("unemployment_rate_horizontal_barchart_ggplot2_2021.jpg", width = 800, height = 600)


p <- ggplot(df, aes(x = reorder(industry_names, data_2021), y = data_2021)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
  labs(title = "Unemployment Rate by Industry in 2021", x = "Industry", y = "Unemployment Rate") +
  coord_flip() +  
  theme_minimal()

print(p)  
dev.off() 

#Vertical_barchart ggplot2
bmp("unemployment_rate_barchart_ggplot2_2022.bmp", width = 800, height = 600, units = "px", res = 300)

p <- ggplot(df, aes(x = reorder(industry_names, data_2022), y = data_2022)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(title = "Unemployment Rate by Industry in 2022", x = "Industry", y = "Unemployment Rate") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  theme_minimal()

print(p)  

dev.off()

#Piechart ggplot2
pie_data <- data.frame(industry_names, data_2021)


jpeg("unemployment_rate_piechart_ggplot2_2021.jpg", width = 800, height = 600)


p <- ggplot(pie_data, aes(x = "", y = data_2021, fill = industry_names)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +  
  labs(title = "Unemployment Rate by Industry in 2021") +
  theme_void() +  
  theme(legend.position = "right")  


print(p)

dev.off() 

#Boxplot ggplot2
boxplot_data <- data.frame(Unemployment_Rate = data_2022)

jpeg("unemployment_rate_boxplot_ggplot2_2022.jpg", width = 800, height = 600)

p <- ggplot(boxplot_data, aes(y = Unemployment_Rate)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of Unemployment Rate in 2022", y = "Unemployment Rate") +
  theme_minimal()
print(p)
dev.off()

#Scatterplot ggplot2
scatter_data <- data.frame(Industry = industry_names, Unemployment_Rate = data_2022)


jpeg("unemployment_rate_scatterplot_ggplot2_2022.jpg", width = 800, height = 600)

p <- ggplot(scatter_data, aes(x = Industry, y = Unemployment_Rate)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Scatterplot of Unemployment Rate by Industry in 2022", 
       x = "Industry", y = "Unemployment Rate") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  theme_minimal()

print(p)
dev.off()
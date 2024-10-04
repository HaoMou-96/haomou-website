# install.packages("readxl")
library(readxl)

#Bar chart 
data <- read_excel("C:/R/coursework/EPPS6356/test1/_site/HPI_country_data.xlsx")

gdp_data <- na.omit(data[, c("Country", "GDP per capita ($)")])
gdp_data <- head(gdp_data, 10)


ggplot(gdp_data, aes(x = reorder(Country, `GDP per capita ($)`), y = `GDP per capita ($)`)) +
  geom_bar(stat = "identity", fill = "skyblue") + 
  coord_flip() + 
  labs(title = "Top 10 Countries by GDP per Capita", 
       x = "Country", 
       y = "GDP per Capita ($)") +
  theme_minimal()


#Column chart
library(ggplot2)


gdp_data <- na.omit(data[, c("Country", "GDP per capita ($)")])
gdp_data <- head(gdp_data, 10)


ggplot(gdp_data, aes(x = Country, y = `GDP per capita ($)`)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Column Chart: GDP per Capita for 10 Selected Countries", 
       x = "Country", 
       y = "GDP per Capita ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

#Variable Width Column Chart
gdp_pop_data <- na.omit(data[, c("Country", "GDP per capita ($)", "Population (thousands)")])
gdp_pop_data <- head(gdp_pop_data, 10)


ggplot(gdp_pop_data, aes(x = Country, y = `GDP per capita ($)`, width = `Population (thousands)` / 10000)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Variable Width Column Chart: GDP and Population", 
       x = "Country", 
       y = "GDP per Capita ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


#Table or Table with Embedded Charts

library(tidyr)
gdp_pop_long <- gather(gdp_pop_data, key = "Variable", value = "Value", -Country)


ggplot(gdp_pop_long, aes(x = Country, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(. ~ Variable) +  
  labs(title = "Table with Embedded Charts: GDP and Population", 
       x = "Country", 
       y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

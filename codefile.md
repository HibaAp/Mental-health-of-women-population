
project code

---
output:
  word_document: default
  html_document: default
  pdf_document: default
---
# US MACRO ECONOMIC DATA
## Name: HIBA AP
## Roll no:MDS202326

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

```

# INDRODUCTION

The dataset provides information on US Economic conditions from 2002-2022 . We analyze the change in different Economic Factors like GDP,INFLATON,PER CAPITA INCOME etc over Time. Various inferences are drawn and the results are visualized using R.

# DATA SET DESCRIPTION

This data set contains month-wise details about all the macro-economic factors of US over two decades from 05/2002 to 05/2022.
https://www.kaggle.com/datasets/sagarvarandekar/macroeconomic-factors-affecting-us-housing-prices

Each columns in the data set are described below:


**UNRATE%** :(Unemployment Rate) The unemployment rate calculates the percentage of the labor force that is without a job and actively seeking employment. It is a critical indicator of labor market conditions.


**CONSUMER CONF INDEX**:The Consumer Confidence Index (CCI) is an economic indicator that measures the level of confidence and optimism among consumers about the state of the economy.


**PPI-CONST MAT**:The "PPI-CONST MAT" refers to the Producer Price Index for Construction Materials. It is an economic indicator that measures the average change in the selling prices received by producers of construction materials over time.


**CPIALLITEMS**:The term "CPIALLITEMS" refers to the Consumer Price Index for All Urban Consumers, All Items. It is a widely used economic indicator that measures the average change in the prices paid by urban consumers for a market basket of goods and services over time.


**INFLATION(%)**:The inflation rate measures the percentage change in the general price level of goods and services over time.


**MORTGAGE INT. MONTHLY AVG(%)**:The term "MORTGAGE INT. MONTHLY AVG(%)" typically refers to the average monthly interest rate on mortgages, expressed as a percentage. This metric is important in the context of real estate and personal finance. 


**MED HOUSEHOLD INCOME**: It is a statistical measure that represents the middle point of all household incomes in a given area or population. Median household income is often used as an indicator of the economic well-being of a particular group or geographic region. 


**CORP. BOND YIELD(%)**:Corporate Bond Yield represents the annual interest rate or yield that investors earn from investing in corporate bonds. 


**MONTHLY HOME SUPPLY**: represents the percentage of available housing units on the market in a given month compared to the total housing inventory.


**% SHARE OF WORKING POPULATION**:The Share of Working Population refers to the proportion of the total population that is actively engaged in the labor force, either employed or actively seeking employment.


**GDP PER CAPITA**:GDP per Capita (Gross Domestic Product per Capita) is a measure of a country's economic output or Gross Domestic Product (GDP) divided by its population.


**QUARTERLY REAL GDP**:Quarterly Real GDP (Gross Domestic Product) refers to the total monetary value of all goods and services produced within a country's borders during a specific quarter of the year, adjusted for inflation.

 
**QUARTERLY GDP GROWTH RATE (%)**:The Quarterly GDP Growth Rate (%) represents the percentage change in real GDP between two consecutive quarters or from the same quarter in the previous year.

 
**CSUSHPISA** :It measures changes in the prices of residential properties, tracking the average price movement of single-family homes in the United States.

```{r,include=FALSE}
library(readxl)
DATA <- read_excel("C:/Users/Admin/Desktop/cmi/Project/DATA.xlsm") 
summary(DATA)
```


```{r,include=FALSE}
#estimating missing values usin linear interpolation

x=DATA$DATE[18:241]
y=DATA$`MED HOUSEHOLD INCOME`[18:241]
hist(y)
plot(x,y,pch=16,col='red',xlab = 'dates',ylab = 'med household income',main='Household Income Over Time')
z=18:241
model=lm(y~z)
plot(z,y,pch=16,col='blue',xlab='time',ylab='med hosehold income')
abline(model,col='green')
summary(model)
df =data.frame(z,y)

linearModel=lm(y~z, data = df)

missing_values =c()

# Loop through values of k
for (k in 1:17) {
  pred1 =predict(linearModel, data.frame(z = k))
  missing_values = c(missing_values, pred1)
}

# Print the missing_values vector
print(missing_values)
y=c(missing_values,y)
y



```




```{r,include=FALSE}
avl=DATA$DATE[6:241]
perinc=DATA$`% SHARE OF WORKING POPULATION`[6:241]
hist(perinc)
plot(avl,perinc,pch=16,col='red',xlab = 'dates',ylab = '%share of working people',main='% Share Of Working People Over Time')
ind=6:241
model1=lm(perinc~ind)
plot(ind,perinc,pch=16,col='blue',xlab='time',ylab='%share of workinf people')
abline(model1,col='green')
summary(model1)
df1 =data.frame(ind,perinc)

linearModel1=lm(perinc~ind, data = df1)
missing_values1 =c()

# Loop through values of k
for (k in 1:5) {
  pred1 =predict(linearModel1, data.frame(ind=k))
  missing_values1 = c(missing_values1, pred1)
}
print(missing_values1)
# Print the missing_values vector
print(missing_values)
perinc=c(missing_values1,perinc)
perinc
timeind=1:241


```


I've Estimated the  missing values using Linear Interpolation and replaced the Dates with some time index (from 1 to 241).


```{r}
crctd_data <- data.frame(
  UNRATE = DATA$`UNRATE(%)`,
  CONSUMER_CONF_INDEX = DATA$`CONSUMER CONF INDEX`,
  PPI_CONST_MAT = DATA$`PPI-CONST MAT.`,
  CPI_ALL_ITEMS = DATA$CPIALLITEMS,
  INFLATION = DATA$`INFLATION(%)`,
  MORTGAGE_INT_MONTHLY_AVG = DATA$`MORTGAGE INT. MONTHLY AVG(%)`,
  MED_HOUSEHOLD_INCOME = y,
  CORP_BOND_YIELD = DATA$`CORP. BOND YIELD(%)`,
  MONTHLY_HOME_SUPPLY = DATA$`MONTHLY HOME SUPPLY`,
  GDP_PER_CAPITA = DATA$`GDP PER CAPITA`,
  QUARTERLY_REAL_GDP = DATA$`QUARTERLY REAL GDP`,
  GDP_GROWTH_RATE = DATA$`QUARTERLY GDP GROWTH RATE (%)`,
  CSUSHPISA = DATA$CSUSHPISA,
  perinc = perinc
)


```

# Graphical Presentation Of Key Variables
```{r,echo=FALSE}
library(corrplot)

cor_matrix <- cor(crctd_data)
corrplot(
  cor_matrix, 
  method = "color",  
  type = 'full',    
  tl.col = "black",  
  tl.srt = 45,       
  tl.cex = 0.8,      
  diag = TRUE,      
  addCoef.col = "black", 
  order = "hclust",  
  hclust.method = "complete", 
  number.cex = 0.7, 
  tl.offset = 0.4    
)
```

```{r,echo=FALSE}
# Load the ggplot2 library if not already loaded
library(ggplot2)
data <- data.frame(Date = as.Date(DATA$DATE),
  GDP = crctd_data$GDP_PER_CAPITA)
# Example data frame with dates (as a Date class) and GDP values
# Replace this with your actual data


# Create a time series plot of GDP using ggplot2 with a custom line color
ggplot(data, aes(x = Date, y = GDP)) +
  geom_line(color = "red") +  # Change color to blue
  labs(x = "Date", y = "GDP per capita", title = "GDP per capita Over Time")


```
```{r,echo=FALSE}
library(ggplot2)

# Create a data frame with time series data
data = data.frame(Time = DATA$DATE, TimeSeries2 = crctd_data$GDP_PER_CAPITA,TimeSeries3=crctd_data$MED_HOUSEHOLD_INCOME)

# Create a plot with ggplot2
ggplot(data, aes(x = Time)) +
  geom_line(aes( y=TimeSeries2, color = "GDP per capita")) +
  geom_line(aes(y=TimeSeries3, color = "Med Household Income "))
  labs(x = "Time", y = "Value") +
  scale_color_manual(values = c("Time Series 2" = "red","Time Series3"="green")) 

```
```{r,echo=FALSE}
# Load the ggplot2 package
library(ggplot2)

# Sample data frame (replace this with your data)
data <- data.frame(
  Year = DATA$DATE,
  Inflation = crctd_data$INFLATION,
  GDP = crctd_data$GDP_PER_CAPITA
)

# Create a scatterplot of Inflation vs. GDP
ggplot(data, aes(x = GDP, y = Inflation)) +
  geom_point() +
  labs(
    x = "GDP",
    y = "Inflation",
    title = "Inflation vs. GDP"
  ) +
  theme_minimal()


```

```{r,echo=FALSE}
# Create a box plot for the UNRATE variable
boxplot(crctd_data$UNRATE, 
        main = "Box Plot of Unemployment Rate (UNRATE)",
        ylab = "Unemployment Rate",
        col = "skyblue",
        border = "black",
        horizontal = TRUE)

```



# CONCLUSION



**.**  When Correlation of variables is plotted,it is seen that many variables are highly correlated.GDP per capita is in correlation with Med Household income and negatively correlate with Unemployment rate.

**.**  Surprisingly,the rate of Inflation is not highly correlated with most of the variables.

**.**  Even though there are some contractions,GDP of US is increasing over time.

**.**  Median Household Income and GDP per capita are highly correlated.

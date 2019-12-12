---
title: "DATA 606 Data Project Proposal"
author: "Dhairav Chhatbar"
---

```{r}
library(ggplot2)
library(kableExtra)
library(scales)
library(dplyr)
```


### Data Preparation

```{r setup, echo=TRUE, results='hide', warning=FALSE, message=FALSE}
# load data
Field <- c("year",	"month",	"carrier",	"carrier_name",	"airport",	"airport_name",	"arr_flights",	"arr_del15",	"carrier_ct",	"weather_ct",	"nas_ct",	"security_ct",	"late_aircraft_ct",	"arr_cancelled",	"arr_diverted",	"arr_delay",	"carrier_delay",	"weather_delay",	"nas_delay",	"security_delay",	"late_aircraft_delay")

Description <- c("Year (yyyy)",	"Month (mm)",	"Airline carrier abbreviation",	"Airline carrier name",	"Airport Code",	"Airport Name",	"Total number of arriving flights in the observation",	"Total number of delayed flights in the observation",	"Number of flights delayed due to air carrier (subset of arr_del15)",	"Number of flights delayed due to weather (subset of arr_del15)",	"Number of flights delayed due to National Aviation System (subset of arr_del15)",	"Number of flights delayed due to airport security (subset of arr_del15)",	"Number of flights delayed due to a previous flight using the same aircraft being late",	"Number of cancelled flights",	"Number of flights diverted",	"Arrival delay in minutes",	"Carrier delay in minutes (subset of arr_delay)",	"Weather delayed in minutes (subset of arr_delay)",	"National Aviation System in minutes (subset of arr_delay)",	"Security delay in minutes (subset of arr_delay)",	"Aircraft delay in minutes (subset of arr_delay)")

VariableType <- c("Qualitative",	"Qualitative",	"Qualitative",	"Qualitative",	"Qualitative",	"Qualitative",	"Quantitative",	"Quantitative",	"Quantitative",	"Quantitative",	"Quantitative",	"Quantitative",	"Quantitative",	"Quantitative",	"Quantitative",	"Quantitative",	"Quantitative",	"Quantitative",	"Quantitative",	"Quantitative",	"Quantitative") 

VariableMeasure <- c("Independent",	"Independent",	"Independent",	"Independent",	"Independent",	"Independent",	"Independent",	"Response",	"Explanatory",	"Explanatory",	"Explanatory",	"Explanatory",	"Explanatory",	"Independent",	"Independent",	"Response",	"Explanatory",	"Explanatory",	"Explanatory",	"Explanatory",	"Explanatory")

FieldDefinitions <- data.frame(Field, VariableType, VariableMeasure, Description)
  

#Load Raw Data
flights_raw <- read.csv("https://raw.githubusercontent.com/dhairavc/DATA606/master/flights_delays.csv")
flights_raw <- flights_raw[, 1:21]
names(flights_raw) <- c("year","month","carrier","carrier_name","airport","airport_name","arr_flights","arr_del15","carrier_ct","weather_ct","nas_ct","security_ct","late_aircraft_ct","arr_cancelled","arr_diverted","arr_delay","carrier_delay","weather_delay","nas_delay","security_delay","late_aircraft_delay")

```


### Research question 

**You should phrase your research question in a way that matches up with the scope of inference your dataset allows for.**  
The arrival delay data provide for some interesting questions:  
1. What is the biggest contribution of flight delays?  
2. Which holiday month (July, November, or December) is the worst for travel?  
3. What is the most efficient airport?  
4. Are airports generally getting more efficent over time?  
5. What is the most efficient airline?  
6. Which airline has degraded performance over time? Which has improved over time?  



### Cases 

**What are the cases, and how many are there?**  
Each case is a an airline by month, year, and airport with some data points around each.  
n=68,153


### Data collection 

**Describe the method of data collection.**  
The Bureau of Transportation Statistics provides a CSV of the raw data for download.  
This CSV will be uploaded to GitHub for analysis in R


### Type of study 

**What type of study is this (observational/experiment)?**  
This is an observational study. The creators of the data set, observed flight arrivals and noted down the total number of flights and some datapoints on the flights that arrived late


### Data Source 

**If you collected the data, state self-collected. If not, provide a citation/link.**  
BUREAU OF TRANSPORTATION STATISTICS  
https://www.transtats.bts.gov/OT_Delay/OT_DelayCause1.asp?pn=1  


### Dependent Variable

**What is the response variable? Is it quantitative or qualitative?**  
```{r, message=FALSE}
library(dplyr)
library(kableExtra)

FieldDefinitions %>% filter(VariableMeasure == "Response") %>% kable() %>% kable_styling()
```



### Independent Variable

**You should have two independent variables, one quantitative and one qualitative.**

```{r}
FieldDefinitions %>% filter(VariableMeasure == "Independent") %>% kable() %>% kable_styling()
```

### Relevant summary statistics 

**Provide summary statistics for each the variables. Also include appropriate visualizations related to your research question (e.g. scatter plot, boxplots, etc). This step requires the use of R, hence a code chunk is provided below. Insert more code chunks as needed.**

```{r, message=FALSE}
library(dplyr)
library(ggplot2)
library(tidyr)

#summary statistics for all quantitative variables 

flights_raw %>% select(arr_flights, arr_del15, carrier_ct, weather_ct, nas_ct, security_ct, late_aircraft_ct, arr_cancelled, arr_diverted, arr_delay, carrier_delay, weather_delay, nas_delay, security_delay, late_aircraft_delay) %>% summary()


#Barchart of arriving and delayed flights
flights_raw %>% select(year, arr_flights, arr_del15) %>% drop_na() %>%group_by(year) %>% summarize(ArrivingFlights=sum(arr_flights), DelayedFlights = sum(arr_del15)) %>% gather(key = "Type", "NumCount", 2:3) %>% ggplot( aes(x=year, y=NumCount, fill=Type)) + geom_col(position = 'dodge') 

#Barchart of volume per airport
flights_raw %>% select(airport, arr_flights) %>% drop_na() %>% group_by(airport) %>% summarise(TotalFlights = sum(arr_flights)) %>% ggplot( aes(x=airport, y=TotalFlights, fill=TotalFlights)) + geom_col() + coord_flip()

```
# P

## Intro
There are thousands of flights that fly within the inter-continental United States on a daily bais. Of these thousands of daily flights many are delayed, and it is these delays that cost the economy substantial finanical loss. It would be of interest to understand if there is a pattern to the delays, then it would be of interest to air line carriers, airports, city/state/federal government entities, business and individal consumers to understand these patterns to either take the appropriate steps to further address them or for individual consumers take the necessary steps to avoid travel during such patterns. Some questions of interest would be:  
1. What is the biggest contribution of flight delays?  
2. Which holiday month (July, November, or December) is the worst for travel?  
3. What is the most efficient airport?  
4. Are airports generally getting more efficent over time?  
5. What is the most efficient airline?  
6. Which airline has degraded performance over time? Which has improved over time?  

## Data
Data collection: Describe how the data were collected.
The data was collected via the Bureau of Transportation Statistics and loaded to R via CSV loaded into GitHub:
https://www.transtats.bts.gov/OT_Delay/OT_DelayCause1.asp?pn=1

```{r}
#Load Raw Data
#flights_raw <- read.csv("https://raw.githubusercontent.com/dhairavc/DATA606/master/flights_delays.csv")
flights_raw <- read.csv("https://drive.google.com/file/d/1wd1DL0777qKFEN2c-rKgYJdXuiqCZZt8/view?usp=sharing")
flights_raw <- flights_raw[, 1:21]
names(flights_raw) <- c("year","month","carrier","carrier_name","airport","airport_name","arr_flights","arr_del15","carrier_ct","weather_ct","nas_ct","security_ct","late_aircraft_ct","arr_cancelled","arr_diverted","arr_delay","carrier_delay","weather_delay","nas_delay","security_delay","late_aircraft_delay")
```


Cases: What are the cases? (Remember: case = units of observation or units of experiment)
```{r}
nrow(flights_raw)
```



Variables: What are the two variables you will be studying? State the type of each variable.

Type of study: Observational study of flight arrival times 

Scope of inference - generalizability: Also discuss any potential sources of bias that might prevent generalizability.
The population of insterest is all late flights in the inter-contienental United States. From the subset of flight arrival times that we get from DOT, 
we would like to make an inference on the entire population of flights. 



Scope of inference - causality: Can these data be used to establish causal links between the variables of interest? Explain why or why not.

## Exploratory Analysis
Exploratory data analysis: Perform relevant descriptive statistics, including summary statistics and visualization of the data. Also address what the exploratory data analysis suggests about your research question.


```{r, warning=FALSE}
#Barchart of arriving and delayed flights
flights_raw %>% select(year, arr_flights, arr_del15) %>% drop_na() %>%group_by(year) %>% summarize(ArrivingFlights=sum(arr_flights), DelayedFlights = sum(arr_del15)) %>% gather(key = "Type", "NumCount", 2:3) %>% ggplot( aes(x=year, y=NumCount, fill=Type)) + geom_col(position = 'dodge') + scale_y_continuous(labels = comma)

#Barchart of volume per airport
flights_raw %>% select(airport, arr_flights) %>% drop_na() %>% group_by(airport) %>% summarise(TotalFlights = sum(arr_flights)) %>% ggplot( aes(x=reorder(airport, -TotalFlights), y=TotalFlights, fill=TotalFlights)) + geom_col() + coord_flip() + scale_y_continuous(labels = comma) + xlab("Airport") +  scale_colour_continuous(labels = comma)

#Flight Delays by Month
by_month <- flights_raw %>% select(month, arr_del15) %>% drop_na() %>% group_by(month) %>% summarize(delayed = n()) 
by_month$month <- recode(by_month$month, `1`="Jan", `2`="Feb", `3`="Mar", `4`="Apr", `5`="May", `6`="Jun", `7`="Jul", `8`="Aug", `9`="Sep", `10`="Oct", `11`="Nov", `12`="Dec")
by_month %>% ggplot( aes(x = month, y=delayed)) + geom_bar(stat="identity") + scale_x_discrete(limits=c("Jan", "Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

#Most Delayed Carrier per Airport by Percentage
flights_raw %>% select(carrier, airport, arr_del15, arr_flights) %>% drop_na() %>% group_by(airport, carrier) %>% summarize_all(funs(sum)) %>% mutate(del_pct = arr_del15/arr_flights) %>%
  ggplot(aes(x=airport, y=carrier, fill=del_pct)) + geom_tile() + theme(axis.text.x=element_text(angle=90, hjust=1)) + scale_fill_gradient(low = "pink", high = "red")

#Delayed Reasons by Year
del_by_year <- flights_raw %>% select(year, carrier_ct, weather_ct, nas_ct, security_ct, late_aircraft_ct) %>% drop_na() %>% group_by(year) %>% summarise_all(funs(sum)) %>% gather(key = del_reason, value = del_count, carrier_ct:late_aircraft_ct) 
ggplot(del_by_year, aes(x=del_by_year$year, y=del_by_year$del_count, group=del_by_year$del_reason, color = del_by_year$del_reason)) + geom_line(size=1) + geom_point() + xlab("Year") + ylab("Count")  + scale_y_continuous(labels = comma)


x <- flights_raw %>% select(carrier, airport, arr_flights) %>% group_by(carrier,airport) %>% summarise(flights_arr = n()) 
hist(x$flights_arr, breaks = 10)
```



## Inference
If your data fails some conditions and you can’t use a theoretical method, then you should use simulation. If you can use both methods, then you should use both methods. It is your responsibility to figure out the appropriate methodology

Check conditions
Theoretical inference (if possible) - hypothesis test and confidence interval
Simulation based inference - hypothesis test and confidence interval
Brief description of methodology that reflects your conceptual understanding


## Conclusion
Write a brief summary of your findings without repeating your statements from earlier. Also include a discussion of what you have learned about your research question and the data you collected. You may also want to include ideas for possible future research.

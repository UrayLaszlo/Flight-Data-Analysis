# Load libraries
library(ggplot2)
library(knitr)
library(dplyr)
library(tidyverse)
install.packages("plotly")
library(plotly)

# Load datasets
first_year <- read.csv('2006.csv')
second_year <- read.csv('2007.csv')


delay_week <- first_year[,c('ArrDelay', 'DepDelay', 'CarrierDelay', 'WeatherDelay', 'NASDelay', 'SecurityDelay', 'LateAircraftDelay')]
total_delay_week <- delay_week %>% group_by('DayOfWeek') %>%
  summarise('DayOfWeek', sum)

# Question 1, part 1
ggplot(first_year, aes(x = CRSDepTime, y=ArrDelay))  + geom_line() + xlab('Time of day')+ggtitle( "Delay per time of day" ) +ylab('Delay') 

# Question 1, part 2
ggplot(first_year, aes(x = DayOfWeek, y=ArrDelay))  + geom_line() + xlab('Day of week')+ggtitle( "Delay per week" ) +ylab('Delay') 

# Question 1, part 3
ggplot(first_year, aes(x = Month, y=CarrierDelay))  + geom_line() + xlab('Month')+ggtitle( "Delay per month" ) +ylab('Delay') 

# Question 2
ggplot(second_year, aes(x = TailNum, y=CarrierDelay))  + geom_line() + xlab('Age of plane')+ggtitle( "Delay per plane age" ) +ylab('Delay') 

# Question 3
# Create quarters to plot
q1 <- second_year %>% filter(Month < 4)
q1 <- table(q1['Dest'])
q1 <- data.frame(rbind(q1))
q1_top <- q1 %>% 
  gather(key = "key", value = "value") %>%
  top_n(10, wt = value) %>%
  arrange(desc(value))

q2 <- second_year %>% filter(Month > 3 & Month <= 6)
q2 <- table(q2['Dest'])
q2 <- data.frame(rbind(q2))
q2_top <- q2 %>% 
  gather(key = "key", value = "value") %>%
  top_n(10, wt = value) %>%
  arrange(desc(value))

q3 <- second_year %>% filter(Month > 6 & Month <= 9)
q3 <- table(q3['Dest'])
q3 <- data.frame(rbind(q3))
q3_top <- q3 %>% 
  gather(key = "key", value = "value") %>%
  top_n(10, wt = value) %>%
  arrange(desc(value))

q4 <- second_year %>% filter(Month > 9)
q4 <- table(q4['Dest'])
q4 <- data.frame(rbind(q4))
q4_top <- q4 %>% 
  gather(key = "key", value = "value") %>%
  top_n(10, wt = value) %>%
  arrange(desc(value))

# Plot different quarters together on a subplot
fig1 <- plot_ly(q1_top, type="scatter",mode = 'lines+markers',
                marker = list(line = list(width = 3)))
fig2 <- plot_ly(q2_top, type="scatter",mode = 'lines+markers',
                marker = list(line = list(width = 3)))
fig3 <- plot_ly(q3_top, type="scatter",mode = 'lines+markers',
                marker = list(line = list(width = 3)))
fig4 <- plot_ly(q4_top, type="scatter",mode = 'lines+markers',
                marker = list(line = list(width = 3)))

fig <- subplot(fig1, fig2, fig3, fig4, nrows = 2)%>% 
  layout(title = 'Number of flights between different locations')

# Question 4
delay_from <- first_year %>% group_by(Origin) %>%
  summarise(ArrDelay=sum(ArrDelay, na.rm = T),
            DepDelay=sum(DepDelay, na.rm = T),
            CarrierDelay=sum(CarrierDelay, na.rm = T),
            WeatherDelay=sum(WeatherDelay, na.rm = T),
            NASDelay=sum(NASDelay, na.rm = T),
            SecurityDelay=sum(SecurityDelay, na.rm = T),
            LateAircraftDelay=sum(LateAircraftDelay, na.rm = T)
            )
total_delay_from <- delay_from



# Question 5

library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3tuning)

num_row <- nrow(first_year)
train_set <- sample(num_row, round(0.5*num_row))
test_set <- setdiff(1:num_row, train_set)

first_year <- drop_na(first_year)

first_year$target <- as.numeric(first_year$ArrDelay > 0)

task <- TaskClassif$new('first_year', backend=first_year, target='target')
task$select(c('ActualElapsedTime', 'DayOfWeek'))
measure <- mlr_measures$get("classif.ce")




























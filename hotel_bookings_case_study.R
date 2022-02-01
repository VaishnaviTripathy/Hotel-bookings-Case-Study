#installing and loading required packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("skimr")
install.packages("janitor")


library(tidyverse)
library(lubridate)
library(skimr)
library(janitor)
library(ggplot2)
library(tidyr)

# checking the current directory
getwd() 

#importing the data set
bookings <- read.csv("hotel_bookings.csv",header = TRUE, sep=",")

#checking column names and checking the data types
colnames(bookings)
str(bookings)
skim_without_charts(bookings)

#rename column names to make them look more consistent 
data <- bookings
data<-rename(data,hotel_type=hotel,
       canceled_status=is_canceled,
       arrival_year=arrival_date_year,
       arrival_month=arrival_date_month,
       arrival_date=arrival_date_day_of_month)
rename_with(data,tolower)

#wrangling data
data<-unite(data,col='arrival_date_', c('arrival_date', 'arrival_month', 'arrival_year'),sep=" ")

data['reservation_status'][data['reservation_status'] == 'No-Show'] <- 'Canceled'
data['children'][data['children'] == 'NA'] <- 0

data<- mutate(data,num_of_childs = (children+babies))


# checking if we successfully replaced the no-show to canceled
filter(data,reservation_status == 'No-Show')
filter(data,children== 'NA')


#checking out the average daily rate for the two different types of hotel
city_hotels <-
  filter(data, hotel_type == 'City Hotel') %>%
  pull('adr')
min(city_hotels)
max(city_hotels)
mean(city_hotels)

resort_hotels <-
  filter(data, hotel_type == 'Resort Hotel') %>%
  pull('adr')
min(resort_hotels)
max(resort_hotels)
mean(resort_hotels)

#people travelling in whim


data %>%
  select(hotel_type,lead_time,country)%>%
  filter(lead_time<1)%>%
  arrange(country)


data %>%
  select(hotel_type,lead_time,country)%>%
  filter(lead_time<1)%>%
  group_by(country,hotel_type)%>%
  tally()%>%
  arrange(country)

data %>%
  select(hotel_type,lead_time,country)%>%
  filter(lead_time<1)%>%
  group_by(hotel_type)%>%
  tally()
 
#num of bookings in all these year for the revenue generated
bookings %>%
  filter(is_canceled==0)%>%
  group_by(arrival_date_year,hotel)%>%
  tally()

ggplot(bookings)+
  geom_bar(mapping=aes(x=arrival_date_year,fill=is_canceled))+
  facet_wrap(~is_canceled)


#bookings involving families with children

data %>%
  select(hotel_type,num_of_childs)%>%
  filter(num_of_childs>=1)%>%
  group_by(hotel_type)%>%
  tally()


#focusing on early bookings
data %>%
  select(hotel_type,lead_time,adults,num_of_childs)%>%
  filter(lead_time>=1,num_of_childs>=1)%>%
  group_by(hotel_type)%>%
  tally()

ggplot(data) +
  geom_jitter(mapping = aes(x = lead_time, y = num_of_childs,color=num_of_childs,size=num_of_childs))+
  labs(title='Customer with early bookings')
  
#weekend stays
data %>%
  select(hotel_type,stays_in_weekend_nights,adults,num_of_childs)%>%
  filter(stays_in_weekend_nights>=1) %>%
  arrange(num_of_childs)%>%
  group_by(num_of_childs,hotel_type)%>%
  tally()

ggplot(data) +
  geom_jitter(mapping = aes(x =stays_in_weekend_nights , y =num_of_childs,colour=stays_in_weekend_nights))+
  labs(title='number of kids Vs stays at weekend nights')

#weekday nights
data %>%
  select(hotel_type,stays_in_week_nights,adults,num_of_childs)%>%
  filter(stays_in_week_nights>=1) %>%
  arrange(num_of_childs)%>%
  group_by(num_of_childs,hotel_type)%>%
  tally()

#bookings for less than 1 day
data %>%
  filter(stays_in_week_nights==0 & stays_in_weekend_nights==0)%>%
  group_by(hotel_type,num_of_childs)%>%
  tally()

#for more than one day
data %>%
  filter(stays_in_week_nights!=0 | stays_in_weekend_nights!=0)%>%
  group_by(hotel_type,num_of_childs)%>%
  tally()

#no of bookings for different hotel types for different num of childs

data%>%
  group_by(num_of_childs)%>%
  tally()
data%>%
  group_by(hotel_type)%>%
  tally()

#total number of canceled bookings
data%>%
  count(canceled_status==1)

#which market segment has highest bookings
market<-(data%>%
           group_by(market_segment,hotel_type)%>%
           tally()%>%
           arrange(hotel_type))
market<- rename(market,num_of_bookings=n)

#num of bookings vs distribution channel
channel<-(data%>%
           group_by(distribution_channel,hotel_type)%>%
           tally()%>%
           arrange(hotel_type))
channel<- rename(channel,num_of_bookings=n)
  
#visualizng the bookings vs market segment and distribution channel

ggplot(data) +
  geom_bar(mapping = aes(x = market_segment, fill=hotel_type)) +
  facet_wrap(~hotel_type)+
  theme(axis.text.x = element_text(angle = 45))+
  labs(title="Comparison of market segments by hotel type for hotel bookings",
       subtitle=paste0("Data from: ", '2015', " to ", '2017'),
       x='Market Segment',
       Y='Num_of_bookings')

ggplot(data) +
  geom_bar(mapping = aes(x = distribution_channel, fill=hotel_type)) +
  facet_grid(~hotel_type)+
  theme(axis.text.x = element_text(angle = 45))+
  labs(title="Comparison of distribution channel by hotel type for hotel bookings",
       subtitle=paste0("Data from: ", '2015', " to ", '2017'),
       x='distribution channel',
       y='num of bookings')

ggplot(data) +
  geom_bar(mapping = aes(x = distribution_channel,fill=deposit_type)) +
  facet_wrap(~deposit_type) +
  theme(axis.text.x = element_text(angle = 45))+
  labs(title="Comparison of distribution channel by hotel type for hotel bookings",
       subtitle=paste0("Data from: ", '2015', " to ", '2017'),
       x='distribution channel',
       y='num of bookings')

ggplot(data) +
  geom_bar(mapping = aes(x = distribution_channel)) +
  facet_wrap(~deposit_type~market_segment) +
  theme(axis.text.x = element_text(angle = 45))+
  labs(title="Comparison of distribution channel by market segment and deposit type for hotel bookings",
       subtitle=paste0("Data from: ", '2015', " to ", '2017'),
       x='distribution channel',
       y='num of bookings')




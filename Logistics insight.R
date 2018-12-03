library(tidyverse)
library(dplyr)
library(stringr)
library(sqldf)
library(data.table)


deliveries <- read.csv("C:/Prabha/Job Search/deliv.co/interview data set - deliveries data.csv")

dim(deliveries)

head(deliveries, n = 20)

# shows blanks in pickup.start.time, pickup.end.time

summary(deliveries)

# no NA, unique Yes and Nos for Questions.

#Qn1  - Which Metro has the most Deliveries

# Checking Nulls in Metro

sqldf("SELECT COUNT(metro) FROM deliveries
      WHERE metro =''")

unique(deliveries$metro)

# check query
#sqldf("select metro, count(*) metro_cash
#      from deliveries
#      where cancelled = 'No'
#      group by metro")

sqldf("select metro, count(metro) 
        from deliveries
      where cancelled = 'No'
      group by metro
      having count(metro) = (select max(metro_cash) from 
      (select metro, count(metro) metro_cash
      from deliveries
      where cancelled = 'No'
      group by metro)
      )")

# or

sqldf("select metro, count(metro) 
        from deliveries
      group by metro
      order by count(metro) desc
      limit 1
      ")


# Excluding Cancelled:

sqldf("select metro, count(*) 
        from deliveries
        where cancelled = 'No'
        group by metro
        having count(metro) = (select max(metro_cash) from 
      (select metro, count(metro) metro_cash
      from deliveries
      where cancelled = 'No'
      group by metro)
      )")

#Ans :

# Including Cancelled:
#---------------------
# Manhattan (FK)     5188

# Excluding Cancelled:
#---------------------
# Manhattan (FK)     5051

library(ggplot2)
summary(deliveries)
count(deliveries)



library(corrplot)
hist(deliveries$metro)

delivery_delay = subset(deliveries, delay.at.drop.off=="Yes")

plot(delivery_delay$metro)

dd1 = subset(delivery_delay, metro=="Miami")
dd1
plot(dd1$metro)

df1 <- table(delivery_delay$metro)

summary(df1)

head(df1)

barplot(df1,main="Count of metros",
        xlab="Metro",
        ylab="Count", ylim = 700, 
        border="red")

?barplot

hist(delivery_delay$metro)
plot(df1)

?plot

#QN2

# Which Metro has the most delivery delays?
setnames(deliveries, old = c("delay.at.drop.off"), new = c("delay_at_drop_off"))

head(deliveries)

unique(deliveries$delay_at_drop_off)

sqldf("select metro, count(*) from deliveries
        where delay_at_drop_off = 'Yes'
        group by metro
        having count(metro) = (select max(metro_cash) from (select metro, count(metro) metro_cash
        from deliveries
        where delay_at_drop_off = 'Yes'
        group by metro))")

#Ans:
#metro count(*)
#1 Manhattan (FK)      669

#Qn3

setnames(deliveries, old = c("delay.at.pickup"), new = c("delay_at_pickup"))

?setnames
  

head(deliveries)

unique(deliveries$delay_at_pickup)

#Which metro has the most pickup delays?

sqldf("select metro, count(*) from deliveries
        where delay_at_pickup = 'Yes'
      group by metro
      having count(metro) = (select max(metro_cash) from (select metro, count(metro) metro_cash
      from deliveries
      where delay_at_pickup = 'Yes'
      group by metro))")

proc.time()

#or

sqldf("select metro, count(*) from deliveries
        where delay_at_pickup = 'Yes'
      group by metro
      order by count(*) desc
      limit 1")

proc.time()

# Ans:
#metro count(*)
#1 San Jose     1389

#Qn4

#Which metro do drivers spend the most time at pickup?

setnames(deliveries, old = c("pickup.start.time", "pickup.end.time"), new = c("pickup_start_time", "pickup_end_time"))

head(deliveries)

class(deliveries$pickup_start_time)

as.character(deliveries$pickup_start_time)

as.character(deliveries$pickup_end_time)

sqldf("SELECT COUNT(pickup_start_time) FROM deliveries
      WHERE pickup_start_time IS NOT NULL")

sqldf("SELECT COUNT(pickup_start_time) FROM deliveries
      WHERE pickup_start_time IS NULL")

sqldf("SELECT COUNT(pickup_start_time) FROM deliveries
      WHERE pickup_start_time =''")

sqldf("SELECT COUNT(pickup_start_time) FROM deliveries
      WHERE pickup_end_time =''")

deliveries1 <- subset(deliveries, deliveries$pickup_start_time !=''| deliveries$pickup_end_time != '' )

dim(deliveries1)

summary(deliveries1)

deliveries1$pickup_start_time1 <- as.POSIXct(deliveries1$pickup_start_time, format = "%m/%d/%y %H:%M")
deliveries1$pickup_end_time1 <- as.POSIXct(deliveries1$pickup_end_time, format = "%m/%d/%y %H:%M")

write.csv(deliveries1, file = "deliveries1.csv")

sqldf("select metro, sum(pickup_end_time1 - pickup_start_time1) as pickup_time 
      from deliveries1
      group by metro
      order by pickup_time desc")


sqldf("select metro, sum(pickup_end_time1 - pickup_start_time1) as pickup_time
      from deliveries1
      group by metro
      having pickup_time = (select max(pickup_time) from (select sum(pickup_end_time1 - pickup_start_time1) as pickup_time 
      from deliveries1
      group by metro))")

# Ans
#      metro pickup_time
  #1 San Jose        952860
     
#Qn5

#Whats the average pickup length by metro and overall?

# Metro pickup Average

metro_pickup <- sqldf("select metro, avg(pickup_end_time1 - pickup_start_time1) as pickup_time
      from deliveries1
      group by metro")

metro_pickup

write.csv(metro_pickup, file = "metro_pickup.csv")

# Overall pickup Average

sqldf("select avg(pickup_end_time1 - pickup_start_time1) as pickup_time
                        from deliveries1")

#   pickup_time
# 1    258.1397

#Qn6

# Are there any patterns around delivery delays?

#Ans

# Delivery Delays are high during 17hrs(201) and 18(199) hrs.


#Qn7

# What was the leading cause of delivery delays?

# Ans

# Peak Hour Traffic - 17hrs, 18 hours
# PIck Up Delays



# Qn8

# What's the percent of deliveries that were delayed at pickup and delayed at drop off.
delivery_drop_off <- subset(deliveries, delay_at_drop_off == "Yes")
pickup_delay <- subset(deliveries, delay_at_pickup == "Yes")

nrow(delivery_drop_off)
nrow(pickup_delay)
nrow(deliveries)

# Percentage Delay Drop Off

Perc_delay_drop_off <- (nrow(delivery_drop_off)/nrow(deliveries))*100
Perc_delay_drop_off

# 5.376975

# Percentage Pickup Delay

Perc_pickup <- (nrow(pickup_delay)/nrow(deliveries))*100
Perc_pickup

# 20.22916

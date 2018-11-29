#MBA-BDA college 2; Wickham & Grolemund, Data Transformations

#summary functions in R

#measures of center
x <- c(5, 12, 6, 9, NA, 10, 12, 21, 25, 0, 23)
mean(x)
mean(x, na.rm = TRUE)
median(x, na.rm = TRUE)
mode(x)

lengte <- rnorm(1200, 186, 10)
mean(lengte); round(mean(lengte), 2)
round(median(lengte), 2)

quantile(lengte)
quantile(lengte, 0.5)
quantile(lengte, 0.25)

#measures of spread
range(lengte) #returns vector with minimum and maximum value
#use formula to calculate range
max(lengte) - min(lengte)

#interquartile range (IQR)
quantile(lengte, 0.75) - quantile(lengte, 0.25) #interquartile range
IQR(lengte) #funcion to calculate IQR

#variance and standard deviation
#by default the sample variance
#and sample sd will be calculated
var(lengte)
sd(lengte)


#p. 43
library(nycflights13)
library(tidyverse)

View(flights)

ggplot(flights, aes(x = arr_delay)) +
  geom_histogram(fill = "black", binwidth = 10)

ggplot(flights, aes(x = dep_delay)) +
  geom_histogram(fill = "black", binwidth = 10)

#warning: this graph takes a lot of time
ggplot(flights, aes(x = dep_delay, y = arr_delay)) +
  geom_point()

#p. 44
View(flights)
fl <- flights
str(fl)
?flights

#p. 45
#dplyr verbs

#p. 45
filter(flights, month == 1, day == 1)
jan1 <- filter(flights, month == 1 | month == 2)
(dec <- filter(flights, month == 12))

#p. 47 logical operations
3 < 4
!(3 < 4)
12 == 10 + 2
sqrt(2)^2 == 2

(10 < 12) | (10 > 12)


#p. 48
#bedenk eerst wat het resultaat zal zijn
filter(flights, month == 11 | month == 12)
filter(flights, month == 1, (day == 1 | day == 31))
tail(filter(flights, month > 6, month < 9))
filter(flights, month %in% 1:6)

filter(flights, !(arr_delay > 120 | dep_delay > 120))

#logical operations with NA
NA > 8
12 == NA
NA == NA
NA > NA
2 * NA; NA^0

#p. 49
x <- c(7, 6, NA, 8)
x
is.na(x)

#MAKE p. 49  EXERCISE 1

#p. 50 EXERCISE 3
df1 <- filter(flights, is.na(dep_time))
nrow(df1) #aantal rijen
rm(df1) #df1 verwijderen

#p. 50
head(arrange(flights, arr_delay))
head(arrange(flights, desc(arr_delay)))
arrange(flights, -arr_delay)

#the NA's are always sorted at the end
tail(arrange(flights, arr_delay))
tail(arrange(flights, desc(arr_delay)))
tail(arrange(flights, -arr_delay))

#MAKE p. 51 EXERCISES 1, 2, 3, 4

#p. 52
select(flights, year, month, day, carrier)
select(flights, year:carrier)
select(flights, -year)

#p. 53
select(flights, ends_with("delay"))
select(flights, flight, ends_with("delay"))

rename(flights, dep.time = dep_time, dep.delay = dep_delay)

select(flights, origin, dest, everything())

arrange(select(flights, origin, dest, everything()), origin, dest)

#p. 54 Exercise 2, 4
select(flights, origin, origin)
#exercise 3
select(flights, one_of("year", "month", "day", "dep_delay", "arr_delay"))
#exercise 4
select(flights, contains("TIME")) #not case sensitive
select(flights, contains("TIME", ignore.case = FALSE))

#p. 54 add new variables, mutate()
flights.sml <- select(flights, year:day,
                      ends_with("delay"),
                      distance, air_time)
flights.sml <- mutate(flights.sml,
                      gain = arr_delay - dep_delay,
                      speed = distance / air_time * 60)

#p. 55
flights.sml <- mutate(flights.sml,
                      gain = arr_delay - dep_delay,
                      hours = air_time / 60,
                      gain_per_hour = gain / hours)
#maybe it's better to round the outcomes
#but be carefull if you use the outcomes of
#rounded numbers in further calculations
flights.sml <- mutate(flights.sml,
                      gain = arr_delay - dep_delay,
                      hours.rounded = round(air_time / 60, 2),
                      gain_per_hour.rounded = round(gain / hours, 2))

#transmute() keeps only the new variables
transmute(flights.sml,
          gain = arr_delay - dep_delay,
          hours = round(air_time / 60, 2),
          gain_per_hour = round(gain / hours, 2))


#p. 56 VECTORIZED FUNCTIONS
#intermezzo: examine the following examples
c(2, 4) + 1
c(2, 4) + c(1, 3)
c(2 , 3) + c(1, 3, 5)

flights$dep_time / 60
 c(2, 4, 6) * c(1, 3) 

 #p. 57
 #lead() and lag() function
 x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) #alternative: x <- 1:10
 lead(x)
 lag(x)
 cumsum(x)
 
#p. 58
#rank functions
 
#10 most delayed arrivals
#solution 1
flights.delayed <- mutate(flights,
                          ranking = min_rank(arr_delay))
flights.delayed <- select(flights.delayed, ranking, everything())
flights.delayed <- arrange(flights.delayed, ranking)
filter(flights.delayed, ranking <= 10)

#solution 2
flights.delayed.10 <- mutate(flights,
                          ranking = min_rank(-arr_delay)) %>%
                   select(ranking, everything()) %>%
                   arrange(ranking) %>%
                   filter(ranking <= 10)

#nog een alternatief
flights.delayed.10 <- flights %>%
  mutate(ranking = min_rank(-arr_delay)) %>%
  select(ranking, everything()) %>%
  arrange(ranking) %>%
  filter(ranking <= 10)

#notice 1: there are 17 observations with ranking <=10
#notice 2: compare base function rank with dplyr:: min_rank()

#p. 59 group_by() and summarize()
#calculate statistics per group
#10 most delayed flights per carrier

flights.del.car <- flights %>%
  group_by(carrier) %>%
  mutate(ranking = min_rank(-arr_delay)) %>%
  filter(ranking <= 10) %>%
  arrange(carrier, ranking) %>%
  select(carrier, ranking, arr_delay, everything())

#p. 59-60 
#relation between distance and average delay
#for each destination

#examine the different destinations
levels(factor(flights$dest))

#number of flights per destination
dest.delay <- flights %>%
  group_by(carrier, dest) %>%
  summarize(n_flights = n())

#same as previous; adding mean distance
#and mean delay
dest.delay <- flights %>%
  group_by(dest) %>%
  summarize(n_flights = n(),
            dist_mean = mean(distance, na.rm = TRUE), 
              #na.rm: remove NA's before calculating mean
              #otherwise result mean function would be NA
            delay_mean = mean(arr_delay, na.rm = TRUE))
  

ggplot(dest.delay, aes(x = dist_mean, y =  delay_mean)) +
  geom_point()

#p. 62-64
not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(delay = mean(arr_delay))
ggplot(delays, aes(x = delay)) +
  geom_histogram(binwidth = 10, fill = "black")

delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(n = n(),
            delay_mean = mean(arr_delay, na.rm = TRUE))

ggplot(delays, aes(x = n, y = delay_mean)) +
  geom_point()

ggplot(delays, aes(x = n, y = delay_mean)) +
  geom_point(alpha = 1/10)

delays %>%
  filter(n>25) %>%
  ggplot(aes(x = n, y = delay_mean)) +
  geom_point(alpha = 1/10)



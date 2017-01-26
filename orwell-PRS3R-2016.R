# setting up working directory
setwd("~/Dropbox")
rm(list=ls())

# reading the .csv file into R
orwell_original <- read.csv("Orwell-PRS3R-2016.csv", header = TRUE)

# loading required package
require(tidyverse)

# subsetting by only selecting relevant variables
orwell <- select(orwell_original, Posting.Date, Machine.Number, Grade, 
                 Material, Order, Employee.Number, Shifts, Confirmed.Qty, 
                 Scrap.Actual, Machining.Plan, Machining.Actual, Labor.Actual)

# changing column names to lower case and easy to read (I like that)
colnames(orwell) <- c("date", "machine", "grade", "material", 
                      "order", "employee", "shift", "qty", "scrap", 
                      "machine_plan", "machine_actual", "labor_actual")

# converting required variables to factor

orwell$scrap <- as.numeric(orwell$scrap)
orwell$qty <- as.numeric(orwell$qty)
orwell$shift <- as.factor(orwell$shift)
orwell$order <- as.factor(orwell$order)
orwell$material <- as.factor(orwell$material)
orwell$date <- as.Date(orwell$date, format = "%d.%m.%Y")
orwell$month <- months(orwell$date)
orwell$monthnum <- as.factor(format(orwell$date, "%m"))
orwell$weekdays <- as.factor(weekdays(orwell$date))

# setting ordered factor for months and weeks

orwell$month <- factor(orwell$month,levels=c(
  "January","February","March","April","May","June", "July",
  "August", "September", "October", "November", "December"),ordered=TRUE)

orwell$weekdays <- factor(orwell$weekdays,levels=c(
  "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
  "Friday", "Saturday"),ordered=TRUE)

# s <- orwell %>% filter(machine != c(
# '#', '002804', '005241', '0', '008449 E', '043101 V', '1271', 
# '1459', '34283', 'INE: 005', '5241100', '7992', '8137', '8150', 
# '8159', '8240', '8276', '8449', '8454', '8559', '90593'))

# or should I just create a data frame containing 10 machine I need
# which one is easier? I think second one
# so lets get started

orwell <- filter(orwell, machine %in% c(
  "28045", "32108", "43101", "50591", "50592", "50593", "50696", "52411", "52412"))

orwell <- filter(orwell, employee %in% c(
  "20648", "20986", "21044", "21161", "21227", "21242",
  "21269", "21361", "61301","63508", "63858", "63877", "63928", 
  "64863", "64891", "65083"))

operator <- orwell %>%
  group_by(machine, monthnum, weekdays, shift, employee) %>%
  summarise(
    total_qty = sum(qty, na.rm=TRUE),
    total_scrap = sum(scrap, na.rm=TRUE)
  )

#ggplot(data=operator, mapping = aes(x = monthnum, y = total_qty)) + 
#  geom_boxplot(mapping = aes(color = shift))

#ggplot(data=operator, mapping = aes(x = monthnum, y = total_qty)) + 
#  geom_boxplot() + facet_grid(machine~weekdays)

# try to sumperimpose shift data over each other
ggplot(data=operator, mapping = aes(x = monthnum, y = total_qty)) + 
  geom_boxplot(mapping = aes(fill = shift), position="identity", alpha=0.40) + facet_grid(.~weekdays)

# can we see how employee output changes over months? 
# To make below work, we need a different tibble
# ggplot(data=operator, mapping = aes(x = monthnum, y = total_qty)) +
#   geom_smooth() + facet_wrap(~employee)


op <- orwell %>%
  group_by(monthnum, employee) %>%
  summarise(
    total_qty = sum(qty, na.rm=TRUE),
    total_scrap = sum(scrap, na.rm=TRUE)
  )

# the bitch was converting monthnum or month from factor into numeric value
# to make geom_smooth work
# THIS IS IMPORTANT TO REMEMBER
# op$monthnum <- as.numeric(op$monthnum)

ggplot(data=op, mapping = aes(x = monthnum, y = total_qty, color = employee)) + 
  geom_point() + geom_smooth() + facet_wrap(~employee)

# this plot is worth looking at 
ggplot(data=op, mapping = aes(x = monthnum, y = total_qty)) + 
  +   geom_point(aes(size = 0.5)) + facet_wrap(~employee)

mat <- orwell %>%
  group_by(material) %>%
  summarise(
    total_qty = sum(qty),
    count = n()
  )

# change the total_qty value to change number of materials selected
# I'm using 10,000 so I can focus on 67 materials only
mat <- filter(mat, total_qty>=10000)

# highest quantity orders at the top
arrange(mat, desc(total_qty))

# now lets see if the machine time for orders reported changed with MII
# for that we need to summarise data by month and order

change <- orwell %>%
  group_by(monthnum, material, order) %>%
  summarise(
    total_machine_plan = sum(machine_plan, na.rm=TRUE),
    total_machine_actual = sum(machine_actual, na.rm=TRUE)
  )




# setting up working directory

setwd("~/Dropbox")

# clear up memory

rm(list=ls())

# reading the .csv file into R

orwell_original <- read.csv("Orwell-PRS3R-2016.csv", header = TRUE)

# loading required package

require(tidyverse)

# filtering only relevant variables from orwell_original dataset

orwell <- select(
  orwell_original, 
  Posting.Date, 
  Machine.Number, 
  Grade, 
  Material, 
  Order, 
  Employee.Number, 
  Shifts, 
  Confirmed.Qty, 
  Scrap.Actual, 
  Machining.Plan, 
  Machining.Actual, 
  Labor.Actual
  )

# changing column names to lower case and easy to read (I like that)

colnames(orwell) <- c(
  "date", 
  "machine", 
  "grade", 
  "material", 
  "order", 
  "employee", 
  "shift", 
  "qty", 
  "scrap", 
  "machine_plan", 
  "machine_actual", 
  "labor_actual")

# converting required variables to factor or numeric and adding month variable

orwell$scrap    <- as.numeric(orwell$scrap)
orwell$qty      <- as.numeric(orwell$qty)
orwell$shift    <- as.factor(orwell$shift)
orwell$order    <- as.factor(orwell$order)
orwell$material <- as.factor(orwell$material)
orwell$date     <- as.Date(orwell$date, format = "%d.%m.%Y")
orwell$month    <- months(orwell$date)
orwell$monthnum <- as.factor(format(orwell$date, "%m"))
orwell$weekdays <- as.factor(weekdays(orwell$date))

# setting ordered factor for months and weeks

orwell$month <- factor(
  orwell$month,levels=c(
    "January",
    "February",
    "March",
    "April",
    "May",
    "June", 
    "July",
    "August", 
    "September", 
    "October", 
    "November", 
    "December"),
  ordered=TRUE)

orwell$weekdays <- factor(
  orwell$weekdays,levels=c(
    "Sunday", 
    "Monday", 
    "Tuesday", 
    "Wednesday", 
    "Thursday", 
    "Friday", 
    "Saturday"),
  ordered=TRUE)

# keeping required machines

orwell <- filter(orwell, machine %in% c(
  "28045", 
  "32108", 
  "43101", 
  "50591", 
  "50592", 
  "50593", 
  "50696", 
  "52411", 
  "52412")
  )

orwell <- filter(orwell, employee %in% c(
  "20648", 
  "20986", 
  "21044", 
  "21161", 
  "21227", 
  "21242",
  "21269", 
  "21361", 
  "61301",
  "63508", 
  "63858", 
  "63877", 
  "63928", 
  "64863", 
  "64891", 
  "65083")
  )





# tibble with total qty and scrap and grouping by key variables

operator <- orwell %>%
  group_by(machine, monthnum, weekdays, shift, employee) %>%
  summarise(
    total_qty = sum(qty, na.rm=TRUE),
    total_scrap = sum(scrap, na.rm=TRUE)
  )

# PLOT PLOT PLOT

#ggplot(data=operator, mapping = aes(x = monthnum, y = total_qty)) + 
#  geom_boxplot(mapping = aes(color = shift))

#ggplot(data=operator, mapping = aes(x = monthnum, y = total_qty)) + 
#  geom_boxplot() + facet_grid(machine~weekdays)

# try to sumperimpose shift data over each other

ggplot(data=operator, mapping = aes(x = monthnum, y = total_qty)) + 
  geom_boxplot(mapping = aes(fill = shift), position="identity", alpha=0.40) + 
  facet_grid(.~weekdays)





# new tibble with grouping by month and employee

op <- orwell %>%
  group_by(monthnum, employee) %>%
  summarise(
    total_qty = sum(qty, na.rm=TRUE),
    total_scrap = sum(scrap, na.rm=TRUE)
  )

# the issue was converting monthnum or month from factor into numeric value
# to make geom_smooth work
# THIS IS IMPORTANT TO REMEMBER for geom_smooth()
# op$monthnum <- as.numeric(op$monthnum)

ggplot(data=op, mapping = aes(x = monthnum, y = total_qty, color = employee)) + 
  geom_point() + 
  geom_smooth() + 
  facet_wrap(~employee)

# this plot is worth looking at 
ggplot(data=op, mapping = aes(x = monthnum, y = total_qty)) + 
  geom_point(aes(size = 0.5)) + 
  facet_wrap(~employee)





# Now I want to focus on high quantity materials only
# so first I'll create a tibble containing materials 
# with highest production order quantity 

mat <- orwell %>%
  group_by(material) %>%
  summarise(
    total_qty = sum(qty),
    count = n()
  )

# change the value of total_qty to change number of materials selected
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






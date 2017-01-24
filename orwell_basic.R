# setting working directory
setwd("~/Dropbox")
rm(list=ls())
require(tidyverse)

# reading the data file
orwell <- read.csv("orwell.csv", header=TRUE)

# converting rquired variables to factor

orwell$scrap <- as.numeric(orwell$scrap)
orwell$setup_plan <-as.numeric(orwell$setup_plan)
orwell$qty <- as.numeric(orwell$qty)
orwell$shift <- as.factor(orwell$shift)
orwell$order <- as.factor(orwell$order)
orwell$date <- as.Date(orwell$date, format = "%d.%m.%Y")
orwell$month <- months(orwell$date)

# setting ordered factor for months
orwell$month <- factor(orwell$month,levels=c("January","February","March","April","May","June"),ordered=TRUE)


orwell$weekdays <- as.factor(weekdays(orwell$date))


# dropping all rows with no machine data
# old method 
# orwell[!is.na(orwell$machine),]

# new method, although may not be required in this case
sub <- orwell %>% drop_na(machine)

# dropping machine "#" 
sub <- orwell %>% filter(machine != '#')

sub$month_num <- match(sub$month, month.abb)

# group_by and summarise``
results <- sub %>% 
  group_by(work_center, machine, month) %>%
  summarise(
    total_qty = sum(qty, na.rm=TRUE), 
    total_scrap = sum(scrap, na.rm=TRUE), 
    total_machine_actual = sum(machine_actual, na.rm=TRUE)
  )
# interesting pattern in above chunk, if I use "<-" instead of " ",
# then I get weird column headers in the results tibble

# we went live with MII in April 2016 
# using boxplot we can see if there was any significant shift in data quality

ggplot(data = results, mapping = aes(x = month, y = total_qty)) + 
  geom_boxplot(mapping = aes(color = work_center))

# setting working directory
setwd("~/Dropbox")
rm(list=ls())

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
orwell$weekdays <- weekdays(orwell$date)

# dropping all rows with no machine data
# old method 
# orwell[!is.na(orwell$machine),]

# new method, although may not be required in this case
sub <- orwell %>% drop_na(machine)

# dropping machine "#" 
sub <- orwell %>% filter(machine != '#')

# group_by and summarise
results <- sub %>% 
  group_by(machine, month) %>%
  summarise(
    tot.qty <- sum(qty, na.rm=TRUE), 
    tot.scrap <- sum(scrap, na.rm=TRUE), 
    tot.machine_actual <- sum(machine_actual, na.rm=TRUE)
    )


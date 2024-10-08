# Load necessary libraries
library(tidyverse)
library(moderndive)
library(skimr)
library(corrplot)

# Load the provided data/functions
source("D:/Uni Resources/Stats/assignment1/fixregday1.pck")

# Load the dataset
data <- read.csv("D:/Uni Resources/Stats/assignment1/d.csv")


head(data)
str(data)
summary(data)
skim(data)

#storm
bootmatsmooth(NOAAGISSWD$Year, NOAAGISSWD$Severe.Storm.Count)
bootmatlin(NOAAGISSWD$Year, NOAAGISSWD$Severe.Storm.Count)

#drought
bootmatsmooth(NOAAGISSWD$Year, NOAAGISSWD$Drought.Count)
bootmatlin(NOAAGISSWD$Year, NOAAGISSWD$Drought.Count)

#floods
bootmatsmooth(NOAAGISSWD$Year, NOAAGISSWD$Flooding.Count)
bootmatlin(NOAAGISSWD$Year, NOAAGISSWD$Flooding.Count)

#freeze
bootmatsmooth(NOAAGISSWD$Year, NOAAGISSWD$Freeze.Count)
bootmatlin(NOAAGISSWD$Year, NOAAGISSWD$Freeze.Count)

#Tropical Cylones
bootmatsmooth(NOAAGISSWD$Year, NOAAGISSWD$Tropical.Cyclone.Count)
bootmatlin(NOAAGISSWD$Year, NOAAGISSWD$Tropical.Cyclone.Count)

#Wildfires
bootmatsmooth(NOAAGISSWD$Year, NOAAGISSWD$Wildfire.Count)
bootmatlin(NOAAGISSWD$Year, NOAAGISSWD$Wildfire.Count)

#Winter Storms
bootmatsmooth(NOAAGISSWD$Year, NOAAGISSWD$Winter.Storm.Count)
bootmatlin(NOAAGISSWD$Year, NOAAGISSWD$Winter.Storm.Count)

#All disasters
bootmatsmooth(NOAAGISSWD$Year, NOAAGISSWD$All.Disasters.Count)
bootmatlin(NOAAGISSWD$Year, NOAAGISSWD$All.Disasters.Count)

#--------------------------------------------------------------------------------------------
#Comparing with delta temperature

#storm
bootmatsmooth(NOAAGISSWD$delta.temp, NOAAGISSWD$Severe.Storm.Count)
bootmatlin(NOAAGISSWD$delta.temp, NOAAGISSWD$Severe.Storm.Count)

#drought
bootmatsmooth(NOAAGISSWD$delta.temp, NOAAGISSWD$Drought.Count)
bootmatlin(NOAAGISSWD$delta.temp, NOAAGISSWD$Drought.Count)

#floods
bootmatsmooth(NOAAGISSWD$delta.temp, NOAAGISSWD$Flooding.Count)
bootmatlin(NOAAGISSWD$delta.temp, NOAAGISSWD$Flooding.Count)

#freeze
bootmatsmooth(NOAAGISSWD$delta.temp, NOAAGISSWD$Freeze.Count)
bootmatlin(NOAAGISSWD$delta.temp, NOAAGISSWD$Freeze.Count)

#Tropical Cylones
bootmatsmooth(NOAAGISSWD$delta.temp, NOAAGISSWD$Tropical.Cyclone.Count)
bootmatlin(NOAAGISSWD$delta.temp, NOAAGISSWD$Tropical.Cyclone.Count)

#Wildfires
bootmatsmooth(NOAAGISSWD$delta.temp, NOAAGISSWD$Wildfire.Count)
bootmatlin(NOAAGISSWD$delta.temp, NOAAGISSWD$Wildfire.Count)

#Winter Storms
bootmatsmooth(NOAAGISSWD$delta.temp, NOAAGISSWD$Winter.Storm.Count)
bootmatlin(NOAAGISSWD$delta.temp, NOAAGISSWD$Winter.Storm.Count)

#All disasters
bootmatsmooth(NOAAGISSWD$delta.temp, NOAAGISSWD$All.Disasters.Count)
bootmatlin(NOAAGISSWD$delta.temp, NOAAGISSWD$All.Disasters.Count)


#years and delta temp

bootmatsmooth(NOAAGISSWD$Year, NOAAGISSWD$delta.temp)
bootmatlin(NOAAGISSWD$Year, NOAAGISSWD$delta.temp)






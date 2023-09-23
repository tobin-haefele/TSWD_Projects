#Load tidyverse
library(tidyverse)
#Load dataset in
CarData = read.csv(file = "dv-carbitrage-raw-data.csv", header = TRUE)
names(CarData)
CarData_filtered = CarData %>% 
    select(make, model)
names(CarData_filtered)
for(i in length(CarData_filtered$make)
    if(CarData_filtered$make[i] = CarData_filtered$make[(i-1)]
        #create list sorted by make of [i]
    )
    
    )
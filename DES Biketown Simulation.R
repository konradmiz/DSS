library(tibble)
library(dplyr)

simulation_scheduler()
source("C:/Users/Konrad/Desktop/Discrete Systems Simulation R Code/DSS-Simulation/Trips Function.R")

#start_time <- Sys.time()

simulation_scheduler <- function()
{
  set_simulation_parameters()
  arrivals <<- arrival_calculator(duration, mean_arrival)
  arrivals <<- trips_calculator()
  arrivals <<- prepare_hub_capacity()
  arrivals <<- balk_calculator()
}

set_simulation_parameters <- function()
{
  duration <<- 1080
  mean_arrival <<- 2
}

arrival_calculator <- function(duration, mean, sd = NA)
{
  arrival_time_vector <- numeric(length = duration/mean)
  arrival_hub_vector <- character(length = duration/mean)
  
  current_time <- 0
  i <- 1
  
  while (current_time <= duration)
  {
    next_arrival <- rexp(1, rate = 1/mean)
    next_hub <- sample(hubs_data$Name, 1, prob = hubs_data$Prop)

    current_time <- current_time + next_arrival
    if (current_time <= duration)
    {
      arrival_time_vector[i] <- current_time #currently no issue with subscripting out of bounds
      arrival_hub_vector[i] <- next_hub
      
      i <- i + 1
    } else
    {
      arrivals_info <- tibble(`Start Time` = arrival_time_vector, `Start Hub` = arrival_hub_vector)
      
      arrivals_info <- arrivals_info %>%
        filter(`Start Hub` != "")
      
      return(arrivals_info)
    }
  }
}


trips_calculator <- function()
{
  arrivals$`End Hub` <- NA
  arrivals$`End Time` <- NA
  arrivals$Duration <- NA
  arrivals$Balk <- FALSE
  
  for (i in 1:nrow(arrivals))
  {
    start_hub <- arrivals$`Start Hub`[i]
    prob_list <- as.vector(t(trips_matrix[start_hub, ]))
    trip_possibilities <- tibble(`End Hub` = colnames(trips_matrix), Probability = prob_list)
    
    end_hub <- sample(trip_possibilities$`End Hub`, 1, prob = trip_possibilities$Probability)
    
    trip_duration <- rexp(1, .2)
    
    arrivals$`End Hub`[i] <- end_hub
    arrivals$Duration[i] <- trip_duration
    arrivals$`End Time`[i] <- arrivals$`Start Time`[i] + arrivals$Duration[i] 
  }
  
  return(arrivals)
}

prepare_hub_capacity <- function()
{
  #hubs_capacity_data <- tibble(Name = hubs_data$Name, Capacity = rep(0, nrow(hubs_data)))
  #hubs_capacity_data <- data.frame(t(hubs_capacity_data), stringsAsFactors = FALSE)
  
  hubs_capacity_data <- data.frame(t(hubs_data$Name), stringsAsFactors = FALSE)
  
  colnames(hubs_capacity_data) <- hubs_capacity_data[1, ]
  hubs_capacity_data <- hubs_capacity_data[-1, ]
  hubs_capacity_data[2:nrow(arrivals), ] <- 0
  
  hubs_capacity_data <- sapply(hubs_capacity_data, as.numeric)
  
  arrivals <- cbind(arrivals, hubs_capacity_data)
  #hubs_capacity <- rep(4, nrow(hubs_data))
  arrivals <- rbind(c(0, NA, NA, 0, 0, FALSE, hubs_data$capacity), arrivals)
  return(arrivals)
}

balk_calculator <- function()
{
  for (i in 2:nrow(arrivals))
  {
    leave_hub <- arrivals$`Start Hub`[i]
    end_hub <- arrivals$`End Hub`[i]
    
    if (arrivals[(i-1), leave_hub] == 0)
    {
      arrivals$Balk[i] <- TRUE
      arrivals[i, 7:ncol(arrivals)] <- arrivals[i-1, 7:ncol(arrivals)]
    } else {
      arrivals$Balk[i] <- FALSE
    }

    if (arrivals$Balk[i] == FALSE)
    {
      if (leave_hub != end_hub)
      {
        arrivals[i, leave_hub] <- arrivals[(i-1), leave_hub] - 1
        arrivals[i, end_hub] <- arrivals[(i-1), end_hub] + 1
        
        arrivals[i, setdiff(colnames(arrivals)[7:ncol(arrivals)], c(leave_hub, end_hub))] <- 
          arrivals[(i-1), setdiff(colnames(arrivals)[7:ncol(arrivals)], c(leave_hub, end_hub))] 
        
      } else
      {
        arrivals[i, 7:ncol(arrivals)] <- arrivals[(i-1), 7:ncol(arrivals)]
      }
    }
   
  }
  return(arrivals)
}

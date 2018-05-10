library(tibble)
library(dplyr)

source("C:/Users/Konrad/Downloads/broken.stick.R")

hubs_capacity <- c(1,1,1,1)

hubs_list <- c("A", "B", "C", "D")
hubs_prob <- c(0.25, 0.2, 0.35, 0.2)



#scheduler <- function()
#{
#  arrival_calculator()
#  trips_calculator()
#}


arrival_calculator <- function(duration, mean, sd = NA)
{
  arrival_time_vector <- numeric(length = duration/mean)
  arrival_hub_vector <- character(length = duration/mean)
  
  current_time <- 0
  i <- 1
  
  while (current_time <= duration)
  {
    next_arrival <- rexp(1, rate = 1/mean)
    next_hub <- sample(hubs_list, 1, prob = hubs_prob)

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

arrivals <- arrival_calculator(100, 10)

trips_calculator <- function(arrivals)
{
  
  hub_combinations <- length(hubs_list)^2
  trip_probabilities <- broken.stick(hub_combinations)

  trips_matrix <- matrix(trip_probabilities[, 2], nrow = length(hubs_list), ncol = length(hubs_list))
  dimnames(trips_matrix) <- list(hubs_list, hubs_list)
  arrivals$`End Hub` <- NA
  arrivals$`End Time` <- NA
  arrivals$Duration <- NA
  
  for (i in 1:nrow(arrivals))
  {
    start_hub <- arrivals$`Start Hub`[i]
    trip_possibilities <- tibble(`End Hub` = hubs_list, Probability = trips_matrix[start_hub, ])
    
    end_hub <- sample(trip_possibilities$`End Hub`, 1, prob = trip_possibilities$Probability)
    
    trip_duration <- rexp(1, .2)
    
    arrivals$`End Hub`[i] <- end_hub
    arrivals$Duration[i] <- trip_duration
    arrivals$`End Time`[i] <- arrivals$`Start Time`[i] + arrivals$Duration[i] 
  }
  
  return(arrivals)
}
arrivals <- trips_calculator(arrivals)


arrivals$A <- NA
arrivals$B <- NA
arrivals$C <- NA
arrivals$D <- NA
arrivals$Balk <- FALSE
arrivals <- rbind(0, arrivals)
arrivals[1, 6:9] <- hubs_capacity
arrivals[-1, 6:9] <- 0

balk_calculator <- function(arrivals)
{
  for (i in 2:nrow(arrivals))
  {
    leave_hub <- arrivals$`Start Hub`[i]
    end_hub <- arrivals$`End Hub`[i]

    if (arrivals[i-1, leave_hub] == 0)
    {
      arrivals$Balk[i] <- TRUE
      arrivals[i, 6:9] <- arrivals[i-1, 6:9]
      
    }
    
    if (arrivals$Balk[i] == FALSE)
    {
      if (leave_hub != end_hub)
      {
        arrivals[i, leave_hub] <- arrivals[i-1, leave_hub] - 1
        arrivals[i, end_hub] <- arrivals[i-1, end_hub] + 1
        
        arrivals[i, setdiff(colnames(arrivals)[6:9], c(leave_hub, end_hub))] <- 
          arrivals[i-1, setdiff(colnames(arrivals)[6:9], c(leave_hub, end_hub))] 
        
      } else
      {
        arrivals[i, 6:9] <- arrivals[(i-1), 6:9]
      }
    }
   
  }
  
  
  
  return(arrivals)
  
  
}

#times <- round(arrivals$`Start Time`, 4)
#time_cuts <- cut(100, times)
arrivals <- balk_calculator(arrivals)

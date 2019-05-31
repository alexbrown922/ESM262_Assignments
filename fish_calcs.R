#' Moorea, French Polynesia: Fish Caught and Revenue 
#' @param catch_location_data  data frame with side of island where fish species were caught and number of fish caught
#' @param price_data data frame with price of fish and number of fish caught
#' @param graph graph of revenue in each location
#' @param total_revenue total revenue
#' @return list with total revenue at each side of the island, most frequently caught species at each side, total revenue at each side of the island, and total revenue 


fish_summary = function(catch_location_data, price_data, graph=FALSE) {
  
  
  ## revenue on each side of island 
  
  if(any(price_data$price < 0)) stop('prevents negative price')
  
  revenues_sideofisland <- left_join(catch_location_data, price_data, by = "fish") %>%
    mutate(west_rev = west*price) %>% 
    mutate(north_rev = north*price) %>%
    mutate(east_rev = east*price)
  
  west_rev = sum(revenues_sideofisland$west_rev)
  north_rev = sum(revenues_sideofisland$north_rev)
  east_rev = sum(revenues_sideofisland$east_rev)
  
  total_revenues_sideofisland <- data_frame(west_rev, north_rev, east_rev) %>%
    magrittr::set_colnames(value = c("rev_west", "rev_north", "rev_east"))
  
  ## most fish caught on each side of island
  
  fish_west <- rep(catch_location_data$fish, catch_location_data$west)
  fish_north <- rep(catch_location_data$fish, catch_location_data$north)
  fish_east <- rep(catch_location_data$fish, catch_location_data$east)
  
  fish_west <- as.factor(fish_west)
  fish-north <- as.factor(fish_north)
  fish_east <- as.factor(fish_east)
  
  freq_west <- names(which.max(summary(fish_west)))
  freq_north <- names(which.max(summary(fish_north)))
  freq_east <- names(which.max(summary(fish_east)))
  
  most_fish_caught <- data_frame(freq_north, freq_west, freq_east) %>%
    magrittr::set_colnames(value = c("freq_north", "freq_west", "freq_east"))
  
  ## total revenue in each fishery   
  
  total_revenues_by_fishery <- left_join(catch_location_data, price_data, by = "fish") %>%
    mutate(totalfish = rowSums(.[2:4])) %>%
    mutate(fishrev = totalfish*price) %>%
    select("fish", "fishrev") %>%
    magrittr::set_colnames(value = c("Fishery", "Total Revenue"))
  
  
  ## total revenue
  
  total_revenue <- sum(west_rev, north_rev, east_rev)
  
  
  ## graph of revenues by side of island caught
  
  if (graph == TRUE) {
    
    graph <- revenues_locations %>%
      magrittr::set_colnames(value = c("fish", "north", "east", "west", "price", "North", "West", "East")) %>%
      gather("North", "West", "East", key = "location", value = "price") %>%
      group_by(location) %>%
      summarize(price=sum(price)) %>%
      ungroup() 
    
    caption <- c("Total Revenue")   
    
    graph_revenue <- ggplot(graph) +
      geom_col(aes(x=location, y = price), fill= "green") +
      ylab("Price") +
      xlab("Side of Island") +
      theme_classic() +
      labs(title ="Total Revenue by Side of Island Caught", caption = paste(caption,total_revenue))
    
    graph_revenue
    
    }
  
  return(list(most_fish_caught, total_revenues_sideofisland, total_revenues_by_fishery, total_revenue, graph_revenue))
  
  }




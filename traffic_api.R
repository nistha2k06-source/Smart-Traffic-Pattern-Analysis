
library(plumber)

# Sample traffic data
traffic_data <- data.frame(
  hour = c("8 AM", "10 AM", "12 PM", "2 PM",
           "4 PM", "6 PM", "7 PM"),
  vehicles = c(75, 60, 40, 50, 85, 140, 110)
)

#* Get traffic data
#* @get /traffic
function() {
  traffic_data
}

#* Analyze traffic
#* @get /analyze
function() {
  peak <- traffic_data[which.max(traffic_data$vehicles), ]
  
  list(
    total_vehicles = sum(traffic_data$vehicles),
    average_vehicles = mean(traffic_data$vehicles),
    peak_hour = peak$hour,
    peak_vehicles = peak$vehicles
  )
}

#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  
  plumber::forward()
}
#* Update traffic data
#* @post /update
#* @parser json
function(req, res) {
  new_data <- jsonlite::fromJSON(req$postBody)
  
  if (length(new_data$vehicles) != nrow(traffic_data) ||
      any(!is.finite(new_data$vehicles)) ||
      any(new_data$vehicles < 0)) {
    res$status <- 400
    return(list(error = "Invalid vehicle counts"))
  }
  
  traffic_data$vehicles <<- as.numeric(new_data$vehicles)
  
  list(message = "Traffic data updated")
}

# SMART TRAFFIC SIGNAL SIMULATOR

# Function to calculate green time
calculate_green_time <- function(vehicles) {
  if (vehicles <= 10) {
    return(20)
  } else if (vehicles <= 20) {
    return(30)
  } else if (vehicles <= 30) {
    return(40)
  } else if (vehicles <= 40) {
    return(50)
  } else {
    return(60)
  }
}

# Vehicle count for each road
roads <- c("North", "South", "East", "West")
vehicles <- c(50, 30, 15, 40)

# Emergency vehicle
# Change "Ambulance" to "None", "Fire Engine", or "Police"
emergency_road <- "East"
emergency_type <- "Ambulance"

# Calculate green time
green_time <- c()

for (i in 1:length(roads)) {
  green_time[i] <- calculate_green_time(vehicles[i])
}

# Display traffic details
cat("\n====================================\n")
cat("   SMART TRAFFIC SIGNAL SIMULATOR\n")
cat("====================================\n\n")

for (i in 1:length(roads)) {
  cat(roads[i], ":", vehicles[i],
      "vehicles | Green Time:",
      green_time[i], "seconds\n")
}

# Check emergency vehicle
if (emergency_type != "None") {
  
  cat("\n====================================\n")
  cat("   EMERGENCY VEHICLE DETECTED!\n")
  cat("====================================\n")
  
  cat(emergency_type, "detected on",
      emergency_road, "Road.\n\n")
  
  cat(emergency_road, "Signal : GREEN\n")
  
  for (i in 1:length(roads)) {
    if (roads[i] != emergency_road) {
      cat(roads[i], "Signal : RED\n")
    }
  }
  
  cat("\nEmergency vehicle has priority.\n")
  cat("After emergency clearance, normal traffic resumes.\n")
  
} else {
  
  cat("\nNo emergency vehicle detected.\n")
}

# Find road with highest traffic
highest <- which.max(vehicles)

cat("\n====================================\n")
cat("     NORMAL TRAFFIC SCHEDULE\n")
cat("====================================\n\n")

for (i in 1:length(roads)) {
  
  if (i == highest) {
    cat(roads[i], "-> GREEN for",
        green_time[i], "seconds\n")
  } else {
    cat(roads[i], "-> RED\n")
  }
}

cat("\nHighest traffic road:",
    roads[highest],
    "with", vehicles[highest], "vehicles.\n")
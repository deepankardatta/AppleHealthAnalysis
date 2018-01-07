

ah_daily_collate <- function(health_data) {



}

# Defines which variable types needed to be summed to give a useful metric
daily_variables_to_sum <- c( "ActiveEnergyBurned" ,
                       "AppleExerciseTime" ,
                       "AppleStandHour" ,
                       "BasalEnergyBurned" ,
                       "DietaryWater" ,
                       "DistanceCycling" ,
                       "DistanceSwimming" ,
                       "DistanceWalkingRunning" ,
                       "FlightsClimbed" ,
                       "MindfulSession" ,
                       "NikeFuel" ,
                       "SleepAnalysis" , # Need to check if this is true
                       "StepCount" ,
                       "SwimmingStrokeCount"
                       )

# Defines which variable types needed to be averaged to give a useful metric
daily_variables_to_mean <- c( "BloodGlucose" ,
                              "BodyFatPercentage" ,
                              "BodyMass" ,
                              "BodyMassIndex" ,
                              "ForcedExpiratoryVolume1" ,
                              "ForcedVitalCapacity" ,
                              "HeartRate" ,
                              "HeartRateVariabilitySDNN" ,
                              "Height" ,
                              "LeanBodyMass" ,
                              "PeakExpiratoryFlowRate" ,
                              "RestingHeartRate" ,
                              "VO2Max" ,
                              "WalkingHeartRateAverage"
                              )

library(dplyr)

summarise(health_data)
health_data %>% filter(type == "DietaryWater")
test <- health_data %>% filter(type == "DietaryWater")
test2 <- test %>% group_by(year,month,date) %>% summarise(delay = sum(value, na.rm = TRUE))

test3 <- test %>% group_by(date) %>% summarise( value = sum(value, na.rm = TRUE) , n = n() )

test4 <- health_data %>% group_by(date,type) %>% summarise( value = sum(value, na.rm = TRUE) , n = n() )

test5 <- health_data %>% filter(type == daily_variables_to_sum ) %>% group_by(date,type) %>% summarise( value = sum(value, na.rm = TRUE) , n = n() )

test6 <- health_data %>% filter(type == daily_variables_to_mean ) %>% group_by(date,type) %>% summarise( value = mean(value, na.rm = TRUE) , n = n() )



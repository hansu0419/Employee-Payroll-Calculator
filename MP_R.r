  ################################################  
  #Language: R                                   #
  #Paradigm(s): Procedural, Functional           #
  ################################################
daily_rate <- 500
max_hours <- 8
overtime_hours <- 0
workdays <- 5 
rest_days <- 2 
in_time_default <- 0900 
out_time_default <- 0900
overtime_rate <- 1.25
night_shift_overtime_rate <- 1.375
rest_day_rate <- 1.3

employee_times <- data.frame(
  day_of_week = 1:7,
  in_time = rep(in_time_default, 7),
  out_time = rep(out_time_default, 7),
  day_type = c(rep("Normal_Day", workdays), rep("Rest_Day", rest_days)),
  stringsAsFactors = FALSE
)

input_time_data <- function(employee_times, day, in_time, out_time, day_type) {
  if(day < 1 || day > 7) {
    cat("Invalid day. Please enter a number between 1 and 7.\n")
    return(employee_times)
  }
  
  employee_times[day, "in_time"] <- in_time
  employee_times[day, "out_time"] <- out_time
  employee_times[day, "day_type"] <- day_type
  
  return(employee_times)
}
calculate_daily_pay <- function(in_time, out_time, day_type) {
  daily_rate <- 500
  max_hours <- 8
  
  base_rates <- c(Normal_Day = 1.0, 
                  Rest_Day = 1.3, 
                  Special_Non_Working = 1.3, 
                  Special_Non_Working_Rest_Day = 1.5, 
                  Regular_Holiday = 2.0, 
                  Regular_Holiday_Rest_Day = 2.6)
  
  night_shift_rate <- 1.10
  
  overtime_rates <- c(Normal_Day = 1.25, Rest_Day = 1.69, 
                      Special_Non_Working = 1.69, 
                      Special_Non_Working_Rest_Day = 1.95, 
                      Regular_Holiday = 2.60, 
                      Regular_Holiday_Rest_Day = 3.38)
  
  night_shift_overtime_rates <- c(Normal_Day = 1.375, Rest_Day = 1.859, 
                                  Special_Non_Working = 1.859, 
                                  Special_Non_Working_Rest_Day = 2.145, 
                                  Regular_Holiday = 2.86, 
                                  Regular_Holiday_Rest_Day = 3.718)
  
  adjusted_out_time <- ifelse(out_time < in_time, out_time + 2400, out_time)
  hours_worked <- (adjusted_out_time - in_time) / 100
  regular_hours <- min(hours_worked, max_hours)
  
  overtime_hours <- 0
  if (in_time < 1800 && adjusted_out_time > 1800) {
    overtime_hours <- min((min(adjusted_out_time, 2200) - 1800) / 100, hours_worked - regular_hours)
  }
  
  night_shift_hours <- 0
  if (adjusted_out_time > 2200 || in_time >= 2200 || adjusted_out_time <= 600) {
    night_shift_hours <- max(min(adjusted_out_time, 600 + 2400) - max(in_time, 2200), 0) / 100
  }
  
  night_shift_overtime_hours <- 0
  if (night_shift_hours > 0) {
    if (in_time < 1800 && in_time >= 0900) {
      if (!(in_time == 900 && adjusted_out_time == 1800)) {
        night_shift_overtime_hours <- min(night_shift_hours, hours_worked)
      }
    } else {
      night_shift_overtime_hours <- min(overtime_hours, night_shift_hours)
    }
  }
  
  base_pay <- (daily_rate / max_hours) * regular_hours * base_rates[day_type]
  
  overtime_rate <- overtime_rates[day_type]
  overtime_pay <- (daily_rate / max_hours) * overtime_hours * overtime_rate
  
  night_shift_overtime_rate <- night_shift_overtime_rates[day_type]
  night_shift_overtime_pay <- (daily_rate / max_hours) * night_shift_overtime_hours * night_shift_overtime_rate
  
  if (night_shift_hours > 0 && night_shift_overtime_hours > 0) {
    night_shift_hours <- night_shift_hours - night_shift_overtime_hours
  }
  night_shift_pay <- (daily_rate / max_hours) * night_shift_hours * night_shift_rate
  
  daily_pay <- base_pay + overtime_pay + night_shift_pay + night_shift_overtime_pay
  return(daily_pay)
}



display_daily_details <- function(employee_times) {
  for (day in 1:nrow(employee_times)) {
    in_time <- employee_times[day, "in_time"]
    out_time <- employee_times[day, "out_time"]
    day_type <- employee_times[day, "day_type"]
    max_hours <- 8
    
    adjusted_out_time <- ifelse(out_time < in_time, out_time + 2400, out_time)
    
    hours_worked <- (adjusted_out_time - in_time) / 100
    regular_hours <- min(hours_worked, max_hours)
    
    overtime_hours <- 0
    if (in_time < 1800 && adjusted_out_time > 1800) {
      overtime_hours <- min((min(adjusted_out_time, 2200) - 1800) / 100, hours_worked - regular_hours)
    } else if (in_time >= 1800 && adjusted_out_time <= 2200) {
      overtime_hours <- (adjusted_out_time - in_time) / 100
    }
    
    night_shift_hours <- 0
    if (adjusted_out_time > 2200 || in_time >= 2200 || adjusted_out_time <= 600) {
      night_shift_hours <- (min(2400, adjusted_out_time) - max(2200, in_time)) / 100
      if (adjusted_out_time > 2400) {
        night_shift_hours <- night_shift_hours + (adjusted_out_time - 2400) / 100
      }
    }
    
    night_shift_hours <- min(night_shift_hours, hours_worked)

    night_shift_overtime_hours <- 0
    if (night_shift_hours > 0) {
      if (in_time < 1800 && in_time>=0900)
        night_shift_overtime_hours <- min(night_shift_hours, hours_worked)
      else 
        night_shift_overtime_hours <- min(overtime_hours, night_shift_hours)
    }
    daily_pay <- calculate_daily_pay(in_time, out_time, day_type)
    
    cat("=================================\n")
    cat("Day", day ,"\nDaily Rate:", daily_rate, "\n")
    cat(sprintf("IN Time: %04d\n", in_time))
    cat(sprintf("OUT Time: %04d\n", out_time))
    cat("Day Type:", day_type, "\n")
    cat("Overtime Hours:", overtime_hours, "(", night_shift_overtime_hours, ")\n" )
    cat("Night Shift Hours:", night_shift_hours, "\n" )
    cat("Salary for the day:", daily_pay, "\n")
  }
}

calculate_weekly_pay <- function(employee_times) {
  weekly_pay <- 0
  for (day in 1:nrow(employee_times)) {
    daily_pay <- calculate_daily_pay(employee_times[day, "in_time"],
                                     employee_times[day, "out_time"],
                                     employee_times[day, "day_type"])
    weekly_pay <- weekly_pay + daily_pay 
  }
  return(weekly_pay)
}
main <- function() {
  repeat {
    cat("1. Show Weekly Pay\n")
    cat("2. Exit\n")
    choice <- readline(prompt = "Enter choice: ")
    if (!is.na(as.numeric(choice)) && choice %in% c("1", "2")) {
      choice <- as.integer(choice)
      if (choice == 1) {
        display_daily_details(employee_times)
        cat("=================================\n")
        weekly_pay <- calculate_weekly_pay(employee_times)
        cat("Total Weekly Pay: ", weekly_pay, "\n") 
        cat("=================================\n\n")
        cat("1. Edit Day\n")
        cat("2. Return\n")
        choice1 <- readline(prompt = "Enter choice: ")
        if (!is.na(as.numeric(choice1)) && choice1 %in% c("1", "2")) {  
          choice1 <- as.integer(choice1)
          if (choice1 == 1) {
            day <- as.integer(readline(prompt = "Enter day of week (1-7): "))
            choice2 <- readline(prompt = "[1]Day Shift (0900)\n[2]Night Shift (1800)\nEnter shift type: ")
            if (!is.na(as.numeric(choice2)) && choice2 %in% c("1", "2")) {  
              choice2 <- as.integer(choice2)
              if (choice2 == 1)  in_time <- 0900
              else if (choice2 == 2)in_time <- 1800
            }
            out_time <- as.integer(readline(prompt = "Enter OUT Time: "))
            cat("[1]Normal Day\n[2]Rest Day\n[3]Special Non-Working Day\n[4]Special Non Working Rest Day\n[5]Regular Holiday\n[6]Regular Holiday Rest Day\n")
            day_type <- as.integer(readline(prompt = "Enter Day Type: "))
            day_type <- switch(day_type, "Normal_Day", "Rest_Day", "Special_Non_Working", "Special_Non_Working_Rest_Day", "Regular_Holiday", "Regular_Holiday_Rest_Day", NA)
            if (!is.na(day_type)) {
              employee_times <<- input_time_data(employee_times, day, in_time, out_time, day_type)
            } else {
              cat("Invalid day type.\n")
            }
          }
          else main()
        } else {
          cat("Invalid choice. Please enter 1 or 2.\n")
        }
      } else if (choice == 2) {
        break
      }  
    } else {
      cat("Invalid choice. Please enter 1 or 2.\n") 
    }
  }
}

 main()

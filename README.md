# Employee Payroll Calculator
<p align="center">

## Description
The Employee Payroll Calculator is an R script designed to compute and display employee payroll details based on daily work hours, overtime, night shifts, and various day types. This tool helps in calculating daily and weekly wages, accounting for regular and overtime hours, night shifts, and special days such as rest days and holidays. It provides an interactive interface to input and update time data, and calculate payroll efficiently. This is a project for one of our subjects in College.

## Features

- **Daily Pay Calculation:** Computes daily wages based on regular hours, overtime, night shift hours, and special day rates.

- **Overtime and Night Shift Handling:** Accurately calculates overtime and night shift pay using predefined rates.

- **Flexible Time Input:** Allows users to input and adjust time data, including in-time, out-time, and day type.

- **Weekly Pay Summary:** Provides a summary of weekly pay by aggregating daily wage calculations.

- **Interactive Interface:** Features a user-friendly command-line interface to view and edit payroll details.

## Requirements

- R 4.x or later

### Running the Script
To execute the script, follow these steps:

1. **Open R or RStudio** on your system.

2. **Load the script** by navigating to the file's location and sourcing it:
   ```R
   source("MP_R.R")
   ```

3. **Run the `main` function** to start the interactive payroll calculator:
   ```R
   main()
   ```

### Functionality Overview

- **`input_time_data()`**: Update employee time data for a specific day.
- **`calculate_daily_pay()`**: Compute daily pay based on time data and day type.
- **`display_daily_details()`**: Show detailed daily pay calculations and work hours.
- **`calculate_weekly_pay()`**: Summarize total weekly pay.
- **`main()`**: Provides an interactive menu for managing and calculating payroll.

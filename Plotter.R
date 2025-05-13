# libraries used:
# dplyr, tidyr, ggplot2, readr


# notes
# use ggplot2 to save graph - see documentation again?
# write to file not working - do you need to reenter library into console?


# population set to 1000
# beta set to 0.3 - infection rate
# gamma set to 0.1 - recovery rate
n <- 1000
beta <- 0.3
gamma <- 0.1

days <- 50
x <- 1:days
current_day <- 0
# initialising numeric vectors to hold all the values
S <- numeric(days)
I <- numeric(days)
R <- numeric(days)

# starting outbreak with 10 infected individuals 
I[1] <- 10
R[1] <- 0
S[1] <- n - I[1]

# go through each day and simulate new infections and recoveries based on model
for (t in 2:days) {
  
  new_infected <- beta * S[t-1] * I[t-1] / n
  new_recovered <- gamma * I[t-1]

  S[t] <- max(0, S[t-1] - new_infected)
  I[t] <- max(0, I[t-1] + new_infected - new_recovered)
  R[t] <- max(0, R[t-1] + new_recovered)
  
  
}
# create data frame
SIR_data <- data.frame(
  Day = x,
  Susceptible = round(S),
  Infected = round(I),
  Recovered = round(R),
  Beta = beta,
  Gamma = gamma
)

# convert data frame into long format so it can be plotted - surely if you created the data in the correct format originally this wouldn't be necessary?
SIR_long_format <- pivot_longer(SIR_data, cols = c(Susceptible, Infected, Recovered), names_to = "Compartment", values_to = "Count")

# create plot using ggplot2
viz <- ggplot(data = SIR_long_format, aes(x=Day, y=Count, color = Compartment)) + geom_line() + labs(title="Outbreak", subtitle="Hypothetical pathogen over 50 days", x = "Day Number", y = "People") + scale_color_manual(values = c(Infected = "red", Susceptible = "green", Recovered = "blue"))
viz





# write to file
write_csv(SIR_data, "History.csv")

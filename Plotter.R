
# population set to 35000
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

I[1] <- 10
R[1] <- 0
S[1] <- n - I[1]


for (t in 2:days) {
  
  new_infected <- beta * S[t-1] * I[t-1] / n
  new_recovered <- gamma * I[t-1]

  S[t] <- max(0, S[t-1] - new_infected)
  I[t] <- max(0, I[t-1] + new_infected - new_recovered)
  R[t] <- max(0, R[t-1] + new_recovered)
  
  #stopifnot(S[t] + I[t] + R[t] == n)
}

plot(x, I, type='l', col = "red", lwd = 2, ylim = c(0,n), xlab="Days", ylab="People")
lines(x, S, col="blue", lwd = 2)
lines(x, R, col="green", lwd = 2)
legend("left", legend = c("Infected", "Susceptible", "Recovered"), col = c("red", "blue", "green"), lwd = 2)

# create data frame
data <- data.frame(
  Day = x,
  Susceptible = round(S),
  Infected = round(I),
  Recovered = round(R),
  Beta = beta,
  Gamma = gamma
)
# write to file
write_csv(data, "History.csv")

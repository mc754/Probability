# Calculating number of loans for 1% probability of losing money
loss_per_foreclosure <- -200000
l <- loss_per_foreclosure
x <- 0.05*180000            #revenue_per_loan = interest rate * loan amount
p <- 0.04                   #default probability
z <- qnorm(0.01)
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)

# Monte Carlo simulation with unknown default probability
B <- 10000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})
mean(profit)
mean(profit < 0)
mean(profit < -10000000)  
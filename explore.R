# Set the working directory
setwd("C:/Users/Dmitriy/Documents/R-Practice/outliers")
source("explore.quantiles.R")

# Testing show.outliers
show.outliers(rnorm(1000), 0.99)

# Testing unif.quantiles.rescaled
unif.quantiles.rescaled(c(0.25, 0.5, 0.75), 10, 20)

# n -  humber of simulations for all distributions
n <- 1000 # for all distributions
# ml, sdl: meanlog and sdlog for lognormal distribution
ml <- 0
sdl <- 1
# rlnorm(n, meanlog = 0, sdlog = 1)
xvals <- rlnorm(n, meanlog = ml, sdlog = sdl)
explore(xvals, nf = 10, title = "Log-Normal(meanlog=1, sdlog=1)", m = "RO", f = 0.5)


manipulate (
  (
    # d: Distribution, n: number of random values, m: mean, s: standard deviation
    # nf: number of quantile frequencies, mt: method - RO, Fraction, Q-Q
    # nm: number of mulinormal distributions
    # uo: 0-10, step 1, initial 0 - number of outliers for uniform distribution
    function(s, nf, mt, f, nm, d, uo) {  
      m <- 0 # Zero Mean
      n <- 1000 # Change the number of random values here
      set.seed(123456789)
      xvals <-
        if (d == "Normal") {print ("Normal");rnorm(n, m, s)}
        else if (d == "Multinormal") {print ("Multinormal");rnorm(n, seq(m, 10, length.out = nm), s)}
        else if (d == "Lognormal") {print ("Lognormal");rlnorm(n, meanlog = m, sdlog = s)}
        else {
          step <- 5
          d <- paste("Uniform: ", uo, " outliers, with ", step, "% step")
          print (d)
          runif.n.outliers (n = n, min = 0, max = 1, uo = uo, uop = step)
      }
      print (paste("distribution=", d, ", method=", mt, "mean=", m, ", sd=",s))
      explore(xvals, nf = nf, title = "", m = mt, f = f, d = d, nmods = nm)
    }
  ) (sd.rvals, qfreq.rvals, method, fraction, nmodes, distr, unif.outliers),
    #n.rvals = slider(1000, 10000,step = 1000, initial = 1000),
    
    sd.rvals = slider(0, 10, step = 1, initial = 1),
    qfreq.rvals = slider(5, 100, step = 1, initial = 5),
    fraction = slider(0, 1, step = 0.05, initial = 0.5),
    method = picker("RO", "Fraction", "Q75-Q75", "IQR75", "Trimmed-LogN"),
    nmodes = slider(2, 10, step = 1, initial = 2), # number of modes for multinormal distribution
    distr = picker("Normal", "Multinormal", "Lognormal", "Uniform"),
    unif.outliers = slider(0, 10, step = 1, initial = 0)
)

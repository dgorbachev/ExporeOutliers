################ Manipulate. Normal Plots #################
library(manipulate)
library(ggplot2)
library(quantreg)

# Displays distribution outliers: number and at most 5 values
get.outliers <- function(x, q) {
  outliers <- sort(round(x[x>q], 4))
  outliers
}

show.outliers <- function(x, q) {
  outliers <- get.outliers(x, q)
  outliers.num <- length(outliers)
  if (outliers.num == 0) 'No outliers' else {
    paste('Outliers (', as.character(outliers.num), '): ', 
          paste(
            if (outliers.num > 5) outliers[1:5] 
            else outliers, collapse = ', '
          )
    )
  }
}

# Rescales quantiles of Uniform[0: 1] distribution to Uniform[minval: maxval]
unif.quantiles.rescaled <- function(f, minval, maxval) {
  minval + f * (maxval - minval)
}

# returns n uniformly distributed values
# where uo: number of outliers, uop as percentage of range - outliers step
runif.n.outliers <- function(n, min = 0, max = 1, uo = 0, uop = 5) {
  if ( uo == 0 ) runif(n, min, max) else {
    n.fix <- n - uo
    step = uop*(max - min)/100
    outliers <- seq(from = max + step, by = step,length.out= uo )
    c(runif(n, min, max), outliers)
  }

  
}

# xvals - Empirical Distribution, 
# nf - number of quantile frequencies for RO method, m - method name
# f - adjusted fraction for Fraction method
# d - the name of empirical distribution
# nmods - number of mods for multinormal distribution
explore <- function(xvals, nf = 10, title = '', m = "RO", f = 0.5, d = '', nmods=1) {  
      #### Empirical Distribution and related numeric quantities ####
      # xvals: Random values from simulated Empirical Distribution: 
      #        n, m, sd - user input, q99 - 99% quantile
      xvals.range <- range(xvals)
      xvals.min <- xvals.range[1]
      xvals.max <- xvals.range[2]
      xvals.q50 <- quantile(xvals, 0.50)
      xvals.q99 <- quantile(xvals, 0.99)
      xvals.q75 <- quantile(xvals, 0.75)
      xvals.iqr <- IQR(xvals)
      xvals.mean <- mean(xvals)
      xvals.sd <- sd(xvals)
      xvals.length <- length(xvals)
      outliers.q99 <- get.outliers(xvals, xvals.q99)
      outliers.q99.length <- length(outliers.q99)
      
      # U - Distribution 75% quantile (U-75) rescaled to xvals range
      unif.q75 <- unif.quantiles.rescaled(c(0.75), xvals.min, xvals.max)
      # The ratio of xvals 75% quantile to U-75% quantile (U-75)
      xvals.q75fr <- xvals.q75 / unif.q75
      print(paste('xvals.q75=',xvals.q75, 'unif.q75=', unif.q75, 'xvals.q75fr=', xvals.q75fr))
      # quantile frequencies: p - user input
      freqs <- seq(0, 1, by=1/nf)
      # Uniform[minval: maxval] quantiles
      unif.quantiles <- unif.quantiles.rescaled(freqs, xvals.min, xvals.max)
      # Empirical quantiles for freqs frequencies
      xvals.quantiles <- quantile(xvals, freqs)
      # Differences between scaled uniform and empirical quantiles
      unif.diff <- (unif.quantiles - xvals.quantiles)
      # Inner Product of the vector of Differences
      unif.diff.prod <- (unif.diff %*% unif.diff) [1, 1]
      # Eucleadian distance between uniform and empirical quantiles
      unif.dist <- sqrt(unif.diff.prod)
      # Eucleadian norm of Uniform[minval: maxval] quantiles vector 
      unif.eucleadian.norm <- sqrt((unif.quantiles %*% unif.quantiles) [1, 1])
      xvals.eucleadian.norm <- sqrt((xvals.quantiles %*% xvals.quantiles) [1, 1])
      # ro shows how far is the simulated empirical distribution
      # from the Uniform[minval: maxval] distribution
      ro <- xvals.eucleadian.norm / unif.eucleadian.norm
      p <- (xvals.q99 - xvals.min) / ( xvals.max -  xvals.min)

      ro.pValue <- pbinom(outliers.q99.length , xvals.length, 1 - p, lower.tail = TRUE)
      kappa <- (1-p)/0.01
      print(paste("nrec=", xvals.length, ", outliers = ",outliers.q99.length,", p=", p, ", pValue=", ro.pValue, ", kappa = ", round(kappa,4)))
      # Adjusted cut-off quantile
      xvals.q99.adj <- 
        if (m == "RO") {
          print("RO: calculating xvals.q99.adj")
          freq.adj <- 0.99 + 0.01 * min(ro, 1) # for a uniform distribution or worse
          print (paste("ro=", ro,",freq.adj=", freq.adj))
          quantile(xvals, freq.adj)
        }
        else if (m == "Fraction") {
          print("Fraction: calculating xvals.q99.adj")
          freq.adj <- 0.99 + 0.01 * min(f, 1)
          quantile(xvals, freq.adj)
        } else if (m == "Q75-Q75") {
          print("Q75-Q75: calculating xvals.q99.adj")
          freq.adj <- 0.99 + 0.01 * min(xvals.q75fr, 1)
          print(paste('xvals.q75=',xvals.q75, 'unif.q75=', unif.q75, 'xvals.q75fr=', xvals.q75fr, 'freq.adj=',freq.adj))
          quantile(xvals, freq.adj)
        } 
        else if (m == "IQR75") {
          iqr75 <- xvals.q75 + 1.5*xvals.iqr
          print(paste("IQR75: calculating 75% + 1.5*IQR: ", iqr75))
          iqr75
        }
        else if (m == "Trimmed-LogN") {
          trim.n <- min(round(log2(xvals.length)),length(outliers.q99))
          trim.q <- tail(outliers.q99, trim.n)[1]
          print(paste("Trimmed-Log-2-N: ", trim.q))
          trim.q
        } 
        else { # defaulting to 99% precentile
          print("99%: calculating xvals.q99.adj")
          xvals.q99
        }
      
      
      #### Plotting the density of the Empirical Distribution with the outliers
      # Title: 99% quantile and cutoff quantile
      plot.title <- paste('Range [', round(xvals.min, 4), ',', round(xvals.max, 4), '],',
                          'Med = ', 
      as.character(round(xvals.q50, 4)),
      ', Mean = ', as.character(round(xvals.mean, 4)),
      ', SD = ', as.character(round(xvals.sd, 1)),
      ', 99% qt = ', as.character(round(xvals.q99, 4)),
      ', cutoff = ', as.character(round(xvals.q99.adj, 4)))
      #plot.title <- if (length(title)>2) paste(title, ': ', plot.title) else plot.title
      
      # Subtitle: 99% percentile outliers and adjusted outliers
      plot.subtitle <- paste(show.outliers(xvals, xvals.q99), ', adjusted: ',
                             show.outliers(xvals, xvals.q99.adj))
      plot.distribution.descr <- paste(
        if (d == 'Multinormal') {
          paste('Multinormal(', nmods, ')')
        }
        else {
          d
        }
      )
      ro.descr <- paste("RO(", round(ro, 4), "): p-Val = ", round(ro.pValue,4), ", kappa = ", round(kappa, 4))
      plot.method.descr <- paste(
        if(m == "Fraction") {
          paste('Fraction(',f,')', round(0.99 + 0.01 * f, 4) )
        } 
        else if(m == "Q75-Q75") {
          paste('Q75-Q75(',xvals.q75fr,')', round(0.99 + 0.01 * xvals.q75fr, 4) )
        } 
        else if (m == "RO") {
          ""
        }
        else {
          m
        }
      )
      plot.method.descr <- paste(plot.method.descr, if (m == "RO") "" else ", ", ro.descr)
      plot.subtitle2 <- paste('Distribution:', plot.distribution.descr, ', Method:', plot.method.descr)
      
      cols <- c("mean"="yellow","99% quantile"="blue","cutoff quantile"="red")
      # Main plot
      ggplot(data = data.frame(notional = xvals),
             aes(x=notional, y=..density..)) +
        geom_density(alpha = .6, fill = "grey",
                     aes(x=notional, y=..density..)) +
        geom_rug(aes(x=notional, y=0, color=notional>xvals.q99.adj)) +
        scale_color_manual(name='> cutoff',values=setNames(c('red', 'green'), c(T, F))) +
        #scale_color_manual(name='Statistics',values=setNames(c('yellow','red', 'green'), c('M','T', 'F'))) +
        scale_x_discrete(limits=round(c(xvals.q50, xvals.q99, xvals.q99.adj), 4)) +
        theme(axis.text.x = element_text(size = 10,angle=45)) +
        geom_vline(aes(xintercept=xvals.q99), color = "blue",
                   linetype = "dotted", size = 0.5) +
        geom_vline(aes(xintercept=xvals.q99.adj), color = "red",
                   linetype = "dashed", size = 0.5) +        
        geom_vline(aes(xintercept=xvals.q50),
                   color = "yellow", size = 1) +
        #ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), ""))))
        ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), atop(italic(.(plot.subtitle2)), ""))    )))
    }


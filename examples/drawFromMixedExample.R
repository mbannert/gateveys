# n is the number of draws from ALL distributions
w <- drawFromMixed(100, list(rnorm = list(mean = 1000, sd = 250),
                             rexp = list(),
                             rchisq = list(df = 10)),
                             c(0.1, 0.5, 0.4))

w1 <- unlist(w) 
names(w1) <- NULL
w1
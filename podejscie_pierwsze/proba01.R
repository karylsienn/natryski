fr <- function(x) sum(x)
gr <- function(x) rep(1, times = 7)
theta <- rep(2, times = 7)

r <- 3.4
P <- 200

ui <- matrix(c(1/4, 3/8, 1/2, 5/8, 3/4, 7/8, 1,
               as.vector(diag(rep(1, times = 7)))), 
             ncol = 7, nrow = 8, byrow = TRUE)

ci <- c(0.95 * P/(pi*r^2), rep(0, times = 7) ) 

.constrOptim <- constrOptim(theta, fr, gr, ui, ci, mu = 1e-06)


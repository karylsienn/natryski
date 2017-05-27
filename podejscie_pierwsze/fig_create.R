n <- 6
srodek_ <- list(x = 2, y = 4)
R_min <- 8
R_max <- 8
R <- runif(n, R_min, R_max)


alpha <- sort(runif(n, .Machine$double.eps, 2*pi))
wielokat <- tibble(x = srodek_$x + c(R*cos(alpha), R[1]*cos(alpha[1])),
                   y = srodek_$y + c(R*sin(alpha), R[1]*sin(alpha[1])),
                   nr = as.integer(c(seq_len(n), 1)))

saveRDS(wielokat, file = "wielokat.Rds")
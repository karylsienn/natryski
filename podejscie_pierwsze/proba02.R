# Wielokąt o n wierzchołkach. 
n <- 6
srodek_ <- list(x = 2, y = 4)
R_min <- 2
R_max <- 8
R <- runif(n, R_min, R_max)


alpha <- sort(runif(n, .Machine$double.eps, 2*pi))
wielokat <- tibble(x = srodek_$x + c(R*cos(alpha), R[1]*cos(alpha[1])),
                   y = srodek_$y + c(R*sin(alpha), R[1]*sin(alpha[1])))


lens <- sapply(2:nrow(wielokat), function(i) {
  p1 <- wielokat[i-1, ]
  p2 <- wielokat[i, ]
  len <- binhf::norm(p1, p2)
  return(len)
})
  
poz_ <- lens %>% which.max

plot(NA, NA, xlim = c(-10, 10), ylim = c(-10, 10))
lines(wielokat, type = "l")
abline(h = 0, v = 0, col = "red")

# kładziemy na boku (poz_, poz_+1)
p1 <- wielokat[poz_, ]
p2 <- wielokat[poz_ + 1, ]

points(p1, col = "blue")
points(p2, col = "green")

wielokat1 <- apply(wielokat, 1, function(s) s - p1) %>%
  unlist
wielokat1 <- tibble(x = wielokat1[which(names(wielokat1) == "x")],
                    y = wielokat1[which(names(wielokat1) == "y")])
  
r_new <- binhf::norm(c(0,0), p2)
beta <- atan( (p1$y - p2$y) / (p1$x - p2$x) )

lines(wielokat1, type = "l", col = "green")

mac_obrotu <- matrix(c(cos(beta), -sin(beta), sin(beta), cos(beta)), 
                     nrow = 2)

wielokat2 <- matrix(ncol = 2)
for (i in 1:nrow(wielokat1)) {
  punkt_old <- wielokat[i, ]
  punkt_new <- t(mac_obrotu %*% t(punkt_old))
  wielokat2 <- rbind(wielokat2, punkt_new)
}

lines(wielokat2, col = "red")

max_min <- raster::extent(wielokat)

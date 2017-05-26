require(tibble)
require(ggplot2)
require(magrittr)
require(dplyr)
require(purrr)

n <- 6
srodek_ <- list(x = 2, y = 4)
R_min <- 2
R_max <- 8
R <- runif(n, R_min, R_max)


alpha <- sort(runif(n, .Machine$double.eps, 2*pi))
wielokat <- tibble(x = srodek_$x + c(R*cos(alpha), R[1]*cos(alpha[1])),
                   y = srodek_$y + c(R*sin(alpha), R[1]*sin(alpha[1])),
                   nr = as.integer(c(seq_len(n), 1)))


ggp <- ggplot(data = wielokat) +
  geom_polygon(
    mapping = aes(x = x, y = y), 
    fill = "red", alpha = 0.5) +
  geom_path(mapping = aes(x = x, y = y)) +
  geom_point(mapping = aes(x = x, y = y, colour = nr)) +
  geom_hline(mapping = aes(yintercept = 0)) +
  geom_vline(mapping = aes(xintercept = 0))


krawedzie <- tibble(xstart = double(), xend = double(),
                    ystart = double(), yend = double(),
                    norma = double())

for (i in 2:nrow(wielokat)) {
  krawedzie %<>%
  add_row(xstart = wielokat[[i-1, "x"]], xend = wielokat[[i, "x"]],
          ystart = wielokat[[i-1, "y"]], yend = wielokat[[i, "y"]],
          norma = binhf::norm(c(xstart, ystart), c(xend, yend))) 
}

poz <- krawedzie %>% 
  select(norma) %>% 
  unlist %>% 
  which.max %>%
  unname

show(ggp)

fun <- function(xx, string) xx - krawedzie[poz, string]
  

wielokat %<>% 
  mutate(
    x = map(.$x, fun, string = "xstart") %>% 
      unlist %>% unname
    ) %<>%
  mutate(
    y = map(.$y, fun, string = "ystart") %>%
      unlist %>% unname
  )



ggp <- ggp + 
  geom_polygon(
    mapping = aes(x = x, y = y), 
    fill = "blue", alpha = 0.5, data = wielokat) +
  geom_path(mapping = aes(x = x, y = y), data = wielokat) +
  geom_point(mapping = aes(x = x, y = y, colour = nr), data = wielokat)

show(ggp)


krawedzie %<>%
  mutate_at(
     c("xstart", "xend"), function(x) map(x, fun, string = "xstart") %>%
       unlist %>% unname
  ) %<>%
  mutate_at(
    c("ystart", "yend"), function(x) map(x, fun, string = "ystart") %>%
      unlist %>% unname
  )

fun2 <- function(poz) (krawedzie[poz, "ystart"] - krawedzie[poz, "yend"]) / (krawedzie[poz, "xstart"] - krawedzie[poz, "xend"])

beta <- atan(fun2(poz)) %>% unlist %>% unname
M <- matrix(c(cos(beta), -sin(beta), sin(beta), cos(beta)), ncol = 2)

for (i in wielokat %>% nrow %>% seq_len) {
  
  wielokat[i, c("x", "y")] %<>% 
    unlist %>% unname %>% 
    matrix %>% `%*%`(M, .) %>% t  
  
}

ggp <- ggp + 
  geom_polygon(
    mapping = aes(x = x, y = y), 
    fill = "green", alpha = 0.5, data = wielokat) +
  geom_path(mapping = aes(x = x, y = y), data = wielokat) +
  geom_point(mapping = aes(x = x, y = y, colour = nr), data = wielokat)

show(ggp)

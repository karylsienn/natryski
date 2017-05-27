wielokat <- readRDS("wielokat.Rds")

ggp <- ggplot(data = wielokat) +
  geom_polygon(
    mapping = aes(x = x, y = y), 
    fill = "yellow", alpha = 0.5) +
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
    fill = "blue", alpha = 0.3, data = wielokat) +
  geom_path(mapping = aes(x = x, y = y), data = wielokat) +
  geom_point(mapping = aes(x = x, y = y, colour = nr), data = wielokat)



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

end <- krawedzie[poz, c("xend", "yend")] %>% 
  unlist %>% unname %>% 
  matrix %>% `%*%`(M, .) %>% t %>%
  .[,1]

if (end < 0) {
  M <- -M
}

for (i in wielokat %>% nrow %>% seq_len) {
  wielokat[i, c("x", "y")] %<>% 
    unlist %>% unname %>% 
    matrix %>% `%*%`(M, .) %>% t  
}

ggp <- ggp + 
  geom_polygon(
    mapping = aes(x = x, y = y), 
    fill = "green", alpha = 0.5, data = wielokat
    ) +
  geom_path(mapping = aes(x = x, y = y), data = wielokat) +
  geom_point(mapping = aes(x = x, y = y, colour = nr), 
             data = wielokat)

w_extent <- raster::extent(wielokat)
wielokat$x <- wielokat$x + abs(w_extent@xmin)
wielokat$y <- wielokat$y + abs(w_extent@ymin) # przy założeniu wypukłości ymin = 0. 

ggp <- ggp + 
  geom_polygon(
    mapping = aes(x = x, y = y), 
    fill = "red", alpha = 0.93, data = wielokat
  ) +
  geom_path(mapping = aes(x = x, y = y), data = wielokat) +
  geom_point(mapping = aes(x = x, y = y, colour = nr), 
             data = wielokat)

show(ggp)

saveRDS(wielokat, file = "wielokat_trans.Rds")
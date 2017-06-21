# wielokat <- readRDS("wielokat_trans.Rds")

# Promień okręgu, którym dysponujemy.
R <- 8.5

# Wysokość trójkąta
h <- sqrt(3)*R

# Współczynniki prostych
a1 <- tan(pi/3)
a2 <- tan(2*pi/3)

# Punkty ekstremalne zbioru
ext <- raster::extent(wielokat)

# Przesunięcie współczynnika wolnego
transl <- -12.5

hh <- 0.9*h

# Liczba prostych równoległych do osi X

na1_up <- floor(abs(ext@ymax - a1*ext@xmin ) / (2*hh))
na1_dwn <- floor(abs(ext@ymin - a1*ext@xmax) / (2*hh))

na2 <- floor(abs(ext@ymax - a2*ext@xmax) / (2*hh))

a1s <- tibble(.intercept = seq(-na1_dwn, na1_up)*2*hh + transl, slope = a1)
a2s <- tibble(.intercept = seq(0, na2)*2*hh + transl, slope = a2)

ggp <- ggplot() +
  geom_polygon(
    mapping = aes(x = x, y = y),
    data = wielokat
  ) +
  geom_abline(
    mapping = aes(slope = slope,
                  intercept = .intercept),
    data = a1s
  ) +
  geom_abline(
    mapping = aes(slope = slope,
                  intercept = .intercept),
    data = a2s
  ) +
  geom_path(
    mapping = aes(x = x, y = y),
    data = tibble(
      x = c(ext@xmin, ext@xmin, ext@xmax, ext@xmax, ext@xmin),
      y = c(ext@ymin, ext@ymax, ext@ymax, ext@ymin, ext@ymin)
      ),
    colour = "green"
    )

denom <- a2 - a1
TD <- expand.grid(a1s$.intercept, a2s$.intercept)

new.TD <- tibble(
  x = apply(TD, 1, function(row) -(row[2] - row[1])/denom),
  y = a1*x + TD$Var1
  )

pip <- apply(
  X = new.TD,
  MARGIN = 1,
  FUN = function(row) 
    sp::point.in.polygon(row[1], row[2], wielokat$x, wielokat$y)
  )

new.TD$pip <- factor(pip, levels = c(0, 1))

circles <- new.TD %>% subset(pip == 1) %>%
  select(c(x,y)) %>% mutate(R = R)

ggp <- ggplot() +
  geom_polygon(
    mapping = aes(x = x, y = y),
    data = wielokat,
    fill = "black", alpha = 1,
    colour = "blue", size = 0.8
    ) +
  geom_circle(
    mapping = aes(x0 = x, y0 = y, r = R),
    data = circles,
    colour = "white", fill = "white",
    alpha = 0.9
  ) + theme_void()

# ggsave("rys04.jpeg", plot = ggp, device = "jpeg")

# readr::write_csv(wielokat, path = "wielokat01.csv")
# readr::write_csv(circles, path = "circles01.csv")

show(ggp)
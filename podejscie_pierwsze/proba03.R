wielokat <- readRDS("wielokat_trans.Rds")



ggp <- ggplot(data = wielokat,
              mapping = aes(x = x, y = y)) +
  geom_polygon(fill = "red", alpha = 0.7) +
  geom_hline(mapping = aes(yintercept = intercept), data = )

show(ggp)

# Promień okręgu, którym dysponujemy.
R <- 5
h <- sqrt(3)/2 * R

ext <- raster::extent(wielokat)

point_rot <- c((ext@xmin + ext@xmax) / 2, 
               (ext@ymin + ext@ymax) / 2)

alpha <- pi/6
w <- apply(wielokat, 1, function(row) rotate_point((row[c(1,2)] %>% as.matrix),
                                                   point_rot, alpha))


# znajdujemy punkty ekstremalne zbioru i mamy ograniczenie dla prostych dla alpha = 0
# obracamy figurę o pi/3 w prawo i to samo, potem wracamy do alpha = 0 i wyliczamy b za pomocą równań ze znalezionymi punktami
# limitem jest bmin, bmax. podobnie obracamy o pi/6 w lewo i mamy. 
# następnie liczymy przecięcia tych prostych i sprawdzamy, który punkt znajduje się w środku wielokąta. 
# na przecięciach rysujemy koła.


# Obróć figurę o kąt alpha, względem wskazanego punktu tej figury.
# point - wektor wartosci c(x, y) który obracamy
# point_rot - wektor punktu względem którego obracamy
# alpha - kąt obrotu
# clockwise - logical, jeśli TRUE obrót zgodny ze wskazówkami zegara,
#             jeśli FALSE - przeciwny
rotate_point <- function(point, point_rot, alpha, clockwise = TRUE) {
  
  x <- point_rot[1]
  y <- point_rot[2]
  
  H1 <- matrix(c(1, 0, -x,
                 0, 1, -y,
                 0, 0, 1), nrow = 3, ncol = 3, byrow = TRUE)
  
  H2 <- -matrix(c(cos(alpha), -sin(alpha), 0,
                 sin(alpha), cos(alpha), 0,
                 0, 0, 1), nrow = 3, ncol = 3, byrow = TRUE)
  
  if(!clockwise) H2 <- -H2
  
  H3 <- matrix(c(1, 0, x,
                 0, 1, y,
                 0, 0, 1), nrow = 3, ncol = 3, byrow = TRUE)
  
  point <- c(point, 1)
  
  new_point <- ( H3 %*% H2 %*% H1 %*% point ) %>% as.vector(mode = "numeric")
  
  return(new_point[c(1,2)])
  
}



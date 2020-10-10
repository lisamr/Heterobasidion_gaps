#figuring out how to convert azimuth/distances to x/y

test <- data.frame(plot = 1, theta = c(0, 5, 90, 95, 180, 185, 270, 275), d = 1)
test2 <- test %>% mutate(plot = 2)
tests <- bind_rows(test, test2)

#split by plot, apply function
L <- split(tests, f = tests$plot)

f <- function(data){
  data %>% 
    mutate( y_multiplier = case_when(
      theta >90 & theta <180 ~ -1,
      theta >=180 & theta <270 ~ -1,
      T ~ 1),
      radians = theta * pi/180,
      x = sin(radians) * d,
      y = sqrt(d^2 - x^2)*y_multiplier) %>% 
    select(-y_multiplier)
}

lapply(L, f)


 
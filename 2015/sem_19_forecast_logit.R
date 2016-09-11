library("mfx")
library("Ecdat")
library("dplyr")
library("broom")

glimpse(Catsup)

catsup2 <- mutate(Catsup,
  y = ifelse(choice == "hunts32", 1, 0))
model <- logitmfx(data = catsup2,
      y ~ price.heinz41 + price.heinz32 + price.heinz28 + price.hunts32)
model # marginal effects


fit <- model$fit
summary(fit) # coefficients

new_prices <- data_frame(
  price.heinz41 = c(3, 5),
  price.heinz32 = c(3, 5),
  price.heinz28 = c(3, 5),
  price.hunts32 = c(3, 5)
)
# broom
new_prices2 <- augment(fit, newdata = new_prices)
new_prices2
new_prices3 <- mutate(new_prices2,
  left1 = plogis(.fitted - 2*.se.fit),
  right1 = plogis(.fitted + 2*.se.fit),
  p_hat = plogis(.fitted),
  se_p = dlogis(.fitted) * .se.fit,
  left2 = p_hat - 2*se_p,
  right2 = p_hat + 2*se_p
)

# лирическое отступление про мешочный синтаксис:
model$fit$control$epsilon

meshok <- list(a = 500, catsup = catsup2,
               inside = list(privet = "Привет, Аня"))
meshok$inside$privet


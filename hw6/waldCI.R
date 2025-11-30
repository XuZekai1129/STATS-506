setClass(
  "waldCI",
  slots = c(
    estimate = "numeric",
    se       = "numeric",
    level    = "numeric",
    lower    = "numeric",
    upper    = "numeric"
  )
)

makeWaldCI <- function(fun, data, seFun, level = 0.95) {
  est <- fun(data)
  se  <- seFun(data)
  z   <- qnorm(1 - (1 - level) / 2)
  ci  <- est + c(-1, 1) * z * se
  
  new(
    "waldCI",
    estimate = est,
    se       = se,
    level    = level,
    lower    = ci[1],
    upper    = ci[2]
  )
}


library(ggplot2)

xs <- seq(10, 35, length.out=100)

get_t_plot <- function(data, col) {
  m <- mean(data)
  d <- sd(data)
  n <- length(data)
  f <- function(x) {dt((x - m)*sqrt(n-1)/d,n-1)}
  ys <- f(xs)
  d <- data.frame(x=xs, y=ys)
  geom_path(data=d, mapping=aes(x=x, y=y), colour=col)
}

get_dispersion_plot <- function(data, col) {
  d <- data.frame(x=data, y=0)
  geom_point(data=d, aes(x=x, y=y), alpha=0.2, colour=col)
}

add_plots <- function(p, data, transmision, col) {
  d <- data[data$transmision==transmision,]$mpg
  p <- p + get_dispersion_plot(d, col)
  p <- p + get_t_plot(d, col)
  p
}

get_test_plot <- function(data) {
  p <- ggplot()
  p <- add_plots(p, data, "auto", "red")
  p <- add_plots(p, data, "manual", "blue")
  p <- p + labs(title="Manual vs Transmission in car milleage", x="mpg", y="")
  p
}

get.logistic <- function(model) {
  b <- model$coefficients[[1]]
  a <- model$coefficients[[2]]
  function(x) {
    l <- a * x + b
    1 / (1 + exp(-l))
  }
}

get.linear <- function(model) {
  b <- model$coefficients[[1]]
  a <- model$coefficients[[2]]
  function(x) 
    a * x + b
}

plot.curve <- function(f, x0=10, x1=35, l=100) {
  x <- seq(x0, x1, length.out=l)
  data <- data.frame(x=x, y=f(x))
  geom_path(data=data, mapping=aes(x=x,y=y), size=1)
}

plot.resid <- function(x, y, f, colour="red") {
  df <- data.frame(x1=x, y1=f(x), x2=x, y2=y)
  geom_segment(aes(x=x1, y=y1, xend=x2, yend=y2), colour=colour, data=df)
}

fit.logistic <- glm(am ~ mpg, family="binomial", mtcars)
fit.linear <- lm(am ~ mpg, mtcars)

get_regresion_plot <- function(fit.type, plot.resid) {
  p <- ggplot(data=mtcars, mapping=aes(x=mpg, y=am)) + geom_point()
  if (fit.type == "linear")
    f <- get.linear(fit.linear)
  else
    f <- get.logistic(fit.logistic)
  p <- p + plot.curve(f)
  if (plot.resid)
    p <- p + plot.resid(mtcars$mpg, mtcars$am, f)
  p
}
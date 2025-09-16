# dat <- data.frame(x = 1:5, y = 10 * exp(0.3 * 1:5) + rnorm(5, 0, 2), uiw = 10 * exp(0.3 * 1:5) + rnorm(5, 0, 2) + (0.1 * (10 * exp(0.3 * 1:5) + rnorm(5, 0, 2)) + 1), liw = 10 * exp(0.3 * 1:5) + rnorm(5, 0, 2) - (0.1 * (10 * exp(0.3 * 1:5) + rnorm(5, 0, 2)) + 1))
# uiw <- dat$uiw

plotCI <- function (
    x = dat
    , 
    y = NULL
    , 
    uiw = dat$uiw
    , 
    liw = uiw
    , 
    sfrac = 0.01
    ){
  
  if (is.list(x)) {
    y <- x$y
    x <- x$x
  }
  if (is.null(y)) {
    if (is.null(x)) stop("both x and y NULL")
    y <- as.numeric(x)
    x <- seq(along = x)
  }
  ui <- y + uiw
  li <- y - liw
  plot(x, y, ylim = range(c(y, ui, li)))
  smidge <- diff(par("usr")[1:2]) * sfrac
  segments(x, li, x, ui)
  x2 <- c(x, x)
  ul <- c(li, ui)
  segments(x2 - smidge, ul, x2 + smidge, ul)
  invisible(list(x = x, y = y))
}



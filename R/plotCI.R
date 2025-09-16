## Original https://stat.ethz.ch/pipermail/r-help/1997-September/001758.html
## Plot with error bars
## Bill Venables wvenable at attunga.stats.adelaide.edu.au
## Sat Sep 20 02:03:37 CEST 1997

plotCI <- function (x, y = NULL, uiw, liw = uiw, ..., sfrac = 0.01)  {
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
  plot(x, y, ylim = range(c(y, ui, li)), ...)
  smidge <- diff(par("usr")[1:2]) * sfrac
  segments(x, li, x, ui)
  x2 <- c(x, x)
  ul <- c(li, ui)
  segments(x2 - smidge, ul, x2 + smidge, ul)
  invisible(list(x = x, y = y))
}

## The arguments are pretty self-explanatory, but here is a brief
## run-down:

## x, y are as you would expect, although both may be replaced by an
##      xy-list, or if y is omitted, x is plotted against 1:length(x),

## uiw is the distance(s) from y to the upper end of the interval,
## liw is the distance(s) from y to the lower end of the interval,
##      (if omitted taken to be the same as uiw),

## ... allows extra arguments to be passed to plot, eg xlab, ylab,
##     type, lty, lwd, &c. (but not passed to segments()),

## sfrac is the fraction of the x-axis width to be used as the
##      half-length of the seriphs that customarily go at each end
##      of the intervals, (by default 1%).

## -- 
## Bill Venables, Head, Dept of Statistics,    Tel.: +61 8 8303 5418
## University of Adelaide,                     Fax.: +61 8 8303 3696
## South AUSTRALIA.     5005.   Email: Bill.Venables at adelaide.edu.au

## use par("usr") to get the range of the x and y axes in the plot.
## NOT RUN
## y <- c(0,2,4)
## uiw <- c(0,1,1)
## plotCI(x=y,uiw= uiw)
## plotCI(x=y,uiw= uiw, pch = 16, cex = 1.9)
## d <- plotCI(x= c(1,5,6), y = y, uiw= uiw)
## d

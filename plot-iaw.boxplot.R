preamble <- c(doc= '
@TITLE iaw.boxplot
@AUTHOR ivo.welch@gmail.com
@DATE 2013
@DESCRIPTION
@USAGE iaw.boxplot (ds, labels = names(ds), scalefactor = 1, ...) 
@ARGUMENTS
@DETAILS
@SEEALSO
@EXAMPLES
', test= '
', changes= '
')

iaw$iaw.boxplot <- function (ds, labels = names(ds), scalefactor = 1, ...) 
{
  cat("






")
  (is.data.frame(ds)) %or% "ds is not a data frame but a {{class(object)}}"
  (length(labels) != length(ds)) %or% "inconsistent length: {{length(labels)}} and {{length(ds)}}"
  my.cex.axis <- 1
  if ((length(ds) > 10) && (length(ds) < 30)) {
    my.cex.axis <- 1 - ((length(ds) - 10)/10)/3.5
    cat("[Warning: iax.boxplot is scaling cex.axis]
")
  }
  plot(c(0, 1), c(0, 1), type = "n", axes = F, xlim = c(1, 
                                                 length(ds)), ...)
  axis(1, at = 1:length(ds), labels = labels, cex.axis = my.cex.axis, 
       col.axis = "black")
  axis(2)
  for (i in 1:length(ds)) {
    v <- ds[[i]]
    h <- hist(v, breaks = quantile(v, probs = seq(0, 1, 0.05), 
                   na.rm = T), plot = F)
    h$density <- h$density * scalefactor
    lines(c(i, i), c(min(v, na.rm = T), max(v, na.rm = T)), 
          col = gray(0.9))
    for (j in 1:length(h$density)) {
      rect(i - h$density[j], h$breaks[j], i + h$density[j], 
           h$breaks[j + 1], col = gray(0.8), lty = 0)
    }
    toplot <- summary(v)
    lines(c(i + 0.1, i + 0.3), c(toplot[4], toplot[4]))
    lines(c(i + 0.25, i + 0.3), c(toplot[4] + toplot[7], 
                                  toplot[4] + toplot[7]))
    lines(c(i + 0.25, i + 0.3), c(toplot[4] - toplot[7], 
                                  toplot[4] - toplot[7]))
    lines(c(i + 0.3, i + 0.3, i + 0.3), c(toplot[4] - toplot[7], 
                                          toplot[4], toplot[4] + toplot[7]))
    lines(c(i - 0.25, i - 0.3), c(toplot[3], toplot[3]), 
          col = "blue")
    lines(c(i - 0.25, i - 0.3), c(toplot[2], toplot[2]), 
          col = "blue")
    lines(c(i - 0.25, i - 0.3), c(toplot[5], toplot[5]), 
          col = "blue")
    lines(c(i - 0.3, i - 0.3, i - 0.3), c(toplot[2], toplot[3], 
                                          toplot[5]))
    box()
  }
}

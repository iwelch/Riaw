
## R defaults are c(bottom, left, top, right).  default is c(5,4,4,2) + 0.1

# iaw defaults are
#   mar = c(4, 4, 1, 1) + 0.1,  outer margins
#   mgp = c(2.5, 0.75, 0)    1=title;  2=axis labels (moves the xaxis numbers);  3=axis line (0 means overlap with square box)

## the outer margin is left of ylab, below xlab

## what we really want is a negative number on the third argument of mgp, *and* move the square surrounding box also right.  we
## don't really have this.

iaw$ylab.left <- function(mar = c(4, 4, 1, 1) + 0.1, mgp = c(2.5, 0.6, 0.0)) {
    par(mgp=mgp)
    par(mar=mar)

}

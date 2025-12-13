preamble <- c(doc= '
@TITLE iaw.lambda2color
@AUTHOR ivo.welch@gmail.com
@DATE 2025
@DESCRIPTION
@USAGE plot( (1:10)/10, col=lambda2color((1:10)/10) )
@ARGUMENTS
@DETAILS
@SEEALSO
@EXAMPLES
', test= '

plot( 0, type="n", xlim=c(0,1), ylim=c(0,1) )

for (i in 0:20) {
    lambda <- i/20
    points( lambda, lambda, bg=iaw$lambda2color(lambda), col=iaw$lambda2color(lambda), cex=5, pch=22)  # ramp is strange
}

', changes= '
')

iaw$default.color.ramp <- c("red", "white", "blue")

iaw$lambda2color <- function( lambdas, color.ramp=iaw$default.color.ramp ) {
    cfun <- colorRamp( color.ramp ) 
    rgb( cfun(lambdas)/256 )
}

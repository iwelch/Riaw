
#' SETUPKNITR
#'
#' @name setupknitr
#'
#'  @details None
#'
#'  @usage setupknitr()
#'

iaw$setup.knitr <- function() {
    library(knitr)
    knitr::opts_chunk$set(echo = TRUE,results = "hold")

    df.round <<- function(df) data.frame( lapply(df, function(dc) if(is.numeric(dc)) round(dc, 2) else dc) )

    use(DT, "datatable")
    p <<- function(...) DT::datatable(...)

    # knit_print.data.frame <- function(x, ...) asis_output(  paste( c("",p(x)) , collapse="\n" )  )
    # registerS3method("knit_print", "data.frame", knit_print.data.frame)
}

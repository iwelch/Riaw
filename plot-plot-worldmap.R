
################  ADVICE:  Use wd=10.  still needs crop at top, though
iaw$plot.worldmap <- function( ccode3, plotvar, plottitle="" ) {
    library("rworldmap")

    dcs <- data.frame( ccode3, plotvar )
    dcs <- dcs[complete.cases(dcs),]
    stopifnot( nrow(dcs) > 2 )

    sPDF <- joinCountryData2Map( dcs, joinCode = "ISO3", nameJoinColumn = "ccode3")
    sPDF <- sPDF[-which(getMap()$ADMIN=="Antarctica"),]

    newgreen <- "#009E73"
    mkrwg <- function( plotvar, col.neg= "red", col.zero= gray(0.9), col.pos= "#009E73" ) {
        length.out <- length(plotvar) + 20
        length.out <- 100 ## not sure how to do this
        rl <- abs(min(plotvar))/(max(plotvar) - min(plotvar))
        rr <- abs(max(plotvar))/(max(plotvar) - min(plotvar))
        numofcols.l <- rl * (length.out+1)
        numofcols.r <- rr * (length.out+1)
        r2w <- head(colorRampPalette(c(col.neg, grey(0.9)), space="Lab")( numofcols.l ), -1)  ## remove one duplicate to merge
        w2g <- colorRampPalette(c(gray(0.9), col.pos), space="Lab")( numofcols.r )
        c(r2w,w2g)
    }

    rwgp <- mkrwg( dcs$plotvar )

    par(mar= c(2,0,8,0), mai= c(0,0,0,0))
    mapCountryData( sPDF,
                   mapTitle="",
                   colourPalette=rwgp,
                   nameColumnToPlot= "plotvar",
                   numCats=max(10,length(plotvar)),
                   catMethod= "pretty",
                   #addLegend=F,
                   oceanCol='lightblue', borderCol="black", missingCountryCol="black" )
    iaw$hline( 0, lwd=0.5 )
    iaw$hline( c(-23.5, 23.5, 66.6), lwd=0.5, lty=3 )
    if (plottitle != "") text( 0.5, -90, plottitle )
}

iaw$plot.wmap <- iaw$plot.worldmap
iaw$plot.world <- iaw$plot.worldmap

################
iaw$pdf.crop <- function( pdfname ) {
    pdfname <- gsub("\\.pdf$", "", pdfname)
    system(paste0("pdfcrop ", pdfname, ".pdf"))
    system(paste0("mv -f ", pdfname, "-crop.pdf ", pdfname, ".pdf"))
    message("[cropped and replaced ", pdfname, "]")
}
iaw$plot.crop <- iaw$pdf.crop

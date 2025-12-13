#' MERGE4SEQ
#'
#' @name merge4seq
#'
#' Merge two data sets, and return: [1] merged, [2] dl leftover, [3] dr leftover
#' Also provides a convenience nrow list return
#'
#' @usage merge4seq( dl, dr )
#'
#' @param dl a data frame
#' @param dr a data frame
#' @param by the common columns by which they should be merged
#'
#' @return a list of data frames
#'

iaw$merge4seq <- function( dl, dr, ... ) {
                                        # for (nm in c(by)) {
                                        # (nm %in% names(dr)) %or% " name {{nm}} does not exist in dr "
                                        # (nm %in% names(dl)) %or% " name {{nm}} does not exist in dl "
                                        # }

    dl$mgwfbck.dl <- 1:nrow(dl)
    dr$mgwfbck.dr <- 1:nrow(dr)

    ds.matched.and.unmatched <- merge(dl, dr, ..., all=TRUE)  ## only matches!
    ds.matched <- subset(ds.matched.and.unmatched, (!is.na(mgwfbck.dl)) & (!is.na(mgwfbck.dr)))

    unmatched.dl <- subset(ds.matched.and.unmatched, is.na(mgwfbck.dr), select="mgwfbck.dl")[,1]
    unmatched.dl <- unique(unmatched.dl[!is.na(unmatched.dl)])  ## unique is not necessary
    dl <- dl[unmatched.dl,]

    unmatched.dr <- subset(ds.matched.and.unmatched, is.na(mgwfbck.dl), select="mgwfbck.dr")[,1]
    unmatched.dr <- unique(unmatched.dr[!is.na(unmatched.dr)])
    dr <- dr[unmatched.dr,]

    list(merged=subset(ds.matched, T, select= -c(mgwfbck.dl, mgwfbck.dr)),
         dl=subset(dl, T, select= -c(mgwfbck.dl)),
         dr=subset(dr, T, select= -c(mgwfbck.dr)),
         nrow= c(merged=nrow(ds.matched), dl=nrow(dl), dr=nrow(dr)))
}

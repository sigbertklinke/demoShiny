#' demoShiny
#'
#' Runs a Shiny app as demo or shows an overview of all Shiny demo apps related to a \code{topic}. 
#' If more than one app relates to \code{topic} a list of apps is returned otherwise the app is run.
#' For more details see \code{vignettes('demoShiny')}.
#'
#' @param topic character: the topic which should be demonstrated
#'
#' @return nothing
#' @importFrom shiny runApp
#' @export
#'
#' @examples
#' # collect all apps of loaded packages
#' demoShiny() 
#' # collect all apps of the package demoShiny or with the name demoShiny
#' demoShiny('demoShiny') 
#' if(interactive()) {
#'   demoShiny('demoShiny::hist') 
#'   demoShiny('silhouette') 
#'   # use partial matching
#'   demoShiny('silh') 
#' }
demoShiny <- function(topic) {
  partialmatch <- function(v, t) {
    # check for exact match 
    res <- v==t
    if (any(res)) return(res)
    # check for start match
    startsWith(v, t)
  }
  # collect all apps
  spkg  <- file.path(find.package(), "shiny")
  spkg  <- spkg[dir.exists(spkg)]
  files <- list.files(spkg, '*.R', full.names = TRUE)
  sfile <- strsplit(files, '/', fixed=TRUE)
  dirs  <- list.dirs(spkg, recursive=FALSE)
  sdirs <- strsplit(dirs, '/', fixed=TRUE)
  loa   <- data.frame(pkg  =c(sapply(sfile, function(e) { e[length(e)-2]}),  
                              sapply(sdirs, function(e) { e[length(e)-2]})),
                      topic=c(sapply(sfile, function(e) { strsplit(e[length(e)], '.R', fixed=TRUE)[[1]] }),
                              sapply(sdirs, function(e) { e[length(e)]})),
                      file =c(files, dirs), stringsAsFactors = FALSE)
  pt    <- paste(loa$pkg, loa$topic, sep='::')
  loa   <- loa[!duplicated(pt),]
  pt    <- paste(loa$pkg, loa$topic, sep='::')
  loa   <- loa[order(pt),]
  index <- 1:nrow(loa)
  if (!missing(topic)) {
    index <- numeric(0)
    ts <- strsplit(topic, '::', fixed=TRUE)        
    for (tsi in ts) {
      if (length(tsi)==1) index <- c(index, which(partialmatch(loa$pkg, tsi[1]) |
                                                  partialmatch(loa$topic, tsi[1])))
      if (length(tsi)==2) index <- c(index, which(partialmatch(loa$pkg, tsi[1]) &
                                                  partialmatch(loa$topic, tsi[2])))
    }
    index <- sort(unique(index))
  }
  loa <- loa[index,]
  if (length(index)!=1) {
    loa$topic <- paste(loa$pkg, loa$topic, sep="::")
    loa$pkg   <- NULL
    print(loa, right=FALSE)
  } else {
    if (dir.exists(loa$file)) 
      runApp(loa$file)
    else 
      source(loa$file)
  }
  invisible(loa)  
}
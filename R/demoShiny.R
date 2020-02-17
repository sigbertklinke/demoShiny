#' demoShiny
#'
#' Runs a Shiny app as demo or shows an overview of all Shiny demo apps related to a \code{topic}. 
#' If more than one app relates to \code{topic} a list of apps is returned otherwise the app is run.
#' For more details see \code{vignettes('demoShiny')}.
#'
#' @param topic character: the topic which should be demonstrated
#' @param verbose logical: should file locations printed, too (default: \code{FALSE}) 
#'
#' @return invisibly a data frame with topic, info and file location
#' @importFrom shiny runApp
#' @export
#'
#' @examples
#' # collect all apps of loaded packages
#' demoShiny() 
#' demoShiny(verbose=TRUE) 
#' # collect all apps of the package demoShiny or with the name demoShiny
#' demoShiny('demoShiny') 
#' if(interactive()) {
#'   demoShiny('demoShiny::hist') 
#'   demoShiny('silhouette') 
#'   # use partial matching
#'   demoShiny('silh') 
#' }
demoShiny <- function(topic, verbose=FALSE) {
  partialmatch <- function(v, t) {
    # check for exact match 
    res <- v==t
    if (any(res)) return(res)
    # check for start match
    startsWith(v, t)
  }
  #
  startsMatch <- function(x, table, nomatch = NA_integer_) {
    ret <- rep(nomatch, length(x))
    tab <- rep(TRUE, length(table))
    op  <- order(nchar(x), decreasing=TRUE)
    #browser()
    for (i in 1:length(op)) {
      tf <- grepl(paste0("^", x[op[i]], "\\s+"), table) & tab
      if (any(tf)) {
        pos        <- which(tf)[1]
        ret[op[i]] <- trimws(substring(table[pos], nchar(x[op[i]])+1))
        tab[pos]   <- FALSE
      }
    }
    ret
  }
  # collect all apps
  #browser()
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
                      info =c(sapply(sfile, function(e) { strsplit(e[length(e)], '.R', fixed=TRUE)[[1]] }),
                              sapply(sdirs, function(e) { e[length(e)]})), 
                      file =c(files, dirs), stringsAsFactors = FALSE)
#
  index  <- list.files(spkg, '00Index', full.names = TRUE)
  if (length(index)) {
    sindex <- strsplit(index, '/', fixed=TRUE)
    pindex <- sapply(sindex, function(e) { e[length(e)-2]})
    for (j in 1:length(pindex)) {
      info   <- readLines(index[j])  
      pkgind <- which(loa$pkg==pindex[j])
      oindex <- order(nchar(loa$topic[pkgind]), decreasing = TRUE)
      oindex <- pkgind[oindex]
      while(length(oindex)) {
        i  <- oindex[1]
        tf <- grepl(paste0("^", loa$topic[i], "\\s+"), info)
        if (any(tf)) {
          pos <- which(tf)[1]
          loa$info[i] <- trimws(substring(info[pos], nchar(loa$topic[i])+1))
        }
        oindex <- oindex[-1]
      }
    }  
  }       
#                  
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
    nlt <- max(nchar(loa$topic))
    for (i in 1:nrow(loa)) {
      cat(sprintf("%-*s %s\n", nlt, loa$topic[i], loa$info[i]))
      if (verbose) cat(' ', loa$file[i], "\n\n")
    }
  } else {
    if (dir.exists(loa$file)) 
      runApp(loa$file)
    else 
      source(loa$file)
  }
  invisible(loa)  
}
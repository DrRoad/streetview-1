#' \code{streetview} package
#'
#' panoids
#'
#' See the README on 
#'
#' @docType package
#' @name streetview
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))


#' @title get_panoids
#'
#' @description Retrieve list of panoids and locations
#' @param path.root location of current dropbox root Example: ~/Dropbox/pkg.data/
#' @param query.addresses character vector of locations to find nearest panoids Example: 11804 Ridge Pkwy, Broomfield, CO 80021
#' @param n.echo Number of queries before backup and display
#' @keywords streetview google panoids
#' @export
#' @importFrom pkg.data.paths paths
#' @importFrom lubridate ymd year quarter
#' @importFrom data.table data.table rbindlist
#' @importFrom pkg.data.paths paths
#' @import utils
get_panoids <- function(path.root = NULL, query.addresses = NULL, n.echo =10){
  pano_id <- NULL
  # Initialize api and paths
  api.key <- api.keys::import.key(str.api.name = 'google')
  # Load package data from dropbox
  get.panoids.paths <- pkg.data.paths::paths(path.root = path.root, str.pkg.name = 'streetview')
  panoids.raw.path <- paste0(get.panoids.paths$pkg.root[[1]], '/raw/panoids.rdata')
  panoids.clean.path <- paste0(get.panoids.paths$pkg.root[[1]], '/clean/panoids.rdata')
  if (!is.null(query.addresses)){
    load(panoids.clean.path)
  } else {
    # Load parcel data to build address list
    if(file.exists(panoids.raw.path)){
      load(panoids.raw.path)
    } else {
      panoids <- data.table::data.table(address = as.character())
    }
    # Subset to only include new addresses
    query.addresses <- query.addresses[!(query.addresses %in% panoids$address)]
    cat('Processing', length(query.addresses), 'addresses')
    i <- 0
    tic <- Sys.time()
    n.echo <- 100
    n.queries <- length(query.addresses)
    for (query.address in query.addresses){
      api <- list()
      api[[1]] <- 'https://maps.googleapis.com/maps/api/streetview/metadata?size=600x300&location='
      api[[2]] <- paste0(URLencode(query.address), '&')
      api[[3]] <- paste0('key=', api.key)
      api.url <- paste0(unlist(api), collapse = '')
      panorama <- try(unlist(rjson::fromJSON(file=api.url)))
      panoid <- try(data.table::data.table(address = query.address, t(panorama)))
      panoids <- try(data.table::rbindlist(list(panoids, panoid), use.names = TRUE, fill=TRUE))
      i <- i + 1;
      if (i %% n.echo == 0){
        toc <- Sys.time()
        time.elapsed <- as.numeric(difftime(toc, tic, units='days'))
        days.remain <- (n.queries-i)/n.echo * time.elapsed
        cat(i, 'of', n.queries, '\n', 'Days remaining: ', days.remain, '\n')
        print(panoid)
        save(panoids, file=panoids.raw.path)
        tic <- Sys.time()
      }
    }
    # Collapse to unique panoids
    # Drop panoids with no results
    data.table::setkey(panoids, pano_id)
    panoids <- unique(panoids, by=c('pano_id', 'date'))
    # Remove NA rows
    na.rows <- unlist(lapply(names(panoids), function(x) which(is.na(panoids[, x, with=FALSE]))))
    panoids <- panoids[!(seq(1,nrow(panoids)) %in% na.rows )]
    panoids$dt.date <- lubridate::ymd(paste0(panoids$date, '-01'))
    panoids$dt.year <- lubridate::year(panoids$dt.date)
    panoids$dt.quarter <- lubridate::quarter(panoids$dt.date)
    panoids$address <- NULL
    panoids$copyright <- NULL
    panoids$date <- NULL
    save(panoids, file=panoids.clean.path)
  }
  return(panoids)
}
#' @title plot_panoids
#'
#' @description Plot panoids by snapshot date
#' @param path.root location of current dropbox root Example: ~/Dropbox/pkg.data/
#' @param l.extent list l.extent$ lng.min, lng.max, lat.min,lat.max 
#' @keywords streetview google panoids plot
#' @export
#' @importFrom pkg.data.paths paths
#' @importFrom ggthemes theme_map theme_tufte
#' @importFrom ggplot2 ggplot geom_point scale_color_brewer geom_bar scale_fill_brewer
plot_panoids <- function(path.root, l.extent= NULL){
  dt.quarter <- NULL; dt.year <- NULL; location.lat <- NULL; location.lng <- NULL; pano_id <- NULL;
  aes <- NULL;
  get.panoids.paths <- pkg.data.paths::paths(path.root = path.root, str.pkg.name = 'streetview')
  panoids.clean.path <- paste0(get.panoids.paths$pkg.root[[1]], '/clean/panoids.rdata')
  load(panoids.clean.path)
  if (!is.null(l.extent)) panoids <- panoids[location.lat > l.extent$lat.min &
                                               location.lat < l.extent$lat.max &
                                               location.lng > l.extent$lng.min &
                                               location.lng < l.extent$lng.max]
  # Define data types for panoids
  # Plot years
  l.plot <- list()
  l.plot$year <- ggplot2::ggplot(panoids, aes(location.lng, location.lat, colour=as.factor(dt.year))) + 
    ggplot2::geom_point(size=0.4, alpha=0.75) + 
    ggplot2::scale_color_brewer(palette = "Spectral", guide = "legend", name=NULL) +
    ggthemes::theme_map(base_size = 12)
  
  l.plot$quarter <- ggplot2::ggplot(panoids, aes(location.lng, location.lat,
                                                 colour=as.factor(dt.quarter))) + 
    ggplot2::geom_point(size=0.4, alpha=0.75) + 
    ggplot2::scale_color_brewer(palette = "RdYlBu", guide = "legend", name='Quarter', direction=-1) +
    ggthemes::theme_map(base_size = 12)
  
  l.plot$hist <-  ggplot2::ggplot(panoids, aes(x=as.factor(dt.year), fill=as.factor(dt.year))) + 
    ggplot2::geom_bar() +
    ggplot2::scale_fill_brewer(palette = 'Spectral', guide = FALSE) +
    ggplot2::xlab(label= NULL) + 
    ggplot2::ylab(label= NULL) +
    ggthemes::theme_tufte()
  return(l.plot)
}

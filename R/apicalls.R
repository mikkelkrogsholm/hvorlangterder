#### route ----
#' Route From A to B
#' @description Caluculate a routed distance between two points, with optional route description
#'
#' @param fromaddress is an adress type: roadname+housenumber+postalcode
#' @param fromcoord is a coordinate type: latitude, longtitude
#' @param toaddress is an adress type: roadname+housenumber+postalcode
#' @param tocoord is a coordinate type: latitude, longtitude
#' @param mot is a mode of transport designator
#' \itemize{
#'   \item car (default)
#'   \item bicycle
#'   \item foot
#'   \item public
#'   \item auto
#' }
#' @param routepolyline append a route in the form of a list of coordinates.
#' \itemize{
#'   \item 0 (default): without route description
#'   \item 1: with route description as a an encoded polyline string
#'   \item 2: with route description as a list of latitude,longtitude pairs.
#' }
#' @param traveltime used in combination with mot = public.
#' \itemize{
#'   \item WEEKSTART (default): 09:00 on the first following weekday.
#'   \item NOW: Use current time
#'   \item datetime: Datetime string in the ISO 8601 format YYYY-MM-DDThh:mm (ie. 2016-10-30T16:56)
#' }
#'
#' @return Returns a list
#'
#' @examples
#'
#' # Route between a pair of coordinates:
#' route(fromcoord = "55.68958,12.55784", tocoord = "55.69043,12.53527")
#'
#' # Route between a pair of addresses:
#' route(fromaddress = "elmegade 5 2200", toaddress = "Rådhuspladsen 2, 1550 København V, 1550")
#'
#' # Route from address to coordinate:
#' route(fromaddress = "elmegade 5 2200", tocoord = "55.69043,12.53527")
#'
#' # Setting mode of transport to bicycle
#' route(fromcoord = "55.68958,12.55784", tocoord = "55.69043,12.53527", mot = "bicycle")
#'
#' # Request polyline as list of coordinates and setting mode of transport to foot
#' route(fromcoord = "55.68958,12.55784", tocoord = "55.69043,12.53527", mot = "foot", routepolyline = 2)
#'
#' @export

route <- function(fromaddress = NULL,
                  fromcoord = NULL,
                  toaddress = NULL,
                  tocoord = NULL,
                  mot = NULL,
                  routepolyline = NULL,
                  traveltime = NULL){

  url <- "http://hvorlangterder.dk/route/"

  request <- list(fromaddress = fromaddress,
                  fromcoord = fromcoord,
                  toaddress = toaddress,
                  tocoord = tocoord,
                  mot = mot,
                  routepolyline = routepolyline,
                  traveltime = traveltime)


  response <- httr::GET(url, query = request)
  response <- httr::content(response)

  message("Data source: 'http://hvorlangterder.dk/'")

  return(response)
}

################################################################################

#### multiroute ----
#' Route to multiple destinations
#' @description Calculate routes from a single point to one or more coordinates.
#'
#' @param fromaddress is an adress type: roadname+housenumber+postalcode
#' @param fromcoord is a coordinate type: latitude, longtitude
#' @param tocoords is a coordinate type: latitude, longtitude. Must be a data frame with a lat and lng column
#' @param mot is a mode of transport designator
#' \itemize{
#'   \item car (default)
#'   \item bicycle
#'   \item foot
#'   \item public
#'   \item auto
#' }
#' @param routepolyline append a route in the form of a list of coordinates.
#' \itemize{
#'   \item 0 (default): without route description
#'   \item 1: with route description as a an encoded polyline string
#'   \item 2: with route description as a list of latitude,longtitude pairs.
#' }
#' @param traveltime used in combination with mot = public.
#' \itemize{
#'   \item WEEKSTART (default): 09:00 on the first following weekday.
#'   \item NOW: Use current time
#'   \item datetime: Datetime string in the ISO 8601 format YYYY-MM-DDThh:mm (ie. 2016-10-30T16:56)
#' }
#'
#' @return Returns a list
#'
#' @examples
#'
#' # Create data frame with coordinates
#' coords <- dplyr::data_frame(
#'   lat = c(55.689298, 55.683842, 55.679184),
#'   lng = c(12.557549, 12.571044, 12.585228))
#'
#' # Request with origintype as coordinate:
#' multiroute(fromcoord = "55.68958,12.557845", tocoords = coords)
#'
#' # Request with origintype as address:
#' multiroute(fromaddress = "elmegade 5 2200", tocoords = coords)
#'
#' # Request compressed polylines for each route:
#' mymultiroute <- multiroute(fromcoord = "55.68958,12.557845", tocoords = coords, routepolyline = 1)
#'
#' # Decode the compressed polyline
#' decodepolyline(mymultiroute[[1]]$routepolyline)
#'
#' @export

multiroute <- function(fromaddress = NULL,
                       fromcoord = NULL,
                       tocoords = NULL,
                       mot = NULL,
                       routepolyline = NULL,
                       traveltime = NULL){

  url <- "http://hvorlangterder.dk/multiroute/"

  tocoords <- lapply(1:nrow(tocoords), function(i){
    stringr::str_c(tocoords$lat[i], tocoords$lng[i], sep = ",")
  })

  tocoords <- stringr::str_c(tocoords, collapse = " ")

  request <- list(fromaddress = fromaddress,
                  fromcoord = fromcoord,
                  tocoords = tocoords,
                  mot = mot,
                  routepolyline = routepolyline,
                  traveltime = traveltime)


  response <- httr::GET(url, query = request)
  response <- httr::content(response)

  message("Data source: 'http://hvorlangterder.dk/'")

  return(response)
}

################################################################################

#### POItypes ----

#' Get POI types
#'
#' @return Returns a vector
#'
#' @examples
#'
#' # List the different POIs
#' POItypes()
#'
#' @export

POItypes <- function(){

  POIs <- c("lake", "forest", "doctor", "supermarket", "school", "daycare", "hospital", "metro",
            "train", "stop", "strain", "junction", "pharmacy", "library", "coast", "airport")

  return(POIs)
}


#### POInear ----
#' Nearest POI of Given Types
#' @description Returns the POI for each requested type that has the shortes route to the outset point.
#'
#' @param fromaddress is an adress type: roadname+housenumber+postalcode
#' @param fromcoord is a coordinate type: latitude, longtitude
#' @param poitypes is a vector poi types to search for
#' @param mot is a mode of transport designator
#' \itemize{
#'   \item car (default)
#'   \item bicycle
#'   \item foot
#'   \item public
#'   \item auto
#' }
#' @param routepolyline append a route in the form of a list of coordinates.
#' \itemize{
#'   \item 0 (default): without route description
#'   \item 1: with route description as a an encoded polyline string
#'   \item 2: with route description as a list of latitude,longtitude pairs.
#' }
#'
#' @return Returns a list
#'
#' @examples
#'
#' # Request with address as origintype:
#' POInear(fromaddress = "elmegade 5 2200",
#'         poitypes = c("daycare", "school", "supermarket", "hospital", "doctor", "train", "stop", "junction", "metro", "strain")
#' )
#'
#' # Request with coordinate as origintype:
#' POInear(fromcoord = "56.16390607394954,10.16510009765625",
#'         poitypes = c("daycare", "school", "supermarket", "hospital", "doctor", "train", "stop", "junction", "metro", "strain")
#' )
#'
#' # Set mode of transport to bicycle:
#' POInear(fromcoord = "56.16390607394954,10.16510009765625",
#'         poitypes = c("daycare", "school", "supermarket", "hospital", "doctor", "train", "stop", "junction", "metro", "strain"),
#'         mot = "bicycle"
#' )
#'
#' # Request compressed polylines for each of the nearest POIs:
#' POInear(fromcoord = "56.16390607394954,10.16510009765625",
#'         poitypes = c("daycare", "school", "supermarket", "hospital", "doctor", "train", "stop", "junction", "metro", "strain"),
#'         routepolyline = 1
#' )
#'
#' @export

POInear <- function(fromaddress = NULL,
                    fromcoord = NULL,
                    poitypes = NULL,
                    mot = NULL,
                    routepolyline = NULL){

  if(is.null(poitypes)) stop("poitypes cannot be empty. See POItypes() for list of POIs")

  poitypes <- stringr::str_c(poitypes, collapse = ",")

  url <- "http://hvorlangterder.dk/multitypes/"

  request <- list(fromaddress = fromaddress,
                  fromcoord = fromcoord,
                  poitypes = poitypes,
                  mot = mot,
                  routepolyline = routepolyline)


  response <- httr::GET(url, query = request)
  response <- httr::content(response)

  message("Data source: 'http://hvorlangterder.dk/'")

  return(response)
}

#### POIradius ----
#' Points of Interest Within A Given Radius
#' @description Returns a list of name and coordinates of POI's based on the search parameters.
#'
#' @param fromaddress is an adress type: roadname+housenumber+postalcode
#' @param fromcoord is a coordinate type: latitude, longtitude
#' @param poitypes is a vector poi types to search for
#'
#' @return Returns a list
#'
#' @examples
#'
#' # POIs of type pharmacy, lake or coast within default 20km radius of address:
#' POIradius(fromaddress = "elmegade 5 2200", poitypes = c("pharmacy", "lake", "coast", "forest"))
#'
#' # POIs of type pharmacy, lake or coast within default 20km radius of coordinate:
#' POIradius(fromcoord = "55.69043,12.53527", poitypes = c("pharmacy", "lake", "coast", "forest"))
#'
#' @export

POIradius <- function(fromaddress = NULL,
                      fromcoord = NULL,
                      poitypes = NULL){

  if(is.null(poitypes)) stop("poitypes cannot be empty. See POItypes() for list of POIs")

  poitypes <- stringr::str_c(poitypes, collapse = ",")

  url <- "http://hvorlangterder.dk/nearest/"

  request <- list(fromaddress = fromaddress,
                  fromcoord = fromcoord,
                  poitypes = poitypes)

  response <- httr::GET(url, query = request)
  response <- httr::content(response)

  message("Data source: 'http://hvorlangterder.dk/'")

  return(response)
}

#### POIwithin ----
#' Points of Interest Within A Geometry
#' @description Returns a list of name and coordinates of POI's based on the search parameters.
#'
#' @param poitypes is a vector poi types to search for.
#' @param bbox is a boundingbox: lower_lat, lower_lng, upper_lat, upper_lng. Must be a data frame with a lat and lng column.
#' @param municipalcode municipalcode: one of 98 danish municipality codes
#'
#' @return Returns a list
#'
#' @examples
#'
#' # Make bounding box coordinates
#' coords <- dplyr::data_frame(
#'   lat = c(54.842508, 56.131587),
#'   lng = c(9.633364, 12.574262))
#'
#' # POIs of type airport within a large bounding box:
#' POIwithin(poitypes = "airport", bbox = coords)
#'
#' # Libraries within municiaplity of Copenhagen:
#' POIwithin(poitypes = "library", municipalcode = 101)
#'
#' @export

POIwithin <- function(poitypes = NULL,
                      bbox = NULL,
                      municipalcode = NULL){

  if(is.null(poitypes)) stop("poitypes cannot be empty. See POItypes() for list of POIs")

  poitypes <- stringr::str_c(poitypes, collapse = ",")

  if(!is.null(bbox)){
    bbox <- lapply(1:nrow(bbox), function(i){
      stringr::str_c(tocoords$lat[i], tocoords$lng[i], sep = ",")
    })

    bbox <- stringr::str_c(bbox, collapse = " ")
  }

  url <- "http://hvorlangterder.dk/poiwithin/"

  request <- list(poitypes = poitypes,
                  bbox = bbox,
                  municipalcode = municipalcode)

  response <- httr::GET(url, query = request)
  response <- httr::content(response)

  message("Data source: 'http://hvorlangterder.dk/'")

  return(response)
}


###############################################################################

#### tsp ----
#' Travelling Salesperson
#' @description Calculate shortest route visiting every destination before returning home.
#'
#' @param fromaddress is an adress type: roadname+housenumber+postalcode
#' @param fromcoord is a coordinate type: latitude, longtitude
#' @param tocoords is a coordinate type: latitude, longtitude. Must be a data frame with a lat and lng column
#' @param metric is a choice of cost metric for optimal route. Either "time" (default) or "distance"
#' @param zvalue is the zoom level for polyline. Integer in the range 1 to 19. Default: 18
#'
#' @return Returns a list
#'
#' @examples
#'
#' # Create data frame with coordinates
#' coords <- dplyr::data_frame(
#'   lat = c(55.70873874051673, 55.68745586048921, 55.67274455523506,
#'           55.70390273988158, 55.62799595426723, 55.7425739894847),
#'   lng = c(12.581062316894531, 12.496261596679688, 12.48870849609375,
#'           12.363052368164062, 12.55256652832031, 12.454376220703125))
#'
#' # TSP optimized for shortest travelling time:
#' tsp(fromcoord = "55.6896,12.55792", tocoords = coords, metric = "time")
#'
#' # TSP optimized for shortest travelling distance:
#' tsp(fromcoord = "55.6896,12.55792", tocoords = coords, metric = "distance")
#'
#' # Decode the compressed polyline
#' mytsp <- tsp(fromcoord = "55.6896,12.55792", tocoords = coords, metric = "distance")
#'
#' decodepolyline(mytsp$steps[[1]]$route_geometry)
#'
#' @export

tsp <- function(fromaddress = NULL,
                fromcoord = NULL,
                tocoords = NULL,
                metric = NULL,
                zvalue = NULL){

  url <- "http://hvorlangterder.dk/tsp/"

  tocoords <- lapply(1:nrow(tocoords), function(i){
    stringr::str_c(tocoords$lat[i], tocoords$lng[i], sep = ",")
  })

  tocoords <- stringr::str_c(tocoords, collapse = " ")

  request <- list(fromaddress = fromaddress,
                  fromcoord = fromcoord,
                  tocoords = tocoords,
                  metric = metric,
                  zvalue = zvalue)


  response <- httr::GET(url, query = request)
  response <- httr::content(response)

  message("Data source: 'http://hvorlangterder.dk/'")

  return(response)
}

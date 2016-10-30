# library(leaflet)
#
# test <- jsonlite::fromJSON("http://hvorlangterder.dk/route/?routepolyline=2&mot=foot&fromcoord=55.68958,12.55784&tocoord=55.69043,12.53527&indent=2")
#
# routeline <- dplyr::as_data_frame(test$routepolyline)
# names(routeline) <- c("lat", "lng")
#
#
# leaflet() %>%
#   addTiles() %>%
#   addPolylines(lng = routeline$lng, lat = routeline$lat)
#
#
# map <- leaflet::leaflet()
# map <- leaflet::addTiles(map)
# map <- leaflet::addPolylines(map, lng = myroute$lng, lat = myroute$lat)
# map
#
#
# leaflet::addPolylines(map, lng = myroute$lng, lat = myroute$lat)
# leaflet::addPolygons(map, lng = myroute$lng, lat = myroute$lat)
#
#
# coords <- dplyr::data_frame(
#   lat = c(55.689298, 55.683842, 55.679184),
#   lng = c(12.557549, 12.571044, 12.585228))
#
# mymultiroute <- multiroute(fromcoord = "55.68958,12.557845", tocoords = coords, routepolyline = 1)
#
# # Decode the compressed polyline
# DecodeLineR(mymultiroute[[1]]$routepolyline)
#
#
# routepolylines <- lapply(mymultiroute, function(x) DecodeLineR(x$routepolyline))
# myroute <- do.call(rbind, routepolylines)
#
# map <- leaflet::leaflet()
# map <- leaflet::addTiles(map)
# map <- leaflet::addPolylines(map, lng = myroute$lng, lat = myroute$lat)
# map

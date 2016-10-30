#' Decode an encoded polyline
#'
#' @param encoded is an encoded polyline
#'
#' @return Returns a data frame
#'
#' @examples
#'
#' # Request compressed polylines for each route:
#' mymultiroute <- multiroute(fromcoord = "55.68958,12.557845", tocoords = coords, routepolyline = 1)
#'
#' # Decode the compressed polyline
#' decodepolyline(mymultiroute[[1]]$routepolyline)
#'
#' @exports

decodepolyline <- function(encoded) {
  len = stringr::str_length(encoded)
  encoded <- strsplit(encoded, NULL)[[1]]
  index = 1
  N <- 100000
  df.index <- 1
  array = matrix(nrow = N, ncol = 2)
  lat <- dlat <- lng <- dlnt <- b <- shift <- result <- 0

  while(index <= len) {

    shift <- result <- 0

    repeat {
      b = as.integer(charToRaw(encoded[index])) - 63
      index <- index + 1
      result = bitops::bitOr(result, bitops::bitShiftL(bitops::bitAnd(b, 0x1f), shift))
      shift = shift + 5
      if(b < 0x20) break
    }
    dlat = ifelse(bitops::bitAnd(result, 1),
                  -(result - (bitops::bitShiftR(result, 1))),
                  bitops::bitShiftR(result, 1))
    lat = lat + dlat;

    shift <- result <- b <- 0

    repeat {
      b = as.integer(charToRaw(encoded[index])) - 63
      index <- index + 1
      result = bitops::bitOr(result, bitops::bitShiftL(bitops::bitAnd(b, 0x1f), shift))
      shift = shift + 5
      if(b < 0x20) break
    }
    dlng = ifelse(bitops::bitAnd(result, 1),
                  -(result - (bitops::bitShiftR(result, 1))),
                  bitops::bitShiftR(result, 1))
    lng = lng + dlng

    array[df.index,] <- c(lat = lat * 1e-6, lng = lng * 1e-6)
    df.index <- df.index + 1
  }

  ret <- data.frame(array[1:df.index - 1,])
  names(ret) <- c("lat", "lng")

  message("Adaped from hrbrmstr: http://stackoverflow.com/a/32477722")

  return(ret)
}


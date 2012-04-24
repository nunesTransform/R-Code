#################################################################################
#
# Source:
# http://allthingsr.blogspot.de/2012/03/geocode-and-reverse-geocode-your-data.html
#  
#
#################################################################################


#################################################################################
#
# Get coordinates from adress:
# geoCodes <- getGeoCode("Palo Alto,California")
#
#################################################################################
getGeoCode <- function(gcStr) {
  #Load Library
  library("RJSONIO") 
  #Encode URL Parameters
  gcStr <- gsub(' ','%20',gcStr) 
  #Open Connection
  connectStr <- paste('http://maps.google.com/maps/api/geocode/json?sensor=false&address=', 
                      gcStr, sep="") 
  con <- url(connectStr)
  data.json <- fromJSON(paste(readLines(con), collapse=""))
  close(con)
  #Flatten the received JSON
  data.json <- unlist(data.json)
  if(data.json["status"]=="OK") {
    lat <- as.numeric(data.json["results.geometry.location.lat"])
    lng <- as.numeric(data.json["results.geometry.location.lng"])
    gcodes <- data.frame(city = gcStr, lat = lat, lng = lng)
    return (gcodes)
  }
}

#################################################################################
#
# Get adress from coordinates
# address <- reverseGeoCode(c(37.4418834, -122.1430195))
#
#################################################################################
reverseGeoCode <- function(latlng) {
  #Collapse and Encode URL Parameters
  latlngStr <- gsub(' ','%20', paste(latlng, collapse=","))
  #Load Library
  library("RJSONIO") 
  #Open Connection
  connectStr <- paste('http://maps.google.com/maps/api/geocode/json?sensor=false&latlng=', 
                      latlngStr, sep="")
  con <- url(connectStr)
  data.json <- fromJSON(paste(readLines(con), collapse=""))
  close(con)
  #Flatten the received JSON
  data.json <- unlist(data.json)
  if(data.json["status"]=="OK") {
    address <- data.json["results.formatted_address"]
  }
  return (address)
}
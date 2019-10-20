library(stplanr)

ptroutes <- stplanr::route_transportapi_public(from = c(-0.095477,51.377621), to = c(-0.111377,51.388298))
carroutes <- stplanr::route_osrm(from = c(-0.095477,51.377621), to = c(-0.111377,51.388298))
# tm_shape(ptroutes) + tm_lines(col = "red", lwd = 3) + 
#   tm_shape(carroutes) + tm_lines(col = "blue", lwd = 3)
# 
# tm_shape(st_sf(sf::st_line_sample(carroutes %>% st_as_sf() %>% st_transform(crs = 27700),n = 100) %>% st_cast(to = "POINT"))[11:15,]) + tm_dots()
# 
# 
# tm_shape(st_sf(sf::st_line_sample(carroutes %>% st_as_sf() %>% st_transform(crs = 27700),n = 100) %>% st_cast(to = "POINT")) %>% 
#            mutate(alphaval = seq(from = 0.01, to = 1, by = 0.01))) + tm_bubbles(alpha = 0.5)
# 
# 
# tm_shape(st_sf(sf::st_line_sample(carroutes %>% st_as_sf() %>% st_transform(crs = 27700),n = 100) %>% st_cast(to = "POINT"))[1,]) + tm_dots()



odroutes <- data.frame(
  startloc = "a",
  endloc = "b",
  originlat = 51.377621,
  originlng = -0.095477,
  destlat = 51.388298,
  destlng = -0.111377,
  starttime = "2018-06-11 08:00:00",
  endtime = "2018-06-11 08:30:00"
)

odroutes$startepoch <- as.numeric(as.POSIXlt(odroutes$starttime))
odroutes$endepoch <- as.numeric(as.POSIXlt(odroutes$endtime))
odroutes %>% mutate(routeid = row_number()) -> odroutes

# carroutes <- stplanr::route_osrm(, to = c(-0.111377,51.388298))
carroutes <- stplanr::viaroute(startlat = odroutes$originlat,
                  startlng = odroutes$originlng,
                  endlat = odroutes$destlat,
                  endlng = odroutes$destlng,
                  instructions = FALSE,
                  alt = FALSE)
carroutes2 <- viaroute2sldf(carroutes) %>% st_as_sf() %>% st_transform(27700)
carroutepoints <- st_sf(st_line_sample(carroutes2, n = 100)) %>% mutate(routeid = row_number()) %>% st_cast("POINT") %>% group_by(routeid) %>%
  mutate(pointid = row_number())


animstarttime <- as.POSIXlt("2018-06-11 08:00:00")
animendtime <- as.POSIXlt("2018-06-11 09:00:00")

curtime <- as.numeric(animstarttime)
i <- 1
while (curtime <= as.numeric(animendtime)) {
  carroutepoints %>%
    inner_join(
  odroutes %>%
    filter(startepoch <= curtime & endepoch >= curtime) %>%
    mutate(
      proptime = floor((curtime-startepoch)/(endepoch-startepoch)*100) + 1),
  by = c("routeid"="routeid", "pointid"="proptime")
    ) -> pointstoplot
  tm_shape(pointstoplot,bbox = matrix(st_bbox(carroutes2), ncol = 2)) + 
    tm_dots() + 
    tm_layout(title = as.character(as.POSIXlt(curtime,origin = "1970-01-01 00:00:00")))-> tempplot
  save_tmap(tempplot,filename = paste0("~/Documents/testanim/plot", stringr::str_pad(i, width = 4, side = "left", pad = "0")))
  curtime <- curtime + 120
  i <- i + 1
}

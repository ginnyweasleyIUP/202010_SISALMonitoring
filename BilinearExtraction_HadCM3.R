library(stacy.hadcm.tools)

HadCM3_LMill_Data <- function(variable, longitude, latitude, year_start, year_stop){
  if(variable == "TEMP"){
    #ncf <- ncdf4::nc_open("/modeldata/hadcm3/surface_temperature/annual_mean/xnapa.nc")
    ncf <- ncdf4::nc_open("/home/ginnyweasley/Dokumente/01_Promotion/06_Daten/05_HadCM3/xnap/xnapa_surface_temperature_annual.nc")
    time <- ncf$dim$t$vals
    data <- ncdf4::ncvar_get(ncf)
    ncdf4::nc_close(ncf)  
  }else if(variable == "PREC"){
    #ncf <- ncdf4::nc_open("/modeldata/hadcm3/surface_temperature/annual_mean/xnapa.nc")
    ncf <- ncdf4::nc_open("/home/ginnyweasley/Dokumente/01_Promotion/06_Daten/05_HadCM3/xnap/xnapa_precipitation_annual.nc")
    time <- ncf$dim$t$vals
    data <- ncdf4::ncvar_get(ncf)
    ncdf4::nc_close(ncf)
  }else if(variable == "ISOT"){
    #ncf <- ncdf4::nc_open("/modeldata/hadcm3/surface_temperature/annual_mean/xnapa.nc")
    ncf <- ncdf4::nc_open("/home/ginnyweasley/Dokumente/01_Promotion/06_Daten/05_HadCM3/xnap/xnapa_isotopes_annual.nc")
    time <- ncf$dim$t$vals
    data <- ncdf4::ncvar_get(ncf, "dO18")
    ncdf4::nc_close(ncf)
  }else if(variable == "SLPR"){
    #ncf <- ncdf4::nc_open("/modeldata/hadcm3/surface_temperature/annual_mean/xnapa.nc")
    ncf <- ncdf4::nc_open("/home/ginnyweasley/Dokumente/01_Promotion/06_Daten/05_HadCM3/xnap/xnapa_sea_level_pressure_annual.nc")
    time <- ncf$dim$t$vals
    data <- ncdf4::ncvar_get(ncf)
    ncdf4::nc_close(ncf)
  }else{
    print("variable not found!")
  }
  
  pos_start = length(time[DaysSinceToAD(time)<year_start])
  pos_stop = length(time) - length(time[DaysSinceToAD(time)>year_stop])
  
  data <- data[,,pos_start:pos_stop]
  time <- time[pos_start:pos_stop]
  
  #lon lat in degrees N and E --> example is for Heidelberg
  #lon_extract = 0
  #convert lon from -180->180 to 0->360 as simulation
  if(longitude<0){longitude = 360+longitude}
  
  
  #extract_grid(lon_extract, lat_extract, d.lon = 3.75, d.lat = 2.5) --> adjust d.lon and d.lat for simulations other than HadCM3    
  ratios <- extract_gridboxes(longitude, latitude)
  
  data_extract <- rowSums(cbind(ratios$E1*data[ratios$E1_lon_pos, ratios$E1_lat_pos,],
                                ratios$E2*data[ratios$E2_lon_pos, ratios$E2_lat_pos,],
                                ratios$E3*data[ratios$E3_lon_pos, ratios$E3_lat_pos,],
                                ratios$E4*data[ratios$E4_lon_pos, ratios$E4_lat_pos,]), na.rm = T)
  
  return(data_extract)
}

#this is the 2D field that you want extracting from 



##############################################################
## extract Data for Cave Site from surrounding grid boxes ####
##############################################################

extract_gridboxes <- function(lon_cave, lat_cave, d.lon = 3.75, d.lat = 2.5){
  result_list <- list()
  lon_list <- seq(from = 0, to = 360-d.lon, by = d.lon)
  lat_list <- seq(from = 90, to = -90, by = -d.lat)
  
  if(lon_cave<0){lon_cave = 360+lon_cave}
  
  #corners of the square around lon_cave, lat_cave with dimensions of d.lon*d.lat
  corner <- list(
    E1_lon = lon_cave+d.lon/2, E1_lat = lat_cave+d.lat/2,
    E2_lon = lon_cave-d.lon/2, E2_lat = lat_cave+d.lat/2,
    E3_lon = lon_cave+d.lon/2, E3_lat = lat_cave-d.lat/2,
    E4_lon = lon_cave-d.lon/2, E4_lat = lat_cave-d.lat/2
  )
  
  #assuming that longitudes are in 0 to 360 and not from -180 to 180
  if(corner$E1_lon<0){corner$E1_lon = 360 + corner$E1_lon}
  if(corner$E1_lon>360){corner$E1_lon = corner$E1_lon-360}
  if(corner$E2_lon<0){corner$E2_lon = 360 + corner$E2_lon}
  if(corner$E2_lon>360){corner$E2_lon = corner$E2_lon-360}
  if(corner$E3_lon<0){corner$E3_lon = 360 + corner$E3_lon}
  if(corner$E3_lon>360){corner$E3_lon = corner$E3_lon-360}
  if(corner$E4_lon<0){corner$E4_lon = 360 + corner$E4_lon}
  if(corner$E4_lon>360){corner$E4_lon = corner$E4_lon-360}
  
  #we desire that lon_real>long_grid and lat_real<lat_grid
  corner$E1_lon_pos <- which.min(abs(lon_list-corner$E1_lon))
  if(lon_list[corner$E1_lon_pos]>corner$E1_lon){corner$E1_lon_pos = corner$E1_lon_pos -1}
  corner$E1_lat_pos <- which.min(abs(lat_list-corner$E1_lat))
  if(lat_list[corner$E1_lat_pos]>corner$E1_lat){corner$E1_lat_pos = corner$E1_lat_pos +1}
  
  corner$E2_lon_pos <- which.min(abs(lon_list-corner$E2_lon))
  if(lon_list[corner$E2_lon_pos]>corner$E2_lon){corner$E2_lon_pos = corner$E2_lon_pos -1}
  corner$E2_lat_pos <- which.min(abs(lat_list-corner$E2_lat))
  if(lat_list[corner$E2_lat_pos]>corner$E2_lat){corner$E2_lat_pos = corner$E2_lat_pos +1}
  
  corner$E3_lon_pos <- which.min(abs(lon_list-corner$E3_lon))
  if(lon_list[corner$E3_lon_pos]>corner$E3_lon){corner$E3_lon_pos = corner$E3_lon_pos -1}
  corner$E3_lat_pos <- which.min(abs(lat_list-corner$E3_lat))
  if(lat_list[corner$E3_lat_pos]>corner$E3_lat){corner$E3_lat_pos = corner$E3_lat_pos +1}
  
  corner$E4_lon_pos <- which.min(abs(lon_list-corner$E4_lon))
  if(lon_list[corner$E4_lon_pos]>corner$E4_lon){corner$E4_lon_pos = corner$E4_lon_pos -1}
  corner$E4_lat_pos <- which.min(abs(lat_list-corner$E4_lat))
  if(lat_list[corner$E4_lat_pos]>corner$E4_lat){corner$E4_lat_pos = corner$E4_lat_pos +1}
  
  ratio <- list()
  
  ratio$E1 <- (corner$E1_lon - lon_list[corner$E1_lon_pos])*(corner$E1_lat - lat_list[corner$E1_lat_pos])/(d.lon*d.lat)
  ratio$E2 <- (lon_list[corner$E2_lon_pos]+d.lon-corner$E2_lon)*(corner$E2_lat - lat_list[corner$E2_lat_pos])/(d.lon*d.lat)
  ratio$E3 <- (corner$E3_lon - lon_list[corner$E3_lon_pos])*(lat_list[corner$E3_lat_pos-1]-corner$E3_lat)/(d.lon*d.lat)
  ratio$E4 <- (lon_list[corner$E4_lon_pos]+d.lon-corner$E4_lon)*(lat_list[corner$E4_lat_pos-1]-corner$E4_lat)/(d.lon*d.lat)
  #ratio$E1+ratio$E2+ratio$E3+ratio$E4
  
  result_list <- c(corner, ratio)
  
  return(result_list)
}
  

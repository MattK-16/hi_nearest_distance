library('move2')
library('sf')
library('osmdata')
library('ggplot2')
library('units')
library('mapview')
library('devtools')
#devtools::install_github('jedalong/wildlifeHI')
#library('wildlifeHI')

hi_distance_local <- function(move,osmdata,...){

  #check if move2
  if(!inherits(move, "move2")){
    print('Input data not of class move2. Returning NULL.')
    return(NULL)
  }
  
  if (missing(osmdata)){
    osmdata <- hi_get_osm(move, ...)
  }
  
  move$nearest_key <- NA
  move$nearest_value <- NA
  move$nearest_distance <- NA
  
  if (!is.null(osmdata)){
    
    #preserve move for end
    sf_pt <- move
    move2::mt_track_id(sf_pt) <- NULL
    sf_pt <- sf::st_as_sf(sf_pt)
    
    #distance to features
    nearest <- sf::st_nearest_feature(sf_pt, osmdata)
    
    #ASSUME 'OSM-like' data with columns named "key" and "value"
    move$nearest_key <- sf::st_drop_geometry(osmdata)[nearest,'key']
    move$nearest_value <- sf::st_drop_geometry(osmdata)[nearest,'value']
    move$nearest_distance <- sf::st_distance(sf_pt,osmdata[nearest,],by_element=TRUE)
  }
  
  
  return(move)
}

hi_get_osm <- function(move,key='highway',value,bbox,geom="line",poly2line=TRUE){
  
  #check if move2
  if(!inherits(move, "move2")){
    print('Input data not of class move2. Returning NULL.')
    return(NULL)
  }
  
  if (missing(bbox)){
    bbox <- sf::st_bbox(move)
    x10 <- (bbox$xmax - bbox$xmin)*0.1
    y10 <- (bbox$ymax - bbox$ymin)*0.1
    bbox <- bbox + c(-x10,-y10,x10,y10)
  }
  
  ## Error handling for large BBOX
  ## ===============================================
  
  if (missing(value)) {
    osmdata <- try ({
      #could modify OSM values to facilitate larger bboxes (e.g., memsize or timeout)
      osmdata::opq (bbox = bbox) |>
        osmdata::add_osm_feature (key = key) |>
        osmdata::osmdata_sf ()
    })
  } else {
    osmdata <- try ({
      #could modify OSM values to facilitate larger bboxes (e.g., memsize or timeout)
      osmdata::opq (bbox = bbox) |>
        osmdata::add_osm_feature (key = key, value=value) |>
        osmdata::osmdata_sf ()
    })
  }
  
  
  if (class (osmdata) [1] == "try-error") {
    writeLines('wildlifeHI Issue: OSM data request timed out. Suggestions include: \n
               - Using the key and value parameters to choose less OSM features \n
               - Subset tracking data to smaller areas \n
               - Subset tracking data by individual(s) \n
               Please see the wildlifeHI documentation for further discussion of this issue.')
    return(NULL)
  } 
  
  
  
  osm_sf <- NULL
  if ('point' %in% geom) {
    temp <- osmdata$osm_points
    if (is.null(temp$osm_id)) { temp$osm_id <- row.names(temp)}
    if (length(temp)>0) {
      temp$key <- key
      temp$value <- sf::st_drop_geometry(temp)[,key]
      osm_sf <- rbind(osm_sf,temp[,c('osm_id','key','value')])
    }
  } 
  if ('line' %in% geom) {
    temp <- osmdata$osm_lines
    if (is.null(temp$osm_id)) { temp$osm_id <- row.names(temp)}
    if (length(temp)>0) {
      temp$key <- key
      temp$value <- sf::st_drop_geometry(temp)[,key]
      temp <- temp[,c('osm_id','key','value')]
      if (!is.null(osmdata$osm_multilines)){
        suppressWarnings(temp2 <- osmdata$osm_multilines |> sf::st_cast('LINESTRING'))
        temp2$key <- key
        temp2$value <- sf::st_drop_geometry(temp2)[,key]
        if (is.null(temp2$osm_id)) { temp2$osm_id <- row.names(temp2)}
        temp <- rbind(temp,temp2[,c('osm_id','key','value')])
      }
      osm_sf <- rbind(osm_sf,temp)
    }
  }
  if ('polygon' %in% geom) {
    temp <- osmdata$osm_polygons
    if (is.null(temp$osm_id)) { temp$osm_id <- row.names(temp)}
    if (length(temp)>0) {
      temp$key <- key
      temp$value <- sf::st_drop_geometry(temp)[,key]
      temp <- temp[,c('osm_id','key','value')]
      if (!is.null(osmdata$osm_multipolygons)){
        suppressWarnings(temp2 <- osmdata$osm_multipolygons |> sf::st_cast('POLYGON'))
        temp2$key <- key
        temp2$value <- sf::st_drop_geometry(temp2)[,key]
        if (is.null(temp2$osm_id)) { temp2$osm_id <- row.names(temp2)}
        temp <- rbind(temp,temp2[,c('osm_id','key','value')])
      }
      if (poly2line) {
        suppressWarnings(temp <- sf::st_cast(temp,"LINESTRING"))
      }
      osm_sf<- rbind(osm_sf, temp)
    }
  }
  if (is.null(osm_sf)){
    writeLines('wildlifeHI Issue: The requested OSM query returned NULL. Please check:
                  - The study area contains OSM features
                  - The specified key and value inputs are correct')
  }
  return(osm_sf)
  
}

rFunction = function(data,r=r,key=key,value=value,geom=geom,poly2line=poly2line) {
  
  #call hi_distance from package
  if (value == 'all'){
    move_dist <- hi_distance_local(move=data,key=key,geom=geom,poly2line=poly2line)
  } else {
    move_dist <- hi_distance_local(move=data,key=key,value=value,geom=geom,poly2line=poly2line)
  }
  
  #give R appropriate units
  units(r) <- units(move_dist$nearest_distance)
  
  move_df <- data.frame(move_dist)
  move_df$timestamp <- mt_time(move_dist)
  move_df$trackId <- mt_track_id(move_dist)
  
  #Distance Plot
  pdfName <- paste0('DistanceAnalysisPlot_r_',r,'.pdf')
  
  dplot <- ggplot(move_df,aes(x=timestamp,y=nearest_distance)) + 
    geom_line() + 
    geom_hline(yintercept=r,linetype=2,color='red') + 
    facet_wrap(~trackId, scales='free_x')
  
  ggsave(appArtifactPath(pdfName), dplot)
  
  
  #Within Distance
  sub_df <- subset(move_df,nearest_distance < r)
  dtab <- table(sub_df$trackId,sub_df$nearest_value)
  csvName <- paste0('DistanceAnalysisTable_r_',r,'.csv')
  write.csv(dtab,appArtifactPath(csvName),row.names=TRUE)
  
  #return move2 with info appended
  return(move_dist)
}


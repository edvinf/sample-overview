require(Rstox)

#
# In order to get an overview of the samples obtained from prøvebåten, before age readings are finished, 
# this script reads partial registrations according to the informal protocoll on board.
# All information is read from the catch-level.
# 
# The convention or informal protocol on board is that catches are registered down to the cacthsample level once sampels are gathered.
# Excempt from this are the fields for total sample weight and total catch numbers, which will be calculated once the individual level is processed.
# Importantly for the functions below, number of individuals in sample, the sample type and the product type is registered almost at once, along with catch positions.
#
# Important issues: 
# can not access platform: Needed to report no of vessels sampled.
#

testfile <- "/Users/a5362/code/masters/proveoversikt/data/11-2017-3654-1.xml"

keys_ca <- c("cruise", "serialno", "species", "samplenumber", "noname", "aphia")
keys_fs <- c("cruise", "serialno")

gearmap_hi <- list("31"="Trawl", # Bunntrål
                   "32"="Trawl", # Reketrål
                   "34"="Trawl", # Trål uspesifisert
                   "35"="Trawl", # Flytetrål
                   "36"="D. Seine", # Snurrevad
                   "37"="Seine", # Not
                   "41"="Gillnet", # Bunngarn/Flytegarn
                   "43"="Trap", # Ruse
                   "51"="Longline", # Line
                   "52"="Longline", # Snøre
                   "53"="Trap") # Teine

#' loads biotic data and flattens it, and annotate gears
#' @param filename biotic xml
#' @return data frame with station and catchsample merged, and a new column "gearname"
load_catch_data <- function(filename){
  bioticdata <- readXMLfiles(filename)
  flat_biotic <- merge(bioticdata$`1_ReadBioticXML_BioticData_FishStation.txt`, bioticdata$`1_ReadBioticXML_BioticData_CatchSample.txt`, by=keys_fs)
  flat_biotic[,"gearname"] <- unlist(lapply(substr(flat_biotic$gear,1,2), FUN=function(x){gearmap_hi[[x]]}))
  return(flat_biotic)
}

# functions for extracting from prøvebåt

#' Extract samples with length, weight, maturation and age
extract_full_samples <- function(flatdata){
  return(flatdata[!is.na(flatdata$sampletype) & !is.na(flatdata$samplemeasurement) & flatdata$sampletype == 20 & flatdata$samplemeasurement==1,])
}
#' Extract samples with at least length, weight, and age
extract_weight_samples <- function(flatdata){
  return(flatdata[!is.na(flatdata$sampletype) & !is.na(flatdata$samplemeasurement) & flatdata$sampletype == 20,])
}
#' Extract samples with at least age
extract_age_samples <- function(flatdata){
  return(flatdata[!is.na(flatdata$sampletype),])
}

#' creates a table where samples are aggregated on some combination of variables.
#' @param flatdata biotic data, as returned by load_data
#' @param grouping vector of variable names, defaults to c("species", "area", "gearname")
#' @return table with columns correspodnign to the grouping variables and the counts listed below
#' @details 
#' For all combinations of values for the given grouping variables (defaults to species and area), the following are reported
#' n_vessels: number of unique vessels (not implemented)
#' n_catches: number of catches, 
#' n_full_samples: number samples with length, weight, maturation and age
#' n_weight: number of samples with at least length, weight and age
#' n_age: number of samples with at least age
make_table_provebat <- function(flatdata, grouping=c("species", "area", "gear")){
  if (!all(grouping %in% names(flatdata))){
    stop("Invalid grouping variables. Must be columns in parameter flatdata.")
  }
  if (any(is.na(flatdata[,grouping]))){
    stop("NAs in grouping variables.")
  }
  agg <- aggregate(list("n_catches"=flatdata$serialno), as.list(flatdata[,grouping]), FUN=function(x){length(unique(x))})
  full_s <- extract_full_samples(flatdata)
  agg <- merge(agg, aggregate(list("n_full_samples"=full_s$lengthsamplecount), as.list(full_s[,grouping]), FUN=function(x)(sum(x, na.rm=T))), all.x=T)
  weight_s <- extract_weight_samples(flatdata)
  agg <- merge(agg, aggregate(list("n_weight"=weight_s$lengthsamplecount), as.list(weight_s[,grouping]), FUN=function(x)(sum(x, na.rm=T))), all.x=T)
  age_s <- extract_age_samples(flatdata)
  agg <- merge(agg, aggregate(list("n_age"=age_s$lengthsamplecount), as.list(age_s[,grouping]), FUN=function(x)(sum(x, na.rm=T))), all.x=T)
  agg[is.na(agg)]<-0 #all data are counts, so they can be set to 0 if missing
  agg[,"n_vessels"] <- NA
  return(agg)
}

#' Plots a map of location for all catches
#' @param flatdata biotic data formatted as parsed by load_data
#' @param col color for points
#' @param main title for plot
plot_map_catches <- function(flatdata, col="blue", main=""){
  library(rworldmap)
  library(rworldxtra)
  library(mapplots)
  data(countriesHigh)
  
  if (nrow(flatdata)==0){
    stop("No samples to plot")
  }
  if (any(is.na(flatdata$longitudestart)) | any(is.na(flatdata$latitudestart))){
    warning("Some catches does not have positions registered.")
  }
  
  min_lat <- min(flatdata$latitudestart, na.rm=T)
  max_lat <- max(flatdata$latitudestart, na.rm=T)
  min_long <- min(flatdata$longitudestart, na.rm=T)
  max_long <- max(flatdata$longitudestart, na.rm=T)
  
  map <- countriesHigh
  basemap(xlim=c(min_long,max_long), ylim=c(min_lat,max_lat), bg="white")
  plot(map, add=T)
  points(flatdata$longitudestart, flatdata$latitudestart, pch=4, col=col) 
  title(main)
}

#
# Example: plot species catches from snurrevad with at least age samples
#
example_cod <- function(file, art="TORSK"){
  fd <- load_catch_data(file)
  tab <- make_table_provebat(fd, grouping=c("area", "noname", "gearname"))
  tab <- tab[tab$noname==art,]
  print(tab)
  fd <- extract_age_samples(fd)
  fd <- fd[fd$noname==art,]
  if (any(duplicated(fd[,c("serialno", "samplenumber")]))){
    stop("duplicated catch registrations numbers.")
  }
  plot_map_catches(fd, col="blue", main=paste(art, "cacthes with at least age"))
}
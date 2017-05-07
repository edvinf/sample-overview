basedir <- "/Users/a5362/code/github/proveoversikt"
outpath <- paste(basedir, "output", sep="/")
datapath <- paste(basedir, "data", sep="/")
landingsfile <- "/Volumes/prosjekt/sluttseddel/sluttseddel_LSS_2005-2015/Fangst/FDIR_HI_LSS_FANGST_2015_PR_2016-12-08.psv"

logbook_frame_file <- paste(datapath, "logbook_frame.dat", sep="/")

read_psv <- function(file, dec_mark="."){
  loc <- default_locale()
  loc$decimal_mark <- dec_mark
  loc$encoding <- "latin1"
  db <- read_delim(file, delim="|", col_names=T, trim_ws=TRUE, na=c("", "na", "NA"),locale=loc)
  return(db)  
}
download_landings <- function(){
  landings <- read_psv(landingsfile, dec_mark = ",")
  landings$`Art (kode)` <- as.integer(landings$`Art (kode)`)
  save(landings, file=landings_frame_file)
}
#load landings data from data folder
load_landings <- function(){
  load(landings_frame_file)
  return(landings)
}

#' extracts landings of fresh / unconserved fish
extract_fresh_landings <- function(landings){
  fresh <- landings[landings$`Konserveringsmåte (bokmål)`=="Fersk/ukonservert",]
  return(fresh)
}

#' Make table reporting landings by the grouping variables
make_table <- function(landings, grouping=c("Art (kode)", "Hovedområde (kode)", "Redskap (kode)")){
  if (!all(grouping %in% names(landings))){
    stop("Invalid grouping variables, must be columns in landings")
  }
  if (any(is.na(landings[,grouping]))){
    stop("NAs in grouping variables.")
  }
  agg <- aggregate(list("total_catch"=landings$Rundvekt), as.list(landings[,grouping]), FUN=function(x){sum(x, na.rm=T)})
  un <- landings[!duplicated(landings[,c(grouping, "Registreringsmerke (seddel)", "Siste fangstdato")]),]
  agg <- merge(agg, aggregate(list("n_landings"=un$"Art (kode)"), as.list(un[,grouping]), FUN=length))
  return(agg)
}

#Example
tab <- make_table(extract_fresh_landings(load_landings()), grouping=c("Art (bokmål)", "Hovedområde (kode)", "Hovedgruppe redskap (bokmål)"))
torsk <- tab[tab$"Art (bokmål)"=="Torsk" | tab$"Art (bokmål)"=="Nordøstarktisk torsk",]
warning("Not matching taxa properly! Annotate with taxa and quarter")
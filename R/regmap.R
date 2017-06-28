#' @title Maps visualization
#'
#' @description Plot any territory given Geojson file containing coordinates.
#' @name mapregions
#' @docType data
#' @usage map(geofile, territory, measure, ...)
#' @param geofile geoJson file to read.
#' @param territory vector of characters/factor; territories field (usualy countries, cities, etc) to choose.
#' @param measure numeric (usually); in the moste cases a quantitative variable to be setted up as measure (for example: location area, etc)
#' @param label string; the name of measure field.
#' @param main string; the title of map.
#' @param mapcol color of specific regions.
#' @param xlimit numeric; x-axis limits.
#' @param ylimit numeric; y-axis limits.
#' @return plot
#' @export
NULL
regnames<-function(dataset,datacol,file,filecol){
  dataset[which(!toupper(dataset[,datacol])%in%file@data[,filecol]),]
}
#notmatch=as.character(regnames(dataset=exemp,datacol=1,file=file1,filecol=1)[,1])

subst<-function(string,replace,dataset,datacol){
  mgsub(string,replace, dataset[,datacol])
}
#distr=subst(string=notmatch,replace=c("CASTELO BRANCO","VIANA DO CASTELO"),dataset=exemp,datacol=1)

matchs<-function(dataset,datacol,file,filecol){
  dataset[match(file@data[,filecol],toupper(distr)),datacol]
}
#file1@data[,2]=matchs(exemp,2,file1,1)
regmap<-function(
  geofile,
  territory,
  measure,
  label,
  main,
  mapcol,
  xlimit,
  ylimit
){
  sp.label <- function(x, label) {
    list("sp.text", coordinates(x), cex=0.5,label)
  }
  numb.sp.label <- function(x) {
    sp.label(x,paste(substr(territory,1,3), measure, sep="-"))
  }
  make.numb.sp.label <- function(x) {
    do.call("list", numb.sp.label(x))
  }
  spplot(
    geofile,
    zcol=label,
    main=main,
    col.regions =colorRampPalette(mapcol)(length(territory)+16),
    sp.layout = make.numb.sp.label(geofile),
    scales=list(draw=T),
    col="grey",
    edge.col="grey",
    colorkey=TRUE,
    xlim = apply(coordinates(geofile),2,range)[,1]+xlimit,
    ylim = apply(coordinates(geofile),2,range)[,2]+ylimit
  )
}

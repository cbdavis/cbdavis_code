#This file contains several functions that are useful utilities in the process of instance matching

library(reshape) #colsplit
library(gdata) #trim
library(RecordLinkage) #string matching

#never ever ever convert strings to factors
options(stringsAsFactors = FALSE)


#http://en.wikipedia.org/wiki/Jaccard_index
#Look at length of intersection and union of tokens in two strings to determine similarity
jaccard_index <- function(text1, text2){
  set1 = tokenize(text1)
  set2 = tokenize(text2)
  jaccard_index = length(intersect(set1, set2)) / length(union(set1, set2))
  return(jaccard_index)
}

#return a set of tokens contained within a string
tokenize <- function(text){
  text = removeTheWeirdness(text)
  tokenList = unique(unlist(strsplit(text, split=" ")))
  return(tokenList)
}

retrieveCountryDataFromEnipedia <- function (country) {
  enipediaData = NULL
  if (country != ""){
    endpoint = "http://enipedia.tudelft.nl/sparql/sparql"
    queryString = paste(getPrefixes(), 
                        "select * where {
                      ?x rdf:type cat:Powerplant .
                        ?x prop:City ?city . 
                        ?x rdfs:label ?name . 
                        ?x prop:Country a:", gsub(" ", "_", country) ," . 
                        ?x prop:State ?state .
                        ?x prop:Ownercompany ?owner . 
                        ?x prop:Point ?point . 
                        }", sep="")
    d <- SPARQL(url=endpoint, query=queryString, format='csv', extra=list(format='text/csv'))
    enipediaData = d$results
    if (dim(enipediaData)[1] > 0){
      coords = extractCoordinates(enipediaData$point)
      enipediaData$lat = coords$lat
      enipediaData$lon = coords$lon
    } else {
      print("no results for country")
    }
  } else {
    print("no country specified")
  }
  return(enipediaData)
}

extractCoordinates <- function(point){
  coords = colsplit(point, split=",", names=c("lat", "lon"))
  
  #make sure that this is a character vector, not a factor vector
  coords$lon = as.character(coords$lon)
  coords$lat = as.character(coords$lat)
  
  #fix up lat and lon values
  eastLonLocs = grep("E", coords$lon)
  coords$lon[eastLonLocs] = gsub(' E', '', coords$lon[eastLonLocs])
  coords$lon[eastLonLocs] = as.numeric(coords$lon[eastLonLocs])
  
  westLonLocs = grep("W", coords$lon)
  coords$lon[westLonLocs] = gsub(' W', '', coords$lon[westLonLocs])
  coords$lon[westLonLocs] = 0 - as.numeric(coords$lon[westLonLocs])
  
  northLatLocs = grep("N", coords$lat)
  coords$lat[northLatLocs] = gsub(' N', '', coords$lat[northLatLocs])
  coords$lat[northLatLocs] = as.numeric(coords$lat[northLatLocs])
  
  southLatLocs = grep("S", coords$lat)
  coords$lat[southLatLocs] = gsub(' S', '', coords$lat[southLatLocs])
  coords$lat[southLatLocs] = 0 - as.numeric(coords$lat[southLatLocs])
  
  coords$lon = as.numeric(coords$lon)
  coords$lat = as.numeric(coords$lat)
  
  return(coords)
}

#These are common prefixes used with the Enipedia SPARQL endpoint
getPrefixes <- function(){
  return("PREFIX a: <http://enipedia.tudelft.nl/wiki/>
          PREFIX prop: <http://enipedia.tudelft.nl/wiki/Property:>
         PREFIX cat: <http://enipedia.tudelft.nl/wiki/Category:>
         PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
         PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
         PREFIX fn: <http://www.w3.org/2005/xpath-functions#>
         PREFIX afn: <http://jena.hpl.hp.com/ARQ/function#>")
}


#get rid of characters that get in the way
#this makes it easier to perform string comparisons
removeTheWeirdness <- function(text){
  text = iconv(text, to="ASCII//TRANSLIT") #work with simple ascii - this doesn't do anything to help with misspellings
  text = gsub('http://enipedia.tudelft.nl/wiki/', '', text)
  text = gsub('\\)', '', text)
  text = gsub('\\(', '', text)
  text = gsub('/', '', text)
  text = gsub(',', ' ', text)
  text = gsub('\\.', ' ', text)
  text = gsub('&', '', text)
  text = gsub('_', ' ', text)
  text = gsub('-', ' ', text)
  text = gsub('  ', ' ', text)
  text = gsub("'", "", text)
  text = trim(tolower(text))
  return(text)
}

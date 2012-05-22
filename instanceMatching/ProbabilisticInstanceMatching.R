library(reshape) #colsplit
library(gdata) #trim
library(geosphere)

#TODO what is this again?
getScore <- function(charVector, countsVector, numEntries){
  totalScore = 1
  for (token in charVector){
    loc = which(names(countsVector) == token)
    if(length(loc) > 0){ #make sure have a match
      #probability based on the number of entries containing this word
      probUnique = (1-(countsVector[loc]/numEntries))
      print(paste("match on ", token, " - ", probUnique, sep=""))
      totalScore =  totalScore * probUnique
    }
  }
  return(totalScore)
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

#this splits up each entry in a character vector by space and then
#counts the frequency of each of these words
#The idea is that this gives an idea of which terms are generic, despite the language
getFrequencyListOfWords <- function(charVector){
  #remove weird characters so can tokenize better
  charVector = removeTheWeirdness(charVector)
  nameTokens = unlist(strsplit(paste(charVector, collapse=" "), split=" "))
  #Thank you internet
  #http://sylvainforet.blogspot.com/2008/06/counting-uniq-elements-of-list-in-r_12.html
  counts = NULL
  s <- sort(nameTokens)
  r <- rle(s)
  counts[r$values] <- r$lengths
  return(counts)  
}

#this shows which entries contain which tokens
getTokenLookupMap <- function(charVector){
  #this is important - must declare as a list
  tokenLookup = list()
  charVector = removeTheWeirdness(charVector)
  allUniqueTokens = unique(unlist(strsplit(paste(charVector, collapse=" "), split=" ")))
  
  #this shows which of the entries contain which of the tokens
  tokenLookup[allUniqueTokens] = NULL
  curIndex = 1
  for (name in charVector){
    nameTokens = unlist(strsplit(name, split=" "))
    for (token in nameTokens){
      tokenLookup[[token]] = append(tokenLookup[[token]], curIndex)
    }
    curIndex = curIndex + 1
  }
  return(tokenLookup)
}

#never ever ever convert strings to factors
options(stringsAsFactors = FALSE)

library(SPARQL)

setwd("/home/cbdavis/Desktop/svn/ChrisDavis/PhD/Enipedia/InstanceMatching/")

data = read.csv("/home/cbdavis/Downloads/power_plants_from_industryabout.csv", header=TRUE, sep=",")
data = data[-which(data$country != "France"),]
#make a soup column for matching - this contains everything that could be used to create the name of the power plant
#TODO need to have a synonym or relationship lookup - need to add more information, give more context to help with matching
#such as SNET is now E.ON France
data$soup = paste(data$name, data$owner, data$area, data$other_name, data$owners)

#take all the names, put them in a giant vector, then split by space
extDataCounts = getFrequencyListOfWords(data$soup)

#find all the stuff in Germany
endpoint = "http://enipedia.tudelft.nl/sparql/sparql"
queryString = paste(getPrefixes(), 
                    "select * where {
                    ?x rdf:type cat:Powerplant .
                    ?x prop:City ?city . 
                    ?x rdfs:label ?name . 
                    ?x prop:Country a:France . 
                    ?x prop:City ?city .
                    ?x prop:State ?state .
                    ?x prop:Ownercompany ?owner . 
                    ?x prop:Point ?point . 
                    }", sep="")
d <- SPARQL(url=endpoint, query=queryString, format='csv', extra=list(format='text/csv'))
enipediaData = d$results
coords = extractCoordinates(enipediaData$point)
enipediaData$lat = coords$lat
enipediaData$lon = coords$lon
enipediaData$soup = paste(enipediaData$name, enipediaData$owner, enipediaData$city, enipediaData$state)

enipediaCounts = getFrequencyListOfWords(enipediaData$soup)

#now do something intelligent... um
#remove weirdness from the soup
enipediaData$soup = removeTheWeirdness(enipediaData$soup)
data$soup = removeTheWeirdness(data$soup)

eniTokenLookup = getTokenLookupMap(enipediaData$soup)
extDataTokenLookup = getTokenLookupMap(data$soup)

#iterate over each set of terms in the soup describing each power plant
index = 1
for (extSoup in data$soup){
  #convert to tokens
  extSoup = removeTheWeirdness(extSoup)
  extDataTokens = unique(unlist(strsplit(extSoup, split=" ")))

  matchesLookup = data.frame(informationEntropy = rep(0, length(enipediaData$name)),
                             distance = rep(0, length(enipediaData$name)))
  
  print("=====================================")
  for (token in extDataTokens){
    informationEntropy = -log10(length(extDataTokenLookup[[token]])/length(data$name))
    #only match based on high entropy tokens - this avoids generic terms
    #find the entries that have the most number of matches
    #if (is.finite(informationEntropy)){
    if (informationEntropy > 1 && is.finite(informationEntropy)){
      #see if this token also has high information entropy in the other dataset
      informationEntropyInOtherDataset = -log10(length(eniTokenLookup[[token]])/length(enipediaData$name))
      if (informationEntropyInOtherDataset > 1 && is.finite(informationEntropyInOtherDataset)){
      #if (is.finite(informationEntropyInOtherDataset)){
        print(token)
        #need to do something intelligent here that rates the suitability of the matches
        #could just do string comparisons here
        #print(eniTokenLookup[token])
        
        #print(informationEntropyInOtherDataset)
        #print(informationEntropy)
        #look at all the matches within a certain radius, and the number of tokens matched
        #need data on the number of matching tokens per possible candidate match
        #collect data on information entry of term, number of matching terms per candidate, and distance of candidates
        eniMatches = unique(eniTokenLookup[[token]])
        
        matchesLookup$informationEntropy[eniMatches] = matchesLookup$informationEntropy[eniMatches] + informationEntropyInOtherDataset
        
        #look at information entropy of coordinates - information entropy of continious variables
        #look at all things within x km of the point?  or of everything in the selection?
        
      }
    }
  }
  
  #ok, who has information entropy above zero? (who has the most unique matching terms?)
  locs = which(matchesLookup > 0)
  if(length(locs) > 0){
    #find the distances to each of these
    
    distances = distCosine(cbind(data$longitude[index], 
                                 data$latitude[index]), 
                           cbind(enipediaData$lon[locs],
                                 enipediaData$lat[locs])
                           )
    matchesLookup$distance[locs] = distances
  }
  
  #TODO need to do emissions comparison also, look for obvious things like hydro and coal
  #the emissions should fit within certain bins
  
  #don't consider these entries
  locs = which(matchesLookup$informationEntropy == 0)
  matchesLookup$distance[locs] = NA
  matchesLookup$informationEntropy[locs] = NA
  
  #this is nice - how does it differ for all of the matches
  #plot(matchesLookup$distance, matchesLookup$informationEntropy)
  
  index = index + 1
}




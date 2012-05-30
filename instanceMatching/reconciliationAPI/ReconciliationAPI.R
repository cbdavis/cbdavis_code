#never ever ever convert strings to factors
options(stringsAsFactors = FALSE)

library(SPARQL)
library(rjson)

#setwd('/home/cbdavis/Desktop/svn/ChrisDavis/PhD/Enipedia/InstanceMatching')
setwd('/var/www')
source('InstanceMatchingUtilities.R')

getMatches <- function (queryRequest, numResults=5) {  
    
  country = ""
  owner = ""
  point = NULL
  latitude = NULL
  longitude = NULL
  #extract the different properties that are used for matching, allow for various spellings to minimize user error
  #TODO include capacity, yearly production, emissions, etc
  for (property in queryRequest$properties){
    #print(property)
    if (tolower(property$p) == "country"){
      country = property$v
    }
    if (tolower(property$p) == "owner" || tolower(property$p) == "ownercompany"){
      owner = property$v
    }
    if (tolower(property$p) == "latitude" || tolower(property$p) == "lat"){
      latitude = property$v
    }
    if (tolower(property$p) == "longitude" || tolower(property$p) == "long" || tolower(property$p) == "lon"){
      longitude = property$v
    }
    if (tolower(property$p) == "point" || tolower(property$p) == "coords" || tolower(property$p) == "coordinates"){
      coords = property$v
      tmp = extractCoordinates(coords)
      latitude = tmp$lat
      longitude = tmp$lon
    }
    
  }
  
  #TODO need some check to correct the country - find closest match if slightly misspelled
  #or give feedback to the user about what they should do
  
  enipediaData = retrieveCountryDataFromEnipedia(country)
  
  #get geographic information
  coords = extractCoordinates(enipediaData$point)
  enipediaData$lat = coords$lat
  enipediaData$lon = coords$lon
  
  #TODO implement matching on a soup consisting of the owner, place, etc.
  enipediaCleanedName = removeTheWeirdness(gsub(' Powerplant', '', enipediaData$name))
  
  
  #write('name to match on is- ', stderr())
  #write(removeTheWeirdness(queryRequest$query), stderr())
  
  #perform string matching
  ldiff = levenshteinSim(removeTheWeirdness(queryRequest$query), 
                         enipediaCleanedName)
  
  jdiff = jarowinkler(removeTheWeirdness(queryRequest$query), 
                      enipediaCleanedName, 
                      r=0.5)
  
  jaccard_index_values = unlist(lapply(enipediaCleanedName, function(x) {jaccard_index(x,removeTheWeirdness(queryRequest$query))}))
  
  #TODO allow json query strings to specify this
  distanceCutoff = 20000
  distanceScores = NULL
  if(!is.null(latitude) && !is.null(longitude)){
    
    distances = distCosine(cbind(longitude, 
                                 latitude), 
                           cbind(enipediaData$lon,
                                 enipediaData$lat)
                           )
    #scale distance scores from 0 to 1, with 0 representing the distance cutoff
    #and 1 meaning that the point is directly on top of it
    distanceScores = distances
    distanceScores[which(distances > distanceCutoff)] = 0
    distanceScores = 1 - (distanceScores / distanceCutoff);
  }
  
  #TODO this will need to accomodate more things like matches based on company name, etc.
  #figure out some robust way to do this
  #may want to also do some sort of soup matching by default with the city, owner, etc.
  #find the distance from the origin
  if (!is.null(distanceScores)){
    dist = sqrt(ldiff^2 + jdiff^2 + jaccard_index_values^2 + distanceScores^2)
  } else {
    dist = sqrt(ldiff^2 + jdiff^2 + jaccard_index_values^2)
  }
  
  locs = sort(dist, decreasing=TRUE, index.return=TRUE)$ix[c(1:numResults)]
    
  allResultsForQuery = list()
  for (loc in locs){
    resultSet = list(id=enipediaData$x[loc],
                     name=paste('name:',enipediaData$name[loc],'|',
                                'owner:',removeTheWeirdness(enipediaData$owner[loc]), '|',
                                'city:',removeTheWeirdness(enipediaData$city[loc]), sep=""),
                     type=list(c(id="http://enipedia.tudelft.nl/wiki/Category:Powerplant",
                                 name="Powerplant")),
                     score=dist[loc],
                     latitude = enipediaData$lat[loc],
                     longitude = enipediaData$lon[loc],
                     match=FALSE) # hard-coded letting the humans always check things off
   
    allResultsForQuery$result = c(allResultsForQuery$result,list(resultSet))
  }
  
  return(allResultsForQuery)  
}



#use this url to test the request
#http://localhost/testMatch.php?query={%22query%22:%22Awirs%22,%22limit%22:3,%22type%22:%22Powerplant%22,%22type_strict%22:%22any%22,%22properties%22:[{%22p%22:%22Country%22,%22v%22:%22Belgium%22}]}

#get the data from the command line arguments
jsonString=commandArgs(trailingOnly = T)

#get rid of escaped single quotes, this breaks things
#escaped quotes are used via the command line call from the php code
jsonString = gsub("\\\\'", "'", jsonString)

# can't get matches on RÖMERBRÜCKE to work correctly
# the php code says that the encoding is ASCII
jsonString = iconv(jsonString, to="ASCII//TRANSLIT") 

#Debugging
#write('jsonString is - ', stderr())
#write(jsonString, stderr())


##### Not supported yet #####
# pid - this ties into freebase
# only type supported is powerplants - would be useful to include energy companies as well
# only property supported is Country - figure out how to add more fuzzy properties - fuel type, etc
# some parameters to try different types of matching strategies?

#TODO - locally cache query results?  just have files for each of the countries?
#Sparql queries on enipedia are cached via Squid anyway, shouldn't be that big of a performance hit
#TODO code is slow since required libraries are loaded all the time - possible to make them always loaded?

#used for multiple queries to build up the json results
allResultsForAllQueries = list()

request = fromJSON(jsonString)

if (names(request)[1] == "q0") { #multiple queries 
  queryCount = 1
  for (queryRequest in request){
    allResultsForQuery = getMatches(queryRequest)    
    
    #this tells us which query we're looking at
    #it's usually something like q0, q1, etc
    queryIdentifier = names(request)[queryCount]

    #add query results here
    allResultsForAllQueries[queryIdentifier] = list(allResultsForQuery)
    
    queryCount = queryCount + 1
  }
  #send output to calling php code
  write(toJSON(allResultsForAllQueries), stdout())
} else { #single query
  allResultsForQuery = getMatches(request)
  #send output to calling php code
  write(toJSON(allResultsForQuery), stdout())
}



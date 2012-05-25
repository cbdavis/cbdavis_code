library(reshape) #colsplit
library(gdata) #trim
library(geosphere)
library(RecordLinkage) #string matching

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


lookForMatches <- function(df1, df2, #the two data frames to compare
                           soupColumnsFromDataSet1, #list of columns to combine together for matching
                           soupColumnsFromDataSet2, 
                           lat1=NULL, #coordinate columns in first data frame
                           lon1=NULL, 
                           lat2=NULL, #coordinate columns in second data frame 
                           lon2=NULL, 
                           outputFilename="test.csv", #file that shows possible matching entries side-by-side from both data sets
                           maxDistance=20000){ #only match if within this distance of each other

  df1$soup = apply(df1[,soupColumnsFromDataSet1], 1, function(x) paste(x, sep="", collapse=" "))
  df1$soup = removeTheWeirdness(df1$soup)
  
  df2$soup = apply(df2[,soupColumnsFromDataSet2], 1, function(x) paste(x, sep="", collapse=" "))
  df2$soup = removeTheWeirdness(df2$soup)

  #now do something intelligent... um
  #remove weirdness from the soup
  #enipediaData$soup = removeTheWeirdness(enipediaData$soup)
  #data$soup = removeTheWeirdness(data$soup)
  
  df1TokenLookup = getTokenLookupMap(df1$soup)
  df2TokenLookup = getTokenLookupMap(df2$soup)
  #eniTokenLookup = getTokenLookupMap(enipediaData$soup)
  #extDataTokenLookup = getTokenLookupMap(data$soup)
  
  #contains indices for matches
  possibleMatches = NULL
  
  #compares possible matches with info from both data sources
  matches = NULL
  
  #iterate over each set of terms in the soup describing each power plant
  index = 1
  for (df1SoupEntry in df1$soup){
    #convert to tokens
    #extSoup = removeTheWeirdness(extSoup)
    df1DataTokens = unique(unlist(strsplit(df1SoupEntry, split=" ")))
    #extDataTokens = unique(unlist(strsplit(extSoup, split=" ")))
    
    #this is created for every entry in the external data set
    matchesLookup = data.frame(informationEntropy = rep(0, dim(df2)[1]),
                               distance = rep(0, dim(df2)[1]))
    
    print("=====================================")
    for (token in df1DataTokens){
      informationEntropy = -log10(length(df1TokenLookup[[token]])/dim(df1)[1])
      #only match based on high entropy tokens - this avoids generic terms
      #find the entries that have the most number of matches
      #if (is.finite(informationEntropy)){
      if (informationEntropy > 1 && is.finite(informationEntropy)){
        #see if this token also has high information entropy in the other dataset
        informationEntropyInOtherDataset = -log10(length(df2TokenLookup[[token]])/dim(df2)[1])
        if (informationEntropyInOtherDataset > 1 && is.finite(informationEntropyInOtherDataset)){
          #if (is.finite(informationEntropyInOtherDataset)){
          print(token)
          df2Matches = unique(df2TokenLookup[[token]])
          matchesLookup$informationEntropy[df2Matches] = matchesLookup$informationEntropy[df2Matches] + informationEntropyInOtherDataset
          }
        }
      }
    
      #ok, who has information entropy above zero? (who has the most unique matching terms?)
      locs = which(matchesLookup > 0)
      if(length(locs) > 0){
        #check if coordinate fields are set
        if (!is.null(lat1) && !is.null(lon1) && !is.null(lat2) && !is.null(lon2)){
          #find the distances between possible matches
          
          distances = distCosine(cbind(df1[index, lon1], 
                                       df1[index, lat1]), 
                                 cbind(df2[locs, lon2],
                                       df2[locs, lat2])
                                 )
          matchesLookup$distance[locs] = distances
        }
      }
    
      #don't consider these entries
      locs = which(matchesLookup$informationEntropy == 0)
      matchesLookup$distance[locs] = NA
      matchesLookup$informationEntropy[locs] = NA
      
      #now put possible matches together into a spreadsheet format where we can verify if everything looks ok
      #TODO parameterize this, make a function out of this whole code
      #find all possible matches within a 20 km radius
      #TODO should just write out entries here for comparison
      locs = which(matchesLookup$distance < 20000)
      if(length(locs) > 0){
        jaccardIndexValues = unlist(lapply(df2$soup[locs], function(x) {jaccard_index(x,df1$soup[index])}))
        possibleMatches = rbind(possibleMatches, cbind(index, locs, jaccardIndexValues, matchesLookup$distance[locs]))
      }
      index = index + 1
    }

  #write out the Jaccard Index as well
  
  ldiff = levenshteinSim(df1$soup[possibleMatches[,1]], 
                         df2$soup[possibleMatches[,2]])
  
  jdiff = jarowinkler(df1$soup[possibleMatches[,1]], 
                      df2$soup[possibleMatches[,2]], 
                      r=0.5)
  
  matches = cbind(df1[possibleMatches[,1],soupColumnsFromDataSet1], 
                  df2[possibleMatches[,2],soupColumnsFromDataSet2],
                  possibleMatches[,3], #jaccard index values
                  possibleMatches[,4], #distance
                  ldiff, #levenshtein distance
                  jdiff) #jaro winkler distance
  
  nCol = length(colnames(matches))
  colnames(matches)[nCol-3] = 'jaccard index'
  colnames(matches)[nCol-2] = 'geo distance'
  colnames(matches)[nCol-1] = 'levenshtein'
  colnames(matches)[nCol] = 'jaro winkler'
  
  #write results to a file where we can compare the data and do a sanity check
  write.csv(matches, file=outputFilename, row.names=FALSE)
}

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
  tokenList = unique(unlist(strsplit(text, split=" ")))
  return(tokenList)
}

#could have a matrix of the scores for all possible matches
match_based_on_jaccard_index <- function(df1, df2){
  for (index in c(1:dim(df2)[1])){
    print(index)
    jaccard_index_values = unlist(lapply(df1$soup, function(x) {jaccard_index(x,df2$soup[index])}))
    maxLoc = which.max(jaccard_index_values)
    print(paste(df1$name[maxLoc], df2$name[index]))
  }
}


#never ever ever convert strings to factors
options(stringsAsFactors = FALSE)

library(SPARQL)

setwd("/home/cbdavis/Desktop/svn/ChrisDavis/PhD/Enipedia/InstanceMatching/")

data = read.csv("/home/cbdavis/Downloads/power_plants_from_industryabout.csv", header=TRUE, sep=",")
data = data[-which(data$country != "Netherlands"),]
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
                    ?x prop:Country a:Netherlands . 
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

lookForMatches(df1 = data, 
               df2 = enipediaData, 
               soupColumnsFromDataSet1 = c('name', 'owner', 'area', 'other_name', 'owners'), 
               soupColumnsFromDataSet2 = c('name', 'owner', 'city', 'state'), 
               lat1 = 'latitude',
               lon1 = 'longitude',
               lat2 = 'lat',
               lon2 = 'lon',
               maxDistance=20000)

#calculate Jaccard Index
#jaccardIndexValues = unlist(lapply(df1$soup, function(x) {jaccard_index(x,df2$soup[1])}))

library(reshape) #colsplit function
library(RecordLinkage) #string matching
library(gdata) #trim function
library(geosphere) #needed for geographic coordinate calculations

getLatLongFromPoint <- function(point){
  coords = colsplit(point, split=",", names=c("latitude", "longitude"))
  
  #make sure that these aren't interpreted as factors
  coords$longitude = as.character(coords$longitude)
  coords$latitude = as.character(coords$latitude)
  
  #fix up lat and lon values
  eastLonLocs = grep("E", coords$longitude)
  coords$longitude[eastLonLocs] = gsub(' E', '', coords$longitude[eastLonLocs])
  coords$longitude[eastLonLocs] = as.numeric(coords$longitude[eastLonLocs])
  
  westLonLocs = grep("W", coords$longitude)
  coords$longitude[westLonLocs] = gsub(' W', '', coords$longitude[westLonLocs])
  coords$longitude[westLonLocs] = 0 - as.numeric(coords$longitude[westLonLocs])
  
  northLatLocs = grep("N", coords$latitude)
  coords$latitude[northLatLocs] = gsub(' N', '', coords$latitude[northLatLocs])
  coords$latitude[northLatLocs] = as.numeric(coords$latitude[northLatLocs])
  
  southLatLocs = grep("S", coords$latitude)
  coords$latitude[southLatLocs] = gsub(' S', '', coords$latitude[southLatLocs])
  coords$latitude[southLatLocs] = 0 - as.numeric(coords$latitude[southLatLocs])
  
  coords$longitude = as.numeric(coords$longitude)
  coords$latitude = as.numeric(coords$latitude)
  
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
  text = gsub('http://enipedia.tudelft.nl/wiki/', '', text)
  text = gsub('\\)', '', text)
  text = gsub('\\(', '', text)
  text = gsub('/', '', text)
  text = gsub('_', ' ', text)
  text = gsub('-', ' ', text)
  text = tolower(text)
  return(text)
}

compareData <- function(df1, df2, columnsToMatch=NULL, coordsLat=NULL, coordsLon=NULL, outputFilename="test.csv", maxDistance=20000){
  
  matchingThreshold = 0.8
  
  numVerticesInAdjMatrix = dim(df1)[1] + dim(df2)[1]
  #symMatrix = as.data.frame(matrix(data=0, nrow=numVerticesInAdjMatrix, ncol=numVerticesInAdjMatrix))
  symMatrix = matrix(data=0, nrow=numVerticesInAdjMatrix, ncol=numVerticesInAdjMatrix)
  
  #names of the vertices indicate the row number in the data frames
  #so df1_15 and df2_23 show which data we're dealing with
  namesOfVertices = c(paste("df1_", c(1:dim(df1)[1]), sep=""), paste("df2_", c(1:dim(df2)[1]), sep=""))
  colnames(symMatrix) = namesOfVertices
  rownames(symMatrix) = namesOfVertices
  
  #TODO more efficient to compare unique values and then trace back which row they came from?
  #TODO if have coordinate data, then filter down possibilities already
  matchedAtPriorityLevel = 1
  for (columnSetToCompare in columnsToMatch) {
    col1 = columnSetToCompare[[1]]
    col2 = columnSetToCompare[[2]]
    scoreWeighting = columnSetToCompare[[3]]
    
    #should just automatically match if above a certian threshold
    for(i in c(1:dim(df1)[1])){
      print(i)

      distances = NULL
      if (is.null(coordsLat) || is.null(coordsLon)){
        #look at everything, no coordinates defined
        lookAtThese = c(1:dim(df2)[1])
      } else {
        distances = distCosine(cbind(df1[i,coordsLon[1]], 
                                     df1[i,coordsLat[1]]), 
                               cbind(df2[,coordsLon[2]],
                                     df2[,coordsLat[2]])
                               )
        lookAtThese = which(distances < maxDistance)
      }
      
      if (length(lookAtThese) > 0){
        
        ldiff = levenshteinSim(trim(tolower(df1[i,col1])), 
                                   trim(tolower(df2[lookAtThese,col2])))

        jdiff = jarowinkler(trim(tolower(df1[i,col1])), 
                                trim(tolower(df2[lookAtThese,col2])), r=0.5)

        #find which locations match already
        locsStringDiff = unique(c(which(ldiff > matchingThreshold), 
                                  which(jdiff > matchingThreshold)))

        locs = lookAtThese[locsStringDiff]
        
        if(length(locs) > 0){
          vertex1 = i
          vertex2 = locs + dim(df1)[1]
          symMatrix[vertex1, vertex2] = symMatrix[vertex1, vertex2] + (ldiff[locsStringDiff]*scoreWeighting) + (jdiff[locsStringDiff]*scoreWeighting)
          symMatrix[vertex2, vertex1] = symMatrix[vertex2, vertex1] + (ldiff[locsStringDiff]*scoreWeighting) + (jdiff[locsStringDiff]*scoreWeighting)
        }
      }
    }

    ###### Code below is not used yet #####
    #TODO should do something to filter out things below a certain threshold
    #add priority indicating at which level this matched on
    matchedAtPriorityLevel = matchedAtPriorityLevel + 1
  }

  count=0
  matches = NULL
  
  rowsDF1 = dim(df1)[1]
  locs = which(symMatrix[c(1:rowsDF1),] > 0,arr.ind=T)
    
  for(i in c(1:dim(locs)[1])){
    count = count + 1
    print(count)
    matches = rbind(matches, cbind(df1[locs[i,1],], df2[locs[i,2]-rowsDF1,]))
  }
  
  write.csv(matches, file=outputFilename)

  return(as.data.frame(symMatrix))
}

#See R entropy package - allows for estimation of mutual information
#http://strimmerlab.org/software/entropy/
#also what about conditional entropy?
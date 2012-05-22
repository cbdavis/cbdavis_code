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
  text = gsub('\\)', '', text)
  text = gsub('\\(', '', text)
  text = gsub('/', '', text)
  text = gsub('http://enipedia.tudelft.nl/wiki/', '', text)
  text = gsub('_', ' ', text)
  text = gsub('-', ' ', text)
  text = tolower(text)
  return(text)
}


#never ever ever convert strings to factors
options(stringsAsFactors = FALSE)

source("compareData.R")

library(SPARQL)

setwd("/home/cbdavis/Desktop/svn/ChrisDavis/PhD/Enipedia/InstanceMatching/")

data = read.csv("eon_wasserkraft_power_plants.csv", header=TRUE, sep=",")
data$name = removeTheWeirdness(data$name)

#find all the stuff in Germany
endpoint = "http://enipedia.tudelft.nl/sparql/sparql"
queryString = paste(getPrefixes(), 
                    "select * where {
                    ?x rdf:type cat:Powerplant .
                    ?x rdfs:label ?name . 
                    ?x prop:Country a:Germany . 
                    ?x prop:Ownercompany ?owner . 
                    }", sep="")
d <- SPARQL(url=endpoint, query=queryString, format='csv', extra=list(format='text/csv'))
enipediaData = d$results
enipediaData$name = removeTheWeirdness(enipediaData$name)

symMatrix = compareData(df1=data,
                        df2=enipediaData, 
                        columnsToMatch = list(list('name', 'name', 1))
                        )


df1=data
df2=enipediaData

count=0
matches = NULL

rowsDF1 = dim(df1)[1]
locs = which(symMatrix[c(1:rowsDF1),] > 0,arr.ind=T)

for(i in c(1:dim(locs)[1])){
  count = count + 1
  print(count)
  matches = rbind(matches, cbind(df1[locs[i,1],], df2[locs[i,2]-rowsDF1,]))
}

write.csv(matches, file="EOnWasserkraft.csv")
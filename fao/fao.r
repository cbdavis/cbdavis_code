### Author: Chris Davis
### E-mail: c.b.davis@tudelft.nl
### Website: http://chrisdavis.weblog.tudelft.nl/
###          http://wiki.tudelft.nl/bin/view/Main/ChrisDavis

#The code below is an exploration of a technique for achieving dimensionality reduction of data
#In practical terms, we're working with FAO data from 2007 that describes the calories per food type per country per day.
#This is significantly detailed data, and you can find out things like who eats the most coconuts and cephalopods
#There are 115 different categories of food types, and what we do here is reduce those 115 dimensions of data per country
#down to two dimensions, which can be represented as a scatter plot, which roughly indicates how similar different countries'
#diets are, as represented by the proximity of the countries to each other.

##TODO make sure to set your working directory to where the data is
setwd("/home/cbdavis/Desktop/cbdavis_code/fao")
#setwd("/location/of/fao/files/")

###TODO download files from FAO to your working directory
#This is based on the Food balance sheets from the FAO
#http://faostat.fao.org/site/354/default.aspx
#You may need to create a login account in order to download the data (in CSV form)

#load in the FAO data.  This is contained in five files, so we merge then together as we read them in
foodBalanceSheet = read.table("FoodBalanceSheets1.csv", head=TRUE, sep="\t", quote="", stringsAsFactors=FALSE)
foodBalanceSheet = rbind(foodBalanceSheet, read.table("FoodBalanceSheets2.csv", head=TRUE, sep="\t", quote="", stringsAsFactors=FALSE))
foodBalanceSheet = rbind(foodBalanceSheet, read.table("FoodBalanceSheets3.csv", head=TRUE, sep="\t", quote="", stringsAsFactors=FALSE))
foodBalanceSheet = rbind(foodBalanceSheet, read.table("FoodBalanceSheets4.csv", head=TRUE, sep="\t", quote="", stringsAsFactors=FALSE))
foodBalanceSheet = rbind(foodBalanceSheet, read.table("FoodBalanceSheets5.csv", head=TRUE, sep="\t", quote="", stringsAsFactors=FALSE))

library(tsne)
library(reshape)

#From the data just read in, we want to get the subset where ElementCode is kcal/capita/day.
#In other words, for a particular type of a food in a specific country, 
#how many calories does a person get from that food in a day
#the c(2,4,100) part below says to get the "Country", "Item", and "X2007" columns.  
#"Item" is the type of food, and X2007 are the values for 2007.
#I'm not sure yet how (or if) the code below could be applied to a time series, so only a single year is used.
kcalPerPersonPerDayPerCountry = foodBalanceSheet[which(foodBalanceSheet$ElementCode == 664),c(2,4,100)]

#need to do a text conversion due to special characters (non-ASCII) in some of the country names
#This is needed so that the code below doesn't break
kcalPerPersonPerDayPerCountry$Country = iconv(kcalPerPersonPerDayPerCountry$Country, "latin1", "ASCII")

kcalPerPersonPerDayPerCountry$X2007 = as.numeric(as.character(kcalPerPersonPerDayPerCountry$X2007))
#transform this data frame so that the columns are created for all the different types of food items
#This means that reading across a row, you first get the country name, and then 114 columns 
#documenting the caloric intake from various types of food.  
#So number of rows = number of countries, number of columns = number of types of food (+ one column for the country name)
#also fill empty data with zero
kcalPerPersonPerDayPerCountry = cast(kcalPerPersonPerDayPerCountry, Country ~ Item, fill=0)

#The "Cottonseed", "Grand Total + (Total)", "Meat, Aquatic Mammals", and "Vegetal Products + (Total)" categories have no data for any of the countries, so remove them
#"Miscellaneous + (Total)" is also removed to leave us with 110 instead of 115 categories.  As shown below this will give us a facet_wrap of 11 columns by 10 rows
#without having a single graph all by itself on the bottom row, so the end visualization looks better.  
#From looking at the data, "Miscellaneous + (Total)" doesn't contribute much to the total calories consumed
#and it doesn't provide much insight other than showing that some contries eat more "Miscellaneous" than others, 
#which may indicate more exotic cuisine.
kcalPerPersonPerDayPerCountry$"Cottonseed" = NULL
kcalPerPersonPerDayPerCountry$"Grand Total + (Total)" = NULL
kcalPerPersonPerDayPerCountry$"Meat, Aquatic Mammals" = NULL
kcalPerPersonPerDayPerCountry$"Vegetal Products + (Total)" = NULL
kcalPerPersonPerDayPerCountry$"Miscellaneous + (Total)" = NULL

#run the t-SNE algorithm
#the line below visualizes how the algorithm is arranging the data points over time
epochCallbackFunction = function(x,y){ plot(x,t='n'); text(x,labels=kcalPerPersonPerDayPerCountry$Country, cex=0.5) }
#actual function call to t-SNE.  
#This takes a long time (tens of minutes), and the creator of the R t-SNE package mentions that his implementation is slow compared to the original 
#(http://scwn.net/2010/02/20/new-t-sne-package-for-r/) so there may be some room for improvement if someone could figure out how to better optimize the code
tsneData = tsne(kcalPerPersonPerDayPerCountry[,2:111], epoch_callback = epochCallbackFunction, epoch=50, max_iter=20000)

#plot the data (same as in epochCallbackFunction)
plot(tsneData$ydata,t='n')
text(tsneData$ydata,labels=kcalPerPersonPerDayPerCountry$Country, cex=0.5)

#### Understanding the effectiveness of the tsne algorithm through graph visualization
# Make a network graph representing probabilities that certain things are connected, based on a certain threshold.
# The idea is that the graph has nodes at locations tsneData$ydata, and edges are shown only for nodes with a 
# tsneData$P value above a user-specified level.  This helps to relate how the layout generated by tsneData$ydata 
# works with the tsneData$P values.

matrixOfProbabilities = tsneData$P
library(igraph)
g = graph.adjacency(matrixOfProbabilities, weighted=TRUE, mode=c("directed"), diag=FALSE)

#max probability is 0.002
edgelocs = which(E(g)$weight > 0.00025)
edgelist = get.edgelist(g, names=TRUE)
tempGraph = graph.edgelist(edgelist[edgelocs,], directed=TRUE)

V(g)$label = kcalPerPersonPerDayPerCountry$Country

l = tsneData$ydata
plot(tempGraph, layout=l, vertex.size=1, vertex.label=V(g)$label, edge.arrow.size=0.3)


#### Plot the data on a map, no more multi-dimensional clustering, just see what it looks like in different parts of the world.
#these are the points for each of the countries
xpts = tsneData$ydata[,1]
ypts = tsneData$ydata[,2]

#find the max calories per food type (i.e. per column)
maxCaloriesPerFoodType = apply(kcalPerPersonPerDayPerCountry[,2:111], 2, max)

#join xpts and ypts to temp2
kcalPerPersonPerDayPerCountry = cbind(xpts, ypts, kcalPerPersonPerDayPerCountry)

kcalPerPersonPerDayPerCountryNormalized = kcalPerPersonPerDayPerCountry
#normalize the values all to one.
#this allows easier comparison between food categories
#transpose has to be applied to do this correctly
kcalPerPersonPerDayPerCountryNormalized[,4:113] = t(t(kcalPerPersonPerDayPerCountry[,4:113]) / maxCaloriesPerFoodType)

#melt both data frames.  This will leave us with the columns 'Country', 'xpts', 'ypts', 'variable', and 'value'
kcalPerPersonPerDayPerCountry = melt(kcalPerPersonPerDayPerCountry, id=c('Country', 'xpts', 'ypts'))
kcalPerPersonPerDayPerCountryNormalized = melt(kcalPerPersonPerDayPerCountryNormalized, id=c('Country', 'xpts', 'ypts'))

library(ggplot2)
#unnormalized plot - shows relative importance of different food groups in feeding the world
ggplot(kcalPerPersonPerDayPerCountry, aes(x=xpts, y=ypts)) + geom_point(aes(size = value)) + facet_wrap(~ variable)
#normalized plot - highlights regions that are the largest consumers of each food type
ggplot(kcalPerPersonPerDayPerCountryNormalized, aes(x=xpts, y=ypts)) + geom_point(aes(size = value)) + facet_wrap(~ variable)


#this came from a geocoding service, where the names of the countries from the FAO were used
countries = read.table("/home/cbdavis/Desktop/CountriesAndCoordinates.csv", head=TRUE, sep=",", quote="", stringsAsFactors=FALSE)

#need to match up coordinates with countries on the main data frame
#xpts and ypts are dropped since they aren't needed below, columns 'Country', 'variable', 'value' are kept
countriesWithCoordinatesUnNormalized = merge(countries, kcalPerPersonPerDayPerCountry[,c('Country', 'variable', 'value')], by.x=c('name'), by.y=c('Country'))
countriesWithCoordinatesNormalized = merge(countries, kcalPerPersonPerDayPerCountryNormalized[,c('Country', 'variable', 'value')], by.x=c('name'), by.y=c('Country'))

library(maps)
world_map <- map_data("world")
names(world_map)[5] <- "country"

#visualize data on a grid of 110 maps
#The part below about "data=world_map[seq(1,dim(world_map)[1],5),]" makes it only plot every fifth data point for the country boundaries.  Without this, your computer might explode

#plot unnormalized data
ggplot(data=countriesWithCoordinatesUnNormalized, aes(longitude, latitude)) + geom_point(aes(size=value)) + facet_wrap(~ variable) + geom_polygon(aes(x=long, y=lat, group = group), data=world_map[seq(1,dim(world_map)[1],5),], colour = "grey10", fill = NA, size = .1) + ylim(-55, 85)
#plot normalized data
ggplot(data=countriesWithCoordinatesNormalized, aes(longitude, latitude)) + geom_point(aes(size=value)) + facet_wrap(~ variable) + geom_polygon(aes(x=long, y=lat, group = group), data=world_map[seq(1,dim(world_map)[1],5),], colour = "grey10", fill = NA, size = .1) + ylim(-55, 85)
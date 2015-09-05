### Author: Chris Davis
### E-mail: c.b.davis@rug.nl
### Website: http://chrisdavis.weblog.tudelft.nl/
###          http://wiki.tudelft.nl/bin/view/Main/ChrisDavis
### 
### This code downloads the input-output make and use tables 
### from the US Bureau of Economic Analysis, and then does some 
### preliminary calculations based on page 12-21 in the 
### "Concepts and Methods of the U.S. Input-Output Accounts" manual
### http://www.bea.gov/papers/pdf/IOmanual_092906.pdf

### There are a few small known issues, see section below on 
### calculations of qError and gError

library(gdata) # read.xls
library(reshape)

#function to clean up the data
cleanThousandsSeparator <- function(x){
  for(i in colnames(x)){x[[i]] <- as.numeric(gsub("\xa0", "", gsub(",","",x[[i]])))}
  return(x)
}

# This takes the data extracted from the spreadsheets and cleans it up into a more sane format for further processing
cleanUpTableFormat <- function(table){
  
  # figure out which rows are mostly filled out
  numEmptyCellsPerRow = apply(table, MARGIN=1, function(x){length(which(x == ""))})
  startRow = head(which(numEmptyCellsPerRow < max(numEmptyCellsPerRow)/2), n=1)
  endRow = tail(which(numEmptyCellsPerRow < max(numEmptyCellsPerRow)/2), n=1)

  table = table[c(startRow:endRow),]

  # we drop the codes since there aren't codes for every column
  # we can integrate them later with a vector that acts like a lookup table
  # these are in the first column and the first row
  
  table = table[-1,]
  table = table[,-1]

  columnNames = tail(as.character(table[1,]), n=-1)
  columnNames = gsub(" /[0-9]/$", "", columnNames) # remove footnotes that appear in names
  rowNames = tail(as.character(table[,1]), n=-1)
    
  table = table[-1,]
  table = table[,-1]
  
  colnames(table) = columnNames
  row.names(table) = rowNames
  
  for (i in c(1:ncol(table))){
    table[,i] = as.numeric(table[,i])
  }
  
  # convert all NA to 0
  table[is.na(table)] <- 0
  
  # Just look at this beautiful table!
  #View(table)
  
  return(table)
}

extractCodesAndDescriptions <- function(table){
  
  # figure out which rows are mostly filled out
  numEmptyCellsPerRow = apply(table, MARGIN=1, function(x){length(which(x == ""))})
  startRow = head(which(numEmptyCellsPerRow < max(numEmptyCellsPerRow)/2), n=1)
  endRow = tail(which(numEmptyCellsPerRow < max(numEmptyCellsPerRow)/2), n=1)
  
  # just want the top two value rows
  table = table[c(startRow:(startRow+1)),]
  # get rid of the first two columns
  table = table[,-c(1:2)]
  
  table[2,] = gsub(" /[0-9]/$", "", table[2,]) # remove footnotes that appear in names
  
  lastEntry = which(table[1,] == "GSLE")
  table = table[,c(1:lastEntry)]
  
  IOCodesToNames = table[1,]
  names(IOCodesToNames) = table[2,]

  return(IOCodesToNames)
}

readIODataFromBEA <- function(year){
  #valid sheets are from from 3 to 14
  years = c(1997:2013)
  yearLocationIndex = c(3:19)
  
  #map the input year to the index of the worksheet
  sheetNum = yearLocationIndex[which(years == year)]

  #Download relevant spreadsheets from the web if they don't exist already in the working directory
  if (file.exists('IOMake_Before_Redefinitions_1997-2013_Summary.xlsx') == FALSE) { # get the make table
    download.file('http://www.bea.gov/industry/xls/io-annual/IOMake_Before_Redefinitions_1997-2013_Summary.xlsx', 'IOMake_Before_Redefinitions_1997-2013_Summary.xlsx')
  }
  
  if (file.exists('IOUse_Before_Redefinitions_PRO_1997-2013_Summary.xlsx') == FALSE) { # get the use table
    download.file('http://www.bea.gov/industry/xls/io-annual/IOUse_Before_Redefinitions_PRO_1997-2013_Summary.xlsx', 'IOUse_Before_Redefinitions_PRO_1997-2013_Summary.xlsx')
  }
  
  #read into the data into preliminary data frames
  makeTable = read.xls("IOMake_Before_Redefinitions_1997-2013_Summary.xlsx", sheet=sheetNum, stringsAsFactors=FALSE)
  useTable = read.xls("IOUse_Before_Redefinitions_PRO_1997-2013_Summary.xlsx", sheet=sheetNum, stringsAsFactors=FALSE)

  # want to be able to translate between industry/commodity codes and names
  IOCodesToNames = extractCodesAndDescriptions(makeTable)
  
  numIndustrialSectors = length(IOCodesToNames)
  
  # clean up the tables - this leaves in the IO names, but strips out the codes
  makeTable = cleanUpTableFormat(makeTable)
  useTable = cleanUpTableFormat(useTable)

  #create a column showing the value added by each industry
  
  valueAdded = useTable[which(row.names(useTable) == "Total Value Added"),
                        c(1:length(IOCodesToNames))] # only get the values that correspond to the industrial sectors
  
  ### From page 12-21 in the Concepts and Methods of the U.S. Input-Output Accounts
  ### page 12-8 seems useful as well
  #q:	 A column vector in which each entry shows the total amount of the output of a commodity. 
  # It is a commodity-by-one vector.
  q_temp = useTable$`Total Commodity Output`[c(1:numIndustrialSectors)]
  q = matrix(q_temp, length(q_temp),1)
  
  #g:	 A column vector in which each entry shows the total amount of each
  #   industry's output, including its production of scrap. It is an industry-by-one
  #  vector.
  g_temp = makeTable$`Total Industry Output`[c(1:numIndustrialSectors)]
  g = matrix(g_temp, length(g_temp), 1)
  
  #U:	 Intermediate portion of the use matrix in which the column shows for a
  #   given industry the amount of each commodity it uses—including
  #    noncomparable imports, scrap, and used and secondhand goods. This is a
  #   commodity-by-industry matrix.
  
  ## In the code below, U and V don't contain scrap and noncomparable imports.  I don't think this is used below
  ## by leaving these out, I don't have to provide indices for a subset of these when doing calculations
  
  U = as.matrix(useTable[c(1:numIndustrialSectors),c(1:numIndustrialSectors)])
  
  #V:	 Make matrix, in which the column shows for a given commodity the
  #   amount produced in each industry. This is an industry-by-commodity
  #  matrix. V has columns showing only zero entries for noncomparable
  # imports and for scrap.
  
  V = as.matrix(makeTable[c(1:numIndustrialSectors),c(1:numIndustrialSectors)])
  # need to zero out noncomparable imports and scrap
  
  #^:	 A symbol that when placed over a vector indicates a square matrix in
  #   which the elements of the vector appear on the main diagonal and zeros
  #  elsewhere.
  
  #B:	 Direct input coefficients matrix in which entries in each column show the
  #   amount of a commodity used by an industry per dollar of output of that
  #  industry. This is a commodity-by-industry matrix.
  #
  #B = Ugˆ^−1 ( 1 )
  B = solve(diag(as.vector(g)),U)
  
  #D:	A matrix in which entries in each column show, for a given commodity
  #(excluding scrap), the proportion of the total output of that commodity
  #produced in each industry. It is assumed that each commodity (other than
  #scrap) is produced by the various industries in fixed proportions (industry-
  #technology assumption). D is an industry-by-commodity matrix. D is also
  #referred to as the market share matrix.
  #
  #D = Vqˆ^−1 ( 2 )
  D = solve(diag(as.vector(q)), V)
  
  #i:	 Unit (summation) vector containing only 1's.
  
  #I:	 
  #    Identity matrix, where I = iˆ .
  
  #e:	 A column vector in which each entry shows the total final demand
  #   purchases for each commodity from the use table.
  #### ? assume this is "Final Uses". On page 12-8 in first forumal of "Mathematics of the requirements tables" 
  #### seems to indicate "Final Demand" is equivalent to "Final Uses"
  e_temp = useTable$`Total Final Uses (GDP)`[c(1:numIndustrialSectors)]
  e = matrix(e_temp, length(e_temp), 1)
  
  #h:	 A column vector in which each entry shows the total amount of each
  #   industry's production of scrap. Scrap is separated to prevent its use as an
  #  input from generating output in the industries in which it originates.
  h_temp = makeTable$`Scrap, used and secondhand goods`[c(1:numIndustrialSectors)]
  h = matrix(h_temp, length(h_temp), 1)
  
  #p:	 A column vector in which each entry shows the ratio of the value of scrap
  #   produced in each industry to the industry's total output.
  p_temp = makeTable$`Scrap, used and secondhand goods`[c(1:numIndustrialSectors)] / 
    makeTable$`Total Industry Output`[c(1:numIndustrialSectors)]
  p = matrix(p_temp, length(p_temp), 1)
  
  #W:	 An industry-by-commodity matrix in which the entries in each column
  #   show, for a given commodity, the proportion of the total output of that
  #  commodity produced in each industry adjusted for scrap produced by the
  # industry. W is referred to as the transformation matrix.
  
  ### ? how do I make this? see page 12-12
  ### This adjustment is accomplished by calculating the ratio of nonscrap
  ### output to industry output for each industry and then applying these ratios to the
  ### market shares matrix in order to account for total industry output
  
  W = D
  nonScrapRatio = as.matrix(1-p)
  # for every row in D, want to adjust it by the value in nonScrapRatio
  ##TODO there's probably a more efficient way to do this
  for (i in c(1:length(nonScrapRatio))){
    W[i,] = W[i,] * nonScrapRatio[i,1]
  }
  
  #From the above definitions, the following identities are derived:
  #q = Ui + e (3)
  #g = Vi + h (4)
  
  qCalculated = (U %*% matrix(1,numIndustrialSectors,1)) + e
  gCalculated = (V %*% matrix(1,numIndustrialSectors,1)) + h
  
  ##error check here
  qError = q - qCalculated

  #Scrap output in each industry is proportional to total output of the industry, then:
  #
  #h = pˆg (5)
  
  h = diag(as.vector(p)) %*% g
  
  #The model expressed in equations (1) through (5) thus involves three
  #constants (B, D, p) and six variables (U, V, h, e, q, g). The model solution is
  #derived as follows:
  #
  #From (1) and (3), we derive:
  #
  #q = Bg + e (6)
  #
  #From (2) and (4), we derive:
  #
  #g - h = Dq (7)
  #
  #Substituting (5) into (7) and solving for g:
  #
  #g − pˆg = Dq
  #
  #(I − pˆ)g = Dq
  #
  #g = (I − pˆ) −1 Dq (8)
  #
  #Let (I − pˆ)^−1 D = W , then
  #
  #g = Wq (9)
  #
  #Substituting (9) into (6) and solving for q:
  #
  # q = BWq + e
  #
  # (I - BW)q = e
  #
  # q = (I − BW ) −1 e (10)
  #
  #Substituting (10) into (9) gives:
  #
  # g = W (I − BW ) −1 e (11)
  #
  #Therefore, three total requirements coefficients matrices are derived.
  #
  #Commodity-by-commodity total requirements matrix:
  #
  # (I − BW ) −1 (12)
  #
  #which shows commodity output required per dollar of each commodity delivered
  #to final users.
  
  commodityByCommodityTotalRequirements = solve((diag(1,numIndustrialSectors) - (B %*% W)))
  
  #Industry-by-commodity total requirements matrix:
  #
  # W (I − BW ) −1 (13)
  #
  #which shows the industry output required per dollar of each commodity delivered
  #to final users.
  
  industryByCommodityTotalRequirements = W %*% solve((diag(1,numIndustrialSectors) - (B %*% W)))
  
  #And the industry-by-industry total requirements matrix:
  #
  # (I − WB) −1 (14)
  #
  #which shows the industry output required per dollar of each industry product
  #delivered to final users.
  industryByIndustryTotalRequirements = solve((diag(1,numIndustrialSectors) - (W %*% B)))
  
  data = list(q = q,
              g = g,
              U = U,
              V = V,
              D = D,
              B = B,
              e = e,
              h = h,
              p = p, 
              W = W, 
              commodityByCommodityTotalRequirements = commodityByCommodityTotalRequirements,
              industryByCommodityTotalRequirements = industryByCommodityTotalRequirements,
              industryByIndustryTotalRequirements = industryByIndustryTotalRequirements, 
              industrySectorCodes = industrySectorCodes,
              industrySectorNames = industrySectorNames, 
              valueAdded = valueAdded)
  
  return(data)
  
}

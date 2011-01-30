This code downloads the input-output make and use tables from the US Bureau of Economic Analysis, and then does some preliminary calculations based on page 12-21 in the "Concepts and Methods of the U.S. Input-Output Accounts" manual (http://www.bea.gov/papers/pdf/IOmanual_092906.pdf)

The gdata and reshape libraries must be installed for this to work.  

Sample code is shown below, where the data from 1998 is retrieved.  Data currently exists for years 1998 to 2009

source("/path/to/readIODataFromBEA.r")
data = readIODataFromBEA(1998)

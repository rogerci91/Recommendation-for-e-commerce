# load packages
pacman::p_load(readxl, tidyverse)

# set path to directory (where your datafiles are present)
setwd("/Users/cherguan/Documents/MTECH/Sem2/04_RS/03_Assignment/submission/")

# Read Product Description Data
productList = read_xlsx('Product Description Input.xlsx')
productList = productList[rowSums(is.na(productList)) == 0,]
productList = productList[, c('Product Title', 'Product Description')]

# Read Reviews Data
reviewList = read.csv("amazon_new.csv")
reviewList = reviewList %>%
  select(name,reviews.date,reviews.rating,reviews.username)

# Read Similarity Matrix
sim_mat_cos_desc <- readRDS("./sim_mat_cos_desc.rds")
sim_mat_cos_name <- readRDS("./sim_mat_cos_name.rds")

#####################################
# To make the testing easier, create a function to find recommended products.
# simm-similarity matrix, prodInd-index of the selected product,
# k-top k products in result,
# productDF- Product Description Dataset
#####################################
findRecProduct = function (simm, prodInd, k, productDF) {
  found <- sort(simm[, prodInd], decreasing = T)[1:(k+1)]
  found <- found[-which(names(found) == prodInd)]
  selected.prod.name = productDF[prodInd, 1]
  selected.prod.desc = productDF[prodInd, 2]
  recProdList = data.frame(matrix(ncol = 8, nrow = 0))
  columns <- c('selected.prod.index','selected.prod.name','selected.prod.desc','recommended.rank','recommended.prod.simm','recommended.prod.index','recommended.prod.name','recommended.prod.desc')
  colnames(recProdList) <- columns
  resindex <- as.integer(names(found))
  for (i in 1:k) {
    record = data.frame(prodInd, selected.prod.name, selected.prod.desc, i, found[i], resindex[i], productDF[resindex[i],1], productDF[resindex[i],2])
    names(record)<-columns
    recProdList = rbind(recProdList, record)
  }
  return(recProdList)
}

#####################################
# To make the testing easier, create a function to find recommended products for User.
# username-user you want to find recommendation for, simm-similarity matrix,
# k1-top k products to search for recommendation, k2-top k recommended products for each search,
# productDF- Product Description Dataset, reviewDF- Product Review Dataset
#####################################
findRecProductForUser = function (username, simm, k1, k2, productDF, reviewDF) {
  userPref = reviewDF %>%
    filter(.[[4]] == username) %>%
    filter(.[[3]] > 3) %>%
    arrange(desc(.[[3]]),desc(.[[2]])) %>%
    group_by(.[[1]]) %>%
    summarise(count = n()) %>%
    top_n(k1, .[[1]]) %>%
    select(1)
  prodInd = which(productDF[[1]] %in% userPref[[1]], arr.ind=T)
  recProdList = data.frame(matrix(ncol = 8, nrow = 0))
  columns <- c('selected.prod.index','selected.prod.name','selected.prod.desc','recommended.rank','recommended.prod.simm','recommended.prod.index','recommended.prod.name','recommended.prod.desc')
  colnames(recProdList) <- columns
  for (i in 1:length(prodInd)) {
    found = findRecProduct(simm, prodInd[i], k2, productDF)
    names(found)<-columns
    recProdList = rbind(recProdList, found)
  }
  return(recProdList)
}

# List all products
productList$`Product Title`

# set variables
selectedProd = 3 #selected Product index
searchRec = 5 #no. of recommendation per selected product

# Recommend Product using Description only
list = findRecProduct(sim_mat_cos_desc, selectedProd, searchRec, productList)
view(list)

# Recommend Product using both Description and Title
list = findRecProduct(sim_mat_cos_desc+sim_mat_cos_name, selectedProd, searchRec, productList)
view(list)


# List all users
unique(reviewList$reviews.username)

#set variables
searchPref = 3 #no. of product to use as user preference
searchRec = 5 #no. of recommendation per selected product

# Recommend Product for user based on its most recent reviews as preference 
# and using both Description and Title
list = findRecProductForUser('Larry', sim_mat_cos_desc+sim_mat_cos_name, searchPref, searchRec, productList, reviewList)
view(list)

    
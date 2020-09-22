# load packages
pacman::p_load(readxl, tm, wordcloud, slam)

# set path to directory (where your datafiles are present)
setwd("/Users/cherguan/Documents/MTECH/Sem2/04_RS/03_Assignment/submission/")

# Read Product Description Data
product = read_xlsx('Product Description Input.xlsx')
product = product[rowSums(is.na(product)) == 0,]
product = product[, c('Product Title', 'Product Description')]

str(product)
head(product,5)

#####################################
# Text mining on Product Description
#####################################
#Check Product Description Length
doclen_desc <- sapply(product$`Product Description`, function(x) length(strsplit(x, " ")[[1]]))
hist(doclen_desc)

#Create Corpus
corpus_desc <- Corpus(VectorSource(product$`Product Description`)) 

#Normalize
myStopwords_desc <- c(stopwords('english'),'get','use','now','even','can')
corpus_desc <- tm_map(corpus_desc, content_transformer(tolower)) #covernt to lower cases
corpus_desc <- tm_map(corpus_desc, removeNumbers) #remove digits
corpus_desc <- tm_map(corpus_desc, removeWords, myStopwords_desc)
corpus_desc <- tm_map(corpus_desc, removePunctuation)
corpus_desc <- tm_map(corpus_desc, stemDocument) #word stemming
corpus_desc <- tm_map(corpus_desc, removeWords, myStopwords_desc) #stopwords removal
corpus_desc <- tm_map(corpus_desc, stripWhitespace) #delete redundent whitespace "a  b"-> "a b"

for( i in 1:6){
  print(corpus_desc[[i]][1])
} 

#creating the matrix
dtm_desc <- DocumentTermMatrix(corpus_desc)
dtm_desc

# Generate word cloud
desc.freq <- colSums(as.matrix(dtm_desc))
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(desc.freq), desc.freq, max.words=100, scale=c(2, .05), rot.per=0.6, colors=dark2)

#creating the TfIdf matrix 
dtm_ti_desc <- weightTfIdf(dtm_desc)
dtm_ti_desc

mat_ti_desc <- as.matrix(dtm_ti_desc)

#Compute distance
sim_mat_cos_desc <- crossprod_simple_triplet_matrix(t(dtm_ti_desc))/(sqrt(col_sums(t(dtm_ti_desc)^2) %*% t(col_sums(t(dtm_ti_desc)^2))))

#####################################
# Text mining on Product Title
#####################################
#Check Product Title Length
doclen_name <- sapply(product$`Product Title`, function(x) length(strsplit(x, " ")[[1]]))
hist(doclen_name)

#Create Corpus
corpus_name <- Corpus(VectorSource(product$`Product Title`))

#Normalize
myStopwords_name <- c(stopwords('english'))
corpus_name <- tm_map(corpus_name, content_transformer(tolower)) #covernt to lower cases
corpus_name <- tm_map(corpus_name, removeNumbers) #remove digits
corpus_name <- tm_map(corpus_name, removeWords, myStopwords_name)
corpus_name <- tm_map(corpus_name, removePunctuation)
corpus_name <- tm_map(corpus_name, stemDocument) #word stemming
corpus_name <- tm_map(corpus_name, removeWords, myStopwords_name) #stopwords removal
corpus_name <- tm_map(corpus_name, stripWhitespace) #delete redundent whitespace "a  b"-> "a b"

for( i in 1:6){
  print(corpus_name[[i]][1])
}

#creating the matrix
dtm_name <- DocumentTermMatrix(corpus_name)
dtm_name

# Generate word cloud
name.freq <- colSums(as.matrix(dtm_name))
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(name.freq), name.freq, max.words=100, scale=c(2, 0.5), rot.per=0.6, colors=dark2)

#creating the TfIdf matrix 
dtm_ti_name <- weightTfIdf(dtm_name)
dtm_ti_name

mat_ti_name <- as.matrix(dtm_ti_name)

#Compute distance
sim_mat_cos_name <- crossprod_simple_triplet_matrix(t(dtm_ti_name))/(sqrt(col_sums(t(dtm_ti_name)^2) %*% t(col_sums(t(dtm_ti_name)^2))))

saveRDS(sim_mat_cos_desc, "./sim_mat_cos_desc.rds")
saveRDS(sim_mat_cos_name, "./sim_mat_cos_name.rds")

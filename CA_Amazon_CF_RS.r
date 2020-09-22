
# requires CF-demolib.R
source('~/Google Drive/NUS MASTER/Yr1 Sem2/Recommender System/CA/Lib/CF-demolib-v3.R')


setwd("/Users/tianyu.liu/Google Drive/NUS MASTER/Yr1 Sem2/Recommender System/CA/Data")
txn <- read.csv("amazon_new.csv", header=TRUE, sep=",") # transaction format!

# remove columns with irrelevant information
cols_to_keep = c('id','name','brand','categories','primaryCategories','manufacturer','reviews.rating','reviews.text','reviews.title','reviews.username')
txn <- txn[,cols_to_keep]

head(txn,2)

summary(txn)

# no missing value in ratings (range from 1 to 5)

# filter columns for Collaborative Filtering recommendation system
cols_for_cf = c('reviews.username','name','reviews.rating')
events <- txn[,cols_for_cf]
names(events) = c("user", "item", "rating")

length(unique(events$item)) # show # of items  
length(unique(events$user)) # show # of users 


# eliminate users with too few ratings
ucnts = aggregate(item ~ user, data = events, FUN=length)
colnames(ucnts) = c("user","numitems") 
activeusers = ucnts$user[ucnts$numitems >= 3] ; length(activeusers)
ev = events[events$user %in% activeusers,]

# eliminate books with too few ratings
bcnts = aggregate(user ~ item, data = events, FUN=length)
colnames(bcnts) = c("item","numusers") 
popularbooks = bcnts$item[bcnts$numusers >= 3] ; length(unique(popularbooks))
ev = ev[ev$item %in% popularbooks,]

head(ev,2)

summary(ev)

length(unique(ev$item))

rm(ucnts)
rm(bcnts)
rm(events)

# now create the ratings matrix from the raw events
users = acast(ev, user ~ item, value.var = "rating", fun.aggregate = mean) # we use an aggregation function since some users have rated the same book more than once
dim(users)
users[1:5,]

fillrate(users)
y = unlist(users)
hist(y) # histo of the ratings
likethresh = 4
likerate = length(which(y>=likethresh))/length(which(y>=1)) ; cat(" of movies that are liked=",likerate*100,"%")

# setup the train/test scheme

set.seed(123)

numtestusers = 100
testnames  = sample(rownames(users), min(numtestusers,nrow(users))); #testnames # identify N users randomly for testing
trainnames = setdiff(rownames(users),testnames); #trainnames # take remaining users for training
train = users[trainnames,]
test  = users[testnames,]

# Pearsonsim
itemsims  = getitemsimsmatrix(train, simfun=pearsonsim)
preds = predictCF(test, itemsims=itemsims, numtestitems=30, random=FALSE)
cat("\navg MAE =",avgMAE(preds))
showCM(preds, like=likethresh)

# cosinesim
itemsims  = getitemsimsmatrix(train, simfun=cosinesim)
preds = predictCF(test, itemsims=itemsims, numtestitems=10, random=FALSE)
cat("\navg MAE =",avgMAE(preds))
showCM(preds, like=likethresh)

# euclidsim
itemsims  = getitemsimsmatrix(train, simfun=euclidsim)
preds = predictCF(test, itemsims=itemsims, numtestitems=10, random=FALSE)
cat("\navg MAE =",avgMAE(preds))
showCM(preds, like=likethresh)

# Try Normalization
usersN = sweep(users, 1, rowMeans(users, na.rm=TRUE) )  # normalise the data
y = unlist(usersN)
hist(y) # histo of the ratings

# setup the train/test scheme

likethreshN = 0.01
numtestusers = 100 
testnames  = sample(rownames(usersN), min(numtestusers,nrow(usersN))); #testnames # identify N users randomly for testing
trainnames = setdiff(rownames(usersN),testnames); #trainnames # take remaining users for training
trainN = usersN[trainnames,]
testN  = usersN[testnames,]

print("Results with pre-normalization: ")

itemsimsN  = getitemsimsmatrix(trainN, simfun=pearsonsim)
preds = predictCF(testN, itemsims=itemsimsN, numtestitems=10, random=FALSE)
cat("\navg MAE =",avgMAE(preds))
showCM(preds, like=likethreshN)

print("Results with pre-normalization: ")

itemsimsN  = getitemsimsmatrix(trainN, simfun=cosinesim)
preds = predictCF(testN, itemsims=itemsimsN, numtestitems=10, random=FALSE)
cat("\navg MAE =",avgMAE(preds))
showCM(preds, like=likethreshN)

print("Results with pre-normalization: ")

itemsimsN  = getitemsimsmatrix(trainN, simfun=euclidsim)
preds = predictCF(testN, itemsims=itemsimsN, numtestitems=10, random=FALSE)
cat("\navg MAE =",avgMAE(preds))
showCM(preds, like=likethreshN)

# get item-item recommendations

target = users["Alaska",]

recList <- getrecommendations_II(target, itemsimsN)

summary(recList)

head(recList)

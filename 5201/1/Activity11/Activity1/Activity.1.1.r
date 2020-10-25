
install.packages("reshape2")
install.packages("ggplot2")
install.packages("corrplot")
library(reshape2)
library(ggplot2)
library(corrplot)
# Load data: it's built in to R, however, you can also get it online
# iris <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"), header = FALSE)
library(datasets)
data(iris)
# take a look at the data
head(iris)
# Shown are 4 measurements (petal & sepal width & length) for 3 species of iris flowers, where sepal is: 
# "One of the usually separate, green parts that surround and protect the flower bud" (or petals)

dim(iris) # 150 x 5 records

install.packages("Cairo")
#a few visualizations wont hurt!
## the followin plot illustrates petal measurments:
ggplot(data=iris, aes(x=Petal.Length, y=Petal.Width, color=Species)) + 
    geom_point() + geom_rug()+ theme_minimal() + ggtitle("Petal Measurements")

## and this one shows the correlation between the features (input variables)
#corrplot.mixed(cor(iris[,-5]), lower="ellipse", upper="number")

# set random seed
set.seed(1234)
# permute iris, shuffle or mix them up
iris <- iris[sample(1:nrow(iris),nrow(iris)),]
# create  training and testing subsets:
train.index = 1:100
train.data <- iris[train.index, -5] # grab the first 100 records, leave out the species (last column)
train.label <- iris[train.index, 5]
test.data <- iris[-train.index, -5] # grab the last 50 records, leave out the species (last column)
test.label <- iris[-train.index, 5]

dim(train.data) # 100 records
dim(test.data) # 50 records

head(iris) # the shuffled records

head(train.data) # the first 100 records without the Species

# define an auxiliary function that calculates the majority votes (or mode!)
majority <- function(x) {
   uniqx <- unique(x)
   uniqx[which.max(tabulate(match(x, uniqx)))]
}

# KNN function (distance should be one of euclidean, maximum, manhattan, canberra, binary or minkowski)
knn <- function(train.data, train.label, test.data, K=3, distance = 'euclidean'){
    ## count number of train samples
    train.len <- nrow(train.data)
    
    ## count number of test samples
    test.len <- nrow(test.data)
    
    ## calculate distances between samples
    dist <- as.matrix(dist(rbind(test.data, train.data), method= distance))[1:test.len, (test.len+1):(test.len+train.len)]
    
    ## for each test sample...
    for (i in 1:test.len){
        ### ...find its K nearest neighbours from training sampels...
        nn <- as.data.frame(sort(dist[i,], index.return = TRUE))[1:K,2]
        
        ###... and calculate the predicted labels according to the majority vote
        test.label[i]<- (majority(train.label[nn]))
    }
    
    ## return the class labels as output
    return (test.label)
}

# let see what is the prediciton of our knn for test samples when K=4
knn(train.data, train.label, test.data, K=4)

# and a confusion matrix for K = 5
prop.table(table(knn(train.data, train.label, test.data, K=5), test.label))*100

# calculate the train and test missclassification rates for K in 1:100 
# THIS MAY TAKE A FEW MINUTES TO COMPLETE!
miss <- data.frame('K'=1:100, 'train'=rep(0,100), 'test'=rep(0,100))
for (k in 1:100){
    miss[k,'train'] <- sum(knn(train.data, train.label, train.data, K=k) != train.label)/nrow(train.data)*100
    miss[k,'test'] <-  sum(knn(train.data, train.label, test.data, K=k)  != test.label)/nrow(test.data)*100
}

# plot misclassification percentage for train and test data sets
miss.m <- melt(miss, id='K') # reshape for visualization
names(miss.m) <- c('K', 'type', 'error')
ggplot(data=miss.m, aes(x=log(1/K), y=error, color=type)) + geom_line() +
       scale_color_discrete(guide = guide_legend(title = NULL)) + theme_minimal() +
       ggtitle("Missclassification Error")



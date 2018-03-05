##### Week 4: Analysing Discrete Data #####

##### 1. Molly's Lottery Problem #####

##### 2. Manufacturing defects #####

##### 3. Accidents in 8 years #####

##### 4. Hong Kong Household Expenditure #####

hongkong = read.csv("data/hongkong.csv", header = TRUE)
hongkong

comb= c(hongkong$Men, hongkong$Women)

comb

# a function to extract the first digit from a number 
bben <- function(k){
as.numeric(head(strsplit(as.character(k),'')[[1]],n=1))
}

first.digit <- sapply(comb, bben) 

truehist(first.digit, nbins=10) 
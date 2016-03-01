## Load functions and packages
source('functions.R') # just single bootstrap function for now
source('required_packages.R') #external package dependencies

## Load input data
#read data example
df <- read.csv('data.csv', header = TRUE)
#break it up in the individual input needed by the algo
Y <- cbind(df[c(1, 12, 35)], data.frame(dollars_raised_per_letter = df[,1] * df[,10])) #the matrix of outcomes
D <- df[, 8, drop = TRUE] #the vector of treatment statuses
sub <- rep(1, length(D)) #the subgroup ID's
numoc <- dim(Y)[2] #the number of outcomes
numsub <- length(unique(sub)) #the number of subgroups
numg <- length(unique(D)) -1 #the number of treatment groups (not including the control group)
combo <- cbind(rep(0, numg), (1:numg)) #We compare each treatment to the control.
numpc <- dim(combo)[1] #the number of pairs of treatment (control) groups of interest
top_percentile <- .95 #used to calculate the p-value threshold at the end of each iteration round in the main algo loop
#create the set of null hypothses
#unlike the Matlab code, which works with a 3 dimensional array,
#the three dimensions are flattened out to a table
select <- expand.grid(numoc = rep(1, numoc), numsub = rep(1, numsub), numpc = rep(1, numpc))
n <- 1e1 # number of bootstrap iterations

##Algo
#this is a restricted implementation of the Matlab code: the set of null hypotheses coincide with
#just the set of outcome. Will extend to the full set of null hypotheses later

#overide some of the empirical data with random data to make debugging easier
Y <- as.data.frame(replicate(n = 1e2, runif(1e3)))
D <- sample(c(0, 1), size = 1e3, replace = TRUE)
#initialize the set of null hypotheses
diff_col <- ncol(Y)
#first two conditions as per step #2, Algorithm 3.1 http://home.uchicago.edu/amshaikh/webfiles/experimental.pdf
while (ncol(Y) >= 1 && diff_col != 0) {
  ncol_Y <- ncol(Y) # number of null hypothesis in S_j
  #run n-many bootstrap (see specific fucntion comments to see how that works)
  #out of all the min p-values, return the one at the top_percentile percentile
  threshold <- replicate(n, individual_bootstrap()) %>% quantile(., top_percentile)
  #run a straight t-test over all null hypothesis
  #leave (for the following iteration) only the tests that have a t-test-derived p-value above the threshold
  #calculated in the previous step
  Y <<- Filter(function(x) t.test(x[D], x[!D])[['p.value']] > threshold, Y)
  diff_col <<- ncol_Y - ncol(Y) # difference in number of null hypothesis in S_j vs. S_(j-1)
  #a few diagnostics metrics to check whether the algo is returning correct results
  #print(diff_col)
  print(ncol(Y))
  #print(threshold[[1]])
}

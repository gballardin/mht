individual_bootstrap <- function() {
  #sampling with replacement
  Y <- Y[sample(nrow(Y)),]
  #for iterations, run a t-test over all the null hypothesis and extract the min p-value across all of them
  min(sapply(Y, function(x) t.test(x[D], x[!D])[['p.value']]), na.rm = TRUE)
}

#To obtain numeric values at various percentiles.

quant_func = function(df){
  x = quantile(df, probs = c(seq(0, .9, .1), seq(.9, .99, .01), seq(.99, 1, .001)), include.lowest = T, na.rm = T)
}

#your_df could be a single column but it has to be of type dataframe.
ints <- which(sapply(your_df, FUN = function(x){ifelse(class(x) == "numeric" | class(x) == "integer", T, F)}))

outlier = do.call(rbind, lapply(your_df, FUN = function(x)quant_func(x)))

outlier = t(outlier)

write.csv(outlier, file = "Outlier.csv")

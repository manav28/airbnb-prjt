#Script to generate summary of data

getTableProps = function(data) {

	library(xlsx)
	df = data.frame(Columns = colnames(data))
  	df$Type = sapply(data, FUN = mode)
  	ints = which(sapply(data, FUN = function(x){ifelse(class(x) == "numeric" | class(x) == "integer", T, F)}))
  	cats = which(sapply(data, FUN = function(x){ifelse((length(unique(x)) < 0.1 * length(x)) | is.factor(x), T, F)}))
  	chars = which(sapply(data, FUN = function(x){ifelse((length(unique(x)) > 0.8 * length(x)) & class(x) == "character", T, F)}))
  	df$cat_var_stats = NA
  	for(i in cats) {
    	a = summary(as.factor(data[, i]))
    	df$cat_var_stats[i] = paste(paste(names(a),a,sep = " : "), collapse = " || ")
  	}
  	df$uniquecount = sapply(data, FUN = function(x){length(unique(x))})
  	df$mean = NA
  	df$median = NA
  	df$max = NA
  	df$min = NA
  	for(iter in ints){
    	df$max[iter] = max(data[, iter])
    	df$min[iter] = min(data[, iter])
    	df$mean[iter] = mean(data[, iter])
    	df$median[iter] = median(data[, iter])
  	}
  	df$mode = sapply(data, FUN = function(x){names(which.max(table(x)))})
  	df$NAs = sapply(data, FUN = function(x){length(which(is.na(x) | x == ""))})
  	return(df)
}







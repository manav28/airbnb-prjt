#Converting a list of values into separate logical columns
new_df = airbnb_df$amenities
new_df2 = gsub('\\{||\\}||\\"', "", new_df)
new_df3 = unique(trimws(tolower(unlist(lapply(new_df2, FUN = function(x){strsplit(x, split = ",")})))))

new_df4 = data.frame(row.names = rownames(airbnb_df))
invisible(lapply(new_df3, FUN = function(x){new_df4[[x]] <<- 1 * grepl(x, airbnb_df$amenities, ignore.case = T)}))
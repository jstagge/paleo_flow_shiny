# *------------------------------------------------------------------
# | FUNCTION NAME: create_site_list
# | FILE NAME: create_site_list.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        file_name - the name of matrix holding site info
# |					res - temporal resolution, either monthly or annual
# |     Out:       site_list - a list formatted for select in shiny
# | 
# |     Desc:      This function creates a list for a select in shiny. 
# *------------------------------------------------------------------


create_site_list <- function(file_name, res="all"){ 

### First, subset to only resolution
file_name_subset <- subset(file_name, resolution==res)

### Extract site_groups
site_group_list <- as.character(unique(file_name_subset$site_group))

site_list <- list()
site_list[[1]] <- c("Select site location"='')

### Loop through site_groups and create a unique list for each
for (n in seq(1, length(site_group_list))) {
	### Test for the group
	group_test <- file_name_subset$site_group == site_group_list[n]
	
	### Create an object with site name as header and site id as object
	site_list_temp <- file_name_subset$col_name[group_test]
	names(site_list_temp) <- file_name_subset$site_name[group_test]
	
	### Add to the longer list
	site_list[[(n+1)]] <- site_list_temp
} 

### Name the groups in the full list
names(site_list) <- c(as.character("NA"), site_group_list)
	
return(site_list)
}


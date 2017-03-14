# *------------------------------------------------------------------
# | FUNCTION NAME: read_in_paleo
# | FILE NAME: read_in_paleo.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        annual_rec - a dataframe with columns "water_year" and "annual_flow"
# |                monthly_prop - a datamframe with columns "month" and "prop"
# |                first_month_wy - a numeric variable with the month that signifies the start of the water year, usually 10
# |     Out:       monthly_ts - a dataframe with results of the null model reconstruction
# | 
# |     Desc:      This function applies the "AP model", creating a monthly
# |                flow reconstruction for an initial annual flow time series.
# |                The AP Model works by assuming the reconstructed mean annual flow
# |                percentile is equivalent to the monthly percentile for the entire water year. 
# *------------------------------------------------------------------


read_in_paleo <- function(site_id, site_id_list, site_name_list){ 

### Create a list to hold the output  
paleo_readin <- list()    
paleo_readin$site_id <- site_id 

### Determine which one of the read in time series from the number
site_list_id <- which(site_id_number == site_id_list)

### Determine the site name  
paleo_readin$site_name <- site_name_list[site_list_id]  
 
### Read in flow data
flow_ts <- read.csv(file.path(write_output_path,paste0("flow_",site_id,".csv"))) 
 
### Read in first month  
start_ts <- read.csv(file.path(write_output_path,paste0("start_",site_id,".csv")))
start_ts <- as.numeric(unlist(start_ts))  

### Create a monthly time series 
paleo_readin$flow_ts <- ts(flow_ts, start=start_ts, frequency=12)

### Output the list of values 
return(paleo_readin) 
}
 
### This function is applied to columns to modify the units
### New units can be added here as long as they are applied to the original m3/s data

unit_conv <- function(x, new_unit, date, temp_resolution, begin_month=1, end_month=12){
	if (new_unit == "cfs"){
		x * 35.31467	### convert m3s to cfs
	} else if(new_unit == "ac-ft"){
		### Convert to cfs
		x <- x * 35.31467
		### Convert to ac-ft per second
		x <- x * (1/43560)
		### Calculate duration
		if (temp_resolution == "annual"){
			begin_date <- as.Date(paste0("1900-", begin_month, "-01"))
			end_date <- rollback(as.Date(paste0("1900-", end_month+1, "-01"))) + 365*(end_month < begin_month)
			duration <- end_date - begin_date + 1
		} else if (temp_resolution == "monthly"){
			duration <- days_in_month(date)
		}

		### Multiply by duration	
		x <- case_when(
			temp_resolution == "annual" ~ x * 60*60*24*as.numeric(duration),
			temp_resolution == "monthly" ~ x *  60*60*24*as.numeric(duration),
			TRUE ~ NA_real_
		)
	} else {
		x
	}
}



unit_conv_nodate_annualonly <- function(x, new_unit){
	if (new_unit == "cfs"){
		x * 35.31467	### convert m3s to cfs
	} else if(new_unit == "ac-ft"){
		### Convert to cfs
		x <- x * 35.31467
		### Convert to ac-ft per second
		x <- x * (1/43560)
		### Multiply by duration		
		 x * 60*60*24*365.25
	} else {
		x
	}
}





unit_conv_nodate <- function(x, new_unit, temp_resolution, begin_month=1, end_month=12){
	if (new_unit == "cfs"){
		x * 35.31467	### convert m3s to cfs
	} else if(new_unit == "ac-ft"){
		### Convert to cfs
		x <- x * 35.31467
		### Convert to ac-ft per second
		x <- x * (1/43560)
		### Calculate duration
		if (temp_resolution == "annual"){
			begin_date <- as.Date(paste0("1900-", begin_month, "-01"))
			end_date <- rollback(as.Date(paste0("1900-", end_month+1, "-01"))) + 365*(end_month < begin_month)
			duration <- end_date - begin_date + 1
		} else if (temp_resolution == "monthly"){
			duration <- 30
		}

		### Multiply by duration	
		x <- case_when(
			temp_resolution == "annual" ~ x * 60*60*24*as.numeric(duration),
			temp_resolution == "monthly" ~ x *  60*60*24*as.numeric(duration),
			TRUE ~ NA_real_
		)
	} else {
		x
	}
}




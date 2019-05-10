### This function is applied to columns to modify the units
### New units can be added here as long as they are applied to the original m3/s data

unit_conv <- function(x, new_unit, date, temp_resolution){
	if (new_unit == "m3/s"){
		x * 1 ### no need to convert if requested m3s, already in m3s
	} else if (new_unit == "cfs"){
		x * 35.31467	### convert m3s to cfs
	} else if(new_unit == "ac-ft"){
		### Convert to cfs
		x <- x * 35.31467
		### Convert to ac-ft per second
		x <- x * (1/43560)
		### Multiply by duration		
		x <- case_when(
			temp_resolution == "annual" ~ x * 60*60*24*(365+as.numeric(leap_year(date))),
			temp_resolution == "monthly" ~ x *  60*60*24*days_in_month(date),
			TRUE ~ NA_real_
		)
		x
	} 
}

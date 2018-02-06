# *------------------------------------------------------------------
# | FUNCTION NAME: round_df
# | FILE NAME: round_df.R
# | DATE: 
# | CREATED BY:  Jeromy Anglim         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        
# |                x: data frame 
# |                digits: number of digits to round
# |     Out:       x: data frame
# | 
# |     Desc:      round all numeric variables
# |					Copied from http://stackoverflow.com/questions/29875914/rounding-values-in-a-dataframe-in-r. 
# *------------------------------------------------------------------

round_df <- function(x, digits) {
    # round all numeric variables
    # x: data frame 
    # digits: number of digits to round
    numeric_columns <- sapply(x, mode) == 'numeric'
    x[numeric_columns] <-  round(x[numeric_columns], digits)
    x
}



# *------------------------------------------------------------------
# | FUNCTION NAME: signif_df
# | FILE NAME: round_df.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        
# |                x: data frame 
# |                digits: number of digits to round
# |     Out:       x: data frame
# | 
# |     Desc:      round all numeric variables
# |				   Copied from http://stackoverflow.com/questions/29875914/rounding-values-in-a-dataframe-in-r. 
# *------------------------------------------------------------------

signif_df <- function(x, digits) {
    # round all numeric variables
    # x: data frame 
    # digits: number of digits to round
    numeric_columns <- sapply(x, mode) == 'numeric'
    x[numeric_columns] <-  signif(x[numeric_columns], digits)
    x
}
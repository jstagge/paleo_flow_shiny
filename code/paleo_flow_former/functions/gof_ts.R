# *------------------------------------------------------------------
# | FUNCTION NAME: gof_ts
# | FILE NAME: gof_ts.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        pred - predicted values
# |                obs - observed values
# |                
# |     Out:       gof_results - a list of gof statistics
# | 
# |     Desc:      Calculated gof statistics for a time series.
# |                
# *------------------------------------------------------------------


gof_ts <- function(pred, obs) {
	### Remove NAs from analysis
	complete_test <- complete.cases(obs) & complete.cases(pred)
	obs <- obs[complete_test]
	pred <- pred[complete_test]
	
	### Calculate errors
	error <- pred-obs
	
	### Calculate gof statistics
	ME <- mean(error, na.rm=TRUE)
	MAE <- mean(abs(error),na.rm=TRUE)
	MSE <- mean((error)^2, na.rm=TRUE)
	RMSE <- MSE^0.5
	nashsut <- 1 - sum((error)^2) / sum((obs-mean(obs))^2)
	
	correl <- cor(pred, obs, method = "pearson")
	correl.spear <- cor(pred, obs, method = "spearman")
	
	### gof_results
	gof_results <- list(ME=ME, MAE=MAE, RMSE=RMSE, NSE=nashsut, R=correl, R.spear=correl.spear)
	
	return(gof_results)
}


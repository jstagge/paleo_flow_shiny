read



require(dcv)
require(tidyverse)


data(val)
test.RE(val[,1],val[,2])





yup <- read.table("salinas.txt", header=TRUE)


yup <- yup %>% 
	mutate(Observed = replace(Observed, Observed == -9999, NA))  %>% 
	filter(!is.na(Observed)

test.RE(yup$Observed_na,yup$Recon)

cor(yup$Observed_na,yup$Recon)^2


require(hydroGOF)
NSE(sim=yup$Recon, obs=yup$Observed_na)



 Reduction of Error(RE)= 0.721 
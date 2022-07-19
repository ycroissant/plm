library(plm)

data(LaborSupply)

dataframe_names <- colnames(LaborSupply)
names(LaborSupply) <- replace(dataframe_names, dataframe_names=='id', 'i^d')
dataframe <- pdata.frame(LaborSupply, index = c('i^d', 'year'), check.names=FALSE)
fml <- lnhr~lnwg + kids + age + disab

# Should not raise Error in this scenario.
plm(fml, data=dataframe, effect='twoway', model='within')

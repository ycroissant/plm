library(plm)
data("Grunfeld", package = "plm")



# corner case make one - id NA and rest non varying 
# non-variation was detected prior to rev. 206
Grunfeld_1NA_nonvari <- Grunfeld
Grunfeld_1NA_nonvari[ 1:20,  "capital"] <- NA
Grunfeld_1NA_nonvari[21:200, "capital"] <- Grunfeld_1NA_nonvari[21:200, "firm"]
print(pvar(Grunfeld_1NA_nonvari))

Grunfeld_1NA_nonvari_time <- Grunfeld
Grunfeld_1NA_nonvari_time[Grunfeld_1NA_nonvari_time$year == 1935, "capital"] <- NA
Grunfeld_1NA_nonvari_time[Grunfeld_1NA_nonvari_time$year != 1935, "capital"] <- Grunfeld_1NA_nonvari_time[Grunfeld_1NA_nonvari_time$year != 1935, "year"]
print(pvar(Grunfeld_1NA_nonvari_time))


## for one id all NA -> time dimension affected
Grunfeld_1NA <- Grunfeld
Grunfeld_1NA[1:20, "capital"] <- NA
print(pvar(Grunfeld_1NA))

Grunfeld_2NA <- Grunfeld
Grunfeld_2NA[1:20, "capital"] <- NA
Grunfeld_2NA[21:40, "value"] <- NA
print(pvar(Grunfeld_2NA))


## one time period all NA -> id dimension affected
Grunfeld_1NA_time <- Grunfeld
Grunfeld_1NA_time[Grunfeld_1NA_time$year == 1935, "capital"] <- NA
print(pvar(Grunfeld_1NA_time))

Grunfeld_2NA_time <- Grunfeld
Grunfeld_2NA_time[Grunfeld_2NA_time$year == 1935, c("value", "capital")] <- NA
print(pvar(Grunfeld_2NA_time))



# input only 1-column data.frame (1 variable) with all NA for one id
Grunfeld_1var <- Grunfeld
Grunfeld_1var <- pdata.frame(Grunfeld, drop.index = TRUE)
Grunfeld_1var$inv <- NULL
Grunfeld_1var$value <- NULL
Grunfeld_1var1NA <- Grunfeld_1var
Grunfeld_1var1NA[c(1:20), "capital"] <- NA
print(pvar(Grunfeld_1var1NA))

Grunfeld_1var1NA_time <- Grunfeld_1var
Grunfeld_1var1NA_time[c(1,21,41,61,81,101,121,141,161,181), "capital"] <- NA
print(pvar(Grunfeld_1var1NA_time))


## data.frame 
print(pvar(Grunfeld, index=c("firm")))

## one variable all NA -> gets removed by pdata.frame
Grunfeld_allNA <- Grunfeld
Grunfeld_allNA[ , "capital"] <- NA
print(pvar(Grunfeld_allNA))




# Matrix
Grunfeld_mat <- as.matrix(Grunfeld)

Grunfeld_mat <- as.matrix(Grunfeld)
pvar(Grunfeld_mat)
pvar(Grunfeld_mat, index=c("firm"))

Grunfeld_mat_allNA <- as.matrix(Grunfeld_allNA)
pvar(Grunfeld_mat_allNA)



## pseries
pGrunfeld <- pdata.frame(Grunfeld)
pvar(pGrunfeld$capital)          # should indicate variation in both dimensions (nothing is printed)
pvar(pGrunfeld[1:20, ]$capital)  # should indicate no indivivual variation (b/c only 1 individual is evaluated)


# library(foreign);library(plm)
# jtrain <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/jtrain.dta")
# 
# # Define panel data (for 1987 and 1988 only)
# jtrain.87.88 <- subset(jtrain,year<=1988)
# jtrain.p<-pdata.frame(jtrain.87.88, index=c("fcode","year"))
# 
# pvar(jtrain.p)
# print(pvar(jtrain.p))
# 
# pvar(jtrain.p[ , c(20:26)])
# print(pvar(jtrain.p[ , c(20:26)]))

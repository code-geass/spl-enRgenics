
# load libraries
library(ggplot2)
library(MASS)
library(maps)
library(maptools)
library(sp)

# plot options for Jupyter Notebook
options(repr.plot.width = 10, repr.plot.height = 4.5)

# rgeos option
gpclibPermit()


# load data from Quantlet
ENRGENICS_ADDOTHER_PATH = "../ENRgenics_AddOther/ENRgenics_AddOther.r"
source(ENRGENICS_ADDOTHER_PATH)
# paths for ImportEIA and climate Data
ENRGENICS_IMPORTEIA_PATH = "../ENRgenics_ImportEIA/ENRgenics_ImportEIA.r"
ENRGENICS_CLIMATE_DATA_PATH = "../data/climdiv-tmpcst-v1.0.0-20160605"
PATH_VARS = c(ENRGENICS_IMPORTEIA_PATH, ENRGENICS_CLIMATE_DATA_PATH)




# location of EIA data file
EIA_DATA_PATH = "../data/sales_revenue.csv.0"

file = EIA_DATA_PATH
df = load_eia_data_with_all_others(file)
head(df)

finalsdf = subset(df, DataStatus=="Final")
mean_sales_by_state = dcast(finalsdf[which(finalsdf$Cat == "TOTAL"),], State ~ . , value.var="Sales", fun.aggregate = mean)
mean_temp_by_state = dcast(finalsdf[which(finalsdf$Cat == "TOTAL"),], State ~ . , value.var="temp", fun.aggregate = mean)
mean_price_by_state = dcast(finalsdf[which(finalsdf$Cat == "TOTAL"),], State ~ . , value.var="Price", fun.aggregate = mean)

mean_sales_by_state = mean_sales_by_state[order(mean_sales_by_state$.),]
mean_temp_by_state = mean_temp_by_state[order(mean_temp_by_state$.),]
mean_temp_by_state = mean_temp_by_state[complete.cases(mean_temp_by_state),]
mean_price_by_state = mean_price_by_state[order(mean_price_by_state$.),]

#tail(mean_sales_by_state)
#head(mean_temp_by_state)
salesClus = kmeans(mean_sales_by_state[, c(".")], 5, nstart = 20, iter.max = 20)
s1Clus = data.frame(cbind(mean_sales_by_state[,1], salesClus$cluster))
colnames(s1Clus) = c("State", "salesClus")

tempClus = kmeans(mean_temp_by_state[, c(".")], 4, nstart = 20, iter.max = 20)
s2Clus = data.frame(cbind(mean_temp_by_state[,1], tempClus$cluster))
colnames(s2Clus) = c("State", "tempClus")

priceClus = kmeans(mean_price_by_state[, c(".")], 4, nstart = 20, iter.max = 20)
s5Clus = data.frame(cbind(mean_price_by_state[,1], priceClus$cluster))
colnames(s5Clus) = c("State", "priceClus")

area = as.data.frame(state.x77)
areaClus = kmeans(area[, c("Area")], 5, nstart = 20, iter.max = 20)
s3Clus = data.frame(cbind(state.abb, areaClus$cluster))
colnames(s3Clus) = c("State", "areaClus")

pop = as.data.frame(state.x77)
popClus = kmeans(pop[, c("Population")], 5, nstart = 20, iter.max = 20)
s4Clus = data.frame(cbind(state.abb, popClus$cluster))
colnames(s4Clus) = c("State", "popClus")

sClus = s1Clus %>% left_join(s2Clus) %>% left_join(s3Clus) %>% left_join(s4Clus) %>% left_join(s5Clus)
finalsdf = finalsdf  %>% left_join(sClus)
head(finalsdf)

print("plots...")
ggplot(mean_sales_by_state, aes(x = reorder(State, .), y = ., fill=as.factor(salesClus$cluster))) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette="Set1")

ggplot(mean_temp_by_state, aes(x = reorder(State, .), y = ., fill=as.factor(tempClus$cluster))) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette="Set1")

ggplot(mean_price_by_state, aes(x = reorder(State, .), y = ., fill=as.factor(priceClus$cluster))) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette="Set1")

ggplot(area, aes(x = reorder(row.names(area), Area), y = Area, fill=as.factor(areaClus$cluster))) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette="Set1")

ggplot(pop, aes(x = reorder(row.names(pop), Population), y = Population, fill=as.factor(popClus$cluster))) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette="Set1")


#totals = subset(finalsdf, popClus == 1)
# tmp = dcast(totals[which(totals$Cat == "TOTAL"),], Year + Month ~ State + Cat, value.var="Sales", fun.aggregate = sum, na.rm = TRUE)
#ts_year_by_state = dcast(totals[which(totals$Cat == "TOTAL"),], Year ~ State, value.var="Sales", fun.aggregate = sum)


#http://www.r-bloggers.com/ggplot2-cheatsheet-for-scatterplots/

# Plot Set 1

for (i in row.names(salesClus$centers)) {
    totals = subset(finalsdf, Cat == "TOTAL" & salesClus == as.integer(i) & (Year > 1997 & Year < 2002))
    totals$DateFormat = as.Date(as.POSIXct(totals$Date, origin="1970-01-01"))
    p1 = ggplot(totals, aes(x = DateFormat, y = Revenue)) + geom_point(aes(color=factor(State)))
    print(p1)
    savefile = paste("output/plt1_",i,".png", sep="")
    ggsave(savefile, width = 30, height = 20, dpi = 120)
}

# Plot Set 2
#forp2 = ts_year_by_state
#forp2$total = apply(ts_year_by_state[,2:ncol(ts_year_by_state)], 1,function(x){sum(x)})
#p2 = ggplot(forp2, aes(x = Year, y = total))
#p2 + geom_point(aes(color=factor(Year)))

# consider weather data
# https://ram-n.github.io/weatherData/

# mapping without advanced libs
# http://stackoverflow.com/questions/24441775/how-do-you-create-a-us-states-heatmap-based-on-some-values

ts_year_by_state = dcast(finalsdf[which(finalsdf$Cat == "TOTAL"),], Year ~ State, value.var="Sales", fun.aggregate = sum)
means_by_state = colMeans(ts_year_by_state[,c(-1,-9)]) # this removes the year column and DC
normalizers = matrix(c(state.x77[,1], state.x77[,2], state.x77[,5], state.area), nrow=50, ncol=4)  # Population, Income, Murder Rate (for fun) and Area


permit = gpclibPermit()
for (i in 1:dim(normalizers)[2]) {
    means_df = data.frame(means_by_state / normalizers[,i])
    dat = means_df
    dat$states <- tolower(state.name[match(rownames(dat),  state.abb)])
    mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
    nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x){x[1]})
    USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))

    idx <- match(unique(nms),  dat$states)
    dat2 <- data.frame(value = dat$means_by_state[idx], state = unique(nms))
    row.names(dat2) <- unique(nms)
    USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)

    print(spplot(USAsp['value']))
}

# https://www.census.gov/popest/data/datasets.html
# https://docs.google.com/spreadsheets/d/1JsV5bNnFoE-4xapsnrpqRP-5wgTkpDjgC0SXKW8zgPI/edit#gid=0



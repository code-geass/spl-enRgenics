
# load enrgenics-quantlet-import
source("enrgenics-quantlet-import.r")


file = "data/sales_revenue.csv.0"
df = load_eia_data(file)

head(df)

totals = subset(df, DataStatus=="Final")
# tmp = dcast(totals[which(totals$Cat == "TOTAL"),], Year + Month ~ State + Cat, value.var="Sales", fun.aggregate = sum, na.rm = TRUE)
ts_year_by_state = dcast(totals[which(totals$Cat == "TOTAL"),], Year ~ State, value.var="Sales", fun.aggregate = sum)
corr_states = cor(ts_year_by_state[, c(-1,-ncol(ts_year_by_state))])
means_by_state = colMeans(ts_year_by_state[,-1])

heatmap(corr_states, Rowv=NA, Colv=NA, col = cm.colors(256), margins=c(5,10))

#http://www.r-bloggers.com/ggplot2-cheatsheet-for-scatterplots/
library(ggplot2)
# plot options for Jupyter Notebook
options(repr.plot.width = 12, repr.plot.height = 5)

# Plot Set 1
totals = subset(df, DataStatus=="Final" & Cat == "TOTAL")
totals$DateFormat = as.Date(as.POSIXct(totals$Date, origin="1970-01-01"))
totals_sub = subset(totals, State=="DE")
totals_sub$DateFormat = as.Date(as.POSIXct(totals_sub$Date, origin="1970-01-01"))
p1 = ggplot(totals_sub, aes(x = DateFormat, y = Revenue))
p1 + geom_point(aes(color=factor(Year)))
ggsave("output/plt1a.png", width = 16, height = 9, dpi = 120)
p1 + geom_point(aes(color=factor(Month)))
ggsave("output/plt1b.png", width = 16, height = 9, dpi = 120)
p1c = ggplot(totals, aes(x = DateFormat, y = Revenue))
p1c + geom_point(aes(color=factor(State)))
ggsave("output/plt1c.png", width = 30, height = 20, dpi = 120)


# Plot Set 2
forp2 = ts_year_by_state
forp2$total = apply(ts_year_by_state[,2:ncol(ts_year_by_state)], 1,function(x){sum(x)})
p2 = ggplot(forp2, aes(x = Year, y = total))
p2 + geom_point(aes(color=factor(Year)))

# consider weather data
# https://ram-n.github.io/weatherData/

# mapping without advanced libs
# http://stackoverflow.com/questions/24441775/how-do-you-create-a-us-states-heatmap-based-on-some-values

library(MASS)
library(maps)
library(maptools)
library(sp)

permit = gpclibPermit()
means_df = data.frame(means_by_state)
dat = means_df
dat$states <- tolower(state.name[match(rownames(dat),  state.abb)])
mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x){x[1]})
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))

idx <- match(unique(nms),  dat$states)
dat2 <- data.frame(value = dat$means_by_state[idx], state = unique(nms))
row.names(dat2) <- unique(nms)
USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
    
spplot(USAsp['value'])
# https://www.census.gov/popest/data/datasets.html
# https://docs.google.com/spreadsheets/d/1JsV5bNnFoE-4xapsnrpqRP-5wgTkpDjgC0SXKW8zgPI/edit#gid=0



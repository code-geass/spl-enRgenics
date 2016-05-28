
# Load Libraries and Data
# download data from https://www.eia.gov/electricity/data/eia826/
options(repos= c("http://cloud.r-project.org/"))
# install.packages(zoo)
library(zoo)
# install.packages(reshape2)
library(reshape2)


check4data = function(file, reverse = FALSE) {
    numlines = 100
    s = readLines(file)
    if(reverse == FALSE) {
        checkLines = c(1:numlines)
    } else {
        tLines = length(s)
        checkLines = rev(c((tLines - numlines):tLines))
    }
    for (i in checkLines) {
        lineVec = unlist(strsplit(s[i], split=","))
        # http://stackoverflow.com/questions/14984989/how-to-avoid-warning-when-introducing-nas-by-coercion
        if (is.na(suppressWarnings(as.integer(lineVec[1]))) == F) {
            return(i)
        }
    }
    return(0)
}

revandcollapse = function(x) {
    x = rev(x)
    x[which(x == "")] = NA
    x = na.locf(x)
    return(x)
}

extractheaderinfo = function(file, lines) {
    raw = readLines(file, n = lines)
    for (i in rev(c(1:lines))) {
        lineVec = unlist(strsplit(raw[i], split=","))
        # the above misses the last element if it's blank.
        if(i != lines) {
            if(length(lineVec) < length(prevline)) {
                lineVec = append(lineVec,"", after = length(lineVec))
            }
        }
        lineVec[which(lineVec == "")] = NA
        t = na.locf(lineVec, na.rm = FALSE)
        t[which(is.na(t))] = ""
        if(i == lines) {
            prevline = t
        } else {
            prevline = paste(prevline, t, sep = "|")
        }
    }
    r = sapply(strsplit(as.character(prevline), "\\|"), function(x) {revandcollapse(x)})
    return(r)
}


c2num = function(data, headers) {
    els = sapply(headers, function(x) {length(x)})
    chidxs = which(els <= 1)
    uhidxs = setdiff(c(1:length(els)), chidxs)
    data[uhidxs] = lapply(data[uhidxs], function(x) {suppressWarnings(as.numeric(gsub(",", "",as.character(x))))})
    data[is.na(data)] = 0
    return(data)
}

splitdata = function(data, headers, catlen = 4) {
    # http://www.statmethods.net/management/reshape.html
    els = sapply(headers, function(x) {length(x)})
    chidxs = which(els <= 1)
    uhidxs = setdiff(c(1:length(els)), chidxs)
    cnames = c(c(sapply(headers[chidxs],function(x) {x[1]})), c(sapply(headers[uhidxs],function(x) {paste(x[2],x[1], sep="_")})))
    colnames(data) = cnames
    # http://stackoverflow.com/questions/21690235/melt-multiple-groups-of-measure-vars
    # http://www.r-bloggers.com/converting-a-dataset-from-wide-to-long/
    r = reshape(data, varying=uhidxs, direction="long",idvar="ID",timevar = "Cat", sep="_")
    return(r)
}

load_file = function(file) {
    firstline = check4data(file) - 1
    # [1] 4
    lastline = check4data(file, reverse = TRUE) - firstline
    # [1] 16014

    alldata = read.csv(file, header = FALSE, skip = firstline, nrows = lastline, stringsAsFactors = FALSE)
    headers = extractheaderinfo(file,firstline)
    alldata = c2num(alldata, headers)
    reformeddata = splitdata(alldata, headers)
    return(reformeddata)
}

reformeddata = load_file("data/sales_revenue_1.csv")
                                                                     
head(reformeddata)

library(MASS)
tail(state.x77)
tail(state.abb)
# https://www.census.gov/popest/data/datasets.html
# https://docs.google.com/spreadsheets/d/1JsV5bNnFoE-4xapsnrpqRP-5wgTkpDjgC0SXKW8zgPI/edit#gid=0




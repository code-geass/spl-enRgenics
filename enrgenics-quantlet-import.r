
# Load Libraries and Data
# download data from https://www.eia.gov/electricity/data/eia826/
# options(repos= c("http://cloud.r-project.org/"))
# install.packages(zoo)
library(zoo)
# install.packages(reshape2)
library(reshape2)


check4data = function(file, reverse = FALSE) {
    numlines = 100
    s = readLines(file)
    if(reverse == FALSE) {
        # from beginning of file
        checkLines = c(1:numlines)
    } else {
        # from end of file
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
        t = gsub("\\s", "", t)
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
    ymd = which(cnames == "Year" | cnames == "Month" | cnames == "Day")
    Dates = apply(data[chidxs], 1, function(x){adddate(x,ymd)})
    data = cbind(data, Dates)
    # http://stackoverflow.com/questions/21690235/melt-multiple-groups-of-measure-vars
    # http://www.r-bloggers.com/converting-a-dataset-from-wide-to-long/
    r = reshape(data, varying=uhidxs, direction="long",idvar="ID",timevar = "Cat", sep="_")
    return(r)
}

# datefields
adddate = function(data, fields) {
    
    names = c("year", "month", "day")
    for (i in 1:length(names)) {
        if(i < length(fields)) {
            assign(names[i], data[fields[i]])
        } else {
            assign(names[i], NULL)
        }
    }
    if(is.null(year)) {
        year = 1920
    }
    if(is.null(month)) {
        month = 1
    }
    if(is.null(day)) {
        day = 1
    }
    d = as.Date(paste(month,day,year, sep="-"), "%m-%d-%Y")
    return(d)
}

load_eia_data = function(file) {
    firstline = check4data(file) - 1
    # [1] 4
    lastline = check4data(file, reverse = TRUE) - firstline
    # [1] 16014

    rawdata = read.csv(file, header = FALSE, skip = firstline, nrows = lastline, stringsAsFactors = FALSE)
    headers = extractheaderinfo(file,firstline)
    alldata = c2num(rawdata, headers)
    reformeddata = splitdata(alldata, headers)
    return(reformeddata)
}

# sample usage
 file = "data/sales_revenue_1.csv"
 reformeddata = load_eia_data(file)
 head(reformeddata)
# write.csv2(reformeddata, paste(file,".out.csv",sep=""))





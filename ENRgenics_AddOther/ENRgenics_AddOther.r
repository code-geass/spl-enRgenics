
# Load Libraries
library(MASS)
library(dplyr)

# Load Variables
source("ENRgenics_AddOtherVars.r")

# Helper Functions
splitel <- function(x) {
  statecode = substr(x,1,3)
  divnum = substr(x,4,4)
  elcode = substr(x,5,6)
  year = substr(x,7,10)
  return(c(statecode, divnum, elcode, year))
}
stfind <- function(x) {
  r = "Other"
  if(x %in% names(statecodelegend)) {
    r = statecodelegend[[x]]
  }
  return(r)
}


load_weather_data <- function(file = "../data/climdiv-tmpcst-v1.0.0-20160605") {
    dat = read.table(file, colClasses = "character")

    c1sp = lapply(dat[,1], splitel)
    c1sp = data.frame(matrix(unlist(c1sp), nrow=length(c1sp), byrow=T),stringsAsFactors=FALSE)
    dat = cbind(dat, c1sp)
    colnames(dat) <- gsub("V","", names(dat))
    library(reshape2)
    dat_long = melt(dat, id.vars=c(names(dat)[1], names(dat)[14:17]), direction="long")
    dat_long$variable = as.numeric(as.character(dat_long$variable)) - 1
    colnames(dat_long) = c("col1", "statecode", "divnum", "elcode", "Year", "Month", "temp")
    statenames = sapply(dat$X1,function(x) {stfind(x)})
    dat_long$statename = statenames
    #st1 = cbind(cbind(state.x77, state.abb),state.name)
    #colnames(st1)[which(colnames(st1) == "state.abb")] = "State"
    #colnames(st1)[which(colnames(st1) == "state.name")] = "statename"
    #st1 = as.data.frame(st1,stringsAsFactors=FALSE)
    #r = dat_long %>% left_join(st1)
    r = dat_long
    numindexes = c("temp")
    r[,numindexes] = sapply(r[,numindexes],as.numeric)
    intindexes = c("Month", "Year")
    r[,intindexes] = sapply(r[,intindexes],as.integer)
    return(r)

}

load_state_data <- function() {
    st1 = cbind(cbind(state.x77, state.abb),state.name)
    colnames(st1)[which(colnames(st1) == "state.abb")] = "State"
    colnames(st1)[which(colnames(st1) == "state.name")] = "statename"
    st1 = as.data.frame(st1,stringsAsFactors=FALSE)
    numindexes = c("Population", "Income", "Illiteracy", "Life Exp", "Murder", "HS Grad", "Frost", "Area")
    st1[,numindexes] = sapply(st1[,numindexes],as.numeric)
    return(st1)
    
}

add_data <- function(df1, df2) {
    return(df1 %>% left_join(df2))
}

load_eia_data_with_all_others <- function(file) {
    source("../ENRgenics_ImportEIA/ENRgenics_ImportEIA.r")
    eiadata = load_eia_data(file)
    otherdata = add_data(load_weather_data(), load_state_data())
    alldata = add_data(eiadata, otherdata[,c("Month", "Year", "State", "statename", "temp", "Population", "Income", "Illiteracy", "Life Exp", "Murder", "HS Grad", "Frost", "Area")])
    return(alldata)
}



# sample usage
#file = "../data/sales_revenue.csv.0"
#alldata = load_eia_data_with_all_others(file)
#head(alldata)



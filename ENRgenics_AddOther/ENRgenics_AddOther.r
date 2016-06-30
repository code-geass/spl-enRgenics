
# Load Libraries
library(MASS)
library(dplyr)

# we've used relative paths.  If you need absolute paths, change these variables or set your
# working directory to the directory of this file.
ENRGENICS_IMPORTEIA_PATH = "/home/david/Programming/github/dhpollack/spl-enRgenics/ENRgenics_ImportEIA/ENRgenics_ImportEIA.r"
ENRGENICS_CLIMATE_DATA_PATH = "../data/climdiv-tmpcst-v1.0.0-20160605"
PATH_VARS = c(ENRGENICS_IMPORTEIA_PATH, ENRGENICS_CLIMATE_DATA_PATH)

# Legend Variable
# see # ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/state-readme.txt
statecodelegend = c("001"="Alabama","030"="New York","002"="Arizona","031"="North Carolina","003"="Arkansas","032"="North Dakota","004"="California","033"="Ohio","005"="Colorado","034"="Oklahoma","006"="Connecticut","035"="Oregon","007"="Delaware","036"="Pennsylvania","008"="Florida","037"="Rhode Island","009"="Georgia","038"="South Carolina","010"="Idaho","039"="South Dakota","011"="Illinois","040"="Tennessee","012"="Indiana","041"="Texas","013"="Iowa","042"="Utah","014"="Kansas","043"="Vermont","015"="Kentucky","044"="Virginia","016"="Louisiana","045"="Washington","017"="Maine","046"="West Virginia","018"="Maryland","047"="Wisconsin","019"="Massachusetts","048"="Wyoming","020"="Michigan","050"="Alaska","021"="Minnesota","022"="Mississippi","023"="Missouri","024"="Montana","025"="Nebraska","026"="Nevada","027"="New Hampshire","028"="New Jersey","029"="New Mexico","049"="Hawaii")

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


load_weather_data <- function(file = PATH_VARS[2]) {
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
    source(PATH_VARS[1])
    eiadata = load_eia_data(file)
    otherdata = add_data(load_weather_data(), load_state_data())
    alldata = add_data(eiadata, otherdata[,c("Month", "Year", "State", "statename", "temp", "Population", "Income", "Illiteracy", "Life Exp", "Murder", "HS Grad", "Frost", "Area")])
    return(alldata)
}


# Sample Usage: Below is the sample usage of how to source this file into 
# other quantlets.  Note, I have used relative paths, but you may need to use 
# absolute paths OR set the correct working directory.  These variables 
# assume the quantlet is being sourced from a parallel folder

# load data from Quantlet
#ENRGENICS_ADDOTHER_PATH = "../ENRgenics_AddOther/ENRgenics_AddOther.r"
#source(ENRGENICS_ADDOTHER_PATH)

# start here to directly load data from this file for testing
# paths for ImportEIA and climate Data
#ENRGENICS_IMPORTEIA_PATH = "../ENRgenics_ImportEIA/ENRgenics_ImportEIA.r"
#ENRGENICS_CLIMATE_DATA_PATH = "../data/climdiv-tmpcst-v1.0.0-20160605"
#PATH_VARS = c(ENRGENICS_IMPORTEIA_PATH, ENRGENICS_CLIMATE_DATA_PATH)
# location of EIA data file
#EIA_DATA_PATH = "../data/sales_revenue.csv.0"
#file = EIA_DATA_PATH
#df = load_eia_data_with_all_others(file)
#head(df)



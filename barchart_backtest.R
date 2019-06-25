##
##  
##

require(quantmod)
require(jsonlite)
require(data.table)

fmt.doll <- function(x){
  paste("$", format(round(x, 2) , big.mark = ",", scientific = F ), sep="")
}

source.file.dir <- "home"


##
##  NOTE:
##  sometimes you have to change name.index to "stocks-options..."
##  other tiumes "options..." works fine
##

source.path.dat <- "C:\\Users\\hornr\\OneDrive\\Documents\\R_stuff\\2018.02.stock_analysis\\output.barchart"

all.files <- list.files(
  path= source.path.dat
)

all.files <- all.files[order(all.files)]

for(i in 1:length(all.files)){}
  
path.1 <- paste0( source.path.dat, "\\", all.files[i])
rundate <- substr(all.files[i], start = 1, 10) # create run date from filename
rundate <- as.Date( rundate, "%Y.%m.%d")

files.1 <- list.files(path.1) # need to develop way to handle null *.csv files
#if file.exists ## BUILD THIS STEP
file.index <- grep(pattern = "^aaa.barch.summary.csv", files.1)
path.2 <- paste0(path.1, "\\", files.1[file.index[1]])
rawdat <- read.csv(file = path.2, stringsAsFactors = F)

s2 <- rawdat$Time[grep("/", s1)]
tickerdate <- max(as.Date(s2, "%m/%d/%Y"))
expdate <- as.Date(rawdat$Exp.Date, "%m/%d/%Y")

# convert dollar flows for analysis
rawdat$dollar.flows.nice <-  as.numeric(gsub(rawdat$dollar.flows.nice, pattern = ",", replacement = ""))
rawdat$OTM <- rawdat$Strike - rawdat$Price
rawdat$pct.OTM <- rawdat$OTM/rawdat$Price*100

# produces sub data.frame with tickers that exhibit following criteria
# call is OTM, pct profit from premium b/w 60-80% and at least 1MM flowing in
consider.ticker <- (rawdat[ rawdat$OTM > 0 & rawdat$pct.prof.prem < .8 & rawdat$pct.prof.prem >.6 & rawdat$dollar.flows.nice > 1000000, ])

# nested loop to check for only ONE call being bought
ticker.drilldown <- unique(consider.ticker$Symbol)
final.index <- 0
for(i in 1:length(ticker.drilldown)){
  ticker <- ticker.drilldown[i]
  ticker.file <- files.1[grep(pattern = paste0("^", ticker, ".*", ".csv"), files.1)]
  xpath.1 <- paste0(path.1, "\\", ticker.file)
  if(nrow(read.csv(xpath.1))<2){
    final.index <- c(final.index, ticker)
  }
}
# produces an index (final.index) that selects only those tickers where only one call is bought

# create index to select only final.index tickers from rawdat file
# create rules to move on to next all.file folder if ticker is NULL
# if ticker is NON NULL, need a way to write a smaller data.frame with iportant data



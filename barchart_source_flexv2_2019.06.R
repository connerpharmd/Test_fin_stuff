
#flexv2 = uses target date variable, pulls in source$time with 
# that date and all trades that have a time stamp only


require(quantmod)
require(jsonlite)
require(data.table)

fmt.doll <- function(x){
  paste("$", format(round(x, 2) , big.mark = ",", scientific = F ), sep="")
}

source.file.dir <- "home"
target.date <- "06/24/19" # targetdate 


##
##  NOTE:
##  sometimes you have to change name.index to "stocks-options..."
##  other tiumes "options..." works fine
##

all.files <- list.files(path="C:\\Users\\hornr\\Downloads")
name.index <- grep(pattern = "^options-volume-leaders", all.files)

# source is downloads (working at home)
if(source.file.dir == "home"){
  source.path<- paste0("C:\\Users\\hornr\\Downloads\\", all.files[name.index])
}

#source is from onedrive
if(source.file.dir == "away"){
  source.path <-("C:\\Users\\hornr\\OneDrive\\Documents\\R_stuff\\2018.02.stock_analysis\\raw_data\\ExportTable.csv")
}

## if you are working with source file on onedrive
source <- read.csv(source.path, stringsAsFactors = F)



# View(source)
# source$Description <- trimws(source$Description)
source$CorP <- substr(source$Type, 1, 1)
# source$UorD <- ifelse(source$Change > 1, 1, 0)
# source$prev <- source$Last - source$Change
# source$pct.chng <- 100*((source$Change)/source$prev)
source$exp.date.r <- as.Date((source$Exp.Date), format = "%m/%d/%y")
source$Exp.Date <- as.Date((source$Exp.Date), format = "%m/%d/%y")
source$exp.date.yr <- format(source$exp.date.r, "%Y")
source$exp.date.mo <- format(source$exp.date.r, "%m")
source$exp.date.dy <- format(source$exp.date.r, "%d")
source$days.to.exp <- source$exp.date.r - Sys.Date()
# source$breakeven <- source$Description[1]
# time's sometime come back as NA from barchart (as shows time of sale, vs. date)

# grab only target date quotes, also any quotes 
# that inducate a time only
# meaning have ":" in source$Time field
step1 <- paste0(":|", target.date )
fix.ind <- grepl( pattern= step1 , source$Time)

source <- source[fix.ind, ]


source$Last <- ifelse( grepl("P",x=source$CorP), source$Last*-1, source$Last)


# test <- rep(NA, length(source$Description))  
# # pulls strike price
# for (i in 1:length(test)){
#          start.val <- regexpr(" ", source$Description[i])+1
#          stop.val <-  c( regexpr("\\(", source$Description[i]), gregexpr(" ", source$Description[i])[[1]][2])
#          stop.val <- stop.val[ stop.val >0]
#          stop.val <- min(stop.val)-1
#          test[i] <- substr(source$Description[i], start = start.val, stop = stop.val)
#          
# }
# # add strike price to table

source$strike <- source$Strike


# add break even price 
source$break.even <- as.numeric(source$strike)+source$Last

# add dollar flow
source$Option.Volume <- source$Volume
source$dollar.flows <- (source$Bid*100)*source$Option.Volume
source$dollar.flows.nice <- format(source$dollar.flows, big.mark=",")

# test <- (source[ grepl(pattern = "C|P", x = source$CorP,ignore.case = T) & source$days.to.exp > 5, ])
# 
# test <- test[order(test$Symbol, test$dollar.flows, test$CorP),]
# 
# View(test)





## Wish list
## export smaller CVS with stock idea listing: 
# 1) both calls and puts, 
# a call to put dollar flow by day
## net dollar flow (calls or puts)
## one table per idea? or all in one table
## 

## grab source file create time
time.name <-  ((file.info(source.path)[5][,1]))
time.name <- gsub(pattern = "-|:", replacement = ".", time.name)
time.name <- gsub(" ", "_", time.name)



# create handle for output file
# dat.outcome.dir.name <- paste(coh.name ,dat.set.name, dat.outcome, today_date, sep="_")

if (!dir.exists(paste("C:\\Users\\hornr\\OneDrive\\Documents\\R_stuff\\2018.02.stock_analysis\\output.barchart", time.name , sep="\\"))){
  dir.create(paste("C:\\Users\\hornr\\OneDrive\\Documents\\R_stuff\\2018.02.stock_analysis\\output.barchart", time.name , sep="\\"))
}

wdor <- getwd()
setwd(paste("C:\\Users\\hornr\\OneDrive\\Documents\\R_stuff\\2018.02.stock_analysis\\output.barchart", time.name , sep="\\"))

# if (!dir.exists(paste("C:/Users/hornr/OneDrive/Documents/R_stuff/003_Survival_curve_estim/output", today_date ,dat.outcome.dir.name, sep="/"))){
#   dir.create(paste("C:/Users/hornr/OneDrive/Documents/R_stuff/003_Survival_curve_estim/output", today_date, dat.outcome.dir.name, sep="/"))
#   setwd(paste("C:/Users/hornr/OneDrive/Documents/R_stuff/003_Survival_curve_estim/output", today_date, dat.outcome.dir.name, sep="/"))
# }


##
##  Current screen identifies only calls
##  also only identifies tickers based on criteria below
##
good.dat1 <- source
# good.dat1 <- good.dat1[, ':=' (sum.all.doll = sum(dollar.flows)), by=list(Symbol, CorP, Exp.Date)][order(good.dat1$Symbol)]
good.dat1$sum.all.doll <- good.dat1$dollar.flows
good.dat1 <- good.dat1[good.dat1$CorP == "C", ]
good.dat1$pct.prof.prem <- good.dat1$Bid/(good.dat1$break.even-good.dat1$Price)
good.dat1$prof.pct.mo <- ((good.dat1$break.even-good.dat1$Price)/good.dat1$Price)/as.numeric(good.dat1$days.to.exp)
good.dat1$prof.pct.mo <- good.dat1$prof.pct.mo*28
good.dat1$prof.pct.mo <- round(good.dat1$prof.pct.mo, 3)*100

# good.dat1 <- good.dat1[, ':=' (sum.all.doll.by.opt.exp = sum(dollar.flows)), by= list(Symbol, exp.date)][order(good.dat1$Symbol, good.dat1$exp.date, good.dat1$strike)]
# good.dat1 <- good.dat1[ good.dat1$days.to.exp > 5, ]
good.dat1 <- good.dat1[ abs(good.dat1$sum.all.doll) > 1000000 & 
                          good.dat1$pct.prof.prem > 0.6 &  
                          good.dat1$days.to.exp > 5 & 
                          good.dat1$days.to.exp < 180 &
                          good.dat1$prof.pct.mo > 2
                        ,  ] # only dollarflows by symbol > 1MM
# good.dat1 <- good.dat1[ good.dat1$pct.prof.prem > 0.8, ]
good.dat1 <- good.dat1[order(good.dat1$Symbol, good.dat1$Exp.Date),]
step1.tkr <- unique(good.dat1$Symbol)


exp.fields <- c("Symbol", 
                "prof.pct.mo",
                "pct.prof.prem",
                "days.to.exp" ,
                "Time" ,
                "Price",
                "break.even",
                "Type", 
                "Strike", 
                "Exp.Date", 
                "Volume", 
                "IV", 
                'dollar.flows.nice')

View(good.dat1[,exp.fields])

write.csv(good.dat1[,exp.fields], file = paste0("aaa.barch.summary.csv"))



for(i in 1:length(step1.tkr)){
  fun.tkr <- step1.tkr[i]
  fun.1 <- source[ source$Symbol == fun.tkr, ]
  fun.1 <- data.frame(fun.1)
  fun.1$Time <- as.Date(fun.1$Time, format = "%m/%d/%y")
  fun.1 <- fun.1[ fun.1$days.to.exp > 5, ]
  call.vol <- sum(fun.1[ fun.1$CorP == "C", "Option.Volume"])
  put.vol <- sum(fun.1[ fun.1$CorP == "P", "Option.Volume"])
  if(put.vol >0) {
    cp.rat <- round(call.vol/put.vol, 3)
  }
  if(put.vol == 0) {
    cp.rat <-  "NA"
  }
  call.flow <- sum(fun.1[ fun.1$CorP == "C", "dollar.flows"])
  put.flow <- sum(fun.1[ fun.1$CorP == "P", "dollar.flows"])
  net.flow <- call.flow+put.flow
  net.flow.name <- paste("$", round(net.flow/1000000,2), "M", sep="")
  net.call.flow.rat <- call.flow/(call.flow+(abs(put.flow)))
  
  fun.result.name <- paste(fun.tkr, net.flow.name, cp.rat, sep="_")
  fun.1 <- fun.1[order(fun.1$CorP, fun.1$dollar.flows), ]
  fun.1 <- fun.1[order(fun.1$CorP, fun.1$Exp.Date),]
  fun.1$strike <- as.numeric(fun.1$strike)
  fun.1$Last <- abs(fun.1$Last)
  
  
  write.csv(fun.1, file = paste(fun.result.name, ".csv", sep=""))
  ##
  ##  Create plotting data
  ##
  # tkr.quote <- NA
  tkr.quote <- max(fun.1$Price)
  tkr.date <- unique(fun.1$Time)
  # tkr.call.ch <- getOptionChain(step1.tkr[i])[[1]]#calls
  # tkr.put.ch <- getOptionChain(step1.tkr[i])[[1]]#puts
  start <- data.frame( Symbol= step1.tkr[i], 
                       CorP = "X", 
                       Exp.Date = tkr.date, 
                       days.to.exp = 0, 
                       strike = as.vector(as.matrix(tkr.quote)),
                       Last = NA,
                       break.even = as.vector(as.matrix(tkr.quote)), 
                       dollar.flows = NA,
                       Last.pct = NA, stringsAsFactors = F)
  fun.2 <- fun.1[c("Symbol", "CorP" ,"Exp.Date", "days.to.exp" ,"strike", "Last",
                   "break.even", "dollar.flows")]
  
  
  fun.2$Last.pct <- paste0(round((fun.2$Last/(fun.2$break.even-as.numeric(tkr.quote))*100), 1), "%")
  fun.2 <- fun.2[order(fun.2$CorP),]
  
  # adding start so we've got current stock price
  fun.2 <- rbind(start, fun.2)
  
  
  fun.2$nice.flows <- paste((round(fun.2$dollar.flows/100000))/10, "MM", sep="")
  fun.2$labels <- paste(fun.2$Last, fun.2$Last.pct, sep="-")
  fun.2$labels <- paste(fun.2$labels, fun.2$nice.flows, sep="-")
  fun.2$pct.change <- NA  
  fun.2[fun.2$CorP=="C", "pct.change"] <- (round((fun.2$break.even - fun.2$break.even[1])/fun.2$break.even[1], 3)*100)[fun.2$CorP=="C"]
  fun.2[fun.2$CorP=="C", "pct.change"] <- round(((fun.2$pct.change/fun.2$days.to.exp)*28)[fun.2$CorP=="C"], 1)
  fun.2[fun.2$CorP=="P", "pct.change"] <- round(((fun.2$Last)/(as.numeric(tkr.quote))), 2)[fun.2$CorP=="P"]*100
  
  ##
  ##  Create new data 
  ##
  fun.3 <- fun.1[,c("Symbol", "Exp.Date", "CorP", "dollar.flows")]  
  
  fun.2$left.labels <- paste("(", fun.2[, "strike"], "|", fun.2$pct.change ,")", sep="")
  fun.2 <- fun.2[order(fun.2$Exp.Date),] # ordering for neatness
  ##----------------------------------------
  ##  Start plotting
  ##----------------------------------------
  ppi <- 600
  png(paste0(fun.tkr, "_coneplot_", time.name,'.png', sep=''), width=12*ppi, height=10*ppi, res=ppi)
  
  ##
  ##  Set Plotting parameters
  ##
  x.min <- min(fun.2$Exp.Date, na.rm=T)-14
  x.max <- max(fun.2$Exp.Date, na.rm=T)+21
  y.min <- min(fun.2$break.even)-(.01*min(fun.2$break.even))
  y.max <- max(fun.2$break.even)+(.01*max(fun.2$break.even))
  ##
  ##  Start plotting
  ##
  plot(x = fun.2$Exp.Date, y=fun.2$break.even, xlim=c(x.min, x.max), ylim=c(y.min, y.max), 
       type = "n",  ylab="", xlab="", xaxt="n", yaxt="n")
  u <- par("usr")
  rect(u[1], u[3], u[2], u[4], col = "grey95", border = "black")
  par(mar=c(5.1, 4.1, 4.1, 2.1))
  par(new=T)
  abline(h = fun.2[fun.2$CorP == "X", "break.even"], lty=2, col="darkgrey")
  abline(v = unique(fun.2$Exp.Date), lty=2, col="darkgrey")
  par(new=T)
  if("C"%in%fun.2$CorP){ # ONLY DO IF THERE ARE PUTS
    ##
    ##  Plot calls
    ##
    plot(x = fun.2[ fun.2$CorP == "C", "Exp.Date"], y=fun.2[fun.2$CorP == "C", "break.even"],
         xlim=c(x.min, x.max), 
         ylim=c(y.min, y.max),
         pch=NA,
         ylab="", 
         xlab="", xaxt="n", yaxt="n")
    
    col.vec <- ifelse(fun.2[ fun.2$CorP == "C", "dollar.flows"]>1000000, "darkgreen", "grey")
    text(x = fun.2[ fun.2$CorP == "C", "Exp.Date"], y=fun.2[fun.2$CorP == "C", "break.even"],
         labels = fun.2[ fun.2$CorP == "C", "break.even"],
         cex=.6,
         font=2)
    ##
    ##  ADD strikes + premium
    ##
    text(x = fun.2[ fun.2$CorP == "C", "Exp.Date"], y=fun.2[fun.2$CorP == "C", "break.even"],
         labels = fun.2[ fun.2$CorP == "C", "labels"],
         col= col.vec,
         offset=0.8,
         pos=4,
         cex=.6,
         font=2)
    
    ##
    ##  ADD Break-evens
    ##
    text(x = fun.2[ fun.2$CorP == "C", "Exp.Date"], y=fun.2[fun.2$CorP == "C", "break.even"],
         labels = fun.2[ fun.2$CorP == "C", "left.labels"],
         col= col.vec,
         offset=0.8,
         pos=2,
         cex=.6,
         font=2)
  }
  par(new=T)
  if("P"%in%fun.2$CorP){ # ONLY DO IF THERE ARE PUTS
    ##
    ##  Plot puts
    ##
    par(new=T)
    plot(x = fun.2[ fun.2$CorP == "P", "Exp.Date"], y=fun.2[fun.2$CorP == "P", "break.even"],
         xlim=c(x.min, x.max), 
         ylim=c(y.min, y.max),
         pch=NA,
         ylab="", 
         xlab="", xaxt="n", yaxt="n")
    
    col.vec <- ifelse(fun.2[ fun.2$CorP == "P", "dollar.flows"]>1000000, "darkred", "grey")
    text(x = fun.2[ fun.2$CorP == "P", "Exp.Date"], y=fun.2[fun.2$CorP == "P", "break.even"],
         labels = fun.2[ fun.2$CorP == "P", "break.even"],
         cex=.6,
         font=2)
    ##
    ##  ADD strikes + premium
    ##
    col.vec <- ifelse(fun.2[ fun.2$CorP == "P", "dollar.flows"]>1000000, "darkred", "grey")
    text(x = fun.2[ fun.2$CorP == "P", "Exp.Date"], y=fun.2[fun.2$CorP == "P", "break.even"],
         labels = fun.2[ fun.2$CorP == "P", "labels"],
         col = col.vec,
         pos=4,
         offset=0.8,
         cex=.6,
         font=2)
    
    ##
    ##  ADD Break-evens
    ##
    col.vec <- ifelse(fun.2[ fun.2$CorP == "P", "dollar.flows"]>1000000, "darkred", "grey")
    text(x = fun.2[ fun.2$CorP == "P", "Exp.Date"], y=fun.2[fun.2$CorP == "P", "break.even"],
         labels = fun.2[ fun.2$CorP == "P", "left.labels"],
         col=col.vec,
         offset=0.8,
         pos=2,
         cex=.6,
         font=2)
  }
  ##
  ##  ADD current price
  ##
  Axis(side=2, at = fun.2[fun.2$CorP == "X", "break.even"], labels=fun.2[fun.2$CorP == "X", "break.even"], cex.axis=1, font=2, las=1)
  
  # this line removes "start" row which is needed for current stock price
  fun.2 <- na.omit(fun.2)
  
  Axis(side=1, at = unique(fun.2$Exp.Date), labels =unique(fun.2$Exp.Date), cex.axis=.6, font=2, las=2)
  
  mtext(side=1,line=-1.5, 
        at = unique(fun.2$Exp.Date), 
        text = paste0(unique(fun.2$days.to.exp), "d"),
        cex=1)
  
  title.text <- paste(step1.tkr[i], "option cone for", gsub(pattern = "_", replacement = " @ ", x = time.name))
  subtit.text <- paste0("(cost per lot:", fmt.doll(fun.2[fun.2$CorP == "X", "break.even",]*100), ")")
  title(main=title.text )
  title(main=subtit.text, line=0.5, cex.main=.9)
  
  dev.off()
  
}



new.f.name1 <- gsub(pattern = "-", "_", all.files[name.index])
index1 <- regexpr(pattern = "([0-9])", new.f.name1)
new.f.name2 <- substr(x = new.f.name1, start = 1, stop=index1-2)
new.f.name3 <- paste("barchart", new.f.name2, sep="_")
new.f.name4 <- paste( new.f.name3, time.name, sep="_")
new.f.name4 <- paste0( new.f.name4, ".csv")
new.pth.name <- gsub(pattern = all.files[name.index], replacement = new.f.name4, source.path)
file.rename(from = source.path, to=new.pth.name)


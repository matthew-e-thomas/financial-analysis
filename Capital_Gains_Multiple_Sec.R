data.AMZN <- read.csv("AMZN.csv", header=TRUE)
library(xts)
library(ggplot2)
data.AMZN$Date <- as.Date(data.AMZN$Date, format="%Y-%m-%d")
data.AMZN <- xts(data.AMZN[, 2:7], order.by=data.AMZN[,1])
names(data.AMZN) <- paste(c("AMZN.Open", "AMZN.High", "AMZN.Low",
                            "AMZN.Close", "AMZN.Adj.Close","AMZN.Volume"))

data.AMZN[c(1:3, nrow(data.AMZN)),]

data.IBM <- read.csv("IBM.csv", header=TRUE)
data.IBM$Date <- as.Date(data.IBM$Date, format="%Y-%m-%d")
data.IBM <- xts(data.IBM[, 2:7], order.by=data.IBM[, 1])
names(data.IBM) <- paste(c("IBM.Open", "IBM.High", "IBM.Low",
                            "IBM.Close", "IBM.Adj.Close","IBM.Volume"))
data.IBM[c(1:3, nrow(data.IBM)),]

data.GOOGL <- read.csv("GOOGL.csv", header=TRUE)
data.GOOGL$Date <- as.Date(data.GOOGL$Date, format="%Y-%m-%d")
data.GOOGL <- xts(data.GOOGL[, 2:7], order.by=data.GOOGL[, 1])
names(data.GOOGL) <- paste(c("GOOGL.Open", "GOOGL.High", "GOOGL.Low",
                            "GOOGL.Close", "GOOGL.Adj.Close","GOOGL.Volume"))
data.GOOGL[c(1:3, nrow(data.GOOGL)),]

#GSPC is the S & P 500
data.GSPC <- read.csv("GSPC.csv", header=TRUE)
data.GSPC$Date <- as.Date(data.GSPC$Date, format="%Y-%m-%d")
data.GSPC <- xts(data.GSPC[, 2:7], order.by=data.GSPC[, 1])
names(data.GSPC) <- paste(c("GSPC.Open", "GSPC.High", "GSPC.Low",
                            "GSPC.Close", "GSPC.Adj.Close", "GSPC.Volume"))
data.GSPC[c(1:3, nrow(data.GSPC)),]

Close.Prices <- data.AMZN$AMZN.Close
Close.Prices <- cbind(Close.Prices, data.GOOGL$GOOGL.Close,
                      data.IBM$IBM.Close, data.GSPC$GSPC.Close)
multi.df <- cbind(index(Close.Prices), data.frame(Close.Prices))
names(multi.df) <- paste(c("date", "AMZN", "GOOGL", "IBM", "GSPC"))
rownames(multi.df) <- seq(1, nrow(multi.df), 1)
multi.df[c(1:3, nrow(multi.df)),]

#Add indexes of normalized security values
multi.df$AMZN.idx <-multi.df$AMZN/multi.df$AMZN[1]
multi.df$GOOGL.idx <-multi.df$GOOGL/multi.df$GOOGL[1]
multi.df$IBM.idx <-multi.df$IBM/multi.df$IBM[1]
multi.df$GSPC.idx <-multi.df$GSPC/multi.df$GSPC[1]
options(digits = 5)
multi.df[c(1:3,  nrow(multi.df)), ]

#Let's see how you would fare with these stocks
y.range <- range(multi.df[, 6:9])
plot(x=multi.df$date, y=multi.df$GSPC.idx, type="l", xlab="Date",
     ylim=y.range,
     ylab="Value of Investment ($)", col="black", lty=1,
     lwd=2, main="Value of $1 Investment in AMZN, GOOGLE, IBM,
     and the S&P 500 Ineex July 6 2016 - July 5 2019")
lines(x=multi.df$date, y=multi.df$AMZN.idx, col="black", lty=2,
      lwd=1)
lines(x=multi.df$date, y=multi.df$GOOGL.idx, col="gray", lty=2,
      lwd=1)
lines(x=multi.df$date, y=multi.df$IBM.idx, col="gray", lty=1,
      lwd=1)
abline(h=1, lty=1, col="black")
legend("topleft", c("AMZN", "GOOGLE", "IBM", "S&P 500 Index"),
       col=c("black", "gray", "gray", "black"), lty=c(2,2,1,1),
       lwd=c(1,1,1,2))
#Amazon killed it, Google did better than the market, IBM not so much

#Let's look more closely at Amazon's numbers
#calculate moving averages based on Close prices

AMZN.ma <- data.AMZN[, 4]
AMZN.ma$ma50 <- rollmeanr(AMZN.ma$AMZN.Close, k=50)
AMZN.ma$ma200 <- rollmeanr(AMZN.ma$AMZN.Close, k=200)
head(AMZN.ma)
#subset it starting 2017 to get rid of the NAs
AMZN.ma <- subset(AMZN.ma, index(AMZN.ma)>="2017-06-01")
AMZN.ma[c(1:3, nrow(AMZN.ma)), ]


y.range <- range(AMZN.ma, na.rm=TRUE)
ggplot(data=AMZN.ma, aes(y=AMZN.ma$AMZN.Close, x=index(AMZN.ma)), color="variable") +
  geom_line(colour="black", size=.5)+ geom_line(aes(y=AMZN.ma$ma50), colour="red", linetype="dotted")+
  geom_line(aes(y=AMZN.ma$ma200), colour="blue", linetype="dashed") +
  labs(title="Amazon Moving Averages", y="Price ($)", x="Date",
       scale_colour_manual(values=c("red", "blue")))
  
#Looks like our 50 day moving average crossed below the 200 day, but not it's back up

#Let's look at stock volatility to see what that tells us

AMZN.bb <- data.AMZN[, 4]
AMZN.bb$avg <- rollmeanr(AMZN.bb$AMZN.Close, k=20)
AMZN.bb$sd <- rollapply(AMZN.bb$AMZN.Close, width=20, FUN=sd, fill=NA)
AMZN.bb[c(1:3, nrow(AMZN.bb)), ]
#subset it to just the last year
AMZN.bb <- subset(AMZN.bb, index(AMZN.bb) >= "2018-07-05")
AMZN.bb$sd2up <- AMZN.bb$avg + 2 * AMZN.bb$sd    
AMZN.bb$sd2down <- AMZN.bb$avg - 2 * AMZN.bb$sd  

y.range <- range(AMZN.bb[, -3], na.rm=TRUE)
ggplot(data=AMZN.bb, aes(y=AMZN.bb$AMZN.Close, x=index(AMZN.bb))) +
  geom_line(colour="black", size=.5)+ geom_line(aes(y=AMZN.bb$avg), colour="red", linetype="dotted")+
  geom_line(aes(y=AMZN.bb$sd2up), colour="blue", linetype="dashed") +
  geom_line(aes(y=AMZN.bb$sd2down), colour="blue", linetype="dashed") +
  labs(title="Bollinger Bands Amazon, 20 day MA", y="Price ($)", x="Date",
       scale_colour_manual(values=c("black", "red", "blue")))
#volatility doesn't look too terrible, but stock does look over sold in late 2018 



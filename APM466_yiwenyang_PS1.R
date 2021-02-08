# Setup
install.packages("jrvFinance")
library("jrvFinance")

# Import clean price and dirty price csv files
# evaluate YTM
clean1 <- read.csv("/Users/yangyiwen/Desktop/clean.csv", header=TRUE)
clean <- as.data.frame(clean1)

# evaluate spot rate and forward rate
dirty1 <- read.csv("/Users/yangyiwen/Desktop/dirty.csv", header=TRUE)
dirty <- as.data.frame(dirty1)

#generate two 11 by 10 matrix (11bonds, 10 days)
#used to calculate yield and spot rate directly from the collected data
a = rep(1,11)
ryield =rspots=  data.frame(a,a,a,a,a,a,a,a,a,a)

#used to interpolate
b = rep(1,10)
fyield = fspots= data.frame(b,b,b,b,b,b,b,b,b,b)

#Define date
date = c("2021-01-18","2021-01-19","2021-01-20","2021-01-21","2021-01-22","2021-01-25","2021-01-26","2021-01-27",
          "2021-01-28","2021-01-29")


#Question 4
#Evaluating YTM
for (i in 1:11) {
  for (j in 1:10) {
    # bond.yield function from jrvFinance package: (settlement date, maturity date, 
    # coupon rate, coupon frequency, clean/close price, daycount,compounding frequency, redemption value)
    # the redemption is defaulted as 100
    ryield[i,j]=bond.yield(date[j],clean$maturity_date[i],clean$coupon[i],freq=2,
                              clean[i,j+9],"ACT/ACT",comp.freq=2,redemption_value = 100)
  }
}
View(ryield)

#Calculate Spot rate with less than 6 months until maturity
#The first bond
for (i in 1:10)
{
  #dirty prices for the first bond
  p=dirty[1,9+i]
  # times 100 to get notional
  coupon=dirty[1,7]*100
  #default 
  face=100
  maturityinyears=dirty[1,6]
  #write the formula
  rspots[1,i]=2*((p/(0.5*coupon+face))^(-1/(2*maturityinyears))-1)
}

#The second bond
for (i in 1:10)
{
  #dirty prices for the second bond
  face=100
  p=dirty[2,9+i]
  coupon=dirty[1,7]*100 # times 100 to get notional
  maturityinyears=dirty[1,6]
  rspots[2,i]=2*((p/(0.5*coupon+face))^(-1/(2*maturityinyears))-1) 
}


#Calculate Spot rate with more than 6 months until maturity
for (i in 3:11){
  for (j in 1:10){
    face=100
    p=dirty[i,9+j]
    coupon=dirty$coupon[i]*100 # times 100 to get notional
    pvcoupon=0
    maturityinyears=dirty$years_to_maturity[i]
    #time we receive coupon payment in years
    coupont = seq((6-dirty$month_since_last_coupon[i])/12,(dirty$month_until_maturity[i]-1)/12,1/2)
    for (h in c(1:length(coupont)))
    {
      pvcoupon=pvcoupon+coupon*(1+rspots[h,j]/2)^(-2*coupont[h])
    }
    #price without present value of coupon
    newprice=p-pvcoupon
    pvcoupon=0
    rspots[i,j]=2*((newprice/(0.5*coupon+face))^(-1/(2*maturityinyears))-1)
  }
}
View(rspots)

#combine the data frames
compound <- data.frame(dirty,rspots)



#Interpolating ytm and spot with the same linear technique

for (i in 1:10){
  for (j in 1:10){
    fyield[j,i] = approx(compound$month_until_maturity,ryield[[i]],xout=6*j)$y
    fspots[j,i]=approx(compound$month_until_maturity,rspots[[i]],xout=6*j)$y
  }
}

#renaming the rows
rownames(fyield)=seq(6,60,6)
rownames(fspots)=seq(6,60,6)


#Plot superimposed YTM curves
plot(seq(6,60,6),fyield$b,type="l", col="blue",
     xlab="Number of Months from Feb.2021",ylab="Yields (compounded semiannually)", main ="Superimposed Yield Curves")
lines(seq(6,60,6),fyield$b.1,col="red")
lines(seq(6,60,6),fyield$b.2,col="yellow")
lines(seq(6,60,6),fyield$b.3,col="cyan")
lines(seq(6,60,6),fyield$b.4,col="darkgreen")
lines(seq(6,60,6),fyield$b.5,col="darkorange1")
lines(seq(6,60,6),fyield$b.6,col="darkviolet")
lines(seq(6,60,6),fyield$b.7,col="deeppink")
lines(seq(6,60,6),fyield$b.8,col="brown")
lines(seq(6,60,6),fyield$b.9,col="bisque")
legend("topright",date,lty=c(1,1), lwd=c(2,2),cex=.5, bty = "n", 
       col=c("blue","red","yellow","cyan","darkgreen","darkorange1","darkviolet","deeppink","brown","bisque"))

#plot superimposed spots curves

plot(seq(6,60,6),fspots$b,type="l", col="blue",
     xlab="Number of Months from Feb.2021",ylab="Spot rate (compounded semiannually)", main ="Superimposed Spot Curves")
lines(seq(6,60,6),fspots$b.1,col="red")
lines(seq(6,60,6),fspots$b.2,col="yellow")
lines(seq(6,60,6),fspots$b.3,col="cyan")
lines(seq(6,60,6),fspots$b.4,col="darkgreen")
lines(seq(6,60,6),fspots$b.5,col="darkorange1")
lines(seq(6,60,6),fspots$b.6,col="darkviolet")
lines(seq(6,60,6),fspots$b.7,col="deeppink")
lines(seq(6,60,6),fspots$b.8,col="brown")
lines(seq(6,60,6),fspots$b.9,col="bisque")
legend("topright",date,lty=c(1,1), cex=.5, bty = "n", 
       col=c("blue","red","yellow","cyan","darkgreen","darkorange1","darkviolet","deeppink","brown","bisque"))


#Forward rate
#define an identity matrix first, and insert values afterwards
c = rep(1,4)
forw = data.frame(c,c,c,c,c,c,c,c,c,c)

#Recall the formula of forward rate
for (j in 1:4){
  for (i in 1:10){
    n_year=(1+fspots[2*j,i]/2)^(2*j)
    one_year_for=(1+fspots[2+2*j,i]/2)^(2+2*j)
    forw[j,i]=2*((one_year_for/n_year)^(1/2)-1)
  }
}

#plot forward rate with four points
plot(seq(1,4),forw$c,type="l", col="blue",
     xlab="Number of Months from Feb.2021",ylab="Forward rate (compounded semiannually)", main ="Superimposed Forward Curves")
lines(seq(1,4),forw$c.1,col="red")
lines(seq(1,4),forw$c.2,col="yellow")
lines(seq(1,4),forw$c.3,col="cyan")
lines(seq(1,4),forw$c.4,col="darkgreen")
lines(seq(1,4),forw$c.5,col="darkorange1")
lines(seq(1,4),forw$c.6,col="darkviolet")
lines(seq(1,4),forw$c.7,col="deeppink")
lines(seq(1,4),forw$c.8,col="brown")
lines(seq(1,4),forw$c.9,col="bisque")
legend("topright",date,lty=c(1,1), cex=.5, bty = "n", 
       col=c("blue","red","yellow","cyan","darkgreen","darkorange1","darkviolet","deeppink","brown","bisque"))



#Question5
#covariance matrix for log-return of yields#
lyields1=lyields2=lyields3=lyields4=lyields5=vector("numeric",9)

for (i in 1:9)
{
  lyields1[i]=log(fyield[2,i]/fyield[2,i+1])
  lyields2[i]=log(fyield[4,i]/fyield[4,i+1])
  lyields3[i]=log(fyield[6,i]/fyield[4,i+1])
  lyields4[i]=log(fyield[8,i]/fyield[8,i+1])
  lyields5[i]=log(fyield[10,i]/fyield[10,i+1])
}

lyields=data.frame(lyields1,lyields2,lyields3,lyields4,lyields5)
cov_lyields=cov(lyields,lyields)


#covariance matrix for forward rates
forw11=forw12=forw13=forw14=vector("numeric",9)
#evaluate based on the given formula
for(i in 1:9)
{
  forw12[i]=log(forw[1,i]/forw[1,i+1])
  forw13[i]=log(forw[2,i]/forw[2,i+1])
  forw14[i]=log(forw[3,i]/forw[3,i+1])
  forw15[i]=log(forw[4,i]/forw[4,i+1])
}

fforw = data.frame(forw11,forw12,forw13,forw14)
cov_fforw=cov(fforw,fforw)


#Question 6
#eigenvalues and eigenvectors for the above two matrices
e_yield=eigen(cov_lyields,symmetric=TRUE)
e_yield
e_forw=eigen(cov_fforw,symmetric=TRUE)
e_forw




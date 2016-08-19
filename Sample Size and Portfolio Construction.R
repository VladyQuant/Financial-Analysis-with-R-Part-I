library('quantmod')
library('XML')
 
#First I get the tickers of Dow Jones's components
url <- c("http://www.cnbc.com/dow-components/")
  symbols <- data.frame(readHTMLTable(url))
tickers <- c(as.vector(symbols$NULL.Symbol))
tickers

#I get adjusted close monthly prices from May 2013 to May 2016, since I need just monthly returns for June 2013-May 2016 in my analysis  
#I use separate arrays for DJIA and its components
#First I download DJIA
getSymbols('^DJI',src="yahoo",from='2013-05-20',
to ='2016-06-12')
#here I put the adjusted close monthly prices of DJIA from May 2013 to May 2016 in a matrix
DJIA_adj_close.monthly_prices <-matrix(nrow = 38, ncol = 1)
DJIA_adj_close.monthly_prices = as.numeric(Ad(to.monthly(DJI)))
#Next I download the components
getSymbols(tickers,src="yahoo",from='2013-05-20',
to ='2016-06-12')

#here I put the adjusted close monthly prices of the components from May 2015 to May 2016 in a matrix
comp_adj_close.monthly_prices <-matrix(nrow = 38, ncol = 30)
for(i in 1:30)
comp_adj_close.monthly_prices[,i] = as.numeric(Ad(to.monthly(eval(parse(text = tickers[i])))))

#here I calculate the monthly returns
#First I calculate them for DJIA
DJIA_monthly_returns <- matrix(nrow = 36, ncol = 1)
for (i in 1:36)
DJIA_monthly_returns[i] = DJIA_adj_close.monthly_prices[i+1]/DJIA_adj_close.monthly_prices[i] -1

#Next I calculate them for the components
comp_monthly_returns <- matrix(nrow = 36, ncol = 30)
for (j in 1:30) for (i in 1:36)
comp_monthly_returns[i,j] = comp_adj_close.monthly_prices[i+1,j]/comp_adj_close.monthly_prices[i,j] -1 

#Here I calculate mean and standard deviation of monthly returns for DJIA
mean_DJIA = mean(DJIA_monthly_returns)
stdev_DJIA = sd(DJIA_monthly_returns)  

#Here I choose an equalweighted portfolio consisting of 5 random stocks from DJIA
rand_combinat <- sort(sample(1:30,5))
comp5_portfolio_monthly_returns <-  comp_monthly_returns[,rand_combinat[1]]
for (i in 2:5) 
{
	comp5_portfolio_monthly_returns = comp5_portfolio_monthly_returns + comp_monthly_returns[,rand_combinat[i]]
	}
comp5_portfolio_monthly_returns = comp5_portfolio_monthly_returns/5
#I calculate mean monthly return and standard deviation
mean_comp5_portfolio_monthly_returns = mean(comp5_portfolio_monthly_returns)
stdev_comp5_portfolio_monthly_returns = sd(comp5_portfolio_monthly_returns)

#I do the same for 10 random stocks
rand_combinat <- sort(sample(1:30,10))
comp10_portfolio_monthly_returns <-  comp_monthly_returns[,rand_combinat[1]]
for (i in 2:10) 
{
	comp10_portfolio_monthly_returns = comp10_portfolio_monthly_returns + comp_monthly_returns[,rand_combinat[i]]
	}
comp10_portfolio_monthly_returns = comp10_portfolio_monthly_returns/10
#I calculate mean monthly return and standard deviation
mean_comp10_portfolio_monthly_returns = mean(comp10_portfolio_monthly_returns)
stdev_comp10_portfolio_monthly_returns = sd(comp10_portfolio_monthly_returns)

#I do the same for 15 random stocks
rand_combinat <- sort(sample(1:30,15))
comp15_portfolio_monthly_returns <-  comp_monthly_returns[,rand_combinat[1]]
for (i in 2:15) 
{
	comp15_portfolio_monthly_returns = comp15_portfolio_monthly_returns + comp_monthly_returns[,rand_combinat[i]]
	}
comp15_portfolio_monthly_returns = comp15_portfolio_monthly_returns/15
#I calculate mean monthly return and standard deviation
mean_comp15_portfolio_monthly_returns = mean(comp15_portfolio_monthly_returns)
stdev_comp15_portfolio_monthly_returns = sd(comp15_portfolio_monthly_returns)

#I do the same for 20 random stocks
rand_combinat <- sort(sample(1:30,20))
comp20_portfolio_monthly_returns <-  comp_monthly_returns[,rand_combinat[1]]
for (i in 2:20) 
{
	comp20_portfolio_monthly_returns = comp20_portfolio_monthly_returns + comp_monthly_returns[,rand_combinat[i]]
	}
comp20_portfolio_monthly_returns = comp20_portfolio_monthly_returns/20
#I calculate mean monthly return and standard deviation
mean_comp20_portfolio_monthly_returns = mean(comp20_portfolio_monthly_returns)
stdev_comp20_portfolio_monthly_returns = sd(comp20_portfolio_monthly_returns)

#I do the same for 25 random stocks
rand_combinat <- sort(sample(1:30,25))
comp25_portfolio_monthly_returns <-  comp_monthly_returns[,rand_combinat[1]]
for (i in 2:25) 
{
	comp25_portfolio_monthly_returns = comp25_portfolio_monthly_returns + comp_monthly_returns[,rand_combinat[i]]
	}
comp25_portfolio_monthly_returns = comp25_portfolio_monthly_returns/25
#I calculate mean monthly return and standard deviation
mean_comp25_portfolio_monthly_returns = mean(comp25_portfolio_monthly_returns)
stdev_comp25_portfolio_monthly_returns = sd(comp25_portfolio_monthly_returns)


#I do the same for all 30 componets of DJIA
combinat <- c(1:30)
comp30_portfolio_monthly_returns <-  comp_monthly_returns[,combinat[1]]
for (i in 2:30) 
{
	comp30_portfolio_monthly_returns = comp30_portfolio_monthly_returns + comp_monthly_returns[,combinat[i]]
	}
comp30_portfolio_monthly_returns = comp30_portfolio_monthly_returns/30
#I calculate mean monthly return and standard deviation
mean_comp30_portfolio_monthly_returns = mean(comp30_portfolio_monthly_returns)
stdev_comp30_portfolio_monthly_returns = sd(comp30_portfolio_monthly_returns)


#Here I calculate the mean excess returns of the sample portfolios.
mean_excess_comp5= mean_comp5_portfolio_monthly_returns - mean_DJIA
mean_excess_comp10= mean_comp10_portfolio_monthly_returns - mean_DJIA
mean_excess_comp15= mean_comp15_portfolio_monthly_returns - mean_DJIA
mean_excess_comp20= mean_comp20_portfolio_monthly_returns - mean_DJIA
mean_excess_comp25= mean_comp25_portfolio_monthly_returns - mean_DJIA
mean_excess_comp30= mean_comp30_portfolio_monthly_returns - mean_DJIA

#Here I calculate the tracking errors of the sample portfolios
track_error_comp5 = sd(comp5_portfolio_monthly_returns-DJIA_monthly_returns)
track_error_comp10 = sd(comp10_portfolio_monthly_returns-DJIA_monthly_returns)
track_error_comp15 = sd(comp15_portfolio_monthly_returns-DJIA_monthly_returns)
track_error_comp20 = sd(comp20_portfolio_monthly_returns-DJIA_monthly_returns)
track_error_comp25 = sd(comp25_portfolio_monthly_returns-DJIA_monthly_returns)
track_error_comp30 = sd(comp30_portfolio_monthly_returns-DJIA_monthly_returns)

x <- c(5,10,15,20,25,30)
#First I grahically represent the mean excess returns of the sample portfolios
mn <- 100 * c(mean_excess_comp5,mean_excess_comp10,mean_excess_comp15,mean_excess_comp20,mean_excess_comp25,mean_excess_comp30)
plot(x,mn,main = "Mean excess returns",xlab = "Number of Components of DJIA", ylab = "Values in %",col = "green",pch = 0)

#Here I graphically represent the tracking errors and risk of the sample portfolios
y <- 100 * c(stdev_comp5_portfolio_monthly_returns,stdev_comp10_portfolio_monthly_returns,stdev_comp15_portfolio_monthly_returns,stdev_comp20_portfolio_monthly_returns,stdev_comp25_portfolio_monthly_returns,stdev_comp30_portfolio_monthly_returns)
z <- 100 * c(track_error_comp5,track_error_comp10,track_error_comp15,track_error_comp20,track_error_comp25,track_error_comp30)
plot(x,y,
main = "Risks and Tracking errors", xlab = "Number of Components of DJIA", ylab = "Values in %",
ylim = c(min(min(y),min(z)),max(max(y),max(z))),  pch = 1, col = "orange")
points(x,z,pch = 2, col = "blue")
legend("right",legend=c("Risk", "Tracking error"),pch = c(1,2),col = c("orange","blue"))

covMat <- read.csv('cov.csv', header = FALSE)
corMat <- read.csv('corMat.csv', header = FALSE)

clustOrder <- hclust(dist(corMat), method = 'single')$order

getIVP <- function(covMat) {
  invDiag <- 1/diag(as.matrix(covMat))
  weights <- invDiag/sum(invDiag)
  return(weights)
}

getClusterVar <- function(covMat, cItems) {
  covMatSlice <- covMat[cItems, cItems]
  weights <- getIVP(covMatSlice)
  cVar <- t(weights) %*% as.matrix(covMatSlice) %*% weights
  return(cVar)
}

getRecBipart <- function(covMat, sortIx) {
  w <- rep(1,ncol(covMat))
  w <- recurFun(w, covMat, sortIx)
  return(w)
}

recurFun <- function(w, covMat, sortIx) {
  subIdx <- 1:trunc(length(sortIx)/2)
  cItems0 <- sortIx[subIdx]
  cItems1 <- sortIx[-subIdx]
  cVar0 <- getClusterVar(covMat, cItems0)
  cVar1 <- getClusterVar(covMat, cItems1)
  alpha <- 1 - cVar0/(cVar0 + cVar1)
  
  # scoping mechanics using w as a free parameter
  w[cItems0] <- w[cItems0] * alpha
  w[cItems1] <- w[cItems1] * (1-alpha)
  
  if(length(cItems0) > 1) {
    w <- recurFun(w, covMat, cItems0)
  }
  if(length(cItems1) > 1) {
    w <- recurFun(w, covMat, cItems1)
  }
  return(w)
}


out <- getRecBipart(covMat, clustOrder)
out

require(tseries)
require(PerformanceAnalytics)
require(quantmod)
require(Quandl)

Quandl.api_key("YOUR_AUTHENTICATION_HERE") # not displaying my own api key, sorry <img src="https://i0.wp.com/s0.wp.com/wp-content/mu-plugins/wpcom-smileys/twemoji/2/72x72/1f626.png?w=456&ssl=1" alt="?" class="wp-smiley jetpack-lazy-image" style="height: 1em; max-height: 1em;" data-recalc-dims="1" data-lazy-src="https://i0.wp.com/s0.wp.com/wp-content/mu-plugins/wpcom-smileys/twemoji/2/72x72/1f626.png?w=456&is-pending-load=1#038;ssl=1" srcset="data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7"><noscript><img src="https://i0.wp.com/s0.wp.com/wp-content/mu-plugins/wpcom-smileys/twemoji/2/72x72/1f626.png?w=456&ssl=1" alt="?" class="wp-smiley" style="height: 1em; max-height: 1em;" data-recalc-dims="1" /></noscript>

# function to append missing (I.E. assets not selected) asset names and sort into original order
appendMissingAssets <- function(wts, allAssetNames, wtsDate) {
  absentAssets <- allAssetNames[!allAssetNames %in% names(wts)]
  absentWts <- rep(0, length(absentAssets))
  names(absentWts) <- absentAssets
  wts <- c(wts, absentWts)
  wts <- xts(t(wts), order.by=wtsDate)
  wts <- wts[,allAssetNames]
  return(wts)
}

symbols <- c("SPY", "VGK",   "EWJ",  "EEM",  "VNQ",  "RWX",  "IEF",  "TLT",  "DBC",  "GLD")  

rets <- list()
for(i in 1:length(symbols)) {
  
  # quandl command to download from EOD database. Free users should use write.zoo in this loop.
  
  returns <- Return.calculate(Quandl(paste0("EOD/", symbols[i]), start_date="1990-12-31", type = "xts")$Adj_Close)
  colnames(returns) <- symbols[i]
  rets[[i]] <- returns
}
rets <- na.omit(do.call(cbind, rets))

invVolWts <- list()
minVolWts <- list()
hrpWts <- list()
ep <- endpoints(rets, on =  "months")
nMonths = 6 # month lookback (6 as per parameters from allocateSmartly)
nVol = 20 # day lookback for volatility (20 ibid)

for(i in 1:(length(ep)-nMonths)) {
  
  # get returns subset and compute absolute momentum
  retSubset <- rets[c(ep[i]:ep[(i+nMonths)]),]
  retSubset <- retSubset[-1,]
  moms <- Return.cumulative(retSubset)
  
  # select top performing assets and subset returns for them
  highRankAssets <- rank(moms) >= 6 # top 5 assets
  posReturnAssets <- moms > 0 # positive momentum assets
  selectedAssets <- highRankAssets & posReturnAssets # intersection of the above
  selectedSubset <- retSubset[,selectedAssets] # subset returns slice
  
  if(sum(selectedAssets)==0) { # if no qualifying assets, zero weight for period
    
    wts <- xts(t(rep(0, ncol(retSubset))), order.by=last(index(retSubset)))
    colnames(wts) <- colnames(retSubset)
    invVolWts[[i]] <- minVolWts[[i]] <- hrpWts[[i]] <- wts
    
  } else if (sum(selectedAssets)==1) { # if one qualifying asset, invest fully into it
    
    wts <- xts(t(rep(0, ncol(retSubset))), order.by=last(index(retSubset)))
    colnames(wts) <- colnames(retSubset)
    wts[, which(selectedAssets==1)] <- 1
    invVolWts[[i]] <- minVolWts[[i]] <- hrpWts[[i]] <- wts
    
  } else { # otherwise, use weighting algorithms
    
    cors <- cor(selectedSubset) # correlation
    volSubset <- tail(selectedSubset, nVol) # 20 day volatility
    vols <- StdDev(volSubset)
    covs <- t(vols) %*% vols * cors
    
    # minimum volatility using portfolio.optim from tseries
    minVolRets <- t(matrix(rep(1, sum(selectedAssets))))
    minVolWt <- portfolio.optim(x=minVolRets, covmat = covs)$pw
    names(minVolWt) <- colnames(covs)
    minVolWt <- appendMissingAssets(minVolWt, colnames(retSubset), last(index(retSubset)))
    minVolWts[[i]] <- minVolWt
    
    # inverse volatility weights
    invVols <- 1/vols 
    invVolWt <- invVols/sum(invVols) 
    invNames <- colnames(invVolWt)
    invVolWt <- as.numeric(invVolWt) 
    names(invVolWt) <- invNames
    invVolWt <- appendMissingAssets(invVolWt, colnames(retSubset), last(index(retSubset)))
    invVolWts[[i]] <- invVolWt
    
    # hrp weights
    clustOrder <- hclust(dist(cors), method = 'single')$order
    hrpWt <- getRecBipart(covs, clustOrder)
    names(hrpWt) <- colnames(covs)
    hrpWt <- appendMissingAssets(hrpWt, colnames(retSubset), last(index(retSubset)))
    hrpWts[[i]] <- hrpWt
  }
}

invVolWts <- round(do.call(rbind, invVolWts), 3) # round for readability
minVolWts <- round(do.call(rbind, minVolWts), 3)
hrpWts <- round(do.call(rbind, hrpWts), 3)

# allocate to cash if no allocation made due to all negative momentum assets
invVolWts$cash <- 0; invVolWts$cash <- 1-rowSums(invVolWts)
hrpWts$cash <- 0; hrpWts$cash <- 1-rowSums(hrpWts)
minVolWts$cash <- 0; minVolWts$cash <- 1-rowSums(minVolWts)

# cash value will be zero
rets$cash <- 0

# compute backtest returns
invVolRets <- Return.portfolio(R = rets, weights = invVolWts)
minVolRets <- Return.portfolio(R = rets, weights = minVolWts)
hrpRets <- Return.portfolio(R = rets, weights = hrpWts)


compare <- cbind(invVolRets, minVolRets, hrpRets)
colnames(compare) <- c("invVol", "minVol", "HRP")
charts.PerformanceSummary(compare)
rbind(table.AnnualizedReturns(compare), maxDrawdown(compare), CalmarRatio(compare))  
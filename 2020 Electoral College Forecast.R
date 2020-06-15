# Jens Olson
# jens.olson@gmail.com

library(rpredictit)
set.seed(5)

# Comment the line below once 'markets' variable has been stored to run additional analysis
markets <- data.frame(all_markets())

# Store variables, PredictIt IDs,
# electoral votes, and population
d <- data.frame(rbind(c('AZ', 5596, 11, 7.279),
                      c('PA', 5543, 20, 12.802),
                      c('MI', 5545, 16, 9.987),
                      c('GA', 5604, 16, 10.617),
                      c('FL', 5544, 29, 21.478),
                      c('MO', 6581, 10, 6.137),
                      c('TX', 5798, 38, 28.996),
                      c('NC', 5599, 15, 10.488),
                      c('IA', 5603, 6, 3.155),
                      c('NH', 5598, 4, 1.360),
                      c('OH', 5600, 18, 11.689),
                      c('NM', 6573, 5, 2.097),
                      c('MN', 5597, 10, 5.640),
                      c('WI', 5542, 10, 5.822),
                      c('SC', 6609, 9, 5.149),
                      c('UT', 6585, 6, 3.206),
                      c('NV', 5601, 6, 3.080),
                      c('WA', 6598, 12, 7.615),
                      c('LA', 6617, 8, 4.649),
                      c('MS', 6628, 6, 2.976),
                      c('TN', 6586, 11, 6.833),
                      c('MT', 6606, 3, 1.069),
                      c('VA', 5602, 13, 8.536),
                      c('NY', 6612, 29, 19.454),
                      c('WV', 6615, 5, 1.792),
                      c('NE02', 6615, 1, .653),
                      c('OR', 6582, 7, 4.218),
                      c('DC', 6644, 3, .706),
                      c('CA', 6611, 55, 39.512),
                      c('IN', 6572, 11, 6.732),
                      c('CO', 5605, 9, 5.759),
                      c('HI', 6631, 4, 1.416),
                      c('KY', 6592, 8, 4.468),
                      c('AK', 6591, 3, .732),
                      c('VT', 6633, 3, .624),
                      c('OK', 6616, 7, 3.957),
                      c('ID', 6623, 4, 1.787),
                      c('AL', 6625, 9, 4.903),
                      c('RI', 6629, 4, 1.059),
                      c('DE', 6636, 3, .974),
                      c('ND', 6637, 3, .762),
                      c('SD', 6638, 3, .885),
                      c('NE01', 6645, 1, .617),
                      c('NE03', 6646, 1, .603),
                      c('ME01', 6647, 1, .683),
                      c('NJ', 6580, 14, 8.882),
                      c('CT', 6587, 7, 3.565),
                      c('MD', 6593, 10, 6.046),
                      c('MA', 6596, 11, 6.950),
                      c('AR', 6597, 6, 3.018),
                      c('IL', 6613, 20, 12.672),
                      c('KS', 6627, 6, 2.913),
                      c('WY', 6632, 3, .579),
                      c('ME02', 6190, 1, .653)))

# columns to numeric; rename columns
cols.num <- c("X2", "X3", "X4")
d[cols.num] <- sapply(d[cols.num], as.numeric)
colnames(d) <- c("State", "ID", "EVs", "Population")

# Functions to get Dem and Repub win probabilities from market data
getDemProb <- function(betID) {
  if (markets[markets$id %in% betID, "contract_name"][1]=="Democratic") {
    d$Democratic <- markets[markets$id %in% betID, "lastTradePrice"][1]
  } else if (markets[markets$id %in% betID, "contract_name"][2]=="Democratic") {
    d$Democratic <- markets[markets$id %in% betID, "lastTradePrice"][2]
  } else {
    d$Democratic <- 0
  }
}

getRepProb <- function(betID) {
  if (markets[markets$id %in% betID, "contract_name"][1]=="Republican") {
    d$Republican <- markets[markets$id %in% betID, "lastTradePrice"][1]
  } else if (markets[markets$id %in% betID, "contract_name"][2]=="Republican") {
    d$Republican <- markets[markets$id %in% betID, "lastTradePrice"][2]
  } else {
    d$Republican <- 0
  }
}

# Set range of polling values
x <- seq(from=.01, to=.99, length.out=1e5)

# Set standard deviation to generate polling
# .08 gets close to results at
# https://www.nytimes.com/elections/2016/results/president
probSD <- .08

# Calculate implied polling from PredictIt win probs
getImpliedDemProb <- function(dWinProb) {
  dProbs <- pnorm(q=.5, mean=x, sd=probSD, lower.tail=F)
  dInd <- which.min((dWinProb-dProbs)^2)
  return(x[dInd])
}

d$demProb <- sapply(d$ID, getDemProb)
d$repProb <- sapply(d$ID, getRepProb)
d$demProbScaled <- d$demProb/(d$demProb+d$repProb)
d$repProbScaled <- 1-d$demProbScaled
d$impliedDemPoll <- sapply(d$demProbScaled, getImpliedDemProb)

nTrials <- 1e5
zs <- matrix(rnorm(n=nrow(d)*nTrials, mean=0, sd=1),
             nrow(d), nTrials)

# Polling standard deviation/margin of error
pollingSD <- .08
dvotes <- d$Population*(d$impliedDemPoll+zs*pollingSD)
popVote <- colSums(dvotes)/sum(d$Population)
results <- colSums(d$EVs*(d$impliedDemPoll+zs*pollingSD>.5))

# Maine and Nebraska population
popME <- sum(d[d$State %in% c("ME01","ME02"), "Population"])
popNE <- sum(d[d$State %in% c("NE01","NE02","NE03"), "Population"])

# Assign each of Maine's and Nebraska's remaining two
# electoral votes to candidate with highest vote state-wide
results <- results + 2*(colSums(dvotes[which(d$State %in% c("ME01", "ME02")), ])>(0.5*popME))

results <- results + 2*(colSums(dvotes[which(d$State %in% c("NE01","NE02","NE03")), ])>(0.5*popNE))

# Plot results
hist(results, col="gray", border=F,
     main=paste("Expected Democratic electoral votes given", nTrials, "trials\n",
                "Democratic win probability:", round(sum(results>=270)/nTrials,3),"\n",
                "Median Democratic electoral votes:", median(results)),
     xlab=paste("Democratic electoral votes\n",
                "(run", format(Sys.time(), "%a %b %d %Y %X"), "UTC)"),
     ylab="Frequency",
     breaks=25)

abline(v=c(270), col=c("red"),
       lty=c(2), lwd=c(4))




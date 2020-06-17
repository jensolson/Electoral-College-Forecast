library(rpredictit)
library(data.table)
# library(dplyr)
library(ggplot2)
set.seed(2)

# Uncomment to download current PredictIt market data
markets <- data.table(all_markets())

# Store variables, PredictIt IDs,
# electoral votes, population, and turnout rates
# Turnout numbers from http://www.electproject.org/2016g
d <- data.table(rbind(c('AZ', 5596, 11, 7.279, .489),
                      c('PA', 5543, 20, 12.802, .61),
                      c('MI', 5545, 16, 9.987, .62),
                      c('GA', 5604, 16, 10.617, .526),
                      c('FL', 5544, 29, 21.478, .569),
                      c('MO', 6581, 10, 6.137, .596),
                      c('TX', 5798, 38, 28.996, .434),
                      c('NC', 5599, 15, 10.488, .602),
                      c('IA', 5603, 6, 3.155, .651),
                      c('NH', 5598, 4, 1.360, .691),
                      c('OH', 5600, 18, 11.689, .61),
                      c('NM', 6573, 5, 2.097, .502),
                      c('MN', 5597, 10, 5.640, .694),
                      c('WI', 5542, 10, 5.822, .662),
                      c('SC', 6609, 9, 5.149, .542),
                      c('UT', 6585, 6, 3.206, .528),
                      c('NV', 5601, 6, 3.080, .494),
                      c('WA', 6598, 12, 7.615, .583),
                      c('LA', 6617, 8, 4.649, .568),
                      c('MS', 6628, 6, 2.976, .533),
                      c('TN', 6586, 11, 6.833, .486),
                      c('MT', 6606, 3, 1.069, .608),
                      c('VA', 5602, 13, 8.536, .608),
                      c('NY', 6612, 29, 19.454, .496),
                      c('WV', 6615, 5, 1.792, .492),
                      c('NE02', 6615, 1, .653, .588),
                      c('OR', 6582, 7, 4.218, .617),
                      c('DC', 6644, 3, .706, .554),
                      c('CA', 6611, 55, 39.512, .47),
                      c('IN', 6572, 11, 6.732, .54),
                      c('CO', 5605, 9, 5.759, .646),
                      c('HI', 6631, 4, 1.416, .383),
                      c('KY', 6592, 8, 4.468, .561),
                      c('AK', 6591, 3, .732, .574),
                      c('VT', 6633, 3, .624, .623),
                      c('OK', 6616, 7, 3.957, .49),
                      c('ID', 6623, 4, 1.787, .55),
                      c('AL', 6625, 9, 4.903, .563),
                      c('RI', 6629, 4, 1.059, .547),
                      c('DE', 6636, 3, .974, .592),
                      c('ND', 6637, 3, .762, .593),
                      c('SD', 6638, 3, .885, .566),
                      c('NE01', 6645, 1, .617, .588),
                      c('NE03', 6646, 1, .603, .588),
                      c('ME01', 6647, 1, .683, .694),
                      c('NJ', 6580, 14, 8.882, .557),
                      c('CT', 6587, 7, 3.565, .583),
                      c('MD', 6593, 10, 6.046, .595),
                      c('MA', 6596, 11, 6.950, .611),
                      c('AR', 6597, 6, 3.018, .494),
                      c('IL', 6613, 20, 12.672, .567),
                      c('KS', 6627, 6, 2.913, .54),
                      c('WY', 6632, 3, .579, .573),
                      c('ME02', 6190, 1, .653, .694)))

# rename columns; columns to numeric
setnames(d, c("State", "ID", "EVs", "Population", "Turnout"))
numericCols <- c("ID", "EVs", "Population", "Turnout")
d[, (numericCols) := lapply(.SD, as.numeric), .SDcols=numericCols]

# Retrieve summary data table with only markets specified in d
# Merge Dem and Repub win probabilities into d
m <- markets[id %in% d$ID & contract_name %in% c("Democratic", "Republican"),
             .(id, contract_name, lastTradePrice)]

m <- dcast.data.table(m, id ~ contract_name, value.var="lastTradePrice")
setnames(m, c('ID', 'dProb', 'rProb'))
d <- merge(d, m, by='ID')

d[, dProbScaled := dProb/(dProb+rProb)]
d[, rProbScaled := 1-dProbScaled]

# Set range of polling values
x <- seq(from=.01, to=.99, length.out=1e5)

# Set standard deviation to generate polling
# .08 gets close to results at
# https://www.nytimes.com/elections/2016/results/president
probSD <- .08

# Calculate implied polling from PredictIt win probs
getImpliedDemPoll <- function(demWinProb) {
  demPolls <- pnorm(q=.5, mean=x, sd=probSD, lower.tail=F)
  dInd <- which.min((demWinProb-demPolls)^2)
  return(x[dInd])
}

# Calculate implied Dem polling from prediction market probabilities
d[, impliedDemPoll := sapply(dProbScaled, getImpliedDemPoll)]

nTrials <- 5*1e3
zs <- matrix(rnorm(n=nrow(d)*nTrials, mean=0, sd=1),
             nrow(d), nTrials)

# Polling standard deviation/margin of error
pollingSD <- .08

# Run hypothetical elections
demVotePct <- d[, impliedDemPoll]+zs*pollingSD
demWin <- demVotePct > 0.5
demVotes <- (d$Population*d$Turnout) %*% demVotePct
demPopVotePct <- t(demVotes/(d$Population %*% d$Turnout)[1])
demEVs <- d$EVs %*% demWin

# Assign state-level electoral votes to winners of Maine and Nebraska popular votes
ME.indices <- d[State %in% c("ME01","ME02"), which=T]
NE.indices <- d[State %in% c("NE01","NE02","NE03"), which=T]

turnoutME <- (d[ME.indices, Population] %*% d[ME.indices, Turnout])[1]
turnoutNE <- (d[NE.indices, Population] %*% d[NE.indices, Turnout])[1]

demVotesME <- (d[ME.indices, Population]*d[ME.indices, Turnout]) %*%
  demVotePct[ME.indices, ]

demVotesNE <- (d[NE.indices, Population]*d[NE.indices, Turnout]) %*%
  demVotePct[NE.indices, ]

# Final EV count
demEVs <- t(demEVs + 2*(demVotesME/turnoutME > 0.5)
                   + 2*(demVotesNE/turnoutNE > 0.5))

# Plot 1: Histogram of Democratic EV outcomes
hist(demEVs, col="gray", border=F,
     main=paste("Expected Democratic electoral votes",
                "given", nTrials, "trials\n",
                "Democratic win probability:",
                round(sum(demEVs>=270)/nTrials,3),"\n",
                "Median Democratic electoral vote expectation:",
                median(demEVs)),
     xlab=paste("Democratic electoral votes\n",
                "(run", format(Sys.time(), "%a %b %d %Y %X"), "UTC)"),
     ylab="Frequency",
     breaks=25)

abline(v=c(270), col=c("red"),
       lty=c(2), lwd=c(4))

# Plot 2: Scatterplot of national popular vote vs. EVs
res <- data.table(EVs=demEVs, popV=demPopVotePct)
setnames(res, c('EVs', 'popV'))
res[popV>.50 & EVs>=270, "Q"] <- 1
res[popV<.50 & EVs>=270, "Q"] <- 2
res[popV<.50 & EVs<270, "Q"] <- 3
res[popV>.50 & EVs<270, "Q"] <- 4

plotColors <- c("royalblue4", "firebrick", "red2", "dodgerblue2")

g <- ggplot(res, mapping=aes(x=popV, y=EVs, group=Q))+
  geom_point(aes(color=factor(Q)), alpha=.5) +
  geom_vline(xintercept=.50) +
  geom_hline(yintercept=270) +
  ggtitle(paste(nTrials, "simulated outcomes of the 2020 US Presidential election"),
          subtitle=paste("Dem probability of victory:", round(nrow(res[Q %in% c(1,2)])/nTrials, 3),
                         "\nDems", round(nrow(res[Q==4])/nrow(res[Q==2]), 1),
                         "times as likely to win popular vote and lose Electoral College than Repubs")) +
  guides(color=F) +
  xlab("Democratic share of popular vote") +
  ylab("Democratic votes in Electoral College") +
  scale_color_manual(breaks=c(1, 2, 3, 4),
                     values=plotColors) +
  scale_x_continuous(labels=scales::percent, breaks=seq(from=.40,to=.70,by=.02)) +
  scale_y_continuous(breaks=c(150,200,250,270,300,350,400,450)) +
  annotate(geom="text", x=.52, y=max(res[,EVs])-15,
           label="Dems win popular vote\n & Electoral College",
           color=plotColors[1]) +
  annotate(geom="text", x=min(res[,popV])+.01, y=325,
           label="Repubs win\n popular vote but lose\n Electoral College",
           color=plotColors[2]) +
  annotate(geom="text", x=min(res[,popV])+.01, y=210,
           label="Repubs win\n popular vote &\n Electoral College",
           color=plotColors[3]) +
  annotate(geom="text", x=.535, y=min(res[,EVs])+25,
           label="Dems win popular vote\n but lose Electoral College",
           color=plotColors[4]) +
  theme_minimal()

print(g)


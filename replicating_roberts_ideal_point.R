##Replicating Roberts example
# https://rpubs.com/RobertMylesMc/Bayesian-IRT-ideal-points-with-Stan-in-R

library(readr)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

data <- read_csv("https://raw.githubusercontent.com/RobertMyles/Bayesian-Ideal-Point-IRT-Models/master/Senate_Example.csv")
unique(data$Vote)
data$Vote[data$Vote=="S"] <- 1
data$Vote[data$Vote=="N"] <- 0
data$Vote[data$Vote  %in% c(NA,"O","A")] <- NA
data$Vote <- as.numeric(data$Vote)
data$FullID <- paste(data$SenatorUpper, data$Party, sep=":")

head(data)

NameID <- unique(data$FullID)
J <- length(unique(NameID))
M <- length(unique(data$VoteNumber))
grep("JOSE AGRIPINO:PFL", NameID)    # 34th place; right-wing; positive ideal point
grep("EDUARDO SUPLICY:PT", NameID)   # 12th; left-wing; negative
NameID <- NameID[c(34, 12, 1:11, 13:33, 35:J)]
y <- matrix(NA,J,M)
Rows <- match(data$FullID, NameID)
Cols <- unique(data$VoteNumber)
Columns <- match(data$VoteNumber, Cols)

for(i in 1:dim(data)[1]){
  y[Rows[i],Columns[i]] <- data$Vote[i]
}

dimnames(y) <- list(unique(NameID), unique(data$VoteNumber))

ldata <- data.frame(FullID=unique(NameID), 
                    Party=data$Party[match(unique(NameID), data$FullID)], 
                    GovCoalition=data$GovCoalition[match(unique(NameID), data$FullID)],
                    Name=data$SenatorUpper[match(unique(NameID), data$FullID)], 
                    State=data$State[match(unique(NameID), data$FullID)], 
                    row.names=NULL, stringsAsFactors=FALSE)

vdata <- data.frame(VoteNumber=unique(data$VoteNumber), 
                    VoteType=data$VoteType[match(unique(data$VoteNumber), data$VoteNumber)],
                    SenNumber=data$SenNumber[match(unique(data$VoteNumber), data$VoteNumber)],
                    Origin=data$Origin[match(unique(data$VoteNumber), data$VoteNumber)],
                    Contentious=data$Contentious[match(unique(data$VoteNumber), 
                                                       data$VoteNumber)], 
                    PercentYes=data$PercentYes[match(unique(data$VoteNumber), data$VoteNumber)],
                    IndGov=data$IndGov[match(unique(data$VoteNumber), data$VoteNumber)],
                    Content=data$Content[match(unique(data$VoteNumber), data$VoteNumber)],
                    Round=data$Round[match(unique(data$VoteNumber), data$VoteNumber)],
                    stringsAsFactors=F)

N <- length(y)
j <- rep(1:J, times=M)
m <- rep(1:M, each=J)

miss <- which(is.na(y))
N <- N - length(miss)
j <- j[-miss]
m <- m[-miss]
y <- y[-miss]

ldata$ThetaStart <- rnorm(J, 0, 1)
ldata$ThetaStart[ldata$Party=="PFL" | ldata$Party=="PTB" | ldata$Party=="PSDB" | ldata$Party=="PPB"] <- 2
ldata$ThetaStart[ldata$Party=="PT" | ldata$Party=="PSOL" | ldata$Party=="PCdoB"] <- -2
ThetaStart <- ldata$ThetaStart

initF <- function() {
  list(theta=ThetaStart, beta=rnorm(M, 0, 2), alpha=rnorm(M, 0, 2))
}

###  Model
stan.code <- "
data {
int<lower=1> J; //Senators
int<lower=1> M; //Proposals
int<lower=1> N; //no. of observations
int<lower=1, upper=J> j[N]; //Senator for observation n
int<lower=1, upper=M> m[N]; //Proposal for observation n
int<lower=0, upper=1> y[N]; //Vote of observation n
}
parameters {
real alpha[M];
real beta[M];
real theta[J];
}
model {
alpha ~ normal(0,5); 
beta ~ normal(0,5); 
theta ~ normal(0,1); 
theta[1] ~ normal(1, .01);
theta[2] ~ normal(-1, .01);  
for (n in 1:N)
y[n] ~ bernoulli_logit(theta[j[n]] * beta[m[n]] - alpha[m[n]]);
}"

stan.data <- list(J=J, M=M, N=N, j=j, m=m, y=y, ThetaStart=ThetaStart)

stan.fit <- stan(model_code=stan.code, data=stan.data, iter=1000, warmup=500, chains=4, thin=5, init=initF, verbose=TRUE, cores=4, seed=1234)

stan_rhat(stan.fit, bins=60)

MS <- As.mcmc.list(stan.fit)
sMS <- summary(MS)

Theta <- sMS$statistics[grep("theta", row.names(sMS$statistics)),1]
ThetaQ <- sMS$quantiles[grep("theta", row.names(sMS$statistics)),c(1,5)]
Theta <- as.data.frame(cbind(Theta, ThetaQ))
rm(ThetaQ)
Theta$FullID <- ldata$FullID
row.names(Theta) <- NULL
colnames(Theta)[1:3] <- c("Mean", "Lower", "Upper")
Theta <- merge(Theta, ldata, by="FullID")
Theta <- Theta[order(Theta$Mean),]
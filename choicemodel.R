### CBC conjoint #############################################################
#
###########################################################################

# attributes

attrib <- list(type = c("Apartment","Condominium","Executive Condominium"), 
               rent = c("1000", "2000", "3000", "4000", "5000","6000", "7000", "8000", "9000", "10000"), 
               space = c("100", "200", "300", "400", "500","600", "700", "800", "900", "1000", "2000"),
               bed = c("0", "1", "2","3", "4"), 
               bath = c("1", "2", "3"),
               region = c("Central Region", "East Region", "North Region", "North-East Region", "West Region"),
               mrt = c("1", "3", "5", "7", "9"),
               bus = c("1", "3", "5", "7", "9"),
               school = c("1", "3", "5", "7", "9"),
               work = c("1", "3", "5", "7", "9") )

attrib

coef.names <- NULL
for (a in seq_along(attrib)) {
  coef.names <- c(coef.names, 
                  paste(names(attrib)[a], 
                        attrib[[a]][-1], sep=""))
}
coef.names
#mu <- c(0.5, 1.0,  -1, -1, 0.5, -1, -2, -1, -2,-1, -1, 0.5, -1, -2, -1, -2, -1, -1, 0.5, -1, -2, -1, -2, -1,-1, -1, 0.5, -1, -2, -1, -2, -1, -1, 0.5, -1, -2, -1, -2,-1, -1, 0.5, -1, -2)
mu <- rnorm(47, 0, 1)
length(mu)
names(mu) <- coef.names
mu
#Sigma <- diag(c( 0.2, 0.3, 0.3,0.2, 0.1, 1, 0.2, 0.3,0.3, 1, 0.1, 0.3, 1, 0.2, 0.3,0.3, 1, 0.1, 0.3, 1, 0.2, 0.3, 0.3, 1, 0.1, 0.3, 1, 0.2, 0.3, 0.3, 1, 0.1, 0.3, 1, 0.2, 0.3,0.3, 1, 0.1, 0.3, 1, 0.2, 0.3))
Sigma <- diag(runif(47, 0,1))
dim(Sigma)
dimnames(Sigma) <- list(coef.names, coef.names)
dimnames(Sigma) 
set.seed(33040)
resp.id <- 1:100 # respondent ids
rental <- sample(c("yes", "no"), size=length(resp.id), replace=TRUE, 
                 prob=c(0.3, 0.7))
rental 
library(MASS)
coefs <- mvrnorm(length(resp.id), mu=mu, Sigma=Sigma)
colnames(coefs) <- coef.names
colnames(coefs) 

head(cbind(rental, coefs))
by(coefs, rental, colMeans)
nques <- 15
nalt <- 3

profiles <- expand.grid(attrib)
nrow(profiles)
head(profiles)
profiles.coded <- model.matrix(~ type + rent + space + bed + bath + region + mrt + bus + school + work, data=profiles)[,-1]
head(profiles.coded)


cbc.df <- data.frame(NULL)
cbc.df <- data.frame(NULL)
for (i in seq_along(resp.id)) {
  profiles.i <- sample(1:nrow(profiles), size=nques*nalt)
  utility <- profiles.coded[profiles.i,] %*% coefs[i,] 
  wide.util <- matrix(data=utility, ncol=nalt, byrow=TRUE)
  probs <- exp(wide.util) / rowSums(exp(wide.util))
  choice <- apply(probs, 1, function(x) sample(1:nalt, size=1, prob=x))
  choice <- rep(choice, each=nalt)==rep(1:nalt, nques)
  conjoint.i <- data.frame(resp.id=rep(i, nques), 
                           ques = rep(1:nques, each=nalt), 
                           alt = rep(1:nalt, nques), 
                           rental = rep(rental[i], nques), 
                           profiles[profiles.i,], 
                           choice = as.numeric(choice))
  cbc.df <- rbind(cbc.df, conjoint.i)
}  

## END data simulation

#cbc.df = cbc.df[,-dim(cbc.df)[2]]
head(cbc.df,21)
# Choice data descriptives (94 respondents,17 questions, 3 alternatives)
#cbc.df<- read.csv("D:/conjoint_data.csv")

cbc.df
dim(cbc.df) 
head(cbc.df)
summary(cbc.df)

#cbc.df$region[cbc.df$region==1]<- "central" 
#cbc.df$region[cbc.df$region==2]<- "central" 
#cbc.df$region[cbc.df$region==3]<- "central" 
#cbc.df$region[cbc.df$region==4]<- "central" 
#cbc.df$region[cbc.df$region==5]<- "central" 
#cbc.df$region[cbc.df$region==6]<- "central" 
#cbc.df$region[cbc.df$region==7]<- "central" 
#cbc.df$region[cbc.df$region==8]<- "central" 
#cbc.df$region[cbc.df$region==9]<- "central" 
#cbc.df$region[cbc.df$region==10]<- "central" 
#cbc.df$region[cbc.df$region==11]<- "central" 
#cbc.df$region[cbc.df$region==12]<- "central" 
#cbc.df$region[cbc.df$region==13]<- "central" 
#cbc.df$region[cbc.df$region==14]<- "central" 
#cbc.df$region[cbc.df$region==15]<- "central" 
#cbc.df$region[cbc.df$region==16]<- "east" 
#cbc.df$region[cbc.df$region==17]<- "east" 
#cbc.df$region[cbc.df$region==18]<- "east" 
#cbc.df$region[cbc.df$region==19]<- "north east" 
#cbc.df$region[cbc.df$region==20]<- "north east" 
#cbc.df$region[cbc.df$region==21]<- "central" 
#cbc.df$region[cbc.df$region==22]<- "west" 
#cbc.df$region[cbc.df$region==23]<- "north" 
#cbc.df$region[cbc.df$region==24]<- "west" 
#cbc.df$region[cbc.df$region==25]<- "north"  
#cbc.df$region[cbc.df$region==26]<- "north"  
#cbc.df$region[cbc.df$region==27]<- "north" 
#cbc.df$region[cbc.df$region==28]<- "north east" 




#xtabs(choice ~ type, data=cbc.df)
#xtabs(choice ~ rent, data=cbc.df)
#xtabs(choice ~ space, data=cbc.df)
#xtabs(choice ~ bed, data=cbc.df)
#xtabs(choice ~ bath, data=cbc.df)
#xtabs(choice ~ region, data=cbc.df)
#xtabs(choice ~ mrt, data=cbc.df)
#xtabs(choice ~ bus, data=cbc.df)
#xtabs(choice ~ school, data=cbc.df)
#xtabs(choice ~ work, data=cbc.df)

# Fitting a choice model with mlogit
#install.packages("mlogit")
library(mlogit)
cbc.mlogit <- mlogit.data(data=cbc.df, choice="choice", shape="long", 
                          varying=3:5, alt.levels=paste("pos",1:3), 
                          id.var="resp.id")


m<- mlogit(choice ~  
          + as.character(type)
          + as.character(rent)
          + as.character(space)
          + bed
          + bath
          + region
          + mrt
          + bus
          + school
          + work ,
          data = cbc.mlogit)
summary(m)
m$coefficients 
write.csv(m$coefficients, "D:/conjointcoeff.csv")
conjointcoeff <- read.csv("conjointcoeff.csv")
conjointcoeff <- conjointcoeff[,-1]

conjointcoeff <- c(0,conjointcoeff[1:3],
                   0,conjointcoeff[4:10],
                   0,conjointcoeff[11:21],
                   0,conjointcoeff[22]*1/4,-conjointcoeff[22]*2/4,-conjointcoeff[22]*3/4,conjointcoeff[22]*4/4,
                   0,conjointcoeff[23]*1/3,conjointcoeff[23]*2/3,conjointcoeff[23]*3/3,
                   0,conjointcoeff[24:27],
                   0,conjointcoeff[28]*1/4,conjointcoeff[28]*2/4,conjointcoeff[28]*3/4,conjointcoeff[28]*4/4,
                   0,conjointcoeff[29]*1/4,conjointcoeff[29]*2/4,conjointcoeff[29]*3/4,conjointcoeff[29]*4/4,
                   0,conjointcoeff[30]*1/4,conjointcoeff[30]*2/4,conjointcoeff[30]*3/4,conjointcoeff[30]*4/4,
                   0,conjointcoeff[31]*1/4,conjointcoeff[31]*2/4,conjointcoeff[31]*3/4,conjointcoeff[31]*4/4
)
type = c("Apartment","Condominium","Executive Condominium")
rent = c("SGD1000", "SGD2000", "SGD3000", "SGD4000", "SGD5000","SGD6000", "SGD7000", "SGD8000", "SGD9000", "SGD10000")
space = c("100sqft", "200sqft", "300sqft", "400sqft", "500sqft","600sqft", "700sqft", "800sqft", "900sqft", "1000sqft", "2000sqft")
bed = c("0(studio)", "1bed", "2beds","3beds", "4beds")
bath = c("1bath", "2baths", "3baths", "4baths")
region = c("Central Region", "East Region", "North Region", "North-East Region", "West Region")
mrt = c("mrt_1min", "mrt_mrt3", "mrt_mrt5", "mrt_mrt7", "mrt_mrt9")
bus = c("bus_1min", "bus_3min", "bus_5min", "bus_7min", "bus_9min")
school =c("school_1min", "school_3min", "school_5min", "school_7min", "school_9min")
work = c("work_1min", "work_3min", "work_5min", "work_7min", "work_9min")

names(conjointcoeff) =c(type, rent, space, bed, bath, region, mrt, bus, school, work)
conjointcoeff
length(conjointcoeff)

# Importance weights
importance = c(
  range(conjointcoeff[type])[2]-range(conjointcoeff[type])[1],
  range(conjointcoeff[rent])[2]-range(conjointcoeff[rent])[1],
  range(conjointcoeff[space])[2]-range(conjointcoeff[space])[1],
  range(conjointcoeff[bed])[2]-range(conjointcoeff[bed])[1],
  range(conjointcoeff[bath])[2]-range(conjointcoeff[bath])[1],
  range(conjointcoeff[region])[2]-range(conjointcoeff[region])[1],
  range(conjointcoeff[mrt])[2]-range(conjointcoeff[mrt])[1],
  range(conjointcoeff[bus])[2]-range(conjointcoeff[bus])[1],
  range(conjointcoeff[school])[2]-range(conjointcoeff[school])[1],
  range(conjointcoeff[work])[2]-range(conjointcoeff[work])[1]
)

names(importance) <- c("type", "rent", "space", "bed", "bath", "region", "mrt", "bus", "school", "work")
importance 


### Maxdiff ############################################################
#
######################################################################

## items
#1 security 
#2 fridge
#3 aircon
#4 washer
#5 dryer
#6 pantry
#7 carpark
#8 mall
#9 fitness
#10 gardenview
#11 riverview
#12 oceanview


maxdiff <-  read.csv("maxdiff_data.csv")

library(mlogit)
maxdiff.mlogit <-mlogit(formula = Choice ~  Item2 + Item3 + Item4 + Item5 + Item6 + Item7 + Item8 + Item9 + Item10 + Item11 + Item12 | 0, data = maxdiff,     alt.levels = 1:4, shape = "long", method = "nr",     print.level = 0)
maxdiff.mlogit$coefficients 
write.csv(maxdiff.mlogit$coefficients, "maxdiffcoeff.csv")
maxdiffcoeff <- read.csv("D:/maxdiffcoeff.csv")
maxdiffcoeff <- maxdiffcoeff[,-1]
maxdiffcoeff <- c(0,maxdiffcoeff)
names(maxdiffcoeff) <- c(
  "security", 
  "fridge",
  "aircon",
  "washer",
  "dryer",
  "pantry",
  "carpark",
  "mall",
  "fitness",
  "gardenview",
  "riverview",
  "oceanview")  
maxdiffcoeff

ratio = .5
weight = exp(c(importance*ratio, maxdiffcoeff*(1-ratio)))/sum(exp(c(importance*ratio, maxdiffcoeff*(1-ratio))))
weight
sum(weight)


###  sunburst graph #####################################################
#
######################################################################

V1=c("structure-type",
     "structure-rent",
     "structure-space",
     "structure-bed",
     "structure-bath",
     "location-region",
     "location-mrt",
     "location-bus",
     "location-school",
     "location-work",
     "amenity-security",
     "furnish-fridge",
     "furnish-aircon",
     "furnish-washer",
     "furnish-dryer",
     "furnish-pantry",
     "amenity-carpark",
     "location-mall",
     "amenity-fitness",
     "view-gardenview",
     "view-riverview",
     "view-oceanview"
)

V2 = weight

sequences <- cbind(V1,as.numeric(as.character(V2)))
sequences <- as.data.frame(sequences)

library(sunburstR)

sunburst(
  sequences,
  explanation = htmlwidgets::JS(
    '
    function(d){
    var explanation = [];
    var parent = d.parent;
    if(d.depth > 1){
    while(parent){
    if(parent.parent){
    explanation.push(
    (100*d.value/parent.value).toPrecision(3) + "%"
    );
    }
    parent = parent.parent;
    };
    }
    explanation.push((100*d.value/this).toPrecision(3) + "%");
    return explanation.join("<br/>");
    }'
  )
  )



##############################################################
#
# ------- Hierarchical Bayes multinomial logit -----------
##############################################################
choice <- rep(0, nrow(cbc.df))
choice[cbc.df[,"alt"]==1] <- cbc.df[cbc.df[,"choice"]==1,"alt"]
head(choice)

cbc.coded <- model.matrix(~ type + rent + space + bed + bath + region + mrt + bus + school + work, data = cbc.df)
cbc.coded <- cbc.coded[, -1] # remove the intercept

choicemodelr.data <- cbind(cbc.df[, 1:3], cbc.coded, choice)
head(choicemodelr.data)

rental <- cbc.df$rental[cbc.df$ques==1 & cbc.df$alt==1]=="yes"
rental <- as.numeric(rental)
choicemodelr.demos <- as.matrix(rental, nrow=length(rental))
str(choicemodelr.demos)
x11()
library(ChoiceModelR)
hb.post <- choicemodelr(data=choicemodelr.data, xcoding=rep(1, 47), 
                        demos=choicemodelr.demos, 
                        mcmc=list(R=20000, use=10000),
                        options=list(save=TRUE))
names(hb.post)
# Model parameters
hb.post$compdraw[[567]]$mu
hb.post$deltadraw[567,]

hb.post$compdraw[[567]]$rooti
crossprod(hb.post$compdraw[[567]]$rooti)

# Individual-level betas
dim(hb.post$betadraw[,,567])
head(hb.post$betadraw[,,567])
str(hb.post$betadraw)

beta.post.mean <- apply(hb.post$betadraw, 1:2, mean)
head(beta.post.mean)

beta.post.q05 <- apply(hb.post$betadraw, 1:2, quantile, probs=c(0.05))
beta.post.q95 <- apply(hb.post$betadraw, 1:2, quantile, probs=c(0.95))
rbind(q05=beta.post.q05[1,], mean=beta.post.mean[1,], q95=beta.post.q95[1,])




# Prediction using individual-level draws
predict.hb.mnl <- function(betadraws, data) {
  # predicting shares from a hierarchical multinomial logit model 
  data.model <- model.matrix(~ type + rent + space + bed + bath + region + mrt + bus + school + work, data = data)
  data.model <- data.model[,-1] # remove the intercept
  nresp <- dim(betadraws)[1]
  ndraws <- dim(hb.post$betadraw)[3]
  shares <- array(dim=c(nresp, nrow(data), ndraws))
  for (d in 1:ndraws) {
    for (i in 1:nresp) {
      utility <- data.model%*%betadraws[i,,d]
      shares[i,,d] = exp(utility)/sum(exp(utility))
    }
  }
  shares.agg <- apply(shares, 2:3, mean)
  cbind(share=apply(shares.agg, 1, mean), 
        pct=t(apply(shares.agg, 1, quantile, probs=c(0.05, 0.95))), 
        data)
}

(new.data <- expand.grid(attrib)[c(8, 7,1, 3, 410, 25, 236), ])
predict.hb.mnl(hb.post$betadraw, new.data)

# Prediction using individual-level partworhs
predict.hb.partworhs <- function(betadraws, data) {
  # predicting individual-level partworhs from a hierarchical multinomial logit model 
  data.model <- model.matrix(~ type + rent + space + bed + bath + region + mrt + bus + school + work, data = data)
  data.model <- data.model[,-1] # remove the intercept
  nresp <- dim(betadraws)[1]
  ndraws <- dim(hb.post$betadraw)[3]
  partworths <- array(dim=c(nresp, nrow(data), ndraws))
  for (d in 1:ndraws) {
    for (i in 1:nresp) {
      utility <- data.model%*%betadraws[i,,d]
      partworths[i,,d] = utility
    }
  }
  partworths.agg <- apply(partworths, 2:3, mean)
  cbind(partworths=apply(partworths.agg, 1, mean), 
        pct=t(apply(partworths.agg, 1, quantile, probs=c(0.05, 0.95))), 
        data)
}


(new.data <- expand.grid(attrib)[c(8, 1, 3, 410, 25, 236), ])
predict.hb.partworhs(hb.post$betadraw, new.data)

#######################################################################
#               Clustering algorithm                                  #     
#######################################################################

#install and load packages
library(dplyr)
library(stringr)
library(cluster)
library(fpc)
library(factoextra)
set.seed(1234)

#import raw data
rentdata = read.csv("rent.csv")
dim(rentdata)
rentdata = rentdata[, -c(1, 19, 38:44)]  # remove resp.id, aversion, CQ7_1~CQ7_7 
rent
x11()
fviz_nbclust(rentdata, pam, method = "silhouette") 

# PAM(Partitioning Around Medroid)
pam.res <- pam(rentdata, k= 5)
print(pam.res)
plot(pam.res)
pam.res$clusinfo
#     size max_diss  av_diss diameter separation
#[1,]   25 15.55635 10.21466 20.07486   7.874008
#[2,]   17 20.12461 13.06892 24.43358   8.366600
#[3,]   24 14.93318 10.34583 19.94994   7.874008
#[4,]   17 18.52026 11.89678 23.97916   9.327379
#[5,]   11 16.15549 11.46634 20.54264  10.677078
pam.res$medoids
#     CQ1 CQ2 CQ3 CQ4 CQ5 CQ6 CQ8 CQ9 CQ10
#[1,]   2   2   2   5   1   3   1   2    1  #Thrifty style
#[2,]   1   5   2   5   4   2   1   3    7  #High-income earner
#[3,]   2   3   2   5   4   5   1   2    2  #Large family 
#[4,]   2   4   2   5   1   4   3   1    4  #mom raising teens
#[5,]   1   3   2   5   1   3   1   3    2  #economic rent
clu <- cbind(rentdata, cluster = pam.res$cluster)
clu$cluster
#[1] 1 2 3 4 3 1 3 2 2 2 1 1 1 3 1 1 3 1 1 4 1 3 1 1 3 5 3 3 4 1
#[31] 1 5 2 4 3 5 3 1 3 5 2 3 3 4 3 1 3 4 3 4 2 5 1 4 2 1 3 3 1 2
#[61] 5 4 2 2 4 1 4 5 4 4 3 1 5 1 1 4 4 3 2 1 5 2 2 3 4 2 4 3 5 2
#[91] 5 2 3 1

x11()
clusplot(rentdata, pam.res$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
plotcluster(rentdata, pam.res$cluster)


rentdata= cbind(rentdata, clu$cluster)  #including cluster membership 

beta.post.mean = cbind(beta.post.mean, clu$cluster) # not working currently

clu$cluster


#######################################################################
#                          recommendatation system                    #     
#######################################################################

property = read.csv("property.csv")
property[1,]


partworth = property 
head(partworth)

maxdiffCoeff = c(0, maxdiffcoeff[1], 
                 0, maxdiffcoeff[2], 
                 0, maxdiffcoeff[3], 
                 0, maxdiffcoeff[4], 
                 0, maxdiffcoeff[5], 
                 0, maxdiffcoeff[6], 
                 0, maxdiffcoeff[7], 
                 0, maxdiffcoeff[8], 
                 0, maxdiffcoeff[9], 
                 0, maxdiffcoeff[10], 
                 0, maxdiffcoeff[11], 
                 0, maxdiffcoeff[12]) 

str(partworth)


partworth = mutate(partworth, Type = ifelse(type !=  "Apartment", ifelse(type != "Condominium",conjointcoeff[3],conjointcoeff[2]),conjointcoeff[1]))


partworth = mutate(partworth, Rent = ifelse(rent < 1500, conjointcoeff[4],
                                     ifelse(rent < 2500, conjointcoeff[5], conjointcoeff[6])))
                                     

partworth = mutate(partworth, Rent = ifelse(rent < 1500, conjointcoeff[4],
                                     ifelse(rent < 2500, conjointcoeff[5], 
                                     ifelse(rent < 3500, conjointcoeff[6],
                                     ifelse(rent < 4500, conjointcoeff[7],        
                                     ifelse(rent < 5500, conjointcoeff[8], 
                                     ifelse(rent < 6500, conjointcoeff[9],    
                                     ifelse(rent < 7500, conjointcoeff[10], 
                                     ifelse(rent < 8500, conjointcoeff[11], 
                                     ifelse(rent < 9500, conjointcoeff[12], conjointcoeff[13]))))))))))     

partworth = mutate(partworth, Space = ifelse(space< 150, conjointcoeff[14],
                                      ifelse(space< 250, conjointcoeff[15], 
                                      ifelse(space < 350, conjointcoeff[16],
                                      ifelse(space < 450, conjointcoeff[17],        
                                      ifelse(space < 550, conjointcoeff[18], 
                                      ifelse(space < 650, conjointcoeff[19],    
                                      ifelse(space < 750, conjointcoeff[20], 
                                      ifelse(space < 850, conjointcoeff[21], 
                                      ifelse(space < 950, conjointcoeff[22], 
                                      ifelse(space < 1500, conjointcoeff[23],conjointcoeff[24]))))))))))) 

partworth = mutate(partworth, Bed = ifelse(bed < 1, conjointcoeff[25],
                                    ifelse(bed < 2, conjointcoeff[26], 
                                    ifelse(bed < 3, conjointcoeff[27], 
                                    ifelse(bed < 4, conjointcoeff[28], conjointcoeff[29]))))) 
                

partworth = mutate(partworth, Bath =ifelse(bath < 1, conjointcoeff[30],
                                    ifelse(bath < 2, conjointcoeff[31], 
                                    ifelse(bath < 3, conjointcoeff[32], 
                                    ifelse(bath < 4, conjointcoeff[33], conjointcoeff[33]))))) 


partworth = mutate(partworth, Region = ifelse(region !=  "Central Region", 
                                       ifelse(region != "East Region" ,
                                       ifelse(region != "North Region" ,
                                       ifelse(region != "North-East Region", conjointcoeff[38], conjointcoeff[37]),conjointcoeff[36]),conjointcoeff[35]),conjointcoeff[34])) 
                                              

partworth = mutate(partworth, Mrt = ifelse(mrt < 2, conjointcoeff[39],
                                    ifelse(mrt < 4, conjointcoeff[40], 
                                    ifelse(mrt < 6, conjointcoeff[41], 
                                    ifelse(mrt < 8, conjointcoeff[42], conjointcoeff[43]))))) 


partworth = mutate(partworth, Bus = ifelse(bus < 2, conjointcoeff[44],
                                    ifelse(bus < 4, conjointcoeff[45], 
                                    ifelse(bus < 6, conjointcoeff[46], 
                                    ifelse(bus < 8, conjointcoeff[47], conjointcoeff[48]))))) 


partworth = mutate(partworth, School = ifelse(school < 2, conjointcoeff[49],
                                       ifelse(school < 4, conjointcoeff[50], 
                                       ifelse(school < 6, conjointcoeff[51], 
                                       ifelse(school < 8, conjointcoeff[52], conjointcoeff[53]))))) 


partworth = mutate(partworth, Work = ifelse(work < 2, conjointcoeff[54],
                                     ifelse(work < 4, conjointcoeff[55], 
                                     ifelse(work < 6, conjointcoeff[56], 
                                     ifelse(work < 8, conjointcoeff[57], conjointcoeff[58]))))) 



partworth = mutate(partworth, Security = ifelse(security == 0,  maxdiffCoeff[1], maxdiffCoeff[2]))


partworth = mutate(partworth, Fridge = ifelse(fridge == 0,  maxdiffCoeff[3], maxdiffCoeff[4]))


partworth = mutate(partworth, Aircon = ifelse(aircon == 0,  maxdiffCoeff[5], maxdiffCoeff[6]))


partworth = mutate(partworth, Washer = ifelse(washer == 0,  maxdiffCoeff[7], maxdiffCoeff[8]))


partworth = mutate(partworth, Dryer = ifelse(dryer == 0,  maxdiffCoeff[9], maxdiffCoeff[10]))


partworth = mutate(partworth, Pantry = ifelse(pantry == 0,  maxdiffCoeff[11], maxdiffCoeff[12]))

partworth = mutate(partworth, Carpark = ifelse(carpark == 0,  maxdiffCoeff[13], maxdiffCoeff[14]))


partworth = mutate(partworth, Mall = ifelse(mall == 0,  maxdiffCoeff[13], maxdiffCoeff[14]))



partworth = mutate(partworth, Mall =  ifelse(mall < 1, maxdiffCoeff[15],
                                      ifelse(mall < 2, maxdiffCoeff[16]*0.1, 
                                      ifelse(mall < 3, maxdiffCoeff[16]*0.2,
                                      ifelse(mall < 4, maxdiffCoeff[16]*0.3,        
                                      ifelse(mall < 5, maxdiffCoeff[16]*0.4, 
                                      ifelse(mall < 6, maxdiffCoeff[16]*0.5,    
                                      ifelse(mall < 7, maxdiffCoeff[16]*0.6, 
                                      ifelse(mall < 8, maxdiffCoeff[16]*0.7,                  
                                      ifelse(mall < 9, maxdiffCoeff[16]*0.8,
                                      maxdiffCoeff[16]*0.9))))))))))


partworth = mutate(partworth, Fitness = ifelse(fitness == 0,  maxdiffCoeff[17], maxdiffCoeff[18]))


partworth = mutate(partworth, Gardenview = ifelse(gardenview == 0,  maxdiffCoeff[19], maxdiffCoeff[20]))


partworth = mutate(partworth, Riverview = ifelse(riverview == 0,  maxdiffCoeff[21], maxdiffCoeff[22]))


partworth = mutate(partworth, Oceanview = ifelse(oceanview == 0,  maxdiffCoeff[23], maxdiffCoeff[24]))


sumutility = partworth[,24:45]

totalsum = apply(sumutility, 1, sum)

partworth =cbind(partworth, totalsum)

sumutility = cbind(sumutility, totalsum)

RNUM = order(sumutility$totalsum, decreasing = TRUE)

partworth[RNUM,c(23,46)]



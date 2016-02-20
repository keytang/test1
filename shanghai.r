setwd("C:/rdata")
load('shanghai.rData')
data=shanghai
data1 = data[data$designated == 1,]

set.seed(12345)
data = data[data$KINDCODE != "BZ",]
data$sample = runif(nrow(data))
data1 = data1[data1$KINDCODE != "BZ",]
data1$sample = runif(nrow(data1))
require(statmod)
formula1 = claim ~ offset(log(offset))   + policy_year  + RUNAREA+ RUNMILES  + MULTI + designated

tweedie_parameter_check = data.frame(parameter = 1+8:13/20,deviance = rep(0,6))
for (i in 1:nrow(tweedie_parameter_check)){
  glm1 = glm(formula1 , tweedie(tweedie_parameter_check$parameter[i],0),data[data$sample < 0.8,],weight = exposure,y=F,model = F)
  tweedie_parameter_check$deviance[i]= glm1$deviance
  cat(tweedie_parameter_check$parameter[i],"\t",glm1$deviance,"\n")
}


summary(glm1)
data$offset2 = predict(glm1,data,type="response")
data1$offset2 = predict(glm1,data1,type="response")


### Step 2 -- PPV

formula2 = claim ~ offset(log(offset2))   + DRIVERAGE + DRIVERSEX + DRIVINGYEARS 
glm2 = glm(formula2 , tweedie(1.65,0),data1[data1$sample < 0.8,],weight = exposure,y=F,model = F)
data1$prediction = predict(glm2,data1,type="response")
data1$prediction = data1$prediction *  sum(data1$claim)/sum(data1$prediction)

data$prediction[data$designated == 1] = predict(glm2,data[data$designated == 1,],type="response") *  sum(data1$claim)/sum(data1$prediction)
data$prediction[data$designated == 0] = data$offset2[data$designated == 0]

data$prediction = data$prediction*sum(data$claim)/sum(data$prediction)

summary(glm2)
save(glm2,file="glm2.rData")


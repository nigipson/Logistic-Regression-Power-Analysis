######
#The code was pulled and amended from this website:
# https://data.library.virginia.edu/simulating-a-logistic-regression-model/


############
#How to do a power analysis for a logistic regression
#Authored by: Nia Gipson
###########

####################################
#                                  #
#         EXPERIMENT 1             #
#                                  #
####################################
#
#####
#First we are going to create a fictional data set based
#on our predicted probabilities so we can run a LR and find
#predicted beta weights and intercept values
#######

options(digits = 4) #I'm just limiting the numbers after the decimal point
library(tidyverse)
#######################
#STEP 1:
#Create Predicted Data
######################

#I have 20 unique conditions in this experiment. So You will see that I repeat
#this code 20 times. For each chunck of code below, I am simply creating a 
#data frame with 50 observations using rbinom() and the arguement "n =". Then I 
#am telling R that there are only two possibilities for each observation 
#-- 0 or 1 -- using the "size =" arguement. I lastly tell R with what 
#probability "1" should appear in the distribution using the "prob = " argument.
#The seq() function separately creates a column containing numbers within the 
#range.The last three lines of code provide a label for each row in this newly 
#create data frame. Critically, in order for this to appear as a dataframe, I 
#wrapped everything in the cbind() function and gave it a label using an 
#assignment operator. 


#Predicted probabilities for react, TA, 0 view condition
rta0 <- cbind(seq(1, 50), 
              rbinom(n = 50, size = 1, prob = 0.05), 
              rep(list("react"), times = 50), #label for condition "react"
              rep(list("TA"), times = 50), #lable for condition "TA"
              rep(list(0), times = 50)) #label for condition "0 views"

#Predicted probabilities for react, TP, 0 view condition
rtp0 <- cbind(seq(51, 100), 
              rbinom(n = 50, size = 1, prob = 0.05), 
              rep(list("react"), times = 50),
              rep(list("TP"), times = 50),
              rep(list(0), times = 50))

#Predicted probabilities for react, TA, 1 view condition
rta1 <- cbind(seq(101, 150), 
              rbinom(n = 50, size = 1, prob = 0.1625), 
              rep(list("react"), times = 50),
              rep(list("TA"), times = 50),
              rep(list(1), times = 50))

#Predicted probabilities for react, TP, 1 view condition
rtp1 <- cbind(seq(151, 200), 
              rbinom(n = 50, size = 1, prob = 0.1625), 
              rep(list("react"), times = 50),
              rep(list("TP"), times = 50),
              rep(list(1), times = 50))

#Predicted probabilities for react, TA, 5 view condition
rta5 <- cbind(seq(201, 250), 
              rbinom(n = 50, size = 1, prob = 0.275), 
              rep(list("react"), times = 50),
              rep(list("TA"), times = 50),
              rep(list(5), times = 50))

#Predicted probabilities for react, TP, 5 view condition
rtp5 <- cbind(seq(251, 300), 
              rbinom(n = 50, size = 1, prob = 0.275), 
              rep(list("react"), times = 50),
              rep(list("TP"), times = 50),
              rep(list(5), times = 50))

#Predicted probabilities for react, TA, 10 view condition
rta10 <- cbind(seq(301, 350), 
               rbinom(n = 50, size = 1, prob = 0.3875), 
               rep(list("react"), times = 50),
               rep(list("TA"), times = 50),
               rep(list(10), times = 50))

#Predicted probabilities for react, TP, 10 view condition
rtp10 <- cbind(seq(351, 400), 
               rbinom(n = 50, size = 1, prob = 0.3875), 
               rep(list("react"), times = 50),
               rep(list("TP"), times = 50),
               rep(list(10), times = 50))

#Predicted probabilities for react, TA, 20 view condition
rta20 <- cbind(seq(401, 450), 
               rbinom(n = 50, size = 1, prob = 0.5), 
               rep(list("react"), times = 50),
               rep(list("TA"), times = 50),
               rep(list(20), times = 50))

#Predicted probabilities for react, TP, 20 view condition
rtp20 <- cbind(seq(451, 500), 
               rbinom(n = 50, size = 1, prob = 0.5), 
               rep(list("react"), times = 50),
               rep(list("TP"), times = 50),
               rep(list(20), times = 50))

#Predicted probabilities for predict, TA, 0 view condition
pta0 <- cbind(seq(501, 550), 
              rbinom(n = 50, size = 1, prob = 0.05), 
              rep(list("predict"), times = 50),
              rep(list("TA"), times = 50),
              rep(list(0), times = 50))

#Predicted probabilities for predict, TP, 0 view condition
ptp0 <- cbind(seq(551, 600), 
              rbinom(n = 50, size = 1, prob = 0.05), 
              rep(list("predict"), times = 50),
              rep(list("TP"), times = 50),
              rep(list(0), times = 50))

#Predicted probabilities for predict, TA, 1 view condition
pta1 <- cbind(seq(601, 650), 
              rbinom(n = 50, size = 1, prob = 0.1125), 
              rep(list("predict"), times = 50),
              rep(list("TA"), times = 50),
              rep(list(1), times = 50))

#Predicted probabilities for predict, TP, 1 view condition
ptp1 <- cbind(seq(651, 700), 
              rbinom(n = 50, size = 1, prob = 0.2125), 
              rep(list("predict"), times = 50),
              rep(list("TP"), times = 50),
              rep(list(1), times = 50))

#Predicted probabilities for predict, TA, 5 view condition
pta5 <- cbind(seq(701, 750), 
              rbinom(n = 50, size = 1, prob = 0.175), 
              rep(list("predict"), times = 50),
              rep(list("TA"), times = 50),
              rep(list(5), times = 50))

#Predicted probabilities for predict, TP, 5 view condition
ptp5 <- cbind(seq(751, 800), 
              rbinom(n = 50, size = 1, prob = 0.375), 
              rep(list("predict"), times = 50),
              rep(list("TP"), times = 50),
              rep(list(5), times = 50))

#Predicted probabilities for predict, TA, 10 view condition
pta10 <- cbind(seq(801, 850), 
               rbinom(n = 50, size = 1, prob = 0.2375), 
               rep(list("predict"), times = 50),
               rep(list("TA"), times = 50),
               rep(list(10), times = 50))

#Predicted probabilities for predict, TP, 10 view condition
ptp10 <- cbind(seq(851, 900), 
               rbinom(n = 50, size = 1, prob = 0.5375), 
               rep(list("predict"), times = 50),
               rep(list("TP"), times = 50),
               rep(list(10), times = 50))

#Predicted probabilities for predict, TA, 20 view condition
pta20 <- cbind(seq(901, 950), 
               rbinom(n = 50, size = 1, prob = 0.3), 
               rep(list("predict"), times = 50),
               rep(list("TA"), times = 50),
               rep(list(20), times = 50))

#Predicted probabilities for predict, TP, 20 view condition
ptp20 <- cbind(seq(951, 1000), 
               rbinom(n = 50, size = 1, prob = 0.7), 
               rep(list("predict"), times = 50),
               rep(list("TP"), times = 50),
               rep(list(20), times = 50))


#Now we want to put all 20 dataframes into one large data frame. Essentially,
#I am stacking each of these dataframes on top of one another using rbind() and
#the data.frame() function to further coerce this obj into a dataframe
predicted_results <- data.frame(rbind(rta0, rtp0, rta1, rtp1, 
                                      rta5, rtp5, rta10, rtp10,
                                      rta20, rtp20, pta0, ptp0, 
                                      pta1, ptp1, pta5, ptp5, 
                                      pta10, ptp10, pta20, ptp20))
 
#rename the columns to give them meaningful labels
colnames(predicted_results) <- c("ID", "chosen", "engage",
                                 "lineup", "views")

#Let's see what we have now:
View(predicted_results)

#the code below helped to coerce R into reading the items within 
#the data frame. Without this you get an error code that the list is
#"unimplemented" which simply means R does not know how to factorize it
#because it's not recognizing the type of list it is. Do this to be able to
#later factorize the variables
predicted_results$ID <- as.numeric(predicted_results$ID)
predicted_results$chosen <- as.numeric(predicted_results$chosen)
predicted_results$views <- as.numeric(predicted_results$views)
predicted_results$engage <- as.character(predicted_results$engage)
predicted_results$lineup <- as.character(predicted_results$lineup)

#factorize variables
predicted_results[,1:5] <- lapply(predicted_results[,1:5], factor) 


############################################
#STEP 2:
#Now it's time to run a logistic regression!
############################################

#Here we are going to run a logistic regression with the predicted data. So, 
#we can get some coefficient estimates to use later in our simulation.

#always check the contrast to make sure you can understand
#the results of the LR. 0 refers to the reference group
contrasts(predicted_results$chosen) 
contrasts(predicted_results$engage)
contrasts(predicted_results$lineup)
contrasts(predicted_results$views)

#use this line of code if you want to change the reference group
contrasts(predicted_results$engage) <- c(1, 0)




#we created an object called "pred_mod" which captures our logistic regression
#results. For "data =" put the name of the data frame. Then, the "outcome 
#varible" should be listed before the tilde (~).Predictors should be listed
#after the tilde with a + or : to separate them.

#ex: column name ~ predictor 1 column name + predictor 2 column name + 

#predictor 1 column name : predictor 2 column name. The color (:) indicates that
#you're including an interaction in the model. Keep in mind that interactions
#will complicate the model so consider thinking more critically about what 
#interactions should be included in the model.

pred_mod <- 
  glm(data = predicted_results, chosen ~ engage + lineup + views
      + engage:lineup + lineup:views + engage:views + engage:lineup:views,
      family = "binomial")

summary(pred_mod) #use to see the results of logistic regression


#################################################
#BONUS CONTENT
#R already dichotomies our variables with more than two levels
#and this is necessary in order to run a logistic regression.
#below you can see that if we manually dummy code our variables 
#we will get the same output results as what we find when we 
#simply run "views" in our logistic regression. Also, check out
#how the contrasts R uses maps up with our manully made dummy codes
######
predicted_results$view1 <- 0
predicted_results$view2 <- 0
predicted_results$view3 <- 0
predicted_results$view4 <- 0

predicted_results$view1 [predicted_results$views == 1] <- 1
predicted_results$view2 [predicted_results$views == 5] <- 1
predicted_results$view3 [predicted_results$views == 10] <- 1
predicted_results$view4 [predicted_results$views == 20] <- 1

pred_mod_dum <- 
  glm(data = predicted_results, chosen ~ engage + lineup + 
        view1 + view2 + view3 + view4,
      family = "binomial")

summary(pred_mod_dum)

pred_mod_simp <- 
  glm(data = predicted_results, chosen ~ engage + lineup + 
        views,
      family = "binomial")

summary(pred_mod_simp)


################################################
#STEP 3:
#Check the assumptions for logistic regressions
################################################

#You can check the assumptions of LR if you want
#We should be good since this is a true experiment
#with truly independent variables

#Let's examine tolerance and VIF
car::vif(pred_mod) 

# Tolerance is just 1/VIF
1/vif(pred_mod) #keep in mind you need the "car" package to run this code

# We're looking for Tol > .1 and VIF < 10


#Linearity of the Logit
#You only need to do this if you have a CONTINUOUS VARIBLE
#To check for linearity in the logit, we need to create interaction terms of
# each continuous IV and its natural log, then do a logistic regression with all
# of these terms and interacions; the products should NOT be significant.
predicted_results %>%
  glm(data = ., chosen ~ views +
        I(views * log(views)),
      family = "binomial") %>%
  summary
#Note this code will not work since my "views" variable is a factor. But this
#code is included as an example of how it should look. To run it, simply change
#the "views" variable to numeric.
#######



#####################
#STEP 4
#Running Simulations
#####################

#This is the most involved part of the code. Depending on how many groups you
#have, we need to create a matrix that produces a unique code for each 
#condition. You can try making smaller matrices to check your work. Ultimately,
#I am using a unique code of zeros and ones to represent the levels of each 
#conditions. Then I am able to pull the beta estimates from the logistic 
#regression ran ealier with predicted data. We use these estimates to give each
#predictor a weight. We lastly have to specifiy what predictors will appear in
#our final model. The code for this process is below as well as a step-by-step
#annotation of what each chunck of code is specifying. 


#################################################
#Two-way interaction simulation - fully annotated
#################################################
n <- 1000
simFit <- function(n){
  engagesim <- rep(c(0,1), each = n/2)
  lineupsim <- rep(c(0,1), times = n/2)
  view1sim <- rep(c(0,1,0,0,0,0,1,0,0,0), each = n/10) 
  view2sim <- rep(c(0,0,1,0,0,0,0,1,0,0), each = n/10)
  view3sim <- rep(c(0,0,0,1,0,0,0,0,1,0), each = n/10)
  view4sim <- rep(c(0,0,0,0,1,0,0,0,0,1), each = n/10) #this is the least fun
  #part. Pretty much we need to ensure that each conditions is equally 
  #represented (fully crossed) by playing around with the generated matrix
  #of 0s and 1s. The above code essentially creates the variables and their
  #levels for our simulated data. Keep in mind that all variables must be 
  #dichotomized. Check out the constrast() function for help on dichotomizing
  #each variable
  xb <- (-2.90866 + (-1.87779)*engagesim + 0.29514*lineupsim + 
           1.5198*view1sim + 2.6885*view2sim + 2.8553*view3sim + 
           3.3384*view4sim + 1.58265*(engagesim == "1")*(lineupsim == "1"))
  #this is where we insert our log odd weights for the intercept and each 
  #predictor. Not that in order to express the weights associated with an 
  #interaction you must use the code that appears after the final plus sign
  p <- 1/(1 + exp(-xb))
  #this is just the equation for logistic regression placed into an object called "p
  y <- rbinom(n = n, size = 1, prob = p)
  #we use the code above to generate simulated data based on the weights we 
  #identified above. N can be any number divisible by our conditions and we 
  #defined n before writing out this chunk of code. Size allows us to produce a 
  #vector of 0s and 1s (we want a binomial output for a logistic regression). 
  #And prob is any proportion. We are using the obj 'p' which is a proportion
  #generated from our predicted weights
  modsim <- glm(y ~ engagesim + lineupsim + view1sim + 
                  view2sim + view3sim + view4sim + 
                  engagesim:lineupsim + engagesim:view1sim +
                  engagesim:view2sim + engagesim:view3sim +
                  engagesim:view4sim + lineupsim:view1sim +
                  lineupsim:view2sim + lineupsim:view3sim +
                  lineupsim:view4sim, family = binomial)
  #We are just setting up the logistic regression similarly to how we did 
  #previously. 
  s.out <- summary(modsim)
  s.out$coefficients["engagesim:lineupsim","Pr(>|z|)"] < 0.05 #here you simply
  #insert the interaction or main effect you want to test the power for
  
}

powerEst <- function(N, n){
  r.out <- replicate(n = N, simFit(n = n))
  mean(r.out) #I pulled this code directly from the website
}

powerEst(N = 500, n = 200) #this code let's you test the power estimate of
#one particular sample size, in this example it's a sample of 200 tested
#500 times

ss <- seq(100,1000,100) # various sample sizes
p.out <- sapply(ss, function(x)powerEst(N = 1000, n = x))


ss[p.out > 0.8] #here you can indicate the level of power that you 
#are interested in testing for

plot(ss, p.out, type = "l", 
     xlab = "sample size", ylab = "power")
abline(h = 0.8, lty = 2)
#This is a figure that you can generate to observe the power trends at
#each sample size
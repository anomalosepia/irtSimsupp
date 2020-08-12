
## if you have not installed the following packages, do so now. 
require(psych)
require(mirt)
require(ggplot2)
require(tidyverse)
require(ggpubr)
require(ggsci)
require(ggExtra)
require(eRm)

####
set.seed(222)


rawScore  = runif(100)
logits = logit(rawScore)
plot(rawScore~ logits)



itemDifficulties = logit(c(2,2,3,3,4,4,5,5,6,6,7,7,8,8) * .1) # probabilities to logits. 
nPeople = 30

personAbilities = rnorm(nPeople) # normally distributed abilities. 

responseMatrix = matrix(nrow = nPeople, ncol = length(itemDifficulties))
for(i in seq(1,nPeople)){
  for(j in seq(1,length(itemDifficulties))){
    # this is a slow loop but the idea here is that we're going through each person and estimating a response 
    # rasch model for estimating correctness same as 1pl, 
    # any generating process would work here. 
    theResponse =  ifelse(runif(1) <(exp(personAbilities[i] - itemDifficulties[j])  ) / (1+  exp(personAbilities[i] - itemDifficulties[j])  ),
                          1,
                          0)
    #print(theResponse)
    responseMatrix[i,j] = theResponse
  }
}
responseMatrix = as.data.frame(responseMatrix)
names(responseMatrix) = paste("item", seq(1,length(itemDifficulties)))

# this is the person that we will assess throughout the rest of the simulations
person1 = responseMatrix[1,]




# wrap all that up in a function that makes new responses but keeps the proportion of correct responses for each item
makeResponses <-function(nPeople, itemDifficulties, oldResMat){
  
  csm = colSums(oldResMat)
  personAbilities = rnorm(nPeople) # normally distributed abilities. 
  
  responseMatrix = matrix(nrow = nPeople, ncol = length(itemDifficulties))
  for(j in seq(1,length(itemDifficulties))){
    newPat = 1.1 # can't have a fraction so this is ok just so we don't break the loop at the start
    originalPatternforItem = csm[j]
    while(newPat !=originalPatternforItem){  
      for(i in seq(1,nPeople)){
        # this is a slow loop but the idea here is that we're going through each person and estimating a response 
        # rasch model for estimating correctness same as 1pl, 
        # any generating process would work here. 
        theResponse =  ifelse(runif(1) <(exp(personAbilities[i] - itemDifficulties[j])  ) / (1+  exp(personAbilities[i] - itemDifficulties[j])  ),
                              1,
                              0)
        #print(theResponse)
        responseMatrix[i,j] = theResponse
      }
      newPat = sum(responseMatrix[,j])
    }
  }
  responseMatrix = as.data.frame(responseMatrix)
  names(responseMatrix) = paste("item", seq(1,length(itemDifficulties)))
  return(responseMatrix)
}

# this function takes some responses, and a single person response and returns the ability estimates. 
returnScoreSingle <-function(responses, patternPerson){

  rasch4 = RM(bind_rows(responses,patternPerson))
  rr4 = person.parameter(rasch4)
  twoPlModel = mirt(bind_rows(responses,patternPerson),
                    1,
                    itemtype = "2PL",
                    method = "MHRM",
                    technical = list(NCYCLES = 3000)
                    )
 
  ttpp = as.matrix(fscores(twoPlModel))[,1]
  
 tibble( "rsm" = dplyr::last(rr4$theta.table[,1]),
         "tpl" = dplyr::last(ttpp),
         "raschScores" =rr4$theta.table,
         "tplScores" = as.matrix(fscores(twoPlModel))[,1])
}




# make 9 different runs and extract ability estimates. 

nineSets = rerun(9,
                 returnScoreSingle(bind_rows(makeResponses(nPeople, 
                                                           itemDifficulties, 
                                                           responseMatrix)),
                                   person1))




# restructure the data so that it's easier to plot
allSets <-
  nineSets %>%
  enframe %>%
  unnest



theme_set(theme_light())

# this makes the fancy plots and they are completely inspired from this excellent post by
# Cedric scherer.
# https://cedricscherer.netlify.app/2019/05/17/the-evolution-of-a-ggplot-ep.-1/
# we have multiplied the IRT scores by 2 to get them on the same scaler as the Rasch scores. 

rIrtplot = allSets %>%
  mutate(theRun = fct_reorder(as.factor(name), -tpl)) %>%
  mutate(tpl2 = tplScores*2) %>%
  mutate(meanrsm = mean(rsm)) %>%
  ggplot(aes(x = theRun, y = tpl2, color = as.factor(theRun))) + 
  coord_flip() +
  scale_color_uchicago() +
  removeGrid() + 
  geom_hline(aes(yintercept = meanrsm), color = "gray70", size = 0.6) +
  geom_point(aes(y = tpl*2),size = 5) +
  geom_point(aes(y = rsm),size = 5, shape = 15) + 
  geom_jitter(seed = 2019,size = 3, alpha = 0.15, width = 0.15) +
  theme(legend.position = "none")  + 
  theme(panel.background = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(colour = "black")) + 
  geom_segment(
    aes(x = theRun, xend = theRun,
        y = tpl*2, yend = rsm),
    size = 0.8
  ) + ylab("theta") + xlab("") + theme(
    axis.text.y = element_blank())


write.csv(allSets, "plotDataOut.csv")
ggsave("raschIRTplot.pdf",rIrtplot, w = 7, h = 4)



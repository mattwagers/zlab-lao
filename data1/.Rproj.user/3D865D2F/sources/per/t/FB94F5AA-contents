library(magrittr)
library(dplyr)
library(lme4)

## Get data files

file_list <- list.files(path = "./data1/", pattern = ".csv", full.names = TRUE)
length(file_list)

dataset <- NULL

for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.csv(file, header=TRUE, comment.char="#", as.is=TRUE)
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.csv(file, header=TRUE, comment.char="#", as.is=TRUE)
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  
}

## Clean up data frame
dataset -> target.df
target.df <- target.df[ , c(72, 69, 66, 40, 77, 1)]

colnames(target.df) <- c("Subject","Sequence", "Item", "Condition", "Response", "InitiationTime")
target.df$Item %<>% as.factor
target.df$Subject %<>% as.factor

## Attach parse key
parse <- read.csv("parse.csv", header=TRUE)
target.df %<>% merge(parse, by = "Item")
target.df$Parse <- ifelse(target.df$SRC==target.df$Response, "SRC", "ORC")

# Attach animacy key
animacy <- read.csv("animacy.csv", header=TRUE)
target.df %<>% merge(animacy, by = "Item")

#### Exclude participants (too young) I manually excluded these items instead. 
##target.df %>% subset(Subject!="127" & Subject!="228" & Subject!="209") %>% droplevels -> target.df 

## Check how many observations per subject
#print(summary(target.df$Subject))

#################### Choice data ####################

## Get descriptives
target.df %>% group_by(Condition, Parse) %>%
  summarize(n.obs = n()) -> summary.df_overall_choice

target.df %>% group_by(Condition, Animacy, Parse) %>%
  summarize(n.obs = n()) -> summary.df_by_animacy_choice

## Prep for modeling
target.df$Parse %<>% as.factor
target.df$Parse %<>% relevel(., ref = "ORC")
contrasts(target.df$Parse)

## Set contrast for classifier. Negative coefficient for No. Positive coefficient for Yes.
target.df$Classifier <- ifelse(target.df$Condition=="a" | target.df$Condition=="b" | target.df$Condition=="c" | target.df$Condition=="d", "No", "Yes")
target.df$Classifier %<>% as.factor
target.df$Classifier %<>% relevel(., ref = "Yes")
contrasts(target.df$Classifier) <- (1/2) * contr.sum(2)

## Set contrast for subject clitic. Negative coefficient for No. Positive coefficient for Yes.
target.df$SubClit <- ifelse(target.df$Condition=="a" | target.df$Condition=="b" | target.df$Condition=="e" | target.df$Condition=="f", "Yes", "No")
target.df$SubClit %<>% as.factor
target.df$SubClit %<>% relevel(., ref = "Yes")
contrasts(target.df$SubClit) <- (1/2) * contr.sum(2)

## Set contrast for overt object. Negative coefficient for No. Positive coefficient for Yes.
target.df$OvOb <- ifelse(target.df$Condition=="a" | target.df$Condition=="c" | target.df$Condition=="e" | target.df$Condition=="g", "Yes", "No")
target.df$OvOb %<>% as.factor
target.df$OvOb %<>% relevel(., ref = "Yes")
contrasts(target.df$OvOb) <- (1/2) * contr.sum(2)

## Mixed logit (for choice data)
# collapsing across animacy
#target.df %>% glmer(Parse~Classifier*SubClit*OvOb + 
#                     (1|Subject) + (Classifier|Subject) + (SubClit|Subject) + (OvOb|Subject) + 
#                      (Classifier:SubClit|Subject) + (Classifier:OvOb|Subject) + (SubClit:OvOb|Subject) + (Classifier:SubClit:OvOb|Subject) + 
#                      (1|Item) + (Classifier|Item) + (SubClit|Item) + (OvOb|Item) + 
#                      (Classifier:SubClit|Item) + (Classifier:OvOb|Item) + (SubClit:OvOb|Item) + (Classifier:SubClit:OvOb|Item), 
#                    family = binomial, data=., 
#                    control = glmerControl(optCtrl=list(maxfun=100000))) ->  model_choice.full

#summary(model_choice.full)

#################### Initiation times ####################

## Concatenate item and condition for duration key
target.df$File <- as.factor(paste(target.df$Item, target.df$Condition, ".wav", sep=""))

target.df -> zlab

## Attach duration key
duration <- read.table("durations.txt", header=TRUE)
duration$Duration = duration$Duration*1000

duration$File %<>% as.factor

target.df %<>% merge(duration, by = "File")
target.df$TouchRT = target.df$InitiationTime - target.df$Duration

summary(target.df$TouchRT)

target.df %<>% subset(., (target.df$TouchRT > median(target.df$TouchRT) -  2.5*sd(target.df$TouchRT)) & (target.df$TouchRT < median(target.df$TouchRT) +  2.5*sd(target.df$TouchRT)))
cat("Observations BEFORE exclusion", nrow(zlab), sep=" : ")
cat("Observations AFTER exclusion", nrow(target.df), sep=" : ")
cat("Percentage of observations excluded using deviation around median", round((nrow(zlab) - nrow(target.df))/nrow(zlab)*100, 2), sep = " : ")

## Get descriptives

target.df %>% group_by(Condition, Parse) %>%
  summarize(n.obs = n(),
            meanRT = mean(TouchRT),
            sdRT = sd(TouchRT),
            seRT = round(sdRT/sqrt(n.obs),2)) -> summary.df_overall_touchRT

target.df %>% group_by(Condition, Animacy, Parse) %>%
  summarize(n.obs = n(),
            sdRT = sd(TouchRT),
            seRT = round(sdRT/sqrt(n.obs),2)) -> summary.df_by_animacy_touchRT
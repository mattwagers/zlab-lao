library(ggplot2)
library(ggthemes)
library(grid)
library(magrittr)
library(dplyr)
library(lme4)

target.df %>%
  group_by(Item, Classifier, SubClit, OvOb) %>%
  mutate(SRC=I(Parse=="SRC")) %>%
  summarize(n.src=sum(SRC), n.obs=n(), p.src=(n.src+0.5)/(n.obs+1)) %>%
  group_by(Classifier, SubClit, OvOb) %>%
  summarize(m.p = mean(p.src), s.p=sd(p.src), n.item=n(), se.p=s.p/sqrt(n.item)) -> p_summary.tab

###
target.df %>%
  group_by(Subject, OvOb, SubClit) %>%
  mutate(SRC=I(Parse=="SRC")) %>%
  summarize(n.src=sum(SRC), n.obs=n(), p.src=(n.src+0.5)/(n.obs+1)) %>%
  group_by(Subject, OvOb, SubClit) -> pSRC.by_subj.tbl
###

pSRC.by_subj.tbl %>% ggplot(aes(x=p.src)) + facet_grid(SubClit~.) + geom_density(position="stack", aes(fill=OvOb), adjust=1/7, alpha=0.5) + xlim(0,1) + scale_fill_colorblind()

p_summary.tab$Label <- c("V=Cl DP", "V=Cl", "V DP Pro", "V + DP")
p_summary.tab$ExpectedAmbiguous <- c("Unambiguous", "Ambiguous", "Unambiguous", "Ambiguous")
p_summary.tab$Condition <- c(3,2,4,1)

p_summary.tab$Label[c(4,2,1,3)] -> labs
p_summary.tab$Classifier <- c(rep("No Classifier",4), rep("Classifier",4))

p_summary.tab %>% ggplot(aes(y=m.p, ymin=m.p-se.p, ymax=m.p+se.p)) + labs(y = "p(SRC)")-> p

# 830 x 622 PNG

p + geom_pointrange(aes(x=Condition, col=Classifier), size=1.5, fatten=2) + 
  theme_minimal() + ylim(0.4,1) + 
  geom_hline(yintercept=0.5) + 
  geom_hline(yintercept=c(0.7, 0.9), lwd=0.1) + 
  scale_x_continuous(labels=labs, position='bottom') +
  scale_color_colorblind() +
  scale_y_continuous(trans="logit") + 
  labs(title="Probability of SRC interpretation", subtitle="Error bars: standard error over item means") +
  theme(plot.margin=unit(c(1/2,1,1/2,1/2), "cm"), 
        text = element_text(size=14), axis.title.x=element_blank())

condition_labels <- read.csv("condition-labels.csv")
target.df %<>% left_join(condition_labels)
target.df$Condition -> target.df$Condition2
levels(target.df$Condition2) <- rep(c("DP+Cl", "Cl", "In", "DP"), 2)
contrasts(target.df$Condition2) <- contr.helmert(4) 
contrasts(target.df$Condition2)[,1] <- contrasts(target.df$Condition2)[,1]/2# Coeff 1 is comparison of 2 clitic conds
contrasts(target.df$Condition2)[,2] <- c(-1,-1,3,-1)/4 # ORC v. all else
contrasts(target.df$Condition2)[,3] <- c(-1,-1,0,2)/3 # DP only v. S clitic

glm(Parse ~ Condition2, data=target.df, family=binomial) %>% summary
glmer(Parse ~ Condition2 + (Condition2|Subject), data=target.df, family=binomial) %>% summary

# Compare to ref level ORC ("In")
target.df$Condition -> target.df$Condition3
levels(target.df$Condition3) <- rep(c("DP+Cl", "Cl", "In", "DP"), 2)
target.df$Condition3 %<>% relevel(ref="In")
contrasts(target.df$Condition3) <- contr.treatment(4)
glm(Parse ~ Condition3, data=target.df, family=binomial) %>% summary
glmer(Parse ~ Condition3 + (Condition3|Item), data=target.df, family=binomial) %>% summary

## 
target.df %>% filter(zTRT < 3 & TouchRT<10000) %>% group_by(Subject) %>% summarize(medRT = median(TouchRT), muRT = mean(TouchRT), spread=IQR(TouchRT)) -> subject_means.tbl
subject_means.tbl %<>% mutate(ranked = rank(medRT))
subject_means.tbl %>% ggplot(aes(y=medRT)) -> subject_means.plot
subject_means.plot + geom_linerange(aes(x=ranked,ymin=medRT-0.5*spread,ymax=medRT+0.5*spread)) + geom_point(aes(x=ranked, y=muRT), col="red") + scale_y_log10()

write.csv(target.df, file="zapotec-lao.csv")


## 
target.df %>% filter(zTRT < 3 & TouchRT<10000) %>% group_by(Subject) %>% summarize(medRT = median(TouchRT), muRT = mean(TouchRT), spread=IQR(TouchRT)) -> subject_means.tbl
subject_means.tbl %<>% mutate(ranked = rank(medRT))
subject_means.tbl %>% ggplot(aes(y=medRT)) -> subject_means.plot
subject_means.plot + geom_linerange(aes(x=ranked,ymin=medRT-0.5*spread,ymax=medRT+0.5*spread)) + geom_point(aes(x=ranked, y=muRT), col="red") + scale_y_log10()

# RT Table

target.df %>% filter(zTRT < 3 & TouchRT<10000) %>% group_by(Subject, SubClit, OvOb, Parse) %>%
  summarize(medRT=median(TouchRT)) %>%
  group_by(SubClit,OvOb,Parse) %>%
  summarize(mRT=mean(medRT), sRT=sd(medRT), n=n(), seRT=sRT/sqrt(n)) %>%
  spread(key=Parse, value=list(mRT) -> TouchRT_summary.tbl
         

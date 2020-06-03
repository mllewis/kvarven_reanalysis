library(tidyverse)
library(here)
library(langcog)
library(compute.es)


# params
T1_PATH <- here("data/lewis2016_reanalysis/anonymized/turk_replication_1_A.csv")
T2_PATH <- here("data/lewis2016_reanalysis/anonymized/turk_replication_2_A.csv")
L1_PATH <- here("data/lewis2016_reanalysis/anonymized/inlab_replication.csv")
T3_PATH <- here("data/lewis2016_reanalysis/anonymized/turk_replication_3_A.csv")
T4_PATH <- here("data/lewis2016_reanalysis/anonymized/turk_replication_4_A.csv")

basic_strict = FALSE
sub_strict = TRUE

# functions
get_bootstrapped_props <- function(df, conditionName, expName){
  print(expName)

  names(df)[which(names(df) == conditionName)] = "response.cat"

  df$response.cat = ordered(df$response.cat, levels = c("basic", "sub"))
  df$Answer.sample = ordered(df$Answer.sample, levels = c("learner", "teacher"))

  df = df %>%
    mutate(w.ans.cat.sub = as.numeric(response.cat) - 1,
           w.ans.cat.b = 1 - as.numeric(response.cat) + 1) %>%
    group_by(Answer.sample)

  ns = df %>%
    group_by(response.cat, Answer.sample) %>%
    summarise(n = n())

  ns_cond = df %>%
    group_by(Answer.sample) %>%
    summarise(n_cond = n())

  # get props
  props = df %>%
    multi_boot_standard(col = "w.ans.cat.b")  %>%
    mutate(response.cat = "basic")
  props = multi_boot_standard(df, col = "w.ans.cat.sub") %>%
    mutate(response.cat = "sub") %>%
    rbind(props) %>%
    left_join(ns, by = c("response.cat", "Answer.sample")) %>%
    left_join(ns_cond, by = c("Answer.sample"))

  props$response.cat = as.factor(props$response.cat)
  props$response.cat = ordered(props$response.cat, levels = c("sub", "basic"))
  props$Answer.sample = ordered(props$Answer.sample, levels = c("teacher", "learner"))

  names(props)[2:4] = c( "cill", "ciul", "prop")
  props$exp = expName

  return(props)
}

######## T1 ###########
t1 <- read_csv(T1_PATH)

# make factors
t1$Answer.sample <- factor(t1$Answer.sample, labels=c('teacher','learner')) # sample0 = teacher, sample1 = learner
t1$Answer.label <- factor(t1$Answer.label, labels=c('nolabel','label')) # 0 = nolabel, 1 = label
#t1 <- colwise(as.factor)(t1)

t1.f  <-  t1 %>%
  filter(Answer.label == 'label') %>% #remove nolabel participants (run 4)
  filter(!duplicated(workerids)) %>% #participants who completed multiple runs
  filter(Answer.click1 == "\"correct\"" &
           Answer.click2 == "\"correct\"") %>% #correct training items (learning only)
  filter(Answer.Qwcheck == 0)  %>% #check generalization question
  filter(Answer.question1 == 'TRUE') #filter question

if (sub_strict){
  sub <- t1.f$Answer.Qwsmm1 == 0 & t1.f$Answer.Qwsmm2 == 0 &
    t1.f$Answer.Qwsm1 == 1 & t1.f$Answer.Qwsm2 == 1
} else {
  sub <- t1.f$Answer.Qwsmm1 == 0 & t1.f$Answer.Qwsmm2 == 0 &
    (t1.f$Answer.Qwsm1 == 1 | t1.f$Answer.Qwsm2 == 1)
}

if (basic_strict){
  basic <- (t1.f$Answer.Qwsmm1 == 1 & t1.f$Answer.Qwsmm2 == 1) &
    t1.f$Answer.Qwsm1 == 1 & t1.f$Answer.Qwsm2 == 1
} else {
  basic <- (t1.f$Answer.Qwsmm1 == 1 | t1.f$Answer.Qwsmm2 == 1) &
    t1.f$Answer.Qwsm1 == 1 & t1.f$Answer.Qwsm2 == 1
}

t1.f$w.ans.cat <- "other"
t1.f$w.ans.cat[sub] <- "sub"
t1.f$w.ans.cat[basic] <- "basic"
t1.f$w.ans.cat = factor(t1.f$w.ans.cat, levels = c("sub", "basic", "other"))

# get other responeses
others.t1 = filter(t1.f, w.ans.cat == "other")

# filter out other responses
t1.f = t1.f[t1.f$w.ans.cat != "other",]
t1.f$w.ans.cat = droplevels(t1.f$w.ans.cat)

######## T2 ###########
t2 <- read_csv(T2_PATH)
t2  <- t2 [,c(-1:-19,-21:-32,-36,-41:-43,-54:-55,-56)] # remove unnecessary columns

# make factors
t2$Answer.sample <- factor(t2$Answer.sample,
                           labels = c('teacher','learner')) # sample0 = teacher, sample1 = learner
#t2 <- colwise(as.factor)(t2)

t2.f  <-  t2 %>%
  filter(Answer.click1 == "\"correct\"" &
           Answer.click2 == "\"correct\"") %>% # take out those who click on wrong training items
  filter(Answer.Qcheck1 == 0 & Answer.Qcheck2 == 0) %>% # take out if missed check generalization question
  filter(Answer.question1 != 'true' | Answer.question2 != 'true' |
           Answer.question3 == 'true' & Answer.question4 == 'true')  # take out those who missed attention check questions

if (sub_strict) {
  sub <-t2.f$Answer.Qproper1 == 1 & t2.f$Answer.Qproper2 == 1 &
    t2.f$Answer.Qproper3 == 1 & t2.f$Answer.Qsub1 == 1 &
    t2.f$Answer.Qsub2 == 1 & t2.f$Answer.Qbasic1 == 0 &
    t2.f$Answer.Qbasic2 == 0 & t2.f$Answer.Qbasic3 == 0
} else {
  sub <-  t2.f$Answer.Qproper1 == 1 & t2.f$Answer.Qproper2 == 1 &
    t2.f$Answer.Qproper3 == 1 & (t2.f$Answer.Qsub1 == 1 |
                                   t2.f$Answer.Qsub2 == 1) & t2.f$Answer.Qbasic1 == 0 &
    t2.f$Answer.Qbasic2 == 0 & t2.f$Answer.Qbasic3 == 0
}

if (basic_strict){
  basic <- t2.f$Answer.Qproper1 == 1 & t2.f$Answer.Qproper2 == 1 &
    t2.f$Answer.Qproper3 == 1 & t2.f$Answer.Qsub1 == 1 &
    t2.f$Answer.Qsub2 == 1 & t2.f$Answer.Qbasic1 == 1 &
    t2.f$Answer.Qbasic2 == 1 & t2.f$Answer.Qbasic3 == 1
} else {
  basic <- t2.f$Answer.Qproper1 == 1 & t2.f$Answer.Qproper2 == 1 &
    t2.f$Answer.Qproper3 == 1 & t2.f$Answer.Qsub1 == 1 &
    t2.f$Answer.Qsub2 == 1 & (t2.f$Answer.Qbasic1 == 1 |
                                t2.f$Answer.Qbasic2 == 1 | t2.f$Answer.Qbasic3 == 1)
}

t2.f$w.ans.cat <- "other"
t2.f$w.ans.cat[sub] <- "sub"
t2.f$w.ans.cat[basic] <- "basic"
t2.f$w.ans.cat = factor(t2.f$w.ans.cat, levels = c("sub", "basic", "other"))

# get other responeses
others.t2 = filter(t2.f, w.ans.cat == "other")

# filter out other responses
t2.f = t2.f[t2.f$w.ans.cat != "other",]
t2.f$w.ans.cat = droplevels(t2.f$w.ans.cat)

######## L1 ###########
l1 <- read_csv(L1_PATH)

# make factors
l1 <- colwise(as.factor)(l1)
names(l1)[names(l1) == "sample"] = "Answer.sample"

# subset data to those with correct training
l1.f  <-  l1 %>%
  filter(t2_a == 1 & t3_a == 1 & t2_b == 1 & t2_b == 1)

if (sub_strict) {
  sub <- l1.f$basic1_a == 0 & l1.f$basic2_a == 0 &
    l1.f$basic1_b == 0 & l1.f$basic2_b == 0 &
    (l1.f$sub1_a == 1 & l1.f$sub2_a == 1 &
       l1.f$sub1_b == 1 & l1.f$sub2_b == 1)
} else {
  sub <- l1.f$basic1_a == 0 & l1.f$basic2_a == 0 &
    l1.f$basic1_b == 0 & l1.f$basic2_b == 0 &
    (l1.f$sub1_a == 1 | l1.f$sub2_a == 1) &
    (l1.f$sub1_b == 1 | l1.f$sub2_b == 1)
}
if (basic_strict){
  basic <- (l1.f$basic1_a == 1 & l1.f$basic2_a == 1 &
              l1.f$basic1_b == 1 & l1.f$basic2_b == 1) &
    (l1.f$sub1_a == 1 & l1.f$sub2_a == 1 &
       l1.f$sub1_b == 1 & l1.f$sub2_b == 1)

} else {
  basic <- (l1.f$basic1_a == 1 | l1.f$basic2_a == 1 |
              l1.f$basic1_b == 1 | l1.f$basic2_b == 1) &
    (l1.f$sub1_a == 1 & l1.f$sub2_a == 1 &
       l1.f$sub1_b == 1 & l1.f$sub2_b == 1)
}

l1.f$w.ans.cat <- "other"
l1.f$w.ans.cat[sub] <- "sub"
l1.f$w.ans.cat[basic] <- "basic"
l1.f$w.ans.cat <- factor(l1.f$w.ans.cat, c("sub","basic","other"))

# get other responeses
others.l1 = filter(l1.f, w.ans.cat == "other")

# filter out other responses
l1.f = l1.f[l1.f$w.ans.cat != "other",]
l1.f$w.ans.cat = droplevels(l1.f$w.ans.cat)

######## T3 ###########
t3 <- read_csv(T3_PATH)

# make factors
t3$Answer.sample <- factor(t3$Answer.sample,
                           labels=c('teacher','learner')) # sample0 = teacher, sample1 = learner
#t3 <- colwise(as.factor)(t3)

# subset data
t3.f  <-  t3 %>%
  filter(Answer.click1 == "\"correct\"" &
           Answer.click2 == "\"correct\"") %>% # take out those who click on wrong training items
  filter(Answer.Qcheck1 == 0 & Answer.Qcheck2 == 0) %>% # take out if missed check generalization question
  filter(Answer.question1 == 'TRUE' & Answer.question2 == 'TRUE' &
           Answer.question3 == 'TRUE' & Answer.question4 == 'TRUE') # take out those who missed filter question

if(sub_strict){
  sub <-  t3.f$Answer.Qproper1 == 1 & t3.f$Answer.Qproper2 == 1 &
    t3.f$Answer.Qproper3 == 1 & t3.f$Answer.Qsub1 == 1 &
    t3.f$Answer.Qsub2 == 1 & t3.f$Answer.Qbasic1 == 0 &
    t3.f$Answer.Qbasic2 == 0 & t3.f$Answer.Qbasic3 == 0
} else {
  sub <-  t3.f$Answer.Qproper1 == 1 & t3.f$Answer.Qproper2 == 1 &
    t3.f$Answer.Qproper3 == 1 & (t3.f$Answer.Qsub1 == 1 |
                                   t3.f$Answer.Qsub2 == 1) & t3.f$Answer.Qbasic1 == 0 &
    t3.f$Answer.Qbasic2 == 0 & t3.f$Answer.Qbasic3 == 0
}

if (basic_strict){
  basic <- t3.f$Answer.Qproper1 == 1 & t3.f$Answer.Qproper2 == 1 &
    t3.f$Answer.Qproper3 == 1 & t3.f$Answer.Qsub1 == 1 &
    t3.f$Answer.Qsub2 == 1 & t3.f$Answer.Qbasic1 == 1 &
    t3.f$Answer.Qbasic2 == 1 & t3.f$Answer.Qbasic3 == 1
} else {
  basic <- t3.f$Answer.Qproper1 == 1 & t3.f$Answer.Qproper2 == 1 &
    t3.f$Answer.Qproper3 == 1 & t3.f$Answer.Qsub1 == 1 &
    t3.f$Answer.Qsub2 == 1 & (t3.f$Answer.Qbasic1 == 1 |
                                t3.f$Answer.Qbasic2 == 1 | t3.f$Answer.Qbasic3 == 1)
}

t3.f$w.ans.cat <- "other"
t3.f$w.ans.cat[sub] <- "sub"
t3.f$w.ans.cat[basic] <- "basic"
t3.f$w.ans.cat = factor(t3.f$w.ans.cat, levels = c("sub", "basic", "other"))

# get other responses
others.t3 = filter(t3.f, w.ans.cat == "other")

# filter out other responses
t3.f = t3.f[t3.f$w.ans.cat != "other",]
t3.f$w.ans.cat = droplevels(t3.f$w.ans.cat)

######## T4 ###########
t4 <- read_csv(T4_PATH)

# make factors
t4$Answer.sample <- factor(t4$Answer.sample,
                           labels = c('teacher','learner')) # sample0 = teacher, sample1 = learner
#t4 <- colwise(as.factor)(t4)

# subset data
t4.f <- t4 %>%
  filter(Answer.click1 == "\"correct\"" &
           Answer.click2 == "\"correct\"") %>% # take out those who click on wrong training items
  filter(Answer.Qcheck1 == 0 & Answer.Qcheck2 == 0) %>% # take out if missed check generalization question
  filter(Answer.question1 == 'TRUE' & Answer.question2 == 'TRUE' &
           Answer.question3 == 'TRUE' & Answer.question4 == 'TRUE')# take out those who missed filter question

if (sub_strict) {
  sub <-  t4.f$Answer.Qproper1 == 1 & t4.f$Answer.Qproper2 == 1 &
    t4.f$Answer.Qproper3 == 1 & t4.f$Answer.Qsub1 == 1 &
    t4.f$Answer.Qsub2 == 1 & t4.f$Answer.Qbasic1 == 0 &
    t4.f$Answer.Qbasic2 == 0 & t4.f$Answer.Qbasic3 == 0
} else {
  sub <-  t4.f$Answer.Qproper1 == 1 & t4.f$Answer.Qproper2 == 1 &
    t4.f$Answer.Qproper3 == 1 & (t4.f$Answer.Qsub1 == 1 |
                                   t4.f$Answer.Qsub2 == 1) & t4.f$Answer.Qbasic1 == 0 &
    t4.f$Answer.Qbasic2 == 0 & t4.f$Answer.Qbasic3 == 0
}

if (basic_strict){
  basic <- t4.f$Answer.Qproper1 == 1 & t4.f$Answer.Qproper2 == 1 &
    t4.f$Answer.Qproper3 == 1 & t4.f$Answer.Qsub1 == 1 &
    t4.f$Answer.Qsub2 == 1 & t4.f$Answer.Qbasic1 == 1 &
    t4.f$Answer.Qbasic2 == 1 & t4.f$Answer.Qbasic3 == 1
} else {
  basic <- t4.f$Answer.Qproper1 == 1 & t4.f$Answer.Qproper2 == 1 &
    t4.f$Answer.Qproper3 == 1 & t4.f$Answer.Qsub1 == 1 &
    t4.f$Answer.Qsub2 == 1 & (t4.f$Answer.Qbasic1 == 1 |
                                t4.f$Answer.Qbasic2 == 1 | t4.f$Answer.Qbasic3 == 1)
}

t4.f$w.ans.cat <- "other"
t4.f$w.ans.cat[sub] <- "sub"
t4.f$w.ans.cat[basic] <- "basic"
t4.f$w.ans.cat = factor(t4.f$w.ans.cat,
                        levels = c("sub", "basic", "other"))

# filter out "other" responses
others.t4 = filter(t4.f, w.ans.cat == "other")

# filter out other responses
t4.f = t4.f[t4.f$w.ans.cat != "other",]
t4.f$w.ans.cat = droplevels(t4.f$w.ans.cat)

######## COMBINE ALL DATASETS ###########

t1.f$exp = "Exp. 1"
t2.f$exp = "Exp. 2"
l1.f$exp = "Exp. 3"
t3.f$exp = "Exp. 4"
t4.f$exp = "Exp. 5"


all.data.f = rbind(t1.f[,c("Answer.sample", "w.ans.cat", "exp")],
                   t2.f[,c("Answer.sample", "w.ans.cat", "exp")],
                   l1.f[,c("Answer.sample", "w.ans.cat", "exp")],
                   t3.f[,c("Answer.sample", "w.ans.cat", "exp")],
                   t4.f[,c("Answer.sample", "w.ans.cat", "exp")])

# get props by experiment
all.data.f.props = all.data.f %>%
  group_by(exp) %>%
  do(get_bootstrapped_props(.,"w.ans.cat", .$exp[1]))

# spread props and n_cond
all.data.f.props.basic = all.data.f.props %>%
  filter(response.cat == "basic") %>%
  select(-cill, -ciul, -response.cat, -prop, -n) %>%
  spread(Answer.sample, n_cond, fill = F) %>%
  rename(teacher_n = teacher, learner_n = learner)

all.data.f.props.basic = all.data.f.props  %>%
  filter(response.cat == "basic") %>%
  select(-cill, -ciul, -response.cat,
         -n_cond, -n) %>%
  spread(Answer.sample, prop, fill = F) %>%
  rename(teacher_prop = teacher,
         learner_prop = learner) %>%
  left_join(all.data.f.props.basic)

# get all effect sizes (compute.es package)
all.es = propes(all.data.f.props.basic$learner_prop,
                all.data.f.props.basic$teacher_prop,
                all.data.f.props.basic$learner_n,
                all.data.f.props.basic$teacher_n,
                verbose = F)

all.es$exp = all.data.f.props.basic$exp # add exp ids

Fixed effects

# metafor package
fixed.effects.d = rma(d, var.d, data = all.es, method = "FE")
fixed.effects.d
```

Random effects
```{r}
random.effects.d = rma(d, var.d, data = all.es)
random.effects.d
```

par(cex = 1, font = 1)
forest(random.effects.d,
       slab = all.es$exp,
       mlab = "All",
       xlab = "Cohen's d")
par(font = 2)
text(-3.6, 8.55, "Experiment")
text(5.8, 8.55, "Cohen's d [95% CI]")

addpoly(random.effects.d, row = -1, cex = .75,
        annotate = F,  col = "red", mlab = "", efac = 2)


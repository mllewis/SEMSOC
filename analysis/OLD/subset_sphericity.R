
## Subset cps
### Full sample
#### Cue pairs{.tabset}
```{r}
get_trans_prob_group <- function(df, grp, pairs){ 
  # which transitional probabilities are we interested in?
  if(pairs == "sequential") {
    anchor_2 = "a1"
    anchor_3 = "a2"
  } else if (pairs == "cue_anchor"){
    anchor_2 = anchor_3 = "cue"
  }
  
  # get transitional probabilities for each bigram, at each transition
  tp.c1  = df %>%
    group_by_(grp) %>%
    do(get_trans_prob(.,"cue", "a1")) %>%
    mutate(trans = "c1")
  
  tp.12  = df %>%
    group_by_(grp) %>%
    do(get_trans_prob(.,anchor_2, "a2")) %>%
    mutate(trans = "12")
  
  tp.23  = df %>%
    group_by_(grp) %>%
    do(get_trans_prob(.,anchor_3, "a3")) %>%
    mutate(trans = "23")
  
  # bind tps across transitions
  tp.dif.all = bind_rows(tp.c1, tp.12, tp.23) %>%
    mutate(trans = as.factor(trans)) %>%
    mutate(grp = as.factor(grp)) %>%
    group_by_("trans", grp) %>%
    summarise(mean = mean(trans.prob))
  #multi_boot_standard(column = "trans.prob") 
  
  if (pairs == "sequential") {
    levels(tp.dif.all$trans) = c("a1_a2", "a2_a3", "cue_a1")
  } else if (pairs == "cue_anchor"){
    levels(tp.dif.all$trans) = c("cue_a2", "cue_a3", "cue_a1")
  }
  tp.dif.all$trans = factor(tp.dif.all$trans,levels(tp.dif.all$trans)[c(3,1,2)])
  
  return(tp.dif.all)
}

tp.all.ms = d.clean %>%
  mutate(all = "all") %>%
  get_trans_prob_group("all", "cue_anchor")

ggplot(tp.all.ms, aes(y = mean, x = trans, group = 1)) +
  #geom_pointrange(aes(ymax = ci_upper, ymin = ci_lower)) +
  geom_point() + 
  geom_line()+
  xlab("transition") +
  ggtitle("all")+
  ylab("mean tp")+
  theme_bw(base_size = 18)
```

#### Sequential pairs
```{r}
tp.all.ms = d.clean %>%
  mutate(all = "all") %>%
  get_trans_prob_group("all", "sequential")

ggplot(tp.all.ms, aes(y = mean, x = trans, group = 1)) +
  #geom_pointrange(aes(ymax = ci_upper, ymin = ci_lower)) +
  geom_point() + 
  geom_line()+
  xlab("transition") +
  ggtitle("all")+
  ylab("mean tp")+
  theme_bw(base_size = 18)
```


### Gender

#### Cue pairs{.tabset}
```{r}
tp.gender.ms = get_trans_prob_group(d.clean, "gender", "cue_anchor")
ggplot(tp.gender.ms, aes(y = mean, x = trans, group = gender, color = gender)) +
  #geom_pointrange(aes(ymax = ci_upper, ymin = ci_lower)) +
  geom_point() + 
  geom_line()+
  xlab("transition") +
  ylab("mean tp")+
  theme_bw(base_size = 18)
```

#### Sequential pairs
```{r}
tp.gender.ms = get_trans_prob_group(d.clean, "gender", "sequential")
ggplot(tp.gender.ms, aes(y = mean, x = trans, group = gender, color = gender)) +
  #geom_pointrange(aes(ymax = ci_upper, ymin = ci_lower)) +
  geom_point() + 
  geom_line()+
  xlab("transition") +
  ylab("mean tp")+
  theme_bw(base_size = 18)
```

## All bigrams{.tabset}
These differences are essentially the same as the raw frequency analyses...

### Cue pairs
```{r}
tp.all.ms = all.cb %>%
  group_by(pair) %>%
  summarise(mean = mean(trans.prob))

ggplot(tp.all.ms[4:6,], aes(y = mean, x = pair, group = 1)) +
  geom_point() +
  geom_line()+
  xlab("pair") +
  theme_bw(base_size = 18)
```

### Association pairs
```{r}
ggplot(tp.all.ms[1:3,], aes(y = mean, x = pair, group = 1)) +
  geom_point() + 
  geom_line()+
  xlab("pair") +
  theme_bw(base_size = 18)
```

### Sequential pairs
```{r}
ggplot(tp.all.ms[c(4,1,3),], aes(y = mean, x = reorder(pair, mean), group = 1)) +
  geom_point() +
  geom_line()+
  xlab("pair") +
  theme_bw(base_size = 18)
```

### Education

#### Cue pairs{.tabset}
```{r}
tp.educ.ms = get_trans_prob_group(d.clean, "education", "cue_anchor") %>%
  mutate(education = as.factor(education))

ggplot(tp.educ.ms, aes(y = mean, x = trans, group = education, color = education)) +
  #geom_pointrange(aes(ymax = ci_upper, ymin = ci_lower)) +
  geom_point() + 
  geom_line()+
  xlab("transition") +
  ylab("mean tp")+
  theme_bw(base_size = 18)
```


#### Sequential pairs
```{r}
tp.educ.ms = get_trans_prob_group(d.clean, "education", "sequential") %>%
  mutate(education = as.factor(education))

ggplot(tp.educ.ms, aes(y = mean, x = trans, group = education, color = education)) +
  #geom_pointrange(aes(ymax = ci_upper, ymin = ci_lower)) +
  geom_point() + 
  geom_line()+
  xlab("transition") +
  ylab("mean tp")+
  theme_bw(base_size = 18)
```


### Age

#### Cue pairs{.tabset}
```{r}
d.pos.age = d.clean %>%
  filter(age > 15 & age < 75) %>%
  mutate(age.bin = cut_width(age, width = 10))

d.clean = d.clean  %>%
  left_join(d.pos.age %>% group_by(userID) %>% slice(1) 
            %>% ungroup() %>% select(age.bin, userID)) %>%
  filter(!is.na(age.bin))
tp.age.ms  = get_trans_prob_group(d.clean, "age.bin", "cue_anchor") 

ggplot(tp.age.ms , aes(y = mean, x = trans, group = age.bin, color = age.bin)) +
  #geom_pointrange(aes(ymax = ci_upper, ymin = ci_lower)) +
  geom_point() + 
  geom_line()+
  xlab("transition") +
  ylab("mean tp")+
  theme_bw(base_size = 18)
```

#### Sequential pairs
```{r}
tp.age.ms  = get_trans_prob_group(d.clean, "age.bin", "sequential") 
ggplot(tp.age.ms , aes(y = mean, x = trans, group = age.bin, color = age.bin)) +
  #geom_pointrange(aes(ymax = ci_upper, ymin = ci_lower)) +
  geom_point() + 
  geom_line()+
  xlab("transition") +
  ylab("mean tp")+
  theme_bw(base_size = 18)
```

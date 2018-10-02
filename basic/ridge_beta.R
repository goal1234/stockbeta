# 按照成交量加权的beta估计，对于总体而言的方法

require(dplyr)
require(ridge)
require(lubridate)
require(fBasics)

index <- test %>% group_by(date) %>% 
  summarize(wei_vol = weighted.mean(close, volume)) %>% 
  mutate(change = returns(wei_vol)*100) %>% na.omit(.) 


pre_beta <- left_join(qret, index, by = "date") %>% arrange(code, date)
pre_beta <- na.omit(pre_beta)

p <- pre_beta %>% group_by(code) 



beta_lm <- p %>% do(mod = lm(change~changepercent +0, data = .)) %>% 
  mutate(beta = coef(mod)[1]) %>% select(-mod)

require(quantreg)
beta_rq <- 
  p %>% do(mod = rq(change~changepercent +0, data = .)) %>% 
    mutate(beta = coef(mod)[1]) %>% select(-mod)



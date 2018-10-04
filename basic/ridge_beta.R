# 按照成交量加权的beta估计，对于总体而言的方法

require(dplyr)
require(ridge)
require(lubridate)
require(fBasics)


# 对于所有的股票而言的
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



#################### 
#对于行业进行

index <- test %>% group_by(c_name, date) %>% 
  summarize(wei_vol = weighted.mean(close, volume)) %>% 
  mutate(change = returns(wei_vol)*100) %>% na.omit(.) 


pre_beta <- left_join(qret, index, by = c("c_name","date")) %>% arrange(code, date)
pre_beta <- na.omit(pre_beta)

p <- pre_beta %>% group_by(c_name, code) 



beta_lm <- p %>% do(mod = lm(change~changepercent +0, data = .)) %>% 
  mutate(beta = coef(mod)[1]) %>% select(-mod)

require(quantreg) #奇异矩阵 rq nlrq失败
require(MASS)#rlm失败
require(gbm)
beta_rq <- 
  p %>% do(mod = gbm(change~changepercent + 0, data = ., distribution = "laplace")) %>% 
  mutate(beta = coef(mod)[1]) %>% select(-mod)


A <- p %>% mutate(n = n())

require(xgboost)
beta_rq <- 
  p %>% do(mod = gbm(change~changepercent + 0, data = ., distribution = "laplace")) %>% 
  mutate(beta = coef(mod)[1]) %>% select(-mod)


##########################################
# 按照行业季度

index <- qret %>% group_by(c_name, q, date) %>% 
  summarize(wei_vol = weighted.mean(close, volume)) %>% 
  mutate(change = returns(wei_vol)*100) %>% na.omit(.) 


pre_beta <- left_join(qret, index, by = c("c_name", "q", "date")) %>% arrange(code, date)
pre_beta <- na.omit(pre_beta)

p <- pre_beta %>% group_by(c_name, q, code) 



beta_lm <- p %>% do(mod = lm(change~changepercent +0, data = .)) %>% 
  mutate(beta = coef(mod)[1]) %>% select(-mod)

require(quantreg) #奇异矩阵 rq nlrq失败
require(MASS)#rlm失败
require(gbm)
beta_rq <- 
  p %>% do(mod = rq(change~changepercent + 0, data = .)) %>% 
  mutate(beta = coef(mod)[1]) %>% select(-mod)


A <- p %>% mutate(n = n())

require(xgboost)
beta_rq <- 
  p %>% do(mod = gbm(change~changepercent + 0, data = ., distribution = "laplace")) %>% 
  mutate(beta = coef(mod)[1]) %>% select(-mod)


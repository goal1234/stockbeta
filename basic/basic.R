
require(fBasics)
require(dplyr)
qret <- test %>% group_by(code) %>%arrange(date) %>% mutate(changepercent = returns(close)*100)

qret <- qret %>% mutate(y = year(date), m = month(date), d = day(date), q = quarter(date), w = week(date))

qret <- na.omit(qret)

me <- qret %>% group_by(c_name,y) %>% summarise(avg = mean(changepercent))


# 估计beta值
require(ridge)

formula <- "y~-1+"
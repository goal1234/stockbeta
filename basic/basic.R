


me <- qret %>% group_by(c_name,y) %>% summarise(avg = mean(changepercent))


# 估计beta值
require(ridge)

formula <- "y~-1+"
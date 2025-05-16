set.seed(123)
data = data.frame(i = 1:40, group = sample(c("A","B","C"), 40,replace = TRUE))
foreign::write.dta(data,"data.dta")


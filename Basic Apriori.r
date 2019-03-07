install.packages("arules")
install.packages("printr")


library(arules)
library(printr)

data(AdultUCI)
data(Adult)
p_data <- AdultUCI
t_data <- Adult


rules <- apriori(t_data,
                 parameter = list(support = .4, confidence = .7),
                 appearance = list(rhs = c("race=White", "sex=Male"), default = "lhs"))


rules.sorted <- sort(rules, by = "lift")
top5.rules <- head(rules.sorted, 5)
as(top5.rules, "data.frame")

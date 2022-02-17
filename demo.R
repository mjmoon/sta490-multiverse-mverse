library(tidyverse)
library(mverse)
soccer <- read.csv("https://raw.githubusercontent.com/mjmoon/sta490-soccer/main/data/soccer.csv")
soccer["skintone"] <- (soccer["rater1"] + soccer["rater2"]) / 2
# 02-logistic-regression
model <- formula(cbind(redCards, games - redCards) ~ skintone)
fit <- glm(model, family = binomial(link = "logit"), data = soccer)
res <- broom::tidy(fit, exponentiate = TRUE, conf.int = TRUE, conf.level = .95)
save(fit, res, file = "main/02-logistic-regression.RData")
# 03-multiverse-anlaysis
mv <- mverse(soccer)
new_position <- mutate_branch(
  position, # use position as is
  forcats::fct_collapse(
    # forcats::fct_collapse() is a convenient helper function for regrouping factors
    position, 
    Forward = c("Center Forward", "Right Winger", "Left Winger"), 
    Midfielder = c("Defensive Midfielder", "Attacking Midfielder", 
                   "Right Midfielder", "Center Midfielder", "Left Midfielder"), 
    Defender = c("Center Back", "Right Fullback", "Left Fullback"), 
    Goalkeeper = c("Goalkeeper")), # groups position into 4 groups
  if_else(position == "Goalkeeper", "Goalkeeper", "Fielder") # group all fielders
)
model <- formula_branch(
  cbind(redCards, games - redCards) ~ skintone, # do not use position information
  cbind(redCards, games - redCards) ~ skintone + new_position # include position variable
)
logit_reg <- family_branch(binomial(link = "logit"))
mv <- mv %>%
  add_mutate_branch(new_position) %>%
  add_formula_branch(model) %>%
  add_family_branch(logit_reg)
condition_position <- branch_condition(
  cbind(redCards, games - redCards) ~ skintone, # formula with no position option
  position # position as is option 
)
mv <- add_branch_condition(mv, condition_position)
multiverse_fit <- glm_mverse(mv)
multiverse_est <- summary(multiverse_fit, conf.int = FALSE) %>%
  filter(term == 'skintone')
multiverse_res <- summary(multiverse_fit, conf.int = FALSE) %>%
  filter(term == 'skintone') %>%
  mutate(
    conf.low = exp(estimate + qnorm(.025) * std.error),
    conf.high = exp(estimate + qnorm(.975) * std.error),
    estimate = exp(estimate)
  ) %>%
  select(universe, estimate, conf.low, conf.high, new_position_branch, model_branch)
save(multiverse_fit, multiverse_est, multiverse_res, 
     file = "main/03-multiverse-analysis.RData")

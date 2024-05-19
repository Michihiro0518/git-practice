# ライブラリのロード
library(tidyverse)
library(MatchIt)
library(pwr)

# 架空データの生成
set.seed(123)
data <- tibble(
  patient_id = 1:200,
  age = rnorm(200, mean = 60, sd = 10),
  sex = sample(c("Male", "Female"), 200, replace = TRUE),
  history = sample(c("Yes", "No"), 200, replace = TRUE),
  treatment = sample(c("A", "B"), 200, replace = TRUE),
  side_effect = rbinom(200, 1, 0.3) # 30%の確率で副作用発生
)

# プロペンシティスコアの計算とマッチング
data <- data %>%
  mutate(treatment = as.factor(treatment),
         sex = as.factor(sex),
         history = as.factor(history))

ps_model <- glm(treatment ~ age + sex + history, family = binomial, data = data)
data <- data %>%
  mutate(pscore = ps_model$fitted.values)

matchit_model <- matchit(treatment ~ age + sex + history, method = "nearest", data = data)
matched_data <- match.data(matchit_model)

# マッチング前のデータでカイ二乗検定
chisq_test_before <- chisq.test(table(data$treatment, data$side_effect))

# マッチング後のデータでカイ二乗検定
chisq_test_after <- chisq.test(table(matched_data$treatment, matched_data$side_effect))

# マッチング前の効果量と検定力の計算
tbl_before <- table(data$treatment, data$side_effect)
effect_size_before <- sqrt(chisq_test_before$statistic / sum(tbl_before))
n_before <- sum(tbl_before)
power_before <- pwr.chisq.test(w = effect_size_before, N = n_before, df = 1)$power
cat("マッチング前の検定力: ", power_before, "\n")

# マッチング後の効果量と検定力の計算
tbl_after <- table(matched_data$treatment, matched_data$side_effect)
effect_size_after <- sqrt(chisq_test_after$statistic / sum(tbl_after))
n_after <- sum(tbl_after)
power_after <- pwr.chisq.test(w = effect_size_after, N = n_after, df = 1)$power
cat("マッチング後の検定力: ", power_after, "\n")

# 必要なサンプルサイズの計算
required_sample_size_before <- ceiling(pwr.chisq.test(w = effect_size_before, power = 0.8, df = 1)$N)
required_sample_size_after <- ceiling(pwr.chisq.test(w = effect_size_after, power = 0.8, df = 1)$N)
cat("マッチング前に必要なサンプルサイズ: ", required_sample_size_before, "\n")
cat("マッチング後に必要なサンプルサイズ: ", required_sample_size_after, "\n")

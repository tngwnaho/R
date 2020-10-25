

############### make a data ##################

library("tidyverse")

#データの読み込み
email_data <- read_csv("http://www.minethatdata.com/Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv")

#dataframeではなさそう。色々含んでいる
email_data


# tidy関数を利用することで、各パラメータについて、
# 変数名、推定値、標準誤差、t値、p値といった値が入ったデータフレームを出力してくれる
library("broom")


## 女性向けメールが配信されたデータを削除したデータを作成
male_df <- email_data %>%
  
  #女性向けメールが配信されたデータを削除
  filter(segment != 'Womens E-Mail') %>%
  
  #介入をあらわすtreatment変数を追加
  #mutateはその中に変数名とその定義を記入すると、%>%で渡された入力にその変数を追加した結果を出力してくれる関数
  mutate(treatment = if_else(segment == "Mens E-Mail", 1, 0))


#シードを固定
set.seed(1)

#条件に反応するサンプルの量を半分にする
obs_rate_c <- 0.5
obs_rate_t <- 0.5

# バイアスのあるデータを作成
# biased_data <- male_df %>% mutate(
#   obs_rate_c= if_else(
#     (history > 300) | (recency < 6) | (channel == "Multichannel"), obs_rate_c, 1
#   ),
#   
#   obs_rate_t = if_else(
#     (history > 300) | (recency < 6) | (channel == "Multichannel"), 1, obs_rate_t
#   ),
#   random_number = runif(n= NROW(male_df))
# )%>%
#   filter(
#     (treatment == 0 & random_number < obs_rate_c) | (treatment == 1 & random_number < obs_rate_t)
#   )


############### 傾向スコア ##################

install.packages("MatchIt")

library("MatchIt")

#傾向スコアを利用したマッチング
# matchitでは、基本的にATTの推定が行われる。
m_near <- matchit(formula = treatment ~ recency + history + channel, data=biased_data, method = "nearest", replace=TRUE)

#マッチング後のデータを作成
#matched_dataを使うことで、マッチングが行われた後のデータフレームを程に入れることが可能
#後は、そのデータに対して、平均の差を求めることで効果を推定できる。
matched_data <- match.data(m_near)

# マッチング前後でケースとコントロールのMean Diffが減少していることを確認する。 
summary(m_near)

## マッチング後のデータで効果の推定
PSM_result <- matched_data %>% lm(spend ~ treatment, data = .) %>% tidy()


# estimateは推定されたATTの結果を表している
# メールによる介入で平均的に$0.919程度の売り上げの増加が起きたものと考えられる
PSM_result


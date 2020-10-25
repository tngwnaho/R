
#メールマーケティングの効果を確認
# install.packages("tidyverse")

library("tidyverse")

#データの読み込み
email_data <- read_csv("http://www.minethatdata.com/Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv")

#dataframeではなさそう。色々含んでいる
email_data



## 女性向けメールが配信されたデータを削除したデータを作成
male_df <- email_data %>%
  
  #女性向けメールが配信されたデータを削除
  filter(segment != 'Womens E-Mail') %>%
  
  #介入をあらわすtreatment変数を追加
  #mutateはその中に変数名とその定義を記入すると、%>%で渡された入力にその変数を追加した結果を出力してくれる関数
  mutate(treatment = if_else(segment == "Mens E-Mail", 1, 0))


male_df

############### RCTデータでの回帰分析 #######################
## 回帰分析の実行

rct_reg <- lm(data = male_df, formula = spend ~ treatment + history)


#結果を見ると、メールを送信することで、売り上げが平均0.7ほど増えるという解釈が可能
summary(rct_reg)
# > summary(biased_reg)
# 
# Call:
#   lm(formula = spend ~ treatment + history, data = male_df)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -5.04  -1.34  -1.16  -0.54 498.57 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.359586   0.123254   2.917  0.00353 ** 
#   treatment   0.767450   0.145219   5.285 1.26e-07 ***
#   history     0.001217   0.000283   4.301 1.70e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 14.99 on 42610 degrees of freedom
# Multiple R-squared:  0.001092,	Adjusted R-squared:  0.001046 
# F-statistic:  23.3 on 2 and 42610 DF,  p-value: 7.693e-11


# tidy関数を利用することで、各パラメータについて、
# 変数名、推定値、標準誤差、t値、p値といった値が入ったデータフレームを出力してくれる
library("broom")

rct_reg_coef <- tidy(rct_reg)

rct_reg_coef


################## セレクションバイアスのあるデータを利用した場合 ####################

#シードを固定
set.seed(1)

#条件に反応するサンプルの量を半分にする
obs_rate_c <- 0.5
obs_rate_t <- 0.5

# バイアスのあるデータを作成
biased_data <- male_df %>% mutate(
  obs_rate_c= if_else(
    (history > 300) | (recency < 6) | (channel == "Multichannel"), obs_rate_c, 1
    ),
  
  obs_rate_t = if_else(
    (history > 300) | (recency < 6) | (channel == "Multichannel"), 1, obs_rate_t
    ),
  random_number = runif(n= NROW(male_df))
  )%>%
  filter(
    (treatment == 0 & random_number < obs_rate_c) | (treatment == 1 & random_number < obs_rate_t)
  )

# セレクションバイアスのあるデータで平均を比較
summary_by_segment_biased <- biased_data %>%
  group_by(treatment) %>%
  summarize(conversion_rate = mean(conversion),
            spend_mean = mean(spend),
            count = n())
summary_by_segment_biased

# Rの関数であるt.testを使ってt検定を行う
## (a)男性向けメールが配信されたグループの購買データをえる
mens_mail_biased <- biased_data %>%
  filter(treatment == 1) %>%
  pull(spend)
mens_mail_biased

## (b)メールが配信されなかったグループの購買データをえる
no_mail_biased <- biased_data %>%
  filter(treatment == 0) %>%
  pull(spend)
no_mail_biased

# (a)(b)の平均の差に対して有意差検定を実行
rct_test_biased <- t.test(mens_mail_biased, no_mail_biased, var.equal = TRUE)

rct_test_biased

# Two Sample t-test
# 
# data:  mens_mail_biased and no_mail_biased
# t = 5.2814, df = 31885, p-value = 1.29e-07
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.5632905 1.2281137
# sample estimates:
#   mean of x mean of y 
# 1.4850913 0.5893892 



######################　いろんなデータで単回帰して分析結果を比較

# RCTデータでの単回帰分析
rct_reg <- lm(data = male_df, formula = spend ~ treatment)
rct_coef <- summary(rct_reg) %>% tidy()
rct_coef

# > rct_coef
# # A tibble: 2 x 5
# term        estimate std.error statistic  p.value
# <chr>          <dbl>     <dbl>     <dbl>    <dbl>
#   1 (Intercept)    0.653     0.103      6.36 2.09e-10
# 2 treatment      0.770     0.145      5.30 1.16e- 7

#バイアスのあるデータでの単回帰分析
nonrct_reg <- lm(data=biased_data, formula = spend ~ treatment)
nonrct_coef <- summary(nonrct_reg) %>% tidy()
nonrct_coef

# > nonrct_coef
# # A tibble: 2 x 5
# term        estimate std.error statistic      p.value
# <chr>          <dbl>     <dbl>     <dbl>        <dbl>
#   1 (Intercept)    0.548     0.127      4.32 0.0000156   
# 2 treatment      0.979     0.173      5.67 0.0000000143

# バイアスのあるデータに共変量を説明変数に加えてみる
# 共変量を説明変数に加えると、バイアスが少し軽減される
nonrct_mreg <- lm(data=biased_data, formula = spend ~ treatment + recency + channel)
nonrct_mreg_coef <- tidy(nonrct_mreg)
nonrct_mreg_coef

# > nonrct_mreg_coef
# # A tibble: 5 x 5
# term         estimate std.error statistic     p.value
# <chr>           <dbl>     <dbl>     <dbl>       <dbl>
#   1 (Intercept)    1.10      0.311      3.52  0.000429   
# 2 treatment      0.875     0.178      4.91  0.000000924
# 3 recency       -0.0552    0.0254    -2.17  0.0297     
# 4 channelPhone  -0.312     0.282     -1.11  0.268      
# 5 channelWeb    -0.0847    0.282     -0.301 0.764      



############################# OVBを回帰モデルに含める ######################

#介入とvisitの相関を確認するために、モデルを作成してみる
#共変量を含めることで、共変量の影響を取り除いた状態での相関が0.144ということ。
cor_visit_treatment <- lm(
  data=biased_data,
  formula = treatment ~ visit + channel + recency + history
) %>% tidy()
cor_visit_treatment


# treatmentの相関関係が0.875から0.294へと大きく変化してしまった
bad_control_reg <- lm(
  data = biased_data,
  formula = spend ~ treatment + channel + recency + history + visit
) %>% tidy()
bad_control_reg

# > bad_control_reg
# # A tibble: 7 x 5
# term          estimate std.error statistic   p.value
# <chr>            <dbl>     <dbl>     <dbl>     <dbl>
#   1 (Intercept)  -0.438     0.376       -1.16  2.44e-  1
# 2 treatment     0.294     0.177        1.66  9.68e-  2
# 3 channelPhone  0.121     0.300        0.403 6.87e-  1
# 4 channelWeb    0.117     0.299        0.392 6.95e-  1
# 5 recency       0.00988   0.0257       0.385 7.00e-  1
# 6 history       0.000525  0.000371     1.42  1.57e-  1
# 7 visit         7.16      0.242       29.6   3.85e-190
# 

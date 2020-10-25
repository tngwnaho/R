#### Rによるメールマーケティングの効果の検証


#メールマーケティングの効果を確認
install.packages("tidyverse")

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


## メールが配信されたグループとされなかったグループでの購入の発生確率と購入額の平均を計算する
## conversionという変数は、売上が発生すると１、発生しない場合には0となり、spendという変数は売上の金額をあらわす。

## use group_by() and summarize()
summary_by_segment <- male_df %>% 
  
  # grouping of data
  group_by(treatment) %>%
  
  summarize(conversion_rate = mean(conversion),
            # average of spend by groups
            spend_mean = mean(spend),
            # sample num by groups
            count = n()
            )

summary_by_segment


## 有意差検定
### (a)Get a purchase data for groups which sent mens mails
mens_mail <- male_df %>%
  filter(treatment == 1) %>%
  pull(spend)

## (b)Get a purchase data for groups which NOT sent mens mails
no_mail <- male_df %>%
  filter(treatment == 0) %>%
  pull(spend)

## (a)(b)の平均に対して有意差検定を実行
rct_ttest <- t.test(mens_mail, no_mail, var.equal = TRUE)
rct_ttest 

# Two Sample t-test
# 
# data:  mens_mail and no_mail
# t = 5.3001, df = 42611, p-value = 1.163e-07
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:åå
#   0.4851384 1.0545160
# sample estimates:
#   mean of x mean of y 
# 1.4226165 0.6527894 





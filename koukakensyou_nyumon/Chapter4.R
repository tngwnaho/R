##################
###  Chapter4  ###
##################

# 概要
# カリフォルニアで行われた大規模な禁煙キャンペーンのProprositoon99が、
# どの程度タバコの消費に影響を与えたのかを推定する。
# このCampaignでは、
# - タバコ１箱に対して25centの税金をかけ、そこで得られる税収は健康やタバコの健康被害に対する教育などに使われた
# - 室内での空気環境の改善を行うための条例を設立など
# カリフォルニア州全体でこの介入が行われているため、非介入グループをカリフォルニア州の中から用意できない
# よって、DIDのような分析方法を行い、介入が行われなかった場合のカリフォルニアの状態を推測する必要がある。


# 利用するデータセットの情報
# state: state
# year: year
# price: price per pack of cigarrets
# pop : populartion
# pop16 : population above the age of 16
# cpi: consumer price index(1983=100)　消費者物価指数, 消費者が実際に購入する段階での、商品の小売価格（物価）の変動を表す指数
# ndi: per capita disposable income 都市部における一人当たりの可処分所得
# sales : cigarette sales in packs per capita
# pimin : minimum price in adjoining states per pack of cigarettes

#分析するデータのあるパッケージをインストール(初回)
#install.packages("Ecdat")
#install.packages("broom")

#ライブラリの読み込み
library("Ecdat")
library("broom")

#%>%を使うためのライブラリ
#library("dplyr")
library("tidyverse")

#Proposition99の分析：集計による分析

##データ準備

### Common Trend Assumptionのために分析から特定の州を外す
### タバコの税金が1988年以降50せんと以上上がった州のリスト
### Alaska, Hawaii, Maryland, Michigan, New Jersey, NewYork, Washington
skip_state <- c(3,9,10,22,21,23,31,33,48)

### Cigarデータセットの読み込み
### skip_stateに含まれる州のデータを削除
head(Cigar)

Cigar <- Cigar %>% 
  filter(!state %in% skip_state, year >= 70) %>% mutate(area = if_else(state == 5, "CA", "Rest of US"))


###################################
###          DIDの実装          ###
###################################

# 前後比較をする。
# 介入が始まった1988年より後ろか前かでタバコの一人当たりの売り上げを比較する

library(ggplot2)

Cigar_for_plot <- Cigar %>%
  mutate(year_segment = if_else(year < 88, 'before', 'after')) %>%
  group_by(area, year_segment) %>%
  summarize(sales = sum(sales*pop16)/sum(pop16))


Cigar_for_plot2$year_segment <- factor(Cigar_for_plot2$year_segment, levels = c("before", "after"))

Colours2=c("white","gray80","black")
ggplot(Cigar_for_plot2, aes(x = year_segment, y = sales, group = area)) +
  geom_line(aes(linetype = area)) +
  geom_point(aes(shape = area)) +
  ylim(c(0,NA)) +
  theme_bw() +
  scale_fill_manual(values = Colours2)


### DIDのためのデータを準備
## カリフォルニア州とそのほかという２グループのデータ

Cigar_did_sum <- Cigar %>%
  mutate(post = if_else(year > 87, 1, 0),
         ca = if_else(state == 5, 1, 0),
         state = factor(state),
         year_dummy = paste("D", year, sep = "_")) %>%
  group_by(post, year, year_dummy, ca) %>%
  summarize(sales = sum(sales*pop16)/sum(pop16))

## カリフォルニア州とそのほかというグループでの分析
## 2グループでのデータでの分析

# tidy関数を利用することで、各パラメータについて、
# 変数名、推定値、標準誤差、t値、p値といった値が入ったデータフレームを出力してくれる
Cigar_did_sum_reg <- Cigar_did_sum %>%
  lm(data = ., sales ~ ca + post + ca:post + year_dummy) %>% tidy() %>% 
  filter(!str_detect(term, "state"),
         !str_detect(term, "year"))
Cigar_did_sum_reg

Cigar_did_sum_logreg <- Cigar_did_sum %>%
  lm(data = ., log(sales) ~ ca + post + ca:post + year_dummy) %>% tidy() %>% 
  filter(!str_detect(term, "state"),
         !str_detect(term, "year"))
Cigar_did_sum_logreg

Cigar %>%
  group_by(area, year) %>%o
  summarize(sales = sum(sales*pop16)/sum(pop16)) %>%
  ggplot(aes(x = year, y = sales, group = area)) +
    geom_line(aes(linetype = area)) +
    geom_point(aes(shape = area)) +
    geom_vline(aes(xintercept=88), colour="black", linetype="dashed") +
    ylim(c(65,NA)) +
    theme_bw() +
    scale_fill_manual(values = Colours2)

  
###################################
###         Causal Impact       ###
###################################

install.packages("CausalImpact")

# CigarデータをCausal Impact用に整形
### 目的変数としてカリフォルニア州の売上だけ抜き出す
Y <- Cigar %>% filter(state == 5) %>% pull(sales)


### 共変量として他の州の売上を抜きだし整形
### spread関数で、行列の形に整形し直すことができる
X_sales <- Cigar %>%
  filter(state != 5) %>%
  select(state, sales, year) %>%
  spread(state, sales)

### 介入が行われるデータを示す
pre_period <- c(1:nrow(X_sales))[X_sales$year < 88]
post_period <- c(1:nrow(X_sales))[X_sales$year >= 88]

### 目的変数と共変量をバインド
CI_data <- cbind(Y, X_sales) %>% select(-year)

# Causal Impactによる分析
impact <- CausalImpact::CausalImpact(
  CI_data,
  pre.period = c(min(pre_period), max(pre_period)),
  post.period = c(min(post_period), max(post_period))
  )

## 結果のplot
plot(impact)
impact

  
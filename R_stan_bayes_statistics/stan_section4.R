#read package
library(rstan)

#計算の高速化
rstan_options(auto_write = TRUE) #拡張子が.rdsであるRDSファイルが生成される
options(mc.cores = parallel::detectCores()) #計算を並列化するので実行速度が上がる

set.seed(0)

# サンプルデータ作成
sales <- rnorm(100,100,20)
file_beer_sales_1<- data.frame(sales)
colnames(file_beer_sales_1) <- c("sales")

# list形式でまとめる
sample_size <- nrow(file_beer_sales_1)
data_list <- list(sales = file_beer_sales_1$sales, N = sample_size)                  

setwd("/Users/naho/git/R/R_stan_bayes_statistics")
mcmc_result <- stan(
  file = "stan_section4.stan",
  data = data_list,
  seed = 1,
  chains = 4, #チェーン数
  iter = 2000, #乱数生成の繰り返し数
  warmup = 1000, #バーンイン期間
  thin = 1 #間引き数(1なら間引きなし)
)

#結果の表示
print(mcmc_result, probs = c(0.025, 0.5, 0.975))

#muが母平均
#se_meanは事後平均値。50%点は事後中央値
#n_eff MCMCにおける有効サンプルサイズ.これがあまりにも少ないのであれば、モデルの改善が必要になる.100くらいあることが望ましい
#R_hat: MCMCサンプルが収束しているのかどうかを判断した指標Rの計算方法。 1.1未満が望ましい

# Inference for Stan model: stan_section4.
# 4 chains, each with iter=2000; warmup=1000; thin=1; 
# post-warmup draws per chain=1000, total post-warmup draws=4000.
# 
# mean se_mean   sd    2.5%     50%   97.5% n_eff Rhat
# mu     100.42    0.03 1.76   96.90  100.45  103.82  3002    1
# sigma   17.86    0.02 1.27   15.62   17.79   20.61  2819    1
# lp__  -334.70    0.02 0.97 -337.27 -334.39 -333.74  1882    1
# 
# Samples were drawn using NUTS(diag_e) at Sun Oct 25 17:40:13 2020.
# For each parameter, n_eff is a crude measure of effective sample size,
# and Rhat is the potential scale reduction factor on split chains (at 
#                                                                   convergence, Rhat=1).

#バーンイン期間なし = バーンイン期間の後のみプロットされている
traceplot(mcmc_result)

#バーンイン期間あり
#背景が灰色になっているのがバーンイン期間
#MCMCサンプル初期の値は大きく変化しており、安定していないのが見て取れる
traceplot(mcmc_result, inc_warmup = T)
 
########################################
######  第5章: MCMCの結果の評価 ########    
########################################

# MCMCでは、1つ前の時点でのパラメータを使った分布から乱数を生成し、
# 生成された乱数の期待値を次のパラメータの値として利用する
# それを永遠に繰り返す。

# MCMCサンプルの抽出

## Permuted = Falseにすると、サンプルの並び順が保持される。これによって、後ほど利用するbayesplotが利用しやすくなる
## mcmc_sampleは array形式
mcmc_sample <- rstan::extract(mcmc_result, permuted = FALSE)
class(mcmc_sample)
dimnames(mcmc_sample)

##１回目のchainで得られた最初のMCMC sample(バーンイン後)
mcmc_sample[1, "chain:1", "mu"]

## pramameter:mu first chain mcmc sample
mcmc_sample[, "chain:1", "mu"]

#########
# MCMCサンプルの代表値の計算をして、MCMCサンプルの結果がおかしくないか確認
########


## step1: ベクトルにする
mu_mcmc_vec <- as.vector(mcmc_sample[,, "mu"])

## step2: 事後中央値 / 事後期待値/ 95%ベイズ信用区間を計算して、MCMC_resultから出した値と同じになっているか確認する
## 事後中央値を計算する
median(mu_mcmc_vec)
## 事後期待値を計算する
mean(mu_mcmc_vec)
## 95%ベイズ信用区間
quantile(mu_mcmc_vec, c(0.025, 0.975))

## ほとんど同じ値になっているか再確認
print(
  mcmc_result, #MCMCサンプリングの結果
  probs = c(0.025, 0.5, 0.975)  # 事後分布の四分位点を出力
)

# MCMCサンプルを使ってtrace plotをかく
# trace plot = MCMCサンプルを時系列に並べたもの
# install.packages("ggfortify")
library(ggfortify)
autoplot(ts(mcmc_sample[,, "mu"]),
            facet = F, #４つのchainをまとめて１つのグラフにする
            ylab = "mu",
            main = "trace plot"
         )

# ggplot2による事後分布の可視化
## MCMCサンプルを全てまとめてカーネル密度推定を適用すると、パラメータの事後分布がかける
mu_df <- data.frame(mu_mcmc_sample = mu_mcmc_vec)
ggplot(mu_df, aes(x=mu_mcmc_sample)) + geom_density(side = 1.5)

# bayesplotによる事後分布の可視化
library(bayesplot)
## histgram
mcmc_hist(mcmc_sample, pars = c("mu", "sigma"))

## kernel density estimation
mcmc_dens(mcmc_sample, pars = c("mu", "sigma"))

## 事後分布とtrace plotをまとめて表示
mcmc_combo(mcmc_sample, pars = c("mu", "sigma"))

# 事後分布の範囲を比較
mcmc_intervals(
  mcmc_sample,
  pars = c("mu", "sigma"),
  prob = 0.8, #太い線の範囲
  prob_outer = 0.95 #細い線の範囲
)

# 密度を合わせて描画することも可能
mcmc_areas(
  mcmc_sample,
  pars = c("mu", "sigma"),
  prob = 0.6, #薄い青色の範囲
  prob_outer = 0.95 #細い線の範囲 
)

# bayes plotによるMCMC sampleの自己相関の評価
mcmc_acf_bar(mcmc_sample,
             pars = c("mu", "sigma"),
             lags = 50
)

###########
## 推定されたモデルを総合的に評価する方法
#########
#モデルがデータとよく適合しているのかを判断するために事後予測チェックを行う
# このあとは、以下２stepを用意する
# 1. Stanを用いて、事後予測分布を得る方法
# 2. bayesplot packageより、実際の観測データの分布と、事後予測分布を比較する方法


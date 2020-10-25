
#############################
### Rによるブラウザの操作 ###
#############################

library("RSelenium")

#chromeのドライバとSeleniumサーバーの準備
httr::set_config(httr::config(ssl_verifypeer = 0L))
binman::rm_platform("phantomjs")
wdman::selenium(retcommand = TRUE)


# rsDriver() でブラウザを立ち上げる
# chromeでブラウザを立ち上げる
rD <- rsDriver(verbose = FALSE)

# remoteDriverクラスの作成
remDr <- rD[["client"]]

# ブラウザを閉じる
remDr$close()




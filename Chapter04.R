# 『Rによるスクレイピング入門』 第4章
## ----chapter04 section-020 ブログからの抽出

### ブログからの記事タイトル・URLの取得
library(rvest)
library(magrittr)

x <- read_html("https://www.r-bloggers.com")

x %>% html_nodes(css = "body") %>%
  html_children()

#### CSSセレクタで階層の深い箇所を選択するには > を利用する
x %>% html_nodes(css = "body > div#mainwrapper") %>%
  html_children()

x.nodes <- x %>% html_nodes(css = "body > div#mainwrapper > div#leftcontent > div > div")
x.nodes[3]

x.nodes[3] %>% html_structure()

articles <- x.nodes %>% html_nodes("h2") %>% html_text()
links <- x.nodes %>% html_nodes("h2 > a") %>% html_attr(name = "href")

length(articles)
articles[1:3]
links[1]

### 記事本文の取得
articles[27]
links[27]

x <- read_html(links[27])
#### 以下の実行結果は等しい
#### x %>% html_nodes("body > div#mainwrapper > div#leftcontent > div > div.entry") %>% html_children()
x %>% html_nodes("div.entry") %>% html_children()

#### substr()により本文の一部だけを出力しています
x %>% html_nodes("div.entry") %>% html_text(trim = TRUE) %>% substr(1, 200)



# 『Rによるスクレイピング入門』 第4章
## ----chapter04 section-021 HTMLドキュメントに格納された JSONデータを抽出する

### Twitterからスクレイピング対象URLを抽出する
library(twitteR)
setup_twitter_oauth(consumer_key="あなたのCONSUMER_KEY",
                    consumer_secret="あなたのCONSUMER_SECRET")
res <- searchTwitter("#runmeter", n=3000, locale="ja")
urls <- lapply(res, function(x)x$urls$expanded_url)
urls <- unlist(urls)[grepl("^http://runmeter.com", unlist(urls))]

#### スクレイピング対象URLから緯度経度データを抽出する
library(rvest)
js <- read_html(urls[1]) %>% html_nodes(css="head script") %>% html_text()
js <- js[[5]]

library(V8)
ct <- new_context()
ct$eval(js)
ct$get("Object.keys(this)")
jsondata <- ct$get("jsonData")
jsondata <- as.data.frame(jsondata)

scrapeData <- function(url){
  js <- read_html(url) %>% html_nodes("head script") %>% html_text()
  ct <- new_context()
  ct$eval(js[[5]])
  if(any(ct$get("Object.keys(this)") %in% "jsonData")){
    jsondata <- ct$get("jsonData")
    jsondata <- as.data.frame(jsondata)
    if(nrow(jsondata)==0){
      return(NA)
    }
    return(jsondata)
  }else{
    return(NA)
  }
}

library(dplyr)

data_attr <- NULL
for(i in seq(length(urls))){
  data_attr_ <- scrapeData(urls[i])
  if(!is.data.frame(data_attr_)){
    next
  }
  data_attr_$id <- i
  data_attr <- bind_rows(data_attr, data_attr_)
  Sys.sleep(1)
}

#### 取得した緯度経度データを地図上にプロットする（日本以外のデータを除いている）
data_startpoint <- data_attr %>% 
  filter(latitude>=20.2531, latitude<=45.3326, 
         longitude>=122.5601, longitude<=153.5911) %>% 
  group_by(id) %>% slice(1)

library(leaflet)
leaflet(data=data_startpoint) %>%
  addMarkers(~longitude, ~latitude) %>% 
  addTiles() 



# 『Rによるスクレイピング入門』 第4章
## ----chapter04 section-022 HTMLドキュメントに格納されたJSONデータを抽出する

### フォームを介したログイン
library(rvest)
library(magrittr)

#### html_session関数によるsessionオブジェクトの作成
session <- html_session("https://qiita.com")
is.session(session)

forms <- html_form(session)

forms %>% extract2(1)

input.values <- forms %>%
  extract2(1) %>%
  #### フォームに与える値はユーザー自身で設定したものを利用してください
  set_values(identity = "<ユーザー名>", password = "<パスワード>")

#### html_from関数の引数にsessionオブジェクトを与える
input.values <- forms %>% 
  extract2(1) %>% 
  set_values(identity = "uri", password = my.pass)

(login.session <- submit_form(session, input.values))

### ログイン後のウェブスクレイピング
login.session %>% 
  html_nodes("div.userInfo > div > div") %>% 
  html_text()

### セッションを介したページ移動
login.session %>% 
  jump_to("notifications") %>%
  html_nodes(xpath = '//*[@id="main"]/div/div/div/ul/li/a/div[2]') %>% 
  html_text() %>% 
  extract(c(1:4))

login.session %>% 
  follow_link(i = "uri") %>%  #### 文字列で移動先のa要素を含む箇所を指定
  html_nodes(xpath = '//*[@id="main"]/div/div/div[2]/div[1]/div[2]/a[1]/span[1]') %>% 
  html_text()

sessions <- login.session %>% 
  follow_link(i = 5) %>% #### http://qiita.com/ で表示される5番目のa要素のhref属性値
  back() %>% #### http://qiita.com/ に戻る
  jump_to("notifications") %>% #### http://qiita.com/notifications へ移動
  back() %>% #### 戻る
  follow_link(i = "uri") %>% #### リンクテキストが「uri」となっているa要素のリンクに移動
  follow_link(xpath = '//*[@id="main"]/div/div/div[2]/div[2]/div/div[1]/
              div/div[2]/div[2]/div[2]/a') %>% #### 個別の投稿ページへ移動
  back() %>% 
  back()

session_history(sessions)


# 『Rによるスクレイピング入門』 第4章
## ----chapter04 section-023 ログインが必要なページの ウェブスクレイピング(RSelenium編)

#### ログインをRから実行する
library(RSelenium)
remDr <- remoteDriver(browserName = "chrome")
remDr$open()
remDr$navigate("https://hh.pid.nhk.or.jp/pidh02/loginform.do?authImagePosition=top&authImageUrl=https%3A%2F%2Fhh.pid.nhk.or.jp%2Fmygogaku_pc%2Fmygogaku_login.jpg&href=https%3A%2F%2Fwww2.nhk.or.jp%2Fgogaku%2F&n=1")

#### ログインIDおよびパスワードの入力からカレンダーのドキュメント取得
webElem1 <- remDr$findElement(using="name", value="LOGIN_ID")
webElem1$sendKeysToElement(sendKeys = list("マイ語学のログインID"))
webElem2 <- remDr$findElement(using="name", value="PASSWORD")
webElem2$sendKeysToElement(sendKeys = list("マイ語学のパスワード", key="enter"))
remDr$navigate("https://www2.nhk.or.jp/gogaku/mygogaku/calendar/")
txt <- remDr$getPageSource()
remDr$close()

#### カレンダーから情報を取得する
library(rvest)
count <- read_html(txt[[1]]) %>% html_nodes(css=".count-area div .count-big") %>% html_text
count <- gsub("回", "", count) %>% as.numeric
count

# 『Rによるスクレイピング入門』 第3章
## ----chapter03 section-017 Wikipedia からのデータ抽出

### 四国八十八箇所の位置情報の取得
#### パッケージの読み込み
library(rvest)
library(dplyr)
library(magrittr)
library(stringr)

x <- read_html("https://en.wikipedia.org/wiki/Shikoku_Pilgrimage")

### テーブル要素の取得
tab <- x %>% html_table(header = TRUE)
class(tab)
NROW(tab)

pilgr <- tab %>% extract2(2)

pilgr %>% glimpse()

### 位置情報データの加工
pilgr$Coordinates[1]

library(stringr)

coord <- pilgr %>% 
  use_series(Coordinates) %>% 
  str_extract_all(pattern  = "[1-9][1-9]\\.[0-9]+|[1-9][1-9][1-9]\\.[0-9]+", 
                  simplify = TRUE) %>% 
  extract(, 1:2) %>% 
  as_data_frame() %>% 
  mutate_if(is.character, as.numeric) %>% 
  set_colnames(c("lat", "lon"))

#### 作成したデータフレームを確認
head(coord)

### leafletパッケージによる地図作成
library(leaflet)
pilgr %>% bind_cols(coord) %>%
  leaflet %>%
  addTiles() %>%
  addMarkers(lng = ~lon, lat = ~lat, popup = ~Temple)


### アメリカ合衆国州人口密度データの取得
x <- read_html("https://ja.wikipedia.org/wiki/アメリカ合衆国の州")
tabs <- x %>% html_table(header = TRUE, fill = TRUE)
NROW(tabs)

states <- tabs %>% extract2(1)
glimpse(states)

#### 必要な列だけを選択し、変数名を変更する
states %<>% select(region = 州名, pop.dens = 人口密度) %>% 
  mutate(region =  gsub("[ア-ン].+州", "", region))

head(states)

### アメリカ合衆国州人口密度の可視化
library(ggplot2)
library(viridis)

map.states <- map_data("state")
ggplot() +
  geom_polygon(data = map.states, aes(x = long, y = lat, group = group)) +
  geom_map(data = states, map = map.states,
           aes(fill = pop.dens, map_id = tolower(region))) +
  scale_fill_viridis(alpha = 0.8)

### column ggplot2パッケージの導入
library(ggplot2)
#### dataに対象のデータを渡し、aes()を利用してmappingの審美的属性を決めます
p <- ggplot(data = iris,
       mapping = aes(x = Sepal.Length, y = Petal.Width))

#### 散布図を描画する
p + geom_point()
#### 折れ線グラフとしてデータを描画する(これはよくない例ですね)
p + geom_line()

#### 日本語フォントの設定
quartzFonts(ipa = quartzFont(rep("IPAexGothic", 4)))
#### 軸のラベルを変更し、テーマ設定を行う
p +
  geom_point() +
  xlab("萼片長") + ylab("花弁長") +
  theme_classic(base_size = 12, base_family = "ipa")





# 『Rによるスクレイピング入門』 第3章
## ----chapter03 section-018 ウェブ APIサービスを用いたデータ抽出

### Facebook Graph APIの利用

#### アプリID
app_id_facebook <- "あなたのアプリID"
#### app secret
app_secret_facebook <- "あなたのapp secret"

#### 認証

library(httr)
facebook <- oauth_endpoint(authorize ="https://www.facebook.com/dialog/oauth", 
                           access="https://graph.facebook.com/oauth/access_token")
myapp <- oauth_app(appname="facebook", key=app_id_facebook, secret=app_secret_facebook)
scope <- "public_profile"
token_facebook <- oauth2.0_token(endpoint = facebook,
                                 app = myapp,
                                 scope = scope,
                                 type = "application/x-www-form-urlencoded",
                                 cache = FALSE)


#### サービス利用
url <- "https://graph.facebook.com/v2.6/me/friends?fields=id,name"
res <- GET(url, config=token_facebook)
res_parsed <- content(res, as="parsed")
res_parsed$summary$total_count


### Yahoo!デベロッパーネットワークの「ルビ振り」サービスの利用

#### アプリケーションID：
app_id_yahoo <- "発行されたアプリケーションID"
#### シークレット：
app_secret_yahoo <- "発行されたシークレット"

#### データ取得
library(rvest)
library(httr)
sentence <- "猫"
url_request <- paste0("http://jlp.yahooapis.jp/FuriganaService/V1/furigana?appid=", app_id_yahoo, "&sentence=", sentence)

#### httrパッケージを用いた場合
res <- GET(url_request)
res_parsed <- content(res, as="parsed")
res_parsed %>% xml_nodes(xpath="//d1:Furigana") %>% xml_text()

#### xml2パッケージを用いた場合
library(xml2)
read_xml(url_request) %>% xml_nodes(xpath="//d1:Furigana") %>% xml_text()


# 『Rによるスクレイピング入門』 第3章

## ----chapter03 section-019 Rのパッケージを利用したお手軽データ抽出

### パッケージの探し方
library(devtools)
install_github("アカウント名/レポジトリ名")

library(githubinstall)
githubinstall("gh")

### パッケージを使う
install.packages("twitteR")
library(twitteR)

setup_twitter_oauth(consumer_key = "(Consumer Key)",
                    consumer_secret = "(Consumer Secret)")

searchTwitter('#rstats', n=50)



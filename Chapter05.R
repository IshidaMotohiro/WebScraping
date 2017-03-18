# 『Rによるスクレイピング入門』 第5章

## ----chapter05 section-024 e-Stat

### e-Stat APIをRから使う
library(httr)
appId <- "XXXX"
res <- GET(
  url = "http://api.e-stat.go.jp/rest/2.1/app/json/getStatsData",
  query = list(
    appId = appId,
    statsDataId = "0003104180"
  )
)
#### 結果の取得
result <- content(res)

str(result, max.level = 4, list.len = 4)

statistical_data <- result$GET_STATS_DATA$STATISTICAL_DATA
str(statistical_data$DATA_INF$VALUE[[1]])

str(statistical_data$CLASS_INF$CLASS_OBJ[[2]])

do.call(rbind, statistical_data$CLASS_INF$CLASS_OBJ[[2]]$CLASS)
do.call(rbind, statistical_data$CLASS_INF$CLASS_OBJ[[1]]$CLASS)

str(statistical_data$CLASS_INF$CLASS_OBJ[[1]]$CLASS)
str(statistical_data$CLASS_INF$CLASS_OBJ[[2]]$CLASS)

force_rbind <- function(x) {
  x_are_list <- as.logical(lapply(x, is.list))
  
  if(all(x_are_list)) {
    #### ネストしたリストはdo.callでrbindを適用する
    do.call(rbind, x)
  } else {
    #### ただのリストはそのままrbindする
    rbind(x)
  }
}

force_rbind(statistical_data$CLASS_INF$CLASS_OBJ[[2]]$CLASS)
force_rbind(statistical_data$CLASS_INF$CLASS_OBJ[[1]]$CLASS)

library(dplyr)
bind_rows(statistical_data$CLASS_INF$CLASS_OBJ[[2]]$CLASS)
bind_rows(statistical_data$CLASS_INF$CLASS_OBJ[[1]]$CLASS)

data_df <- bind_rows(statistical_data$DATA_INF$VALUE)
data_df

meta_info <- statistical_data$CLASS_INF$CLASS_OBJ %>%
  lapply(function(i) {
    i$CLASS <- bind_rows(i$CLASS)
    i
  })
meta_info[2]


mids <- sapply(meta_info, function(x) x$`@id`)
mids

names(meta_info) <- paste0("@", mids)

meta_info_cat01 <- meta_info[["@cat01"]]
meta_info_cat01

cat01_factor <- factor(data_df[["@cat01"]],
                       levels = meta_info_cat01$CLASS$`@code`,
                       labels = meta_info_cat01$CLASS$`@name`)
head(cat01_factor)

data_df[, "@cat01"] <- cat01_factor

meta_inf_wo_cat01 <- meta_info[names(meta_info) != "@cat01"]
for (colname in names(meta_inf_wo_cat01)) {
  m <- meta_info[[colname]]
  data_df[, colname] <- factor(data_df[[colname]],
                                 levels = m$CLASS$`@code`,
                                 labels = m$CLASS$`@name`)
}

mids   <- names(meta_info)
mnames <- sapply(meta_info, function(x) x$`@name`)
mnames

names(data_df)[match(mids, names(data_df))] <- mnames
data_df

levels(data_df$`時間軸（月）`)
levels(data_df$`男女別`)
levels(data_df$`人口`)
levels(data_df$`年齢5歳階級`)

data_H26_Oct <- data_df %>%
  filter(
    `時間軸（月）` == "平成26年10月",
    `男女別`       == "男女計",
    `人口`         == "総人口",
    grepl(pattern = "^\\d+", x = `年齢5歳階級`)
  ) %>%
  mutate(`推計値[千人]` = as.integer(`$`)) 

data_H26_Oct

library(ggplot2)
ggplot(data_H26_Oct, aes(x = `年齢5歳階級`, y = `推計値[千人]`)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = -45, hjust = 0)) +
  ggtitle("平成26年10月1日現在人口推計")


### estatapiパッケージの利用
install.packages("estatapi")

#### 最新版をインストールする場合
library(devtools)
install_github("yutannihilation/estatapi")

library(estatapi)
population_stats <- estat_getStatsList(appId = appId, searchWord = "人口推計")

nrow(population_stats)

glimpse(population_stats)

result$GET_STATS_DATA$STATISTICAL_DATA$TABLE_INF$TITLE

population_agerange_stats <- population_stats %>%
  filter(`TITLE` == "年齢（５歳階級），男女，月別人口－総人口，日本人人口")
population_agerange_stats

glimpse(estat_getStatsData(appId, statsDataId = "0003104180"))

population_data_list <- lapply(population_agerange_stats$`@id`,
                               function(x) estat_getStatsData(x, appId = appId))

population_data_merged <- population_data_list %>%
  bind_rows %>%
  mutate(
    `年齢5歳階級` = ifelse(`年齢5歳階級` %in% c("85歳以上", "85～89歳", "90～94歳", "95～99歳", "100歳以上"),
                        yes = "85歳以上",
                        no = `年齢5歳階級`)
  ) %>%
  filter(
    grepl(pattern = "年1月", `時間軸（月）`),
    `男女別` == "男女計",
    `人口` == "総人口",
    grepl(pattern = "^\\d+", x = `年齢5歳階級`)
  )
population_data_merged

library(stringr)
#### いったん並べ替えのためのデータフレームをつくる
age_label <- population_data_merged %>%
  transmute(
    `年齢5歳階級`,
    lower_age  = as.integer(str_extract(`年齢5歳階級`, "^\\d+"))
  ) %>%
  distinct() %>%
  arrange(lower_age)
head(age_label)

#### factorにして順序をつける
population_data <- population_data_merged %>%
  mutate(`年齢5歳階級` = factor(`年齢5歳階級`, levels = age_label$`年齢5歳階級`))

ggplot(population_data, aes(`年齢5歳階級`, value)) +
  geom_col() +
  facet_grid(`時間軸（月）` ~ .) +
  theme(axis.text.x = element_text(angle = -60, hjust = 0))

### 類似のAPI
library(eurostat)

datasets <- search_eurostat("Modal split of passenger transport", type = "table")
datasets

dat <- get_eurostat(datasets$code[1], time_format = "num")
head(dat)


# 『Rによるスクレイピング入門』 第5章
## ----chapter05 section-025 アメリカ地質調査所提供の地震データ

### 地震データの入手
library(magrittr)
library(httr)
library(dplyr)
#### APIのエンドポイントをオブジェクトに保存する
base.url <- "http://earthquake.usgs.gov/fdsnws/event/1/"

req <- paste0(base.url, "version") %>% GET()
req %>% status_code()

req %>% content(encoding = "UTF-8")

### パラメータの指定
params <- list(format = "csv",
               starttime = "2016-08-01",
               endtime = "2016-08-07")

params

### queryメソッドの実行
#### パラメータにより、データ取得形式と取得期間の指定を行います
req <- paste0(base.url, "query") %>% GET(query = params)

res <- req %>% content(encoding = "UTF-8")

res %>% glimpse()

### 取得したデータの処理
#### sort引数により降順指定をする
res %>% count(type, sort = TRUE)

nrow(res)
res %<>% filter(type == "earthquake")
nrow(res)

res %<>% select(id, time, lat = latitude, lon = longitude, depth, mag, place)

res %>% 
  summarise_at(vars(depth, mag), c("min", "max", "mean"), na.rm = TRUE)

### geojson形式での取得と可視化
#### formatをgeojsonに変更する
res <- paste0(base.url, "query") %>% 
  GET(query = list(format    = "geojson",
                   starttime = "2016-08-01", 
                   endtime   = "2016-08-07", 
                   limit     = "100")) %>% 
  content(as = "text", encoding = "UTF-8")

res %>% substr(1, 100)

#### install.package("rgdal")
library(rgdal)
df.sp <- readOGR(res, "OGRGeoJSON", stringsAsFactors = FALSE)
#### readOGR()で読み込まれたオブジェクトはS4クラスをもつ
isS4(df.sp)
#### スロット名の確認
slotNames(df.sp)
#### 地震データの本体はdataスロットに保存されている
df.sp@data %>% glimpse()

library(ggplot2)
library(maps)

world <- map_data("world")
worldmap <- ggplot(world, aes(x = long, y = lat, group = group)) +
  geom_path() +
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45)

worldmap +
  geom_point(data = as.data.frame(df.sp),
           aes(x = coords.x1, y = coords.x2,
               group = magType,
               shape = magType,
               size = (mag / 2)))


# 『Rによるスクレイピング入門』 第5章 
## ----chapter05 section-026 Google Cloud Vision API を利用した画像の内容判定

### Google Vision APIのLabel Detectionを利用する
#### 画像を読み込む
f <- "takayanagisan.png"
img <- readBin(f, "raw", file.info(f)[1, "size"])

#### POSTメソッドで判定結果を得る
CROWD_VISION_KEY <- "あなたのキー"
u <- paste0("https://vision.googleapis.com/v1/images:annotate?key=", CROWD_VISION_KEY)
body <- list(requests = list(image=list(content=img),
                             features=list(type="LABEL_DETECTION",
                                             maxResults=10))
)

library(httr)
res <- POST(url=u,
            encode="json",
            body=body,
            content_type_json()
            )
library(jsonlite)
res_text <- fromJSON(content(res, as="text"), flatten=TRUE)
res_text$responses$labelAnnotations


### Google Vision APIのText Detectionを利用する
#### APIの利用準備と画像の読み込み
f_receipt <- "receipt.png"
img_receipt <- readBin(f_receipt, "raw", file.info(f_receipt)[1, "size"])

#### POSTメソッドで判定結果を得る
u_receipt <- paste0("https://vision.googleapis.com/v1/images:annotate?key=", CROWD_VISION_KEY)
body_receipt <- list(requests = list(image=list(content=img_receipt), 
                                     features=list(type="TEXT_DETECTION",
                                                   maxResults=10)
                                     )
)

res_receipt <- POST(url=u_receipt,
                    encode="json",
                    body=body_receipt,
                    content_type_json()
)
res_receipt_text <- fromJSON(content(res_receipt, as="text"), flatten=TRUE)
res_receipt_text$responses$textAnnotations[[1]]$locale[1]
res_receipt_text$responses$textAnnotations[[1]]$description[1]


# 『Rによるスクレイピング入門』 第5章
## ----chapter05 section-027 GitHub

### GitHub APIを使うための準備

library(httr)
res_no_auth <- GET(url = "https://api.github.com/",
                   path = "rate_limit")
content_no_auth <- content(res_no_auth)
str(content_no_auth$resources)

as.POSIXct(content_no_auth$resources$core$reset, origin = "1970-01-01", tz = "Asia/Tokyo")

#### ヘッダに認証情報を与える場合
res_authorized <- GET("https://api.github.com/",
                      path = "rate_limit",
                   add_headers(`Authorization` = "token 1234567890abcdef1234567890abcdef12345678"))

#### クエリパラメータにトークンを指定する場合
#### res_authorized <- GET("https://api.github.com/",
####                       path = "rate_limit",
####                       query = list(access_token = "1234567890abcdef1234567890abcdef12345678"))

content_authorized <- content(res_authorized)
str(content_authorized$resources)

### APIの利用と取得データの操作
base.url <- "https://api.github.com/"
#### これはダミーのトークンです
token <- "1234567890abcdef1234567890abcdef12345678"

### Events API
library(magrittr)
res <- GET(base.url,
           path = "users/hadley/events",
           query = list(access_token = token)) %>% 
  content()

#### 30件分のイベントが取得される
length(res)

#### 1件分のデータだけを取り出して、要素名を取得します
res %>% extract2(1) %>% 
  str(max.level = 1) %>% names()

#### install.packages("purrr")
library(purrr)

#### イベントのタイプを確認する
res %>% map_chr("type")
#### すべてのイベントがpublicなものである
res %>% map_lgl("public")

res %>% map("actor") %>% 
  extract2(1)

### Repositories API
res <- GET(base.url,
    path = "/users/hadley/repos",
    query = list(access_token = token, per_page = 100)
    ) %>% 
  content()

#### 要素名を確認する
res %>% extract2(1) %>% names()

#### 要素名を参照し、データフレームとして扱えるようにする
df.repo <- res %>% map_df(~ .[c("name", "created_at", "size", "stargazers_count")])

library(dplyr)
df.repo %>% glimpse()

df.repo %<>% mutate(created_at = as.POSIXct(created_at)) %>% 
  filter(stargazers_count >= 80)

library(ggplot2)
library(ggrepel)
#### リポジトリ名が評価数順に並ぶように並び替える
sort.name <- with(df.repo, reorder(name, stargazers_count, median))
df.repo %>% ggplot(aes(sort.name, stargazers_count)) +
  geom_point(aes(size = size)) +
  geom_text_repel(aes(label = sort.name)) +
  theme(axis.text.x = element_blank()) +
  xlab("repository")

### column
#### 3つの要素をもつリストを作成する
l <- list(x = 1:3, y = 5:7, z = c(TRUE, FALSE, NA))
lapply(l, FUN = mean) 
Map(mean, l)

library(purrr)

l %>% map(mean)

l %>% map_dbl(mean) #### 返り値は実数のベクトル
l %>% map_chr(mean) #### 文字列として返り値を得る

l %>% map_dbl(mean, na.rm = TRUE)

name.holder <- list(list(Last = c("Motohiro", "Daisuke", "Hiroaki", "Shinya"),
                         First = c("Ishida", "Ichikawa", "Yutani", "Uryu")))

name.holder %>% map(nchar, type = "width")

#### 要素名を参照し、特定の要素について実行するには次のようにします
name.holder %>% map("Last") %>% 
  map(nchar, type = "width")

#### flatten()は階層性のあるリストから、1段浅い階層に引き上げる処理を実行します
name.holder %>% flatten() %>% 
  map(nchar, type = "width")

#### リストの要素を参照し、データフレームとして値を格納する
name.holder %>% 
  map_df(~ .[c("First", "Last")])

#### 前述の例は ~ (チルダ記号)を無名関数のエイリアスとして記述したもの
name.holder %>% map_df(
  function(x) {
    x[c("First", "Last")]
    })


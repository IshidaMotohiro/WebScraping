# 『Rによるスクレイピング入門』 第6章
## ----chapter06 section-029 身近なオープンデータ

### 横浜市のオープンデータ
library(rvest)
#### データ操作のためにmagrittr, dplyrを読み込む
library(magrittr)
library(dplyr)


# df.table <- read_html("http://www.city.yokohama.lg.jp/seisaku/seisaku/opendata/catalog.html") %>% 
#   html_table(header = TRUE) %>% 
#   extract(1)

# head(df.table)

# df.table %>% filter(grepl("区別将来人口推計", データ名))

#### 区別将来人口推計(xls)
#### http://archive.city.yokohama.lg.jp/seisaku/seisaku/chousa/kihou/175/data.html
library(rio)
#### ファイルを読み込み、必要な列だけを選択する
df.pop.forecast <-
  rio::import("http://archive.city.yokohama.lg.jp/seisaku/seisaku/chousa/kihou/175/opendata/kihou175-p15-z6.xls",
              skip = 5,
              range = "G6:M24") %>%
  rename(Ward = `...1`)

# df.pop.forecast %<>% select(Ward = `NA`, everything())
head(df.pop.forecast)

library(tidyr)
df.pop.forecast %<>% gather(year, value, -Ward)
head(df.pop.forecast)

library(ggplot2)
#### 日本語フォントを表示させるための設定
quartzFonts(YuGo = quartzFont(rep("IPAexGothic", 4)))
theme_set(theme_classic(base_size = 12, base_family = "IPAexGothic"))
df.pop.forecast %>% 
  ggplot(aes(year, value, group = Ward, color = Ward)) + 
  geom_line() + 
  xlab("年") + ylab("将来人口") + 
  ggtitle("横浜市 区別将来人口推計")

### 郵便局
download.file(url      = "http://www.post.japanpost.jp/zipcode/dl/roman/ken_all_rome.zip",
              destfile = "ken_all_rome.zip")
unzip(zipfile = "ken_all_rome.zip", overwrite = TRUE)
#### 解凍したファイルの存在を確認します
path2file <- list.files(getwd(), pattern = ".csv$", full.names = TRUE, ignore.case = TRUE)
path2file

library(readr)

df.address <- read_csv("KEN_ALL_ROME.CSV",
                       locale = locale(encoding = "cp932"),
                       col_names = c("郵便番号", "都道府県名", "市区町村名", "町域名", "都道府県名roman", "市区町村名roman", "町域名roman"))

df.address %>% 
  select(都道府県名, 市区町村名) %>% 
  unique() %>% 
  count(都道府県名, sort = TRUE)

## ----chapter06 section-030 LinkDataの活用事例

### 福井県福井市地区別人口の推移
source("http://linkdata.org/api/1/rdf1s4022i/R")

class(chikubetsu201601)

library(dplyr)

glimpse(chikubetsu201601)

library(tidyr)

chikubetsu201601 %>% gather(
  key = obs_month,
  value = row_num,
  chikubetsu201601
) %>% head()

library(purrr)

df.chikubetu <- list(chikubetsu201601,chikubetsu201602, chikubetsu201603, chikubetsu201604,
                     chikubetsu201605, chikubetsu201606, chikubetsu201607, chikubetsu201608,
                     chikubetsu201609, chikubetsu201610)

#### chikubetsu で始まる変数名をgather()のキーとして扱う
df.chikubetu.bind <- df.chikubetu %>% map_df(gather, key = obs_month, value = row_num, starts_with("chikubetsu"))

dim(df.chikubetu.bind)

unique(df.chikubetu.bind$地区名称)
d <- df.chikubetu.bind %>% 
  filter(地区名称 %in% c("社北", "麻生津", "円山")) %>% 
  mutate(obs_month = paste0(substr(obs_month, start = 15, stop = 16), "月")) %>% 
  select(地区名称, 合計, obs_month)

library(ggplot2)

quartzFonts(YuGo = quartzFont(rep("IPAexGothic", 4)))
theme_set(theme_classic(base_size = 12, base_family = "IPAexGothic"))
d %>% ggplot(aes(obs_month, 合計, group = 地区名称)) +
  geom_point() +
  geom_line(aes(linetype = 地区名称)) +
  xlab("月") + ylab("世帯数合計") +
  ggtitle("福井市内3地区の人口数の推移")

### 富山県砺波市へのふるさと納税者コメント
source("http://linkdata.org/api/1/rdf1s4456i/R")
class(hometown_donation_comment)

library(RMeCab)
hometown_donation_comment$コメント[1] %>% RMeCabC() %>% unlist()

comment.morph <- hometown_donation_comment %>% docDF("コメント", type = 1, pos = "名詞")
head(comment.morph[1:8])

comment.morph$count <- comment.morph %>% 
  select(starts_with("Row")) %>% 
  t() %>% 
  as.data.frame() %>% map_int(sum)

comment.morph %>% 
  arrange(desc(count)) %>% 
  select(TERM, count) %>% 
  filter(count >= 10)

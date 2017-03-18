# 『Rによるスクレイピング入門』 第2章

## ----chapter02 section-006

install.packages("rvest")
library(rvest)

simple <- read_html("https://IshidaMotohiro.github.io/sample_check/simple.html")

simple

simple %>% html_nodes("h1")

simple %>% html_nodes("h1") %>% html_text() 

simple %>% html_nodes("a") %>% html_attrs()

simple %>% html_nodes("a") %>% html_attr("href")

library(magrittr)
simple %>% html_nodes("a") %>% html_attr("href") %>% extract(2)

simple %>% html_node("a")

## ----chapter02 section-007  CSS

simple2 <- read_html("https://IshidaMotohiro.github.io/sample_check/simple2.html")
simple2

simple2 %>% html_nodes(css = ".green") 
simple2 %>% html_nodes(css = "#red") 

simple2 %>% html_nodes("body #red")  %>% html_text()

## ----chapter02 section-008  XPath

simple2 %>% html_nodes(xpath = "/html/body/p")

simple2 %>% html_nodes(xpath = "/html/body/div/p")

simple2 %>% html_nodes(xpath = "//p")

simple2 %>% html_nodes(xpath = "//p") %>% html_text()

simple2 %>% html_nodes(xpath = "//body/p")

simple2 %>% html_nodes(xpath = "//body//p")

simple2 %>% html_nodes(xpath = "//body/p")
simple2 %>% html_nodes(xpath = "//body/*/p")

simple2 %>% html_nodes(xpath = "body/p")
simple2 %>% html_nodes(xpath = "body/*/p")


simple2 %>% html_nodes(css = "body > p")
simple2 %>% html_nodes(css = "body p")

###  属性の指定

simple3 <- read_html("https://IshidaMotohiro.github.io/sample_check/attrs.html")

simple3 %>% html_nodes(xpath = "//a[@target]")
simple3 %>% html_nodes(css = "a[target]")

simple3 %>% html_nodes(xpath = "//p[@class = 'green']")
simple3 %>% html_nodes(css = "p[class = 'green']")
simple3 %>% html_nodes(css = "p.green")
#### この例では simple3 %>% html_nodes(css = ".green") でも同じ結果となる

simple3 %>% html_nodes(xpath = "//p[@id = 'red']")
simple3 %>% html_nodes(css = "p[id = 'red']")
simple3 %>% html_nodes(css = "p#red")
#### この例では simple3 %>% html_nodes(css = "#red") でも同じ結果となる

### 表の抽出

dat   <- read_html("https://IshidaMotohiro.github.io/sample_check/xpath.html")

tbls  <- dat %>% html_table()

tbls

#### 添字を使う

tbls[[1]]

#### magrittr パッケージを利用

library(magrittr)

tbls %>% extract2(1)


## ----chapter02 section-009 要素の検証と抽出階層の関係指定

simple3 %>% html_nodes(xpath = "//body/descendant::p")

simple3 %>% html_nodes(css  = "body p")

simple3 %>% html_nodes(xpath = "//p[contains(text(), '使った例')]")

simple3 %>% html_nodes(css = "p:contains('使った例')")

simple3 %>% html_nodes(xpath = "//a[starts-with(@href, 'https')]")

simple3 %>% html_nodes(css = "a[href^='https']")

###  ブラウザの開発ツール

x <- read_html("https://ishidamotohiro.github.io/sample_check/simple.html")

html_nodes(x, css = "body > a:nth-child(2)")


## ----chapter02 section-010 データ構造


### 文字コード
charToRaw("あ")

charToRaw("あA")


a <- "あ"

a <- iconv(a, to = "UTF-8")

a

a1 <- "あ"

charToRaw(a1)

a2  <- iconv ("あ", to = "CP932")

charToRaw(a2)

a1 == a2

library(rvest)

library(dplyr)

x <- read_html("https://IshidaMotohiro.github.io/sample_check/simple3.html")

x %>% html_node(xpath = "//meta[@content | @charset]")

y <- read_html("http://rmecab.jp/R/sjis.html", encoding = "CP932")

y <- read_html("http://rmecab.jp/R/sjis.html")

guess_encoding(y)


# パッケージをインストールする
# install.packages("stringr")
library(stringi)

y %>% html_nodes("h1") %>% html_text()

# Windowsの場合は最後に文字コードの変換が必要な場合があります
# y %>% html_nodes("h1") %>% html_text()) %>% stri_conv(to = "CP932") 
  

## ----chapter02 section-011 XML

library(rvest)

authors <- read_xml("https://IshidaMotohiro.github.io/sample_check/authors.xml")

authors %>% xml_nodes(xpath = "//lastname")

authors %>% xml_nodes(xpath = "//lastname") %>% xml_text()

### 名前空間

ns <- read_xml("https://IshidaMotohiro.github.io/sample_check/namespace.xml")

ns %>% xml_nodes(xpath = "//lastname")

xml_ns(ns)

ns %>% xml_nodes(xpath = "//d1:lastname")


## ----chapter02 section-012 JSON

#### パッケージのインストール
install.packages("jsonlite")

library(jsonlite)

member <- fromJSON("https://IshidaMotohiro.github.io/sample_check/authors.json")

member

member[[1]]

member$authors


## ----chapter02 section-013 httr パッケージと XML パッケージ

###  httr パッケージ
library(httr)

res <- GET("https://IshidaMotohiro.github.io/sample_check/xpath.html")

res

res %>% content()

res %>% headers() %>% head()

###  XML パッケージ

install.packages("XML")

library(XML)

#### DOM
res1 <- htmlParse("http://rmecab.jp/R/test.html")

res1 %>% class()

#### リスト
res2 <- htmlTreeParse("http://rmecab.jp/R/test.html")

res2 %>% class()


#### DOM
res1R <- xmlRoot(res1)

#### リスト
res2R <- xmlRoot(res2)

#### DOM
res1R %>% getNodeSet("//a")

res1R %>% xpathSApply("//a", xmlValue) 

#### リスト
res2R[[2]]["a", all = TRUE] %>% sapply(xmlValue) # %>% iconv(from = "UTF-8")

#### DOM
res1 %>% getHTMLLinks()

res1 %>% xpathSApply("//a", xmlAttrs) 

res1 %>% xpathSApply("//a", xmlGetAttr, "href") 

#### リスト 
res2R[[2]]["a", all = TRUE] %>% sapply(xmlGetAttr, "href")

tab <- readHTMLTable("http://rmecab.jp/R/test.html")

str(tab) #### Windows環境では，この行を実行するとエラーになることがあります

tab[[2]]

#### データ構造の確認
class(tab[[2]])

library(magrittr)

tab %>% extract2(2)

#### DOM オブジェクトとして取得
res3 <- xmlParse("http://rmecab.jp/R/authors.xml")

res3 %>% xpathSApply("//lastname", xmlValue)

xmlToDataFrame("http://rmecab.jp/R/authors.xml")

library(RCurl)

res1 <- htmlParse(getURL("https://IshidaMotohiro.github.io/sample_check/xpath.html"))

res1 %>% class()

res1


## ----chapter02 section-014 正規表現

### 文字列の検索
bun <- c("今年は2017年です", "これは平成29年になります。", "干支は酉。すなわちトリです．")

grep("1", bun)

grep("1", bun, value = TRUE)

grepl("1", bun)


### 文字列の置換
gsub("．", "。", bun)

install.packages("stringr")

library(stringr)

str_replace_all(bun, "．", "。")

str_detect(bun, "1")

str_subset(bun, "1")


### 正規表現
grep("7|9", bun, value = TRUE)

grep("0|1|2|3|4|5|6|7|8|9", bun, value = TRUE)

grep("[0-9]", bun, value = TRUE)

grep("[0-9][0-9][0-9][0-9]", bun, value = TRUE)

grep("\\d\\d\\d\\d", bun, value = TRUE)

grep("。", bun, value = TRUE)

grep("。$", bun, value = TRUE)

str_replace_all(bun, "\\d{4}", "----")

library(stringi)

stri_replace_all_regex("あいうえお", c("あ","う"), c("ア","ウ"), vectorize_all = FALSE)
# stri_replace_all_regex("あいうえお", c("あ","う"), c("ア","ウ"), vectorize_all = TRUE)

### 文字クラス
sentences <- "URL is http://rmecab.jp"

str_extract(sentences, "https?://[^[:space:]]*")

sentences <- "URLはhttp://rmecab.jpです"

str_extract(sentences, "https?://[^[:space:]]*")

str_extract(sentences, "https?://[a-zA-Z0-9:/?#\\[\\]@!$&'()*+,;=\\-._~%]+")   

kawabata <- "トンネルを抜けると     そこは
	雪国だった。"

kawabata

str_replace_all(kawabata, "\\s", "")


# 『Rによるスクレイピング入門』 第2章

## ----chapter02 section-015 HTTP

### HTTPとは
req <- c(
  "GET /index.html HTTP/1.1",
  "Host: example.com",
  ""
)

con <- socketConnection("example.com", port = 80)
writeLines(req, con)

res <- readLines(con)
close(con)

cat(res, sep = "\n")

### HTTPリクエスト
library(httr)
GET("http://example.com/index.html")

GET(url = "", scheme = "http", hostname = "example.com", path = "/index.html")

GET("http://example.com/index.html?name=apple&attr2=gorilla&attr3=rapper&lang=日本語")

GET("http://example.com/index.html",
   query = list(attr1 = "apple", attr2 = "gorilla", attr3 = "rapper", lang = "日本語"))

URLencode(enc2utf8("日本語"))

#### GETメソッドでリクエストを送信する
GET("http://example.com/users")

#### POSTメソッドでリクエストを送信する
POST("http://example.com/users")

#### 前掲の例と同じくGETメソッドのリクエストを送信する
VERB("GET", "http://example.com/users")

#### 特別な関数が用意されていないUPDATEメソッドのリクエストを送信する
VERB("UPDATE", "http://example.com/users")

POST("http://example.com", add_headers(`Content-Type` = "text/plain; charset=UTF-8"), body = enc2utf8("テスト"))

POST("http://httpbin.org/post", body = "test")

POST("http://httpbin.org/post", body = "test", content_type("text/plain"))

POST("http://httpbin.org/post", body = list(keyword = "test"))

POST("http://httpbin.org/post", body = upload_file("/path/to/file"))

POST("http://httpbin.org/post", body = list(attachment = upload_file("/path/to/file")))

POST("http://httpbin.org/post", body = list(keyword = "test"), encode = "json")

POST("http://httpbin.org/post", body = '{"keyword": "test"}', content_type_json())

POST("http://httpbin.org/post", body = list(keyword = I("test")), encode = "json")

VERB("GET", "http://httpbin.org/get", body = "test")

### HTTPレスポンスの構造
res <- GET("http://example.com")
is(res)

status_code(res)

#### 常にステータスコード500を返すURL
res <- GET("http://httpbin.org/status/500")

if (400 <= status_code(res)) {
  stop("リクエストが失敗しています！")
}

message_for_status(res)
warn_for_status(res)

stop_for_status(res)

res <- GET("http://api.e-stat.go.jp/rest/2.1/app/json/getDataCatalog")
#### ステータスは200 OK
status_code(res)

#### contentは`response`オブジェクトからボディを取り出す関数
content(res)$GET_DATA_CATALOG$RESULT$ERROR_MSG

res <- GET("http://httpbin.org/get")
names(res$headers)

headers(res)$`content-type`
headers(res)$`Content-Type`

res <- GET("http://httpbin.org/robots.txt")
content(res)

res <- GET("http://httpbin.org/user-agent")

headers(res)$`content-type`

content(res)

library(jsonlite)

fromJSON('[1,2,3]', simplifyVector = FALSE)
fromJSON('[1,2,3]', simplifyVector = TRUE)

body_raw <- content(res, as = "text")
body_raw

fromJSON(body_raw, simplifyVector = TRUE)

content(res, simplifyVector = TRUE)


# 『Rによるスクレイピング入門』 第2章

## ----chapter02 section-016 認証

### Basic認証/Digest認証
library(openssl)
base64_encode("user:passwd")

#### デフォルトはBasic認証
GET("http://httpbin.org/basic-auth/user/passwd",
    authenticate("user", "passwd"))

#### ほかの認証方式はtype引数を指定する
GET("http://httpbin.org/digest-auth/1/user/passwd",
    authenticate("user", "passwd", type = "digest"))

### OAuth認証
library(httr)

github_endpoint <- oauth_endpoint(
  authorize = "https://github.com/login/oauth/authorize",
  access = "https://github.com/login/oauth/access_token"
)

github_app <- oauth_app(
  appname = "my_dummy_application",
  key = "abcdefgh",
  secret = "12345678"
)

github_token <- oauth2.0_token(github_endpoint, github_app)

GET("https://api.github.com/rate_limit", config = config(token = github_token))

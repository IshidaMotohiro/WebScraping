# 『Rによるスクレイピング入門』 第1章

## ----chapter01 section-003 オブジェクト

### オブジェクト
x <-1:10
x

y

### ベクトル
y <- c(1, 20, 100)
y
z <- c("あ", "いう", "えおか")
z

xyz <- c(1, "1", 2, "二", 3)
xyz

options(width = 50)
letters
letters[23]

### 関数
sum(1:10)

?sum


### データフレーム
df1 <- data.frame(Name = c("犬","猫","猿","雉"),  Japanese = c(55, 66, 77, 88))
df1
df2 <- data.frame(ALPHABET = LETTERS[1:5], alphabet = letters[1:5])
head(df2 )

### リスト
dflist <- list(DF1 = df1, DF2 = df2)
dflist

#### 添字を使う
dflist[[1]]
#### 名前を使う
dflist$DF1

### リストの操作
myList <- list (A = 1:10, B = 101:110, C = 1001:1010)
myList

lapply(myList, mean)

sapply(myList, mean)

alphabet <- list(BIG = LETTERS, small = letters)
alphabet

lapply(alphabet, `[`, 3)

#### リストをデータフレームに変換する
myDF <- do.call("data.frame", myList)
myDF
class(myDF)

df1 <- data.frame(Name = c("犬","猫"),  Japanese = c(55, 66))
df1
df2 <- data.frame(Name = c("猿","雉"),  Japanese = c(77, 88))
df2

do.call("rbind", list(df1, df2)) # この場合単に rbind(df1, df2) で良い

#### 効率性について
x <- 1:10
tmp <- 0
for (i in x) {
    tmp <- tmp + i
}
tmp

x <- 1:10
sum(x)

mySum <- function(vec) {
    tmp <- 0
    for (i in vec){
        tmp <- tmp + i
    }
    tmp
}

x <- 1:10
mySum(x)

x <- 2
if(x != 0) {
    print ("not 0")
}else{
    print ("It's 0")
}


## ----chapter-01 section-004 R パッケージの導入

### パッケージの導入
install.packages("dplyr")

### パッケージの読み込み
library(dplyr)


### 従来の方法
head(ToothGrowth)

?ToothGrowth

head(ToothGrowth[ToothGrowth$supp == "OJ", ])

head(subset(ToothGrowth, supp == "OJ"))

### %>% 演算子 
ToothGrowth %>% filter(supp == "OJ") %>% head()

### データフレームを操作する主な関数
ToothGrowth %>% select(length = len, supp_type = supp) %>% head()

ToothGrowth %>% filter(len > 25, dose  == 1)

ToothGrowth  %>% mutate(len2 = len * 0.039) %>% head()

library(magrittr)
ToothGrowth %<>% mutate(len = len * 0.039) %>% head()

dflist %>% extract2(1)

dflist %>% "[["(1)
#### 上記の処理は dflist[[1]]  と同じこと

vignette("introduction", package = "dplyr")

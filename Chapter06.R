# 『Rによるスクレイピング入門』 第6章
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

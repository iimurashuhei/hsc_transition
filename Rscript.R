#Chapter8 Rスクリプト========
#このファイルでは、Chapter8の分析を実行する再現可能なコードを記述しています。
#このファイルでのスクリプト実行は5つのステップから構成されます。
#（1）前処理
#（2）記述統計量と可視化
#（3）因子分析
#（4）潜在変化得点モデルベースの媒介分析


#ワーキングディレクトリの確認========
setwd("C:/Users/Iimura Shuhei/OneDrive - 中央大学/博士論文研究プロジェクト（高校移行発達プロジェクト）/実証研究章（調査計画書・質問紙・データ等）/第8章 高校移行期の発達的変化における感覚感受性と学校環境変化の影響/Chapter8_Project")
getwd()


#（1）前処理========


#前処理に必要なパッケージ読み込み
library(tidyverse)

#各時点のローデータ読み込み
DataSourceT1 <- read_csv("Time1_rawdata.csv", na = c(".", "")) #Time1のローデータ読み込み
DataSourceT2 <- read_csv("Time2_rawdata.csv", na = c(".", "")) #Time2のローデータ読み込み
head(DataSourceT1) #先頭6行確認
head(DataSourceT2) #先頭6行確認

#各時点のローデータの結合
DataSource <- full_join(DataSourceT1, DataSourceT2, by = "ID", na = c(".", ""))
  #full_join(データ1, データ2, by = 共通のキー変数名)
head(DataSource) #先頭6行確認
tail(DataSource) #末尾6行確認
names(DataSource) #変数名確認

#合計得点の算出と列の追加：インプットデータ作成（# 参考として徳岡さんの資料参考http://rpubs.com/t_macya/333965）
InputData <- DataSource %>% 
  dplyr::mutate(bis1_T2 = 5 - bis1r_T2) %>% #bis1r_T2を逆転しbis1_T2という列を追加
  dplyr::mutate(bis6_T2 = 5 - bis6r_T2) %>% #bis1r_T2を逆転しbis1_T2という列を追加

#HSCSの易興奮性得点算出（T1とT2）
  dplyr::mutate(eoe_mean_T1 = (hsc8_T1 + hsc6_T1 + hsc4_T1 + hsc9_T1+ hsc12_T1)/5, na.rm = TRUE) %>% #T1
  dplyr::mutate(eoe_mean_T2 = (hsc8_T2 + hsc6_T2 + hsc4_T2 + hsc9_T2+ hsc12_T2)/5, na.rm = TRUE) %>% #T2
  
#HSCSの低閾値得点算出（T1とT2）
  dplyr::mutate(lst_mean_T1 = (hsc2_T1 + hsc7_T1 + hsc11_T1)/3, na.rm = TRUE) %>% #T1
  dplyr::mutate(lst_mean_T2 = (hsc2_T2 + hsc7_T2 + hsc11_T2)/3, na.rm = TRUE) %>% #T2
  
#HSCSの美的感受性得点算出（T1とT2）
  dplyr::mutate(aes_mean_T1 = (hsc5_T1 + hsc10_T1 + hsc1_T1 + hsc3_T1)/4, na.rm = TRUE) %>% #T1
  dplyr::mutate(aes_mean_T2 = (hsc5_T2 + hsc10_T2 + hsc1_T2 + hsc3_T2)/4, na.rm = TRUE) %>% #T2
  
#HSCSの合計得点算出（T1とT2）
  dplyr::mutate(hsc_mean_T1 = (eoe_mean_T1 + lst_mean_T1 + aes_mean_T1)/3, na.rm = TRUE) %>% #T1
  dplyr::mutate(hsc_mean_T2 = (eoe_mean_T2 + lst_mean_T2 + aes_mean_T2)/3, na.rm = TRUE) %>% #T2

#精神的健康の合計得点算出（T1とT2）
  dplyr::mutate(health_mean_T1 = (health1_T1 + health2_T1 + health2_T1 + health4_T1 + health5_T1)/5, na.rm = TRUE) %>% #T1
  dplyr::mutate(health_mean_T2 = (health1_T2 + health2_T2 + health2_T2 + health4_T2 + health5_T2)/5, na.rm = TRUE) %>% #T2

#PANASのpositive得点算出（T1）
  dplyr::mutate(positive_mean_T1 = (panas2_T1 + panas4_T1 + panas6_T1 + panas8_T1 + panas10_T1 + panas12_T1 + panas14_T1 + panas16_T1)/8, na.rm = TRUE) %>% #T1
  
#PANASのnegative得点算出（T1）
  dplyr::mutate(negative_mean_T1 = (panas1_T1 + panas3_T1 + panas5_T1 + panas7_T1 + panas9_T1 + panas11_T1 + panas13_T1 + panas15_T1)/8, na.rm = TRUE) %>% #T1
  
#学校環境変化尺度の合計得点算出（T2）
  dplyr::mutate(environment_mean_T2 = (environment1_T2 + environment2_T2 + environment3_T2 + environment4_T2 + environment5_T2 + environment6_T2 + environment7_T2 + environment8_T2 + environment9_T2 + environment10_T2 + environment11_T2)/11, na.rm = TRUE) %>% #T2
  
#BISの得点算出（T2）
  dplyr::mutate(bis_mean_T2 = (bis1_T2 + bis2_T2 + bis3_T2 + bis4_T2 + bis5_T2 + bis6_T2 + bis7_T2)/7, na.rm = TRUE) %>% #T2
  
#BASの得点算出（T2）
  dplyr::mutate(bas_mean_T2 = (bas1_T2 + bas2_T2 + bas3_T2 + bas4_T2 + bas5_T2 + bas6_T2 + bas7_T2 + bas8_T2 + bas9_T2 + bas10_T2 + bas11_T2 + bas12_T2 + bas13_T2)/7, na.rm = TRUE) #T2
  
#インプットデータの確認
head(InputData) #先頭6行確認
names(InputData) #変数名確認


#（2）記述統計量と可視化==========


###全変数の度数分布###

##楽に可視化したいなら以下のコードでGUI上のPlotlyタブをいじればOK！！（超便利）
library(ggplotgui)
ggplot_shiny(dataset = InputData)

##手作業で集計＆可視化するなら以下の通り

##性別の度数分布##

#child_gender_T1の度数分布
child_gender_count_T1 <- dplyr::count(InputData, child_gender_T1)
knitr::kable(child_gender_count_T1) #テーブル化
ggplot(data = InputData, mapping = aes(x = child_gender_T1, fill = factor(child_gender_T1))) + geom_bar() #視覚化

#child_gender_T2の度数分布
child_gender_count_T2 <- dplyr::count(InputData, child_gender_T2)
knitr::kable(child_gender_count_T2) #テーブル化
ggplot(data = InputData, mapping = aes(x = child_gender_T2, fill = factor(child_gender_T2))) + geom_bar() #視覚化

##HSPの度数分布##

#hsc1_T1の度数分布
hsc1_T1_count <- dplyr::count(InputData, hsc1_T1)
knitr::kable(hsc1_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = hsc1_T1, fill = factor(hsc1_T1))) + geom_histogram(binwidth = 1) #視覚化

#hsc2_T1の度数分布
hsc2_T1_count <- dplyr::count(InputData, hsc2_T1)
knitr::kable(hsc2_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = hsc2_T1, fill = factor(hsc2_T1))) + geom_histogram(binwidth = 1) #視覚化

#hsc3_T1の度数分布
hsc3_T1_count <- dplyr::count(InputData, hsc3_T1)
knitr::kable(hsc3_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = hsc3_T1, fill = factor(hsc3_T1))) + geom_histogram(binwidth = 1) #視覚化

#hsc4_T1の度数分布
hsc4_T1_count <- dplyr::count(InputData, hsc4_T1)
knitr::kable(hsc4_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = hsc4_T1, fill = factor(hsc4_T1))) + geom_histogram(binwidth = 1) #視覚化

#hsc5_T1の度数分布
hsc5_T1_count <- dplyr::count(InputData, hsc5_T1)
knitr::kable(hsc5_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = hsc5_T1, fill = factor(hsc5_T1))) + geom_histogram(binwidth = 1) #視覚化

#hsc6_T1の度数分布
hsc6_T1_count <- dplyr::count(InputData, hsc6_T1)
knitr::kable(hsc6_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = hsc6_T1, fill = factor(hsc6_T1))) + geom_histogram(binwidth = 1) #視覚化

#hsc7_T1の度数分布
hsc7_T1_count <- dplyr::count(InputData, hsc7_T1)
knitr::kable(hsc7_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = hsc7_T1, fill = factor(hsc7_T1))) + geom_histogram(binwidth = 1) #視覚化

#hsc8_T1の度数分布
hsc8_T1_count <- dplyr::count(InputData, hsc8_T1)
knitr::kable(hsc8_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = hsc8_T1, fill = factor(hsc8_T1))) + geom_histogram(binwidth = 1) #視覚化

#hsc9_T1の度数分布
hsc9_T1_count <- dplyr::count(InputData, hsc9_T1)
knitr::kable(hsc9_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = hsc9_T1, fill = factor(hsc9_T1))) + geom_histogram(binwidth = 1) #視覚化

#hsc10_T1の度数分布
hsc10_T1_count <- dplyr::count(InputData, hsc10_T1)
knitr::kable(hsc10_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = hsc10_T1, fill = factor(hsc10_T1))) + geom_histogram(binwidth = 1) #視覚化

#hsc11_T1の度数分布
hsc11_T1_count <- dplyr::count(InputData, hsc11_T1)
knitr::kable(hsc11_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = hsc11_T1, fill = factor(hsc11_T1))) + geom_histogram(binwidth = 1) #視覚化

#hsc12_T1の度数分布
hsc12_T1_count <- dplyr::count(InputData, hsc12_T1)
knitr::kable(hsc12_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = hsc12_T1, fill = factor(hsc12_T1))) + geom_histogram(binwidth = 1) #視覚化

#eoe_mean_T1の度数分布
eoe_T1_count <- dplyr::count(InputData, eoe_mean_T1)
knitr::kable(eoe_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = eoe_mean_T1, fill = factor(eoe_mean_T1))) + geom_histogram(binwidth = 0.2) #視覚化

#lst_mean_T1の度数分布
lst_T1_count <- dplyr::count(InputData, lst_mean_T1)
knitr::kable(lst_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = lst_mean_T1, fill = factor(lst_mean_T1))) + geom_histogram(binwidth = 0.4) #視覚化

#aes_mean_T1の度数分布
aes_T1_count <- dplyr::count(InputData, aes_mean_T1)
knitr::kable(aes_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = aes_mean_T1, fill = factor(aes_mean_T1))) + geom_histogram(binwidth = 0.3) #視覚化

#hsc_mean_T1の度数分布
hsc_T1_count <- dplyr::count(InputData, hsc_mean_T1)
knitr::kable(hsc_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = hsc_mean_T1, fill = factor(hsc_mean_T1))) + 
  geom_histogram(binwidth = 0.3) + guides(fill = "none") #視覚化

#hsc1_T2の度数分布
hsc1_T2_count <- dplyr::count(InputData, hsc1_T2)
knitr::kable(hsc1_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = hsc1_T2, fill = factor(hsc1_T2))) + geom_histogram(binwidth = 1) #視覚化

#hsc2_T2の度数分布
hsc2_T2_count <- dplyr::count(InputData, hsc2_T2)
knitr::kable(hsc2_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = hsc2_T2, fill = factor(hsc2_T2))) + geom_histogram(binwidth = 1) #視覚化

#hsc3_T2の度数分布
hsc3_T2_count <- dplyr::count(InputData, hsc3_T2)
knitr::kable(hsc3_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = hsc3_T2, fill = factor(hsc3_T2))) + geom_histogram(binwidth = 1) #視覚化

#hsc4_T2の度数分布
hsc4_T2_count <- dplyr::count(InputData, hsc4_T2)
knitr::kable(hsc4_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = hsc4_T2, fill = factor(hsc4_T2))) + geom_histogram(binwidth = 1) #視覚化

#hsc5_T2の度数分布
hsc5_T2_count <- dplyr::count(InputData, hsc5_T2)
knitr::kable(hsc5_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = hsc5_T2, fill = factor(hsc5_T2))) + geom_histogram(binwidth = 1) #視覚化

#hsc6_T2の度数分布
hsc6_T2_count <- dplyr::count(InputData, hsc6_T2)
knitr::kable(hsc6_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = hsc6_T2, fill = factor(hsc6_T2))) + geom_histogram(binwidth = 1) #視覚化

#hsc7_T2の度数分布
hsc7_T2_count <- dplyr::count(InputData, hsc7_T2)
knitr::kable(hsc7_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = hsc7_T2, fill = factor(hsc7_T2))) + geom_histogram(binwidth = 1) #視覚化

#hsc8_T2の度数分布
hsc8_T2_count <- dplyr::count(InputData, hsc8_T2)
knitr::kable(hsc8_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = hsc8_T2, fill = factor(hsc8_T2))) + geom_histogram(binwidth = 1) #視覚化

#hsc9_T2の度数分布
hsc9_T2_count <- dplyr::count(InputData, hsc9_T2)
knitr::kable(hsc9_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = hsc9_T2, fill = factor(hsc9_T2))) + geom_histogram(binwidth = 1) #視覚化

#hsc10_T2の度数分布
hsc10_T2_count <- dplyr::count(InputData, hsc10_T2)
knitr::kable(hsc10_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = hsc10_T2, fill = factor(hsc10_T2))) + geom_histogram(binwidth = 1) #視覚化

#hsc11_T2の度数分布
hsc11_T2_count <- dplyr::count(InputData, hsc11_T2)
knitr::kable(hsc11_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = hsc11_T2, fill = factor(hsc11_T2))) + geom_histogram(binwidth = 1) #視覚化

#hsc12_T2の度数分布
hsc12_T2_count <- dplyr::count(InputData, hsc12_T2)
knitr::kable(hsc12_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = hsc12_T2, fill = factor(hsc12_T2))) + geom_histogram(binwidth = 1) #視覚化

#eoe_mean_T2の度数分布
eoe_T2_count <- dplyr::count(InputData, eoe_mean_T2)
knitr::kable(eoe_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = eoe_mean_T2, fill = factor(eoe_mean_T2))) + geom_histogram(binwidth = 0.2) #視覚化

#lst_mean_T2の度数分布
lst_T2_count <- dplyr::count(InputData, lst_mean_T2)
knitr::kable(lst_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = lst_mean_T2, fill = factor(lst_mean_T2))) + geom_histogram(binwidth = 0.4) #視覚化

#aes_mean_T2の度数分布
aes_T2_count <- dplyr::count(InputData, aes_mean_T2)
knitr::kable(aes_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = aes_mean_T2, fill = factor(aes_mean_T2))) + geom_histogram(binwidth = 0.3) #視覚化

#hsc_mean_T2の度数分布
hsc_T2_count <- dplyr::count(InputData, hsc_mean_T2)
knitr::kable(hsc_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = hsc_mean_T2, fill = factor(hsc_mean_T2))) + 
  geom_histogram(binwidth = 0.3) + guides(fill = "none") #視覚化

##精神的健康の度数分布##

#health1_T1の度数分布
health1_T1_count <- dplyr::count(InputData, health1_T1)
knitr::kable(health1_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = health1_T1, fill = factor(health1_T1))) + geom_histogram(binwidth = 1) #視覚化

#health2_T1の度数分布
health2_T1_count <- dplyr::count(InputData, health2_T1)
knitr::kable(health2_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = health2_T1, fill = factor(health2_T1))) + geom_histogram(binwidth = 1) #視覚化

#health3_T1の度数分布
health3_T1_count <- dplyr::count(InputData, health3_T1)
knitr::kable(health3_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = health3_T1, fill = factor(health3_T1))) + geom_histogram(binwidth = 1) #視覚化

#health4_T1の度数分布
health4_T1_count <- dplyr::count(InputData, health4_T1)
knitr::kable(health4_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = health4_T1, fill = factor(health4_T1))) + geom_histogram(binwidth = 1) #視覚化

#health5_T1の度数分布
health5_T1_count <- dplyr::count(InputData, health5_T1)
knitr::kable(health5_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = health5_T1, fill = factor(health5_T1))) + geom_histogram(binwidth = 1) #視覚化

#health_mean_T1の度数分布
health_mean_T1_count <- dplyr::count(InputData, health_mean_T1)
knitr::kable(health_mean_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = health_mean_T1, fill = factor(health_mean_T1))) + geom_histogram(binwidth = 0.2) #視覚化

#health1_T2の度数分布
health1_T2_count <- dplyr::count(InputData, health1_T2)
knitr::kable(health1_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = health1_T2, fill = factor(health1_T2))) + geom_histogram(binwidth = 1) #視覚化

#health2_T2の度数分布
health2_T2_count <- dplyr::count(InputData, health2_T2)
knitr::kable(health2_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = health2_T2, fill = factor(health2_T2))) + geom_histogram(binwidth = 1) #視覚化

#health3_T2の度数分布
health3_T2_count <- dplyr::count(InputData, health3_T2)
knitr::kable(health3_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = health3_T2, fill = factor(health3_T2))) + geom_histogram(binwidth = 1) #視覚化

#health4_T2の度数分布
health4_T2_count <- dplyr::count(InputData, health4_T2)
knitr::kable(health4_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = health4_T2, fill = factor(health4_T2))) + geom_histogram(binwidth = 1) #視覚化

#health5_T2の度数分布
health5_T2_count <- dplyr::count(InputData, health5_T2)
knitr::kable(health5_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = health5_T2, fill = factor(health5_T2))) + geom_histogram(binwidth = 1) #視覚化

#health_mean_T2の度数分布
health_mean_T2_count <- dplyr::count(InputData, health_mean_T2)
knitr::kable(health_mean_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = health_mean_T2, fill = factor(health_mean_T2))) + geom_histogram(binwidth = 0.2) #視覚化

##PANASの度数分布##

#panas1_T1の度数分布
panas1_T1_count <- dplyr::count(InputData, panas1_T1)
knitr::kable(panas1_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = panas1_T1, fill = factor(panas1_T1))) + geom_histogram(binwidth = 1) #視覚化

#panas2_T1の度数分布
panas2_T1_count <- dplyr::count(InputData, panas2_T1)
knitr::kable(panas2_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = panas2_T1, fill = factor(panas2_T1))) + geom_histogram(binwidth = 1) #視覚化

#panas3_T1の度数分布
panas3_T1_count <- dplyr::count(InputData, panas3_T1)
knitr::kable(panas3_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = panas3_T1, fill = factor(panas3_T1))) + geom_histogram(binwidth = 1) #視覚化

#panas4_T1の度数分布
panas4_T1_count <- dplyr::count(InputData, panas4_T1)
knitr::kable(panas4_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = panas4_T1, fill = factor(panas4_T1))) + geom_histogram(binwidth = 1) #視覚化

#panas5_T1の度数分布
panas5_T1_count <- dplyr::count(InputData, panas5_T1)
knitr::kable(panas5_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = panas5_T1, fill = factor(panas5_T1))) + geom_histogram(binwidth = 1) #視覚化

#panas6_T1の度数分布
panas6_T1_count <- dplyr::count(InputData, panas6_T1)
knitr::kable(panas6_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = panas6_T1, fill = factor(panas6_T1))) + geom_histogram(binwidth = 1) #視覚化

#panas7_T1の度数分布
panas7_T1_count <- dplyr::count(InputData, panas7_T1)
knitr::kable(panas7_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = panas7_T1, fill = factor(panas7_T1))) + geom_histogram(binwidth = 1) #視覚化

#panas8_T1の度数分布
panas8_T1_count <- dplyr::count(InputData, panas8_T1)
knitr::kable(panas8_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = panas8_T1, fill = factor(panas8_T1))) + geom_histogram(binwidth = 1) #視覚化

#panas9_T1の度数分布
panas9_T1_count <- dplyr::count(InputData, panas9_T1)
knitr::kable(panas9_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = panas9_T1, fill = factor(panas9_T1))) + geom_histogram(binwidth = 1) #視覚化

#panas10_T1の度数分布
panas10_T1_count <- dplyr::count(InputData, panas10_T1)
knitr::kable(panas10_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = panas10_T1, fill = factor(panas10_T1))) + geom_histogram(binwidth = 1) #視覚化

#panas11_T1の度数分布
panas11_T1_count <- dplyr::count(InputData, panas11_T1)
knitr::kable(panas11_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = panas11_T1, fill = factor(panas11_T1))) + geom_histogram(binwidth = 1) #視覚化

#panas12_T1の度数分布
panas12_T1_count <- dplyr::count(InputData, panas12_T1)
knitr::kable(panas12_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = panas12_T1, fill = factor(panas12_T1))) + geom_histogram(binwidth = 1) #視覚化

#panas13_T1の度数分布
panas13_T1_count <- dplyr::count(InputData, panas13_T1)
knitr::kable(panas13_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = panas13_T1, fill = factor(panas13_T1))) + geom_histogram(binwidth = 1) #視覚化

#panas14_T1の度数分布
panas14_T1_count <- dplyr::count(InputData, panas14_T1)
knitr::kable(panas14_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = panas14_T1, fill = factor(panas14_T1))) + geom_histogram(binwidth = 1) #視覚化

#panas15_T1の度数分布
panas15_T1_count <- dplyr::count(InputData, panas15_T1)
knitr::kable(panas15_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = panas15_T1, fill = factor(panas15_T1))) + geom_histogram(binwidth = 1) #視覚化

#panas16_T1の度数分布
panas16_T1_count <- dplyr::count(InputData, panas16_T1)
knitr::kable(panas16_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = panas16_T1, fill = factor(panas16_T1))) + geom_histogram(binwidth = 1) #視覚化

#positive_T1の度数分布
positive_T1_count <- dplyr::count(InputData, positive_mean_T1)
knitr::kable(positive_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = positive_mean_T1, fill = factor(positive_mean_T1))) + geom_histogram(binwidth = 0.3) #視覚化

#negative_T1の度数分布
negative_T1_count <- dplyr::count(InputData, negative_mean_T1)
knitr::kable(negative_T1_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = negative_mean_T1, fill = factor(negative_mean_T1))) + geom_histogram(binwidth = 0.2) #視覚化

##学校環境変化の度数分布##

#environment1_T2の度数分布
environmen1_T2_count <- dplyr::count(InputData, environment1_T2)
knitr::kable(environment1_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = environment1_T2, fill = factor(environment1_T2))) + geom_histogram(binwidth = 1) #視覚化

#environment2_T2の度数分布
environment2_T2_count <- dplyr::count(InputData, environment2_T2)
knitr::kable(environment2_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = environment2_T2, fill = factor(environment2_T2))) + geom_histogram(binwidth = 1) #視覚化

#environment3_T2の度数分布
environment3_T2_count <- dplyr::count(InputData, environment3_T2)
knitr::kable(environment3_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = environment3_T2, fill = factor(environment3_T2))) + geom_histogram(binwidth = 1) #視覚化

#environment4_T2の度数分布
environment4_T2_count <- dplyr::count(InputData, environment4_T2)
knitr::kable(environment4_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = environment4_T2, fill = factor(environment4_T2))) + geom_histogram(binwidth = 1) #視覚化

#environment5_T2の度数分布
environment5_T2_count <- dplyr::count(InputData, environment5_T2)
knitr::kable(environment5_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = environment5_T2, fill = factor(environment5_T2))) + geom_histogram(binwidth = 1) #視覚化

#environment6_T2の度数分布
environment6_T2_count <- dplyr::count(InputData, environment6_T2)
knitr::kable(environment6_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = environment6_T2, fill = factor(environment6_T2))) + geom_histogram(binwidth = 1) #視覚化

#environment7_T2の度数分布
environment7_T2_count <- dplyr::count(InputData, environment7_T2)
knitr::kable(environment7_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = environment7_T2, fill = factor(environment7_T2))) + geom_histogram(binwidth = 1) #視覚化

#environment8_T2の度数分布
environment8_T2_count <- dplyr::count(InputData, environment8_T2)
knitr::kable(environment8_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = environment8_T2, fill = factor(environment8_T2))) + geom_histogram(binwidth = 1) #視覚化

#environment9_T2の度数分布
environment9_T2_count <- dplyr::count(InputData, environment9_T2)
knitr::kable(environment9_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = environment9_T2, fill = factor(environment9_T2))) + geom_histogram(binwidth = 1) #視覚化

#environment10_T2の度数分布
environment10_T2_count <- dplyr::count(InputData, environment10_T2)
knitr::kable(environment10_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = environment10_T2, fill = factor(environment10_T2))) + geom_histogram(binwidth = 1) #視覚化

#environment11_T2の度数分布
environment11_T2_count <- dplyr::count(InputData, environment11_T2)
knitr::kable(environment11_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = environment11_T2, fill = factor(environment11_T2))) + geom_histogram(binwidth = 1) #視覚化

#environment11_T2の度数分布
environment_mean_T2_count <- dplyr::count(InputData, environment_mean_T2)
knitr::kable(environment_mean_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = environment_mean_T2, fill = factor(environment_mean_T2))) + 
  geom_histogram(binwidth = 0.2) +
  guides(fill = "none")#視覚化

##BISの度数分布##

#bis1_T2の度数分布
bis1_T2_count <- dplyr::count(InputData, bis1_T2)
knitr::kable(bis1_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = bis1_T2, fill = factor(bis1_T2))) + geom_histogram(binwidth = 1) #視覚化

#bis2_T2の度数分布
bis2_T2_count <- dplyr::count(InputData, bis2_T2)
knitr::kable(bis2_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = bis2_T2, fill = factor(bis2_T2))) + geom_histogram(binwidth = 1) #視覚化

#bis3_T2の度数分布
bis3_T2_count <- dplyr::count(InputData, bis3_T2)
knitr::kable(bis3_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = bis3_T2, fill = factor(bis3_T2))) + geom_histogram(binwidth = 1) #視覚化

#bis4_T2の度数分布
bis4_T2_count <- dplyr::count(InputData, bis4_T2)
knitr::kable(bis4_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = bis4_T2, fill = factor(bis4_T2))) + geom_histogram(binwidth = 1) #視覚化

#bis5_T2の度数分布
bis5_T2_count <- dplyr::count(InputData, bis5_T2)
knitr::kable(bis5_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = bis5_T2, fill = factor(bis5_T2))) + geom_histogram(binwidth = 1) #視覚化

#bis6_T2の度数分布
bis6_T2_count <- dplyr::count(InputData, bis6_T2)
knitr::kable(bis6_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = bis6_T2, fill = factor(bis6_T2))) + geom_histogram(binwidth = 1) #視覚化

#bis7_T2の度数分布
bis7_T2_count <- dplyr::count(InputData, bis7_T2)
knitr::kable(bis7_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = bis7_T2, fill = factor(bis7_T2))) + geom_histogram(binwidth = 1) #視覚化

#bis_mean_T2の度数分布
bis_mean_T2_count <- dplyr::count(InputData, bis_mean_T2)
knitr::kable(bis_mean_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = bis_mean_T2, fill = factor(bis_mean_T2))) + 
  geom_histogram(binwidth = 0.1) + 
  guides(fill = "none") #視覚化

##BASの度数分布##

#bas1_T2の度数分布
bas1_T2_count <- dplyr::count(InputData, bas1_T2)
knitr::kable(bas1_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = bas1_T2, fill = factor(bas1_T2))) + geom_histogram(binwidth = 1) #視覚化

#bas2_T2の度数分布
bas2_T2_count <- dplyr::count(InputData, bas2_T2)
knitr::kable(bas2_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = bas2_T2, fill = factor(bas2_T2))) + geom_histogram(binwidth = 1) #視覚化

#bas3_T2の度数分布
bas3_T2_count <- dplyr::count(InputData, bas3_T2)
knitr::kable(bas3_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = bas3_T2, fill = factor(bas3_T2))) + geom_histogram(binwidth = 1) #視覚化

#bas4_T2の度数分布
bas4_T2_count <- dplyr::count(InputData, bas4_T2)
knitr::kable(bas4_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = bas4_T2, fill = factor(bas4_T2))) + geom_histogram(binwidth = 1) #視覚化

#bas5_T2の度数分布
bas5_T2_count <- dplyr::count(InputData, bas5_T2)
knitr::kable(bas5_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = bas5_T2, fill = factor(bas5_T2))) + geom_histogram(binwidth = 1) #視覚化

#bas6_T2の度数分布
bas6_T2_count <- dplyr::count(InputData, bas6_T2)
knitr::kable(bas6_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = bas6_T2, fill = factor(bas6_T2))) + geom_histogram(binwidth = 1) #視覚化

#bas7_T2の度数分布
bas7_T2_count <- dplyr::count(InputData, bas7_T2)
knitr::kable(bas7_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = bas7_T2, fill = factor(bas7_T2))) + geom_histogram(binwidth = 1) #視覚化

#bas8_T2の度数分布
bas8_T2_count <- dplyr::count(InputData, bas8_T2)
knitr::kable(bas8_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = bas8_T2, fill = factor(bas8_T2))) + geom_histogram(binwidth = 1) #視覚化

#bas9_T2の度数分布
bas9_T2_count <- dplyr::count(InputData, bas9_T2)
knitr::kable(bas9_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = bas9_T2, fill = factor(bas9_T2))) + geom_histogram(binwidth = 1) #視覚化

#bas10_T2の度数分布
bas10_T2_count <- dplyr::count(InputData, bas10_T2)
knitr::kable(bas10_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = bas10_T2, fill = factor(bas10_T2))) + geom_histogram(binwidth = 1) #視覚化

#bas11_T2の度数分布
bas11_T2_count <- dplyr::count(InputData, bas11_T2)
knitr::kable(bas11_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = bas11_T2, fill = factor(bas11_T2))) + geom_histogram(binwidth = 1) #視覚化

#bas12_T2の度数分布
bas12_T2_count <- dplyr::count(InputData, bas12_T2)
knitr::kable(bas12_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = bas12_T2, fill = factor(bas12_T2))) + geom_histogram(binwidth = 1) #視覚化

#bas13_T2の度数分布
bas13_T2_count <- dplyr::count(InputData, bas13_T2)
knitr::kable(bas13_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = bas13_T2, fill = factor(bas13_T2))) + geom_histogram(binwidth = 1) #視覚化

#bas_mean_T2の度数分布
bas_mean_T2_count <- dplyr::count(InputData, bas_mean_T2)
knitr::kable(bas_mean_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = bas_mean_T2, fill = factor(bas_mean_T2))) + 
  geom_histogram(binwidth = 0.2) + 
  guides(fill = "none") #視覚化

###全変数の記述統計量###

summary(InputData) #一気にポン（SDは出力されないが…）

##HSC_T1の記述統計量##

hsc_T1_discriptive <- 
  InputData %>% 
  dplyr::summarise(n = n (), #グループの人数を出力
                   hsc1.T1.mean = mean (hsc1_T1), #hsc1_T1の平均
                   hsc1.T1.sd = sd (hsc1_T1), #hsc1_T1のSD
                   hsc2.T1.mean = mean (hsc2_T1), 
                   hsc2.T1.sd = sd (hsc2_T1),
                   hsc3.T1.mean = mean (hsc3_T1), 
                   hsc3.T1.sd = sd (hsc3_T1),
                   hsc4.T1.mean = mean (hsc4_T1), 
                   hsc4.T1.sd = sd (hsc4_T1),
                   hsc5.T1.mean = mean (hsc5_T1), 
                   hsc5.T1.sd = sd (hsc5_T1),
                   hsc6.T1.mean = mean (hsc6_T1), 
                   hsc6.T1.sd = sd (hsc6_T1),
                   hsc7.T1.mean = mean (hsc7_T1), 
                   hsc7.T1.sd = sd (hsc7_T1),
                   hsc8.T1.mean = mean (hsc8_T1), 
                   hsc8.T1.sd = sd (hsc8_T1),
                   hsc9.T1.mean = mean (hsc9_T1), 
                   hsc9.T1.sd = sd (hsc9_T1),
                   hsc10.T1.mean = mean (hsc10_T1), 
                   hsc10.T1.sd = sd (hsc10_T1),
                   hsc11.T1.mean = mean (hsc11_T1), 
                   hsc11.T1.sd = sd (hsc11_T1),
                   hsc12.T1.mean = mean (hsc4_T1), 
                   hsc12.T1.sd = sd (hsc4_T1),
                   eoe.mean.T1 = mean (eoe_mean_T1),
                   eoe.sd.T1 = sd (eoe_mean_T1),
                   lst.mean.T1 = mean (lst_mean_T1),
                   lst.sd.T1 = sd (lst_mean_T1),
                   aes.mean.T1 = mean (aes_mean_T1),
                   aes.sd.T1 = sd (aes_mean_T1),
                   hsc.mean.T1 = mean (hsc_mean_T1),
                   hsc.sd.T1 = sd (hsc_mean_T1)) #あまり賢い記述じゃないww #ditydataにしたほうがbetter

knitr::kable(hsc_T1_discriptive) #出力

##HSC_T2の記述統計量##
hsc_T2_discriptive <- 
  InputData %>% 
  drop_na() %>% #T2は欠損値があるのでdrop_na()を挟む
  dplyr::summarise(n = n (), #グループの人数を出力
                   hsc1.T2.mean = mean (hsc1_T2), #hsc1_T2の平均
                   hsc1.T2.sd = sd (hsc1_T2), #hsc1_T2のSD
                   hsc2.T2.mean = mean (hsc2_T2), 
                   hsc2.T2.sd = sd (hsc2_T2),
                   hsc3.T2.mean = mean (hsc3_T2), 
                   hsc3.T2.sd = sd (hsc3_T2),
                   hsc4.T2.mean = mean (hsc4_T2), 
                   hsc4.T2.sd = sd (hsc4_T2),
                   hsc5.T2.mean = mean (hsc5_T2), 
                   hsc5.T2.sd = sd (hsc5_T2),
                   hsc6.T2.mean = mean (hsc6_T2), 
                   hsc6.T2.sd = sd (hsc6_T2),
                   hsc7.T2.mean = mean (hsc7_T2), 
                   hsc7.T2.sd = sd (hsc7_T2),
                   hsc8.T2.mean = mean (hsc8_T2), 
                   hsc8.T2.sd = sd (hsc8_T2),
                   hsc9.T2.mean = mean (hsc9_T2), 
                   hsc9.T2.sd = sd (hsc9_T2),
                   hsc10.T2.mean = mean (hsc10_T2), 
                   hsc10.T2.sd = sd (hsc10_T2),
                   hsc11.T2.mean = mean (hsc11_T2), 
                   hsc11.T2.sd = sd (hsc11_T2),
                   hsc12.T2.mean = mean (hsc4_T2), 
                   hsc12.T2.sd = sd (hsc4_T2),
                   eoe.mean.T2 = mean (eoe_mean_T2),
                   eoe.sd.T2 = sd (eoe_mean_T2),
                   lst.mean.T2 = mean (lst_mean_T2),
                   lst.sd.T2 = sd (lst_mean_T2),
                   aes.mean.T2 = mean (aes_mean_T2),
                   aes.sd.T2 = sd (aes_mean_T2),
                   hsc.mean.T2 = mean (hsc_mean_T2),
                   hsc.sd.T2 = sd (hsc_mean_T2)) #あまり賢い記述じゃないww #ditydataにしたほうがbetter

knitr::kable(hsc_T2_discriptive) #出力

##精神的健康_T1の記述統計量##
health_T1_discriptive <- 
  InputData %>% 
  dplyr::summarise(n = n (), #グループの人数を出力
                   health1.T1.mean = mean (health1_T1), #health1_T1の平均
                   health1.T1.sd = sd (health1_T1), #health1_T1のSD
                   health2.T1.mean = mean (health2_T1), 
                   health2.T1.sd = sd (health2_T1),
                   health3.T1.mean = mean (health3_T1), 
                   health3.T1.sd = sd (health3_T1),
                   health4.T1.mean = mean (health4_T1), 
                   health4.T1.sd = sd (health4_T1),
                   health5.T1.mean = mean (health5_T1), 
                   health5.T1.sd = sd (health5_T1),
                   health.mean.T1 = mean (health_mean_T1),
                   health.sd.T1 = sd (health_mean_T1))

knitr::kable(health_T1_discriptive) #出力

##精神的健康_T2の記述統計量##
health_T2_discriptive <- 
  InputData %>% 
  drop_na() %>%
  dplyr::summarise(n = n (), #グループの人数を出力
                   health1.T2.mean = mean (health1_T2), #health1_T2の平均
                   health1.T2.sd = sd (health1_T2), #health1_T2のSD
                   health2.T2.mean = mean (health2_T2), 
                   health2.T2.sd = sd (health2_T2),
                   health3.T2.mean = mean (health3_T2), 
                   health3.T2.sd = sd (health3_T2),
                   health4.T2.mean = mean (health4_T2), 
                   health4.T2.sd = sd (health4_T2),
                   health5.T2.mean = mean (health5_T2), 
                   health5.T2.sd = sd (health5_T2),
                   health.mean.T2 = mean (health_mean_T2),
                   health.sd.T2 = sd (health_mean_T2))

knitr::kable(health_T2_discriptive) #出力

##PANAS_T1の記述統計量##
panas_T1_discriptive <- 
  InputData %>% 
  dplyr::summarise(n = n (), #グループの人数を出力
                   panas1.T1.mean = mean (panas1_T1), #panas1_T1の平均
                   panas1.T1.sd = sd (panas1_T1), #panas1_T1のSD
                   panas2.T1.mean = mean (panas2_T1), 
                   panas2.T1.sd = sd (panas2_T1),
                   panas3.T1.mean = mean (panas3_T1), 
                   panas3.T1.sd = sd (panas3_T1),
                   panas4.T1.mean = mean (panas4_T1), 
                   panas4.T1.sd = sd (panas4_T1),
                   panas5.T1.mean = mean (panas5_T1), 
                   panas5.T1.sd = sd (panas5_T1),
                   panas6.T1.mean = mean (panas6_T1), 
                   panas6.T1.sd = sd (panas6_T1),
                   panas7.T1.mean = mean (panas7_T1), 
                   panas7.T1.sd = sd (panas7_T1),
                   panas8.T1.mean = mean (panas8_T1), 
                   panas8.T1.sd = sd (panas8_T1),
                   panas9.T1.mean = mean (panas9_T1), 
                   panas9.T1.sd = sd (panas9_T1),
                   panas10.T1.mean = mean (panas10_T1), 
                   panas10.T1.sd = sd (panas10_T1),
                   panas11.T1.mean = mean (panas11_T1), 
                   panas11.T1.sd = sd (panas11_T1),
                   panas12.T1.mean = mean (panas12_T1), 
                   panas12.T1.sd = sd (panas12_T1),
                   panas13.T1.mean = mean (panas13_T1), 
                   panas13.T1.sd = sd (panas13_T1),
                   panas14.T1.mean = mean (panas14_T1), 
                   panas14.T1.sd = sd (panas14_T1),
                   panas15.T1.mean = mean (panas15_T1), 
                   panas15.T1.sd = sd (panas15_T1),
                   panas16.T1.mean = mean (panas16_T1), 
                   panas16.T1.sd = sd (panas16_T1),
                   positive.mean.T1 = mean (positive_mean_T1),
                   positive.sd.T1 = sd (positive_mean_T1),
                   negative.mean.T1 = mean (negative_mean_T1),
                   negative.sd.T1 = sd (negative_mean_T1))

knitr::kable(panas_T1_discriptive) #出力

##学校環境変化_T2の記述統計量##
environment_T2_discriptive <- 
  InputData %>% 
  drop_na() %>%
  dplyr::summarise(n = n (), #グループの人数を出力
                   environment1.T2.mean = mean (environment1_T2), #environment1_T2の平均
                   environment1.T2.sd = sd (environment1_T2), #environment1_T2のSD
                   environment2.T2.mean = mean (environment2_T2), 
                   environment2.T2.sd = sd (environment2_T2),
                   environment3.T2.mean = mean (environment3_T2), 
                   environment3.T2.sd = sd (environment3_T2),
                   environment4.T2.mean = mean (environment4_T2), 
                   environment4.T2.sd = sd (environment4_T2),
                   environment5.T2.mean = mean (environment5_T2), 
                   environment5.T2.sd = sd (environment5_T2),
                   environment6.T2.mean = mean (environment6_T2), 
                   environment6.T2.sd = sd (environment6_T2),
                   environment7.T2.mean = mean (environment7_T2), 
                   environment7.T2.sd = sd (environment7_T2),
                   environment8.T2.mean = mean (environment8_T2), 
                   environment8.T2.sd = sd (environment8_T2),
                   environment9.T2.mean = mean (environment9_T2), 
                   environment9.T2.sd = sd (environment9_T2),
                   environment10.T2.mean = mean (environment10_T2), 
                   environment10.T2.sd = sd (environment10_T2),
                   environment11.T2.mean = mean (environment11_T2), 
                   environment11.T2.sd = sd (environment11_T2),
                   environment.T2.mean = mean (environment_mean_T2),
                   environment.T2.sd = sd (environment_mean_T2))
                   
knitr::kable(environment_T2_discriptive) #出力

##BIS_T2の記述統計量##
bis_T2_discriptive <- 
  InputData %>% 
  drop_na() %>%
  dplyr::summarise(n = n (), #グループの人数を出力
                   bis1.T2.mean = mean (bis1_T2), #bis1_T2の平均
                   bis1.T2.sd = sd (bis1_T2), #bis1_T2のSD
                   bis2.T2.mean = mean (bis2_T2), 
                   bis2.T2.sd = sd (bis2_T2),
                   bis3.T2.mean = mean (bis3_T2), 
                   bis3.T2.sd = sd (bis3_T2),
                   bis4.T2.mean = mean (bis4_T2), 
                   bis4.T2.sd = sd (bis4_T2),
                   bis5.T2.mean = mean (bis5_T2), 
                   bis5.T2.sd = sd (bis5_T2),
                   bis6.T2.mean = mean (bis6_T2), 
                   bis6.T2.sd = sd (bis6_T2),
                   bis7.T2.mean = mean (bis7_T2), 
                   bis7.T2.sd = sd (bis7_T2),
                   bis.T2.mean = mean (bis_mean_T2), 
                   bis.T2.sd = sd (bis_mean_T2))

knitr::kable(bis_T2_discriptive) #出力

##BAS_T2の記述統計量##
bas_T2_discriptive <- 
  InputData %>% 
  drop_na() %>%
  dplyr::summarise(n = n (), #グループの人数を出力
                   bas1.T2.mean = mean (bas1_T2), #bas1_T2の平均
                   bas1.T2.sd = sd (bas1_T2), #bas1_T2のSD
                   bas2.T2.mean = mean (bas2_T2), 
                   bas2.T2.sd = sd (bas2_T2),
                   bas3.T2.mean = mean (bas3_T2), 
                   bas3.T2.sd = sd (bas3_T2),
                   bas4.T2.mean = mean (bas4_T2), 
                   bas4.T2.sd = sd (bas4_T2),
                   bas5.T2.mean = mean (bas5_T2), 
                   bas5.T2.sd = sd (bas5_T2),
                   bas6.T2.mean = mean (bas6_T2), 
                   bas6.T2.sd = sd (bas6_T2),
                   bas7.T2.mean = mean (bas7_T2), 
                   bas7.T2.sd = sd (bas7_T2),
                   bas8.T2.mean = mean (bas8_T2), 
                   bas8.T2.sd = sd (bas8_T2),
                   bas9.T2.mean = mean (bas9_T2), 
                   bas9.T2.sd = sd (bas9_T2),
                   bas10.T2.mean = mean (bas10_T2), 
                   bas10.T2.sd = sd (bas10_T2),
                   bas11.T2.mean = mean (bas11_T2), 
                   bas11.T2.sd = sd (bas11_T2),
                   bas12.T2.mean = mean (bas12_T2), 
                   bas12.T2.sd = sd (bas12_T2),
                   bas13.T2.mean = mean (bas13_T2), 
                   bas13.T2.sd = sd (bas13_T2),
                   bas.T2.mean = mean (bas_mean_T2), 
                   bas.T2.sd = sd (bas_mean_T2))

knitr::kable(bas_T2_discriptive) #出力

##メイン変数の性別ごとの記述統計量##

#Time1変数の統計量
Time1_discriptive_by_gender <- 
  InputData %>% 
  dplyr::group_by(child_gender_T1) %>% #性別でグルーピング
  dplyr::summarise(n = n (), #グループの人数を出力
                   hsc_T1_mean = mean (hsc_mean_T1), #hsc_T1の平均
                   hsc_T1_sd = sd (hsc_mean_T1), #hcs_T1のSD
                   eoe_T1_mean = mean (eoe_mean_T1), #eoe_T1の平均
                   eoe_T1_sd = sd (eoe_mean_T1), #eoe_T1のSD
                   lst_T1_mean = mean (lst_mean_T1), #lst_T1の平均
                   lst_T1_sd = sd (lst_mean_T1), #lst_T1のSD
                   aes_T1_mean = mean (aes_mean_T1), #aes_T1の平均
                   aes_T1_sd = sd (aes_mean_T1), #aes_T1のSD
                   health_T1_mean = mean (health_mean_T1), #health_T1の平均
                   health_T1_sd = sd (health_mean_T1), #health_T1のSD
                   positive_T1_mean = mean (positive_mean_T1), #positive_T1の平均値
                   positive_T1_sd = sd (positive_mean_T1), #positive_T1のSD
                   negative_T1_mean = mean (negative_mean_T1), #negative_T1の平均値
                   negative_T1_sd = sd (negative_mean_T1)) #negative_T1のSD

knitr::kable(Time1_discriptive_by_gender) #出力

#Time2変数の統計量
Time2_discriptive_by_gender <- 
  InputData %>%
  drop_na() %>%
  dplyr::group_by(child_gender_T1) %>% #性別でグルーピング
  dplyr::summarise(n = n (), #グループの人数を出力
                   hsc_T2_mean = mean (hsc_mean_T2), #hsc_T2の平均
                   hsc_T2_sd = sd (hsc_mean_T2), #hcs_T2のSD
                   eoe_T2_mean = mean (eoe_mean_T2), #eoe_T2の平均
                   eoe_T2_sd = sd (eoe_mean_T2), #eoe_T2のSD
                   lst_T2_mean = mean (lst_mean_T2), #lst_T2の平均
                   lst_T2_sd = sd (lst_mean_T2), #lst_T2のSD
                   aes_T2_mean = mean (aes_mean_T2), #aes_T2の平均
                   aes_T2_sd = sd (aes_mean_T2), #aes_T2のSD
                   health_T2_mean = mean (health_mean_T2), #health_T2の平均
                   health_T2_sd = sd (health_mean_T2), #health_T2のSD
                   environment_T2_mean = mean (environment_mean_T2), #environment_T2の平均値
                   environment_T2_sd = sd (environment_mean_T2), #environment_T2のSD
                   bis_T2_mean = mean (bis_mean_T2), #bis_T2の平均値
                   bis_T2_sd = sd (bis_mean_T2), #bis_T2のSD
                   bas_T2_mean = mean (bas_mean_T2), #bas_T2の平均値
                   bas_T2_sd = sd (bas_mean_T2)) #bas_T2のSD) 

knitr::kable(Time2_discriptive_by_gender) #出力


###信頼性係数###
  #出力の見方は<https://mumu.jpn.ph/forest/computer/2016/05/29/5049/>など
  #psychパッケージのomega関数を使用する
  #GPArotationパッケージも必要
  #omega(データ, nfactor=尺度の因子数, fm=推定法)
　#因子数のデフォは3。1にするとエラー。
　#推定法のデフォは最小残差法

library(psych)
library(GPArotation)

omega(InputData[,11:22],3, fm="ml") #hscT1
omega(InputData[,64:75],3, fm="ml") #hscT2
omega(InputData[,c(18,16,14,19,22)],3, fm="ml") #eoeT1
omega(InputData[,c(71,69,67,72,75)],3, fm="ml") #eoeT2
omega(InputData[,c(12,17,21)],2, fm="ml") #lstT1
omega(InputData[,c(65,70,74)],2, fm="ml") #lstT2
omega(InputData[,c(15,20,11,13)],3, fm="ml") #aesT1
omega(InputData[,c(68,73,64,66)],3, fm="ml") #aesT2
omega(InputData[,23:27],3, fm="ml") #mentalT1
omega(InputData[,76:80],3, fm="ml") #mentalT2
omega(InputData[,c(29,31,33,35,37,39,41,43)],3, fm="ml") #positiveT1
omega(InputData[,c(28,30,32,34,36,38,40,42)],3, fm="ml") #negativeT1
omega(InputData[,53:63],3, fm="ml") #environmentT2
omega(InputData[,c(101,86,91,93,95,102)],3, fm="ml") #bisT2
omega(InputData[,c(82,83,84,85,87,88,89,91,92,94,96,97,99)],3, fm="ml") #basT2

##相関係数##

#論文化に必要な変数間の相関係数を算出
#相関表1列目
cor.test(InputData$hsc_mean_T1, InputData$hsc_mean_T2, method = "pearson") #hscT1-hscT2の相関
cor.test(InputData$hsc_mean_T1, InputData$eoe_mean_T1, method = "pearson") #hscT1-eoeT1の相関
cor.test(InputData$hsc_mean_T1, InputData$eoe_mean_T2, method = "pearson") #hscT1-eoeT2の相関
cor.test(InputData$hsc_mean_T1, InputData$lst_mean_T1, method = "pearson") #hscT1-lstT1の相関
cor.test(InputData$hsc_mean_T1, InputData$lst_mean_T2, method = "pearson") #hscT1-lstT2の相関
cor.test(InputData$hsc_mean_T1, InputData$aes_mean_T1, method = "pearson") #hscT1-aesT1の相関
cor.test(InputData$hsc_mean_T1, InputData$aes_mean_T2, method = "pearson") #hscT1-aesT2の相関
cor.test(InputData$hsc_mean_T1, InputData$health_mean_T1, method = "pearson") #hscT1-healthT1の相関
cor.test(InputData$hsc_mean_T1, InputData$health_mean_T2, method = "pearson") #hscT1-healthT2の相関
cor.test(InputData$hsc_mean_T1, InputData$positive_mean_T1, method = "pearson") #hscT1-positiveT1の相関
cor.test(InputData$hsc_mean_T1, InputData$negative_mean_T1, method = "pearson") #hscT1-negativeT1の相関
cor.test(InputData$hsc_mean_T1, InputData$environment_mean_T2, method = "pearson") #hscT1-environmentT2の相関
cor.test(InputData$hsc_mean_T1, InputData$bis_mean_T2, method = "pearson") #hscT1-bisT2の相関
cor.test(InputData$hsc_mean_T1, InputData$bas_mean_T2, method = "pearson") #hscT1-basT2の相関
#相関表2列目
cor.test(InputData$hsc_mean_T2, InputData$eoe_mean_T1, method = "pearson") #hscT2-eoeT1の相関
cor.test(InputData$hsc_mean_T2, InputData$eoe_mean_T2, method = "pearson") #hscT2-eoeT2の相関
cor.test(InputData$hsc_mean_T2, InputData$lst_mean_T1, method = "pearson") #hscT2-lstT1の相関
cor.test(InputData$hsc_mean_T2, InputData$lst_mean_T2, method = "pearson") #hscT2-lstT2の相関
cor.test(InputData$hsc_mean_T2, InputData$aes_mean_T1, method = "pearson") #hscT2-aesT1の相関
cor.test(InputData$hsc_mean_T2, InputData$aes_mean_T2, method = "pearson") #hscT2-aesT2の相関
cor.test(InputData$hsc_mean_T2, InputData$health_mean_T1, method = "pearson") #hscT2-healthT1の相関
cor.test(InputData$hsc_mean_T2, InputData$health_mean_T2, method = "pearson") #hscT2-healthT2の相関
cor.test(InputData$hsc_mean_T2, InputData$positive_mean_T1, method = "pearson") #hscT2-positiveT1の相関
cor.test(InputData$hsc_mean_T2, InputData$negative_mean_T1, method = "pearson") #hscT2-negativeT1の相関
cor.test(InputData$hsc_mean_T2, InputData$environment_mean_T2, method = "pearson") #hscT2-envirionmentT2の相関
cor.test(InputData$hsc_mean_T2, InputData$bis_mean_T2, method = "pearson") #hscT2-bisT2の相関
cor.test(InputData$hsc_mean_T2, InputData$bas_mean_T2, method = "pearson") #hscT2-basT2の相関
#相関表3列目
cor.test(InputData$eoe_mean_T1, InputData$eoe_mean_T2, method = "pearson") #eoeT1-eoeT2の相関
cor.test(InputData$eoe_mean_T1, InputData$lst_mean_T1, method = "pearson") #eoeT1-lstT1の相関
cor.test(InputData$eoe_mean_T1, InputData$lst_mean_T2, method = "pearson") #eoeT1-lstT2の相関
cor.test(InputData$eoe_mean_T1, InputData$aes_mean_T1, method = "pearson") #eoeT1-aesT1の相関
cor.test(InputData$eoe_mean_T1, InputData$aes_mean_T2, method = "pearson") #eoeT1-aesT2の相関
cor.test(InputData$eoe_mean_T1, InputData$health_mean_T1, method = "pearson") #eoeT1-healthT1の相関
cor.test(InputData$eoe_mean_T1, InputData$health_mean_T2, method = "pearson") #eoeT1-healthT2の相関
cor.test(InputData$eoe_mean_T1, InputData$positive_mean_T1, method = "pearson") #eoeT1-positiveT1の相関
cor.test(InputData$eoe_mean_T1, InputData$negative_mean_T1, method = "pearson") #eoeT1-negativeT1の相関
cor.test(InputData$eoe_mean_T1, InputData$environment_mean_T2, method = "pearson") #eoeT1-envirionmentT2の相関
cor.test(InputData$eoe_mean_T1, InputData$bis_mean_T2, method = "pearson") #eoeT1-bisT2の相関
cor.test(InputData$eoe_mean_T1, InputData$bas_mean_T2, method = "pearson") #eoeT1-basT2の相関
#相関表4列目
cor.test(InputData$eoe_mean_T2, InputData$lst_mean_T1, method = "pearson") #eoeT2-lstT1の相関
cor.test(InputData$eoe_mean_T2, InputData$lst_mean_T2, method = "pearson") #eoeT2-lstT2の相関
cor.test(InputData$eoe_mean_T2, InputData$aes_mean_T1, method = "pearson") #eoeT2-aesT1の相関
cor.test(InputData$eoe_mean_T2, InputData$aes_mean_T2, method = "pearson") #eoeT2-aesT2の相関
cor.test(InputData$eoe_mean_T2, InputData$health_mean_T1, method = "pearson") #eoeT2-healthT1の相関
cor.test(InputData$eoe_mean_T2, InputData$health_mean_T2, method = "pearson") #eoeT2-healthT2の相関
cor.test(InputData$eoe_mean_T2, InputData$positive_mean_T1, method = "pearson") #eoeT2-positiveT1の相関
cor.test(InputData$eoe_mean_T2, InputData$negative_mean_T1, method = "pearson") #eoeT2-negativeT1の相関
cor.test(InputData$eoe_mean_T2, InputData$environment_mean_T2, method = "pearson") #eoeT2-envirionmentT2の相関
cor.test(InputData$eoe_mean_T2, InputData$bis_mean_T2, method = "pearson") #eoeT2-bisT2の相関
cor.test(InputData$eoe_mean_T2, InputData$bas_mean_T2, method = "pearson") #eoeT2-basT2の相関
#相関表5列目
cor.test(InputData$lst_mean_T1, InputData$lst_mean_T2, method = "pearson") #lstT1-lstT2の相関
cor.test(InputData$lst_mean_T1, InputData$aes_mean_T1, method = "pearson") #lstT1-aesT1の相関
cor.test(InputData$lst_mean_T1, InputData$aes_mean_T2, method = "pearson") #lstT1-aesT2の相関
cor.test(InputData$lst_mean_T1, InputData$health_mean_T1, method = "pearson") #lstT1-healthT1の相関
cor.test(InputData$lst_mean_T1, InputData$health_mean_T2, method = "pearson") #lstT1-healthT2の相関
cor.test(InputData$lst_mean_T1, InputData$positive_mean_T1, method = "pearson") #lstT1-positiveT1の相関
cor.test(InputData$lst_mean_T1, InputData$negative_mean_T1, method = "pearson") #lstT1-negativeT1の相関
cor.test(InputData$lst_mean_T1, InputData$environment_mean_T2, method = "pearson") #lstT1-envirionmentT2の相関
cor.test(InputData$lst_mean_T1, InputData$bis_mean_T2, method = "pearson") #lstT1-bisT2の相関
cor.test(InputData$lst_mean_T1, InputData$bas_mean_T2, method = "pearson") #lstT1-basT2の相関
#相関表6列目
cor.test(InputData$lst_mean_T2, InputData$aes_mean_T1, method = "pearson") #lstT2-aesT1の相関
cor.test(InputData$lst_mean_T2, InputData$aes_mean_T2, method = "pearson") #lstT2-aesT2の相関
cor.test(InputData$lst_mean_T2, InputData$health_mean_T1, method = "pearson") #lstT2-healthT1の相関
cor.test(InputData$lst_mean_T2, InputData$health_mean_T2, method = "pearson") #lstT2-healthT2の相関
cor.test(InputData$lst_mean_T2, InputData$positive_mean_T1, method = "pearson") #lstT2-positiveT1の相関
cor.test(InputData$lst_mean_T2, InputData$negative_mean_T1, method = "pearson") #lstT2-negativeT1の相関
cor.test(InputData$lst_mean_T2, InputData$environment_mean_T2, method = "pearson") #lstT2-envirionmentT2の相関
cor.test(InputData$lst_mean_T2, InputData$bis_mean_T2, method = "pearson") #lstT2-bisT2の相関
cor.test(InputData$lst_mean_T2, InputData$bas_mean_T2, method = "pearson") #lstT2-basT2の相関
#相関表7列目
cor.test(InputData$aes_mean_T1, InputData$aes_mean_T2, method = "pearson") #aesT1-aesT2の相関
cor.test(InputData$aes_mean_T1, InputData$health_mean_T1, method = "pearson") #aesT1-healthT1の相関
cor.test(InputData$aes_mean_T1, InputData$health_mean_T2, method = "pearson") #aesT1-healthT2の相関
cor.test(InputData$aes_mean_T1, InputData$positive_mean_T1, method = "pearson") #aesT1-positiveT1の相関
cor.test(InputData$aes_mean_T1, InputData$negative_mean_T1, method = "pearson") #aesT1-negativeT1の相関
cor.test(InputData$aes_mean_T1, InputData$environment_mean_T2, method = "pearson") #aesT1-envirionmentT2の相関
cor.test(InputData$aes_mean_T1, InputData$bis_mean_T2, method = "pearson") #aesT1-bisT2の相関
cor.test(InputData$aes_mean_T1, InputData$bas_mean_T2, method = "pearson") #aesT1-basT2の相関
#相関表8列目
cor.test(InputData$aes_mean_T2, InputData$health_mean_T1, method = "pearson") #aesT2-healthT1の相関
cor.test(InputData$aes_mean_T2, InputData$health_mean_T2, method = "pearson") #aesT2-healthT2の相関
cor.test(InputData$aes_mean_T2, InputData$positive_mean_T1, method = "pearson") #aesT2-positiveT1の相関
cor.test(InputData$aes_mean_T2, InputData$negative_mean_T1, method = "pearson") #aesT2-negativeT1の相関
cor.test(InputData$aes_mean_T2, InputData$environment_mean_T2, method = "pearson") #aesT2-envirionmentT2の相関
cor.test(InputData$aes_mean_T2, InputData$bis_mean_T2, method = "pearson") #aesT2-bisT2の相関
cor.test(InputData$aes_mean_T2, InputData$bas_mean_T2, method = "pearson") #aesT2-basT2の相関
#相関表9列目
cor.test(InputData$health_mean_T1, InputData$health_mean_T2, method = "pearson") #healthT1-healthT2の相関
cor.test(InputData$health_mean_T1, InputData$positive_mean_T1, method = "pearson") #healthT1-positiveT1の相関
cor.test(InputData$health_mean_T1, InputData$negative_mean_T1, method = "pearson") #healthT1-negativeT1の相関
cor.test(InputData$health_mean_T1, InputData$environment_mean_T2, method = "pearson") #healthT1-envirionmentT2の相関
cor.test(InputData$health_mean_T1, InputData$bis_mean_T2, method = "pearson") #healthT1-bisT2の相関
cor.test(InputData$health_mean_T1, InputData$bas_mean_T2, method = "pearson") #healthT1-basT2の相関
#相関表10列目
cor.test(InputData$health_mean_T2, InputData$positive_mean_T1, method = "pearson") #healthT2-positiveT1の相関
cor.test(InputData$health_mean_T2, InputData$negative_mean_T1, method = "pearson") #healthT2-negativeT1の相関
cor.test(InputData$health_mean_T2, InputData$environment_mean_T2, method = "pearson") #healthT2-envirionmentT2の相関
cor.test(InputData$health_mean_T2, InputData$bis_mean_T2, method = "pearson") #healthT2-bisT2の相関
cor.test(InputData$health_mean_T2, InputData$bas_mean_T2, method = "pearson") #healthT2-basT2の相関
#相関表11列目
cor.test(InputData$positive_mean_T1, InputData$negative_mean_T1, method = "pearson") #positiveT1-negativeT1の相関
cor.test(InputData$positive_mean_T1, InputData$environment_mean_T2, method = "pearson") #positiveT1-envirionmentT2の相関
cor.test(InputData$positive_mean_T1, InputData$bis_mean_T2, method = "pearson") #positiveT1-bisT2の相関
cor.test(InputData$positive_mean_T1, InputData$bas_mean_T2, method = "pearson") #positiveT1-basT2の相関
#相関表12列目
cor.test(InputData$negative_mean_T1, InputData$environment_mean_T2, method = "pearson") #negativeT1-envirionmentT2の相関
cor.test(InputData$negative_mean_T1, InputData$bis_mean_T2, method = "pearson") #negativeT1-bisT2の相関
cor.test(InputData$negative_mean_T1, InputData$bas_mean_T2, method = "pearson") #negativeT1-basT2の相関
#相関表13列目
cor.test(InputData$environment_mean_T2, InputData$bis_mean_T2, method = "pearson") #environmentT2-bisT2の相関
cor.test(InputData$environment_mean_T2, InputData$bas_mean_T2, method = "pearson") #environmentT2-basT2の相関
#相関表14列目
cor.test(InputData$bis_mean_T2, InputData$bas_mean_T2, method = "pearson") #bisT2-basT2の相関

#感想：一気に相関検定してくれるコードがあればいいのだが…。アナログな方法しか思いつかない

##相関係数の可視化（残布図）##
library(GGally)

cordata <- InputData %>% #散布図用のデータセットcordata作成
  select(child_gender_T1, eoe_mean_T1:bas_mean_T2) %>% #性別とeoeからbasまでの列を抽出
  select(-na.rm) #na.rmという謎変数が含まれていたので除外
names(cordata) #変数名確認

png("figure/corplot.png", width = 1200, height = 1200) #図の保存先指定
cor_plot <- ggpairs(cordata, mapping = aes(colour = factor(child_gender_T1), alpha=0.5)) #性別で色分けで作図
print(cor_plot) #出力
dev.off() #保存


#欠損値分析
library(BaylorEdPsych)
library(mvnmle)
x <- InputData %>% 
  select(eoe_mean_T1, eoe_mean_T1,lst_mean_T1,aes_mean_T1,hsc_mean_T1,health_mean_T1,positive_mean_T1, negative_mean_T1, eoe_mean_T2) #メインの分析で使う変数のみのデータセット作成
x <- x %>% mutate(particitation = if_else(eoe_mean_T2 >=1, "1", "0")) #ここでT2参加者を1にカテゴリ化
x <- x %>% select(-eoe_mean_T2)
x$particitation[is.na(x$particitation)] <- 0 #particitation列のNAを0に置換
names(x) #変数名確認
x$particitation #念のため列が1,0で置換されたか確認
x$particitation <- as.factor(x$particitation) #factor型に変換
class(x$particitation)
head(x)
#LittleMCAR(x) なぜか使えない

#代替として、particitationを独立変数としてMANOVA
result <- manova(cbind(eoe_mean_T1,lst_mean_T1,aes_mean_T1, health_mean_T1,positive_mean_T1, negative_mean_T1) ~ particitation, data = x)
summary(result)
#結果メモ：従属変数間にparticipationの効果は見られなかった。


write_csv(x, "manovadata.csv")
#（3）因子分析========

library(lavaan)
library(semPlot)
library(semTools)

##HSCSの（縦断的）確認的因子分析##

##手順としては、(1)因子不変性の比較と最適なモデルの選択、(2)最適なモデルの適合度や因子負荷量や信頼性係数の算出


#縦断的な測定不変性を検討するため、因子分析に用いるデータをtidydata型にする。
InputData_tidy <- InputData %>% 
  gather(key = "time", value = "hsc1", hsc1_T1, hsc1_T2) %>% 
  gather(key = "jikan1", value = "hsc2", hsc2_T1, hsc2_T2) %>% 
  gather(key = "jikan2", value = "hsc3", hsc3_T1, hsc3_T2) %>% 
  gather(key = "jikan3", value = "hsc4", hsc4_T1, hsc4_T2) %>% 
  gather(key = "jikan4", value = "hsc5", hsc5_T1, hsc5_T2) %>% 
  gather(key = "jikan5", value = "hsc6", hsc6_T1, hsc6_T2) %>% 
  gather(key = "jikan6", value = "hsc7", hsc7_T1, hsc7_T2) %>% 
  gather(key = "jikan7", value = "hsc8", hsc8_T1, hsc8_T2) %>% 
  gather(key = "jikan8", value = "hsc9", hsc9_T1, hsc9_T2) %>% 
  gather(key = "jikan9", value = "hsc10", hsc10_T1, hsc10_T2) %>% 
  gather(key = "jikan10", value = "hsc11", hsc11_T1, hsc11_T2) %>% 
  gather(key = "jikan11", value = "hsc12", hsc12_T1, hsc12_T2)

InputData_tidy$time <- sub("hsc1_T", "", InputData_tidy$time) #time列のデータから"hsc_T1"文字を削除
InputData_tidy <- InputData_tidy %>% select(-starts_with("jikan")) #jikan列を削除
InputData_tidy$time <- as.numeric(InputData_tidy$time) #整数型に変換

head(InputData_tidy[,95:107]) #データ変換ができたか確認
names(InputData_tidy) #列名も確認


#モデル記述
model <-'
EOE =~ hsc4 + hsc6 + hsc8 + hsc9 + hsc12
LST =~ hsc2 + hsc7 + hsc11
AES =~ hsc1 + hsc3 + hsc5 + hsc10 
'

##ひととおり、各モデルを推定してみる #まあ、正直、ここは省略しても可。
#配置不変モデル（時点間で同じ因子構造だけど、パス係数など異なる）
config <- cfa(model, data = InputData_tidy, group = "time", missing = "fiml")
summary(config, fit.measures = TRUE, standardized = TRUE) #列が長いから結構推定時間長くかかるよ！
cfa.config.plot <- semPaths(config, "std", edge.label.cex=.8, fade = FALSE, gray = TRUE, mar=c(6,1,3,1), style="lisrel") #作図
#弱測定不変性モデル（因子負荷量だけ同じ）
weak <- cfa(model, data = InputData_tidy, group = "time", group.equal= "loadings", missing = "fiml")
summary(weak, fit.measures = TRUE, standardized = TRUE)
cfa.weak.plot <- semPaths(weak, "std", edge.label.cex=.8, fade = FALSE, gray = TRUE, mar=c(6,1,3,1), style="lisrel") #作図
#強測定不変モデル（因子負荷量、切片が同じ）
strong <- cfa(model, data = InputData_tidy, group = "time", group.equal= c("loadings", "intercepts"), missing = "fiml")
summary(strong, fit.measures = TRUE, standardized = TRUE)
cfa.strong.plot <- semPaths(strong, "std", edge.label.cex=.8, fade = FALSE, gray = TRUE, mar=c(6,1,3,1), style="lisrel") #作図
#厳密な測定不変モデル（さらに誤差分散も同じ）
strict <- cfa(model, data = InputData_tidy, group = "time", group.equal= c("loadings", "intercepts", "residuals"), missing = "fiml")
summary(strict, fit.measures = TRUE, standardized = TRUE)
cfa.strict.plot <- semPaths(strict, "std", edge.label.cex=.8, fade = FALSE, gray = TRUE, mar=c(6,1,3,1), style="lisrel") #作図

##配置不変から測定不変までのモデルを一気に比較してみる #semToolsパッケージが必要
measurementInvariance(model = model, data = InputData_tidy, std.lv = TRUE, strict = TRUE, fit.measures = c("cfi", "rmsea", "aic"), group= "time", missing = "fiml") #もの～すごく推定時間かかる

#等値制約が少ないモデルと多いモデルのχ2を比較して、p値が有意（χ2値が悪化：増加）であれば等値制約が少ないほうのモデルを採用。p値が有意であれば多いほうのモデルを採用。
#以下の比較結果をみると、配置不変モデルが最も適合度がよいモデルであることがわかる
#結果の出力を一応貼っておく（推定時間ながいので、再度分析するのは面倒だから）
#Measurement invariance models:
# 
#Model 1 : fit.configural
#Model 2 : fit.loadings
#Model 3 : fit.intercepts
#Model 4 : fit.residuals
#Model 5 : fit.means
#
#Chi Square Difference Test
#
#                Df   AIC      BIC     Chisq   Chisq diff    Df  diff Pr(>Chisq)    
#fit.configural 102 60625609 60626571 395260                                  
#fit.loadings   111 60632591 60633442 402261     7000.3       9    < 2e-16 ***
#fit.intercepts 120 60633771 60634512 403459     1198.1       9    < 2e-16 ***
#fit.residuals  132 60647243 60647836 416955    13496.1      12    < 2e-16 ***
#fit.means      135 60647248 60647803 416966       10.7       3    0.01344 *  
#  ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Fit measures:
#  
#                cfi  rmsea    aic     cfi.delta  rmsea.delta  aic.delta
#fit.configural 0.864 0.068 60625609        NA          NA            NA
#fit.loadings   0.862 0.066 60632591     0.002       0.002      6982.252
#fit.intercepts 0.861 0.063 60633771     0.000       0.002      1180.121
#fit.residuals  0.857 0.061 60647243     0.005       0.002     13472.133
#fit.means      0.857 0.060 60647248     0.000       0.001         4.704


#配置不変モデルが妥当ということで、再度、配置不変モデルを推定
config <- cfa(model, data = InputData_tidy, group = "time", missing = "fiml")
summary(config, fit.measures = TRUE, standardized = TRUE) #列が長いから結構推定時間長くかかるよ！
cfa.config.plot <- semPaths(config, "std", edge.label.cex=.8, fade = FALSE, gray = TRUE, mar=c(6,1,3,1), style="lisrel") #作図

##結果についてのコメントや解釈##
#適合度は上記のfit.configural行のとおり。CFIはやや低いが、rmseaは許容可能。
#信頼性係数は尺度全体でT1 = 0.79, T2 = 0.79で良好である。eoeはT1 = 0.74, T2 = 0.80, lstは T1 = 0.55(omegaTotal=0.69), T2 = 0.57(omegaTotal=0.69), aesはT1 = 0.69(ωTotal =0.79), T2 = 0.65(ωTotal=0.79)であった。
#信頼性は全体はいいが、lstとaesでやや低い。ただωはまあまあ良好。lstは項目数が少ないからしかたないかも。
#因子間相関はeoeとlstが2時点ともに0.7でやや高め。
#因子負荷量は、T1はすべての項目で0.3以上負荷、T2はaesの項目1(hst1)で0.21とやや低い。
#総評：以上のような問題点はあるが、概ねオリジナルの因子構造が再現されたと判断する。


##学校環境変化尺度の探索的因子分析##

##分析の手順としては、（1）因子数の決定、（2）それにもとづく回転と因子負荷量の推定、（3）信頼性係数の算出、（4）再度合計得点の算出と列追加

library(psych)
library(MASS)
library(GPArotation)

#因子分析用データセットの作成
environment.data <- InputData %>% select(environment1_T2:environment11_T2)
names(environment.data) #変数名確認

#平行分析
fa.parallel(environment.data, fm = "ml", fa = "fa") #fm =最尤法推定, fa=主因子法抽出
  #結果メモ：4因子解が推奨された

#MAPテスト
VSS(environment.data, n = 10) #n = 10因子までの数値を算出
  #結果メモ：1因子解が推奨された

#4因子解の因子分析
factor4 <- fa(environment.data, nfactors = 4, rotate = "promax", fm = "ml", scores = TRUE)
print(factor4, sort = TRUE)
  #結果メモ：項目1-2, 3-4といった項目順で因子が構成されてしまっている。因子負荷は十分だが、1を超えるものもある。1因子2項目ずつでは少ないし、結果が頑健でなくなるかも。
  #結果メモ：1因子解を採用した方がよさそう。

#1因子解の因子分析（1回目）
factor1 <- fa(environment.data, nfactors = 1, rotate = "none", fm = "ml", scores = TRUE)
print(factor1, sort = TRUE) #結果出力
  #結果メモ：項目7,9,10の負荷量が0.35以下と低い。これらを削除して、再度因子分析。

#1因子解の因子分析（2回目, 項目7,9,10削除）
environment.data.2 <- environment.data %>% select(environment1_T2:environment6_T2, environment8_T2, environment11_T2) #項目7,9,10削除データ作成
names(environment.data.2) #変数名確認
factor1.second <- fa(environment.data.2, nfactors = 1, rotate = "none", fm = "ml", scores = TRUE) #再度因子分析
print(factor1.second, sort = TRUE) #結果出力
  #結果メモ：項目8が因子負荷0.35でまあギリギリ。これで確定とする。

#信頼性係数の算出
omega(environment.data.2,3, fm = "ml") #算出
  #結果メモ：alpha=0.88で十分、omegatotal=0.91

#再度合計点を算出し、InputDataに列追加
InputData <- InputData %>% 
  dplyr::mutate(environment_mean_afterEFA_T2 = (environment1_T2 + environment2_T2 + environment3_T2 + environment4_T2 + environment5_T2 + environment6_T2 + environment8_T2 + environment11_T2)/8, na.rm = TRUE)
names(InputData) #合計得点追加されたか確認

#因子分析後の学校環境変化尺度の度数分布
environment_mean_afterEFA_T2_count <- dplyr::count(InputData, environment_mean_afterEFA_T2)
knitr::kable(environment_mean_afterEFA_T2_count) #テーブル化
ggplot(data = InputData, mapping = aes(x = environment_mean_afterEFA_T2, fill = factor(environment_mean_afterEFA_T2))) + 
  geom_histogram(binwidth = 0.2) + 
  guides(fill = "none") #視覚化

#因子分析後の学校環境変化尺度の基礎統計量
environment_mean_afterEFA_T2_discriptive <- 
  InputData %>% 
  drop_na() %>%
  dplyr::summarise(n = n (), #グループの人数を出力
                   environment.mean = mean (environment_mean_afterEFA_T2), #environment_mean_afterEFA_T2の平均
                   environment.sd = sd (environment_mean_afterEFA_T2)) #environment_mean_afterEFA_T2のSD
environment_mean_afterEFA_T2_discriptive  #出力                 


#確定した学校環境変化尺度と他の変数の相関
cor.test(InputData$environment_mean_afterEFA_T2, InputData$hsc_mean_T1, method = "pearson") #hscT1の相関
cor.test(InputData$environment_mean_afterEFA_T2, InputData$hsc_mean_T2, method = "pearson") #hscT2の相関
cor.test(InputData$environment_mean_afterEFA_T2, InputData$eoe_mean_T1, method = "pearson") #eoeT1の相関
cor.test(InputData$environment_mean_afterEFA_T2, InputData$eoe_mean_T2, method = "pearson") #eoeT2の相関
cor.test(InputData$environment_mean_afterEFA_T2, InputData$lst_mean_T1, method = "pearson") #lstT1の相関
cor.test(InputData$environment_mean_afterEFA_T2, InputData$lst_mean_T2, method = "pearson") #lstT2の相関
cor.test(InputData$environment_mean_afterEFA_T2, InputData$aes_mean_T1, method = "pearson") #aesT1の相関
cor.test(InputData$environment_mean_afterEFA_T2, InputData$aes_mean_T2, method = "pearson") #aesT2の相関
cor.test(InputData$environment_mean_afterEFA_T2, InputData$health_mean_T1, method = "pearson") #healthT1の相関
cor.test(InputData$environment_mean_afterEFA_T2, InputData$health_mean_T2, method = "pearson") #healthT2の相関
cor.test(InputData$environment_mean_afterEFA_T2, InputData$positive_mean_T1, method = "pearson") #positiveT1の相関
cor.test(InputData$environment_mean_afterEFA_T2, InputData$negative_mean_T1, method = "pearson") #negativeT1の相関
cor.test(InputData$environment_mean_afterEFA_T2, InputData$bis_mean_T2, method = "pearson") #bisT2の相関
cor.test(InputData$environment_mean_afterEFA_T2, InputData$bas_mean_T2, method = "pearson") #basT2の相関


#（4）精神的健康の潜在差得点算出、K-means法クラスタ分析（潜在プロファイル分析もやる）、2要因分散分析=====
##分析手順：1）精神的健康の潜在差得点を算出、2）HSCと学校環境のクラスタ抽出（潜在プロファイル分析もやる）、3）クラスタを独立変数・精神的健康の潜在差得点を従属変数とした2要因分散分析を行う


##1）精神的健康の潜在差得点を算出

lcs.health <- '
T1 =~ 1*health1_T1 + m*health2_T1 + n*health3_T1 + o*health4_T1 + p*health5_T1
T2 =~ 1*health1_T2 + m*health2_T2 + n*health3_T2 + o*health4_T2 + p*health5_T2

## 誤差
health1_T1 ~~ health1_T1
health2_T1 ~~ health2_T1
health3_T1 ~~ health3_T1
health4_T1 ~~ health4_T1
health5_T1 ~~ health5_T1
health1_T2 ~~ health1_T2
health2_T2 ~~ health2_T2
health3_T2 ~~ health3_T2
health4_T2 ~~ health4_T2
health5_T2 ~~ health5_T2

#2時点間の同一項目間の誤差共分散を定義
health1_T1 ~~ health1_T2 #項目1
health2_T1 ~~ health2_T2 #項目2
health3_T1 ~~ health3_T2 #項目3
health4_T1 ~~ health4_T2 #項目4
health5_T1 ~~ health5_T2 #項目5

## free latent variances and covariances
LC ~~ var.LC*LC
T1 ~~ var.T1*T1
T1 ~~ eta*LC

## define latent difference score (fix loading to 1)
LC =~ 1*T2
## fix latent regression to 1 to define latent difference score
T2 ~ 1*T1
## fix FT2 variance to zero to define latent difference score
T2 ~~ 0*T2
## no correlations between FT2 and other latent variables 
LC + T1 ~~ 0*T2

## means
LC ~ beta*1
T1 ~ alpha*1
'

result.lcs <- lavaan(lcs.health, data = InputData, fixed.x = FALSE, missing = "fiml", meanstructure = TRUE)
summary(result.lcs, fit.measures = TRUE)
lcs.plot <- semPaths(result.lcs, "std", edge.label.cex=.8, fade = FALSE, gray = TRUE, mar=c(6,1,3,1), style="mx") #作図

change_score <- predict(object = result.lcs) #算出された個々人の潜在得点の結果を抽出
change_score <- as.data.frame(change_score) #データフレーム型に変換
health_change_score <- change_score %>% select(-T1, -T2) #差得点列だけを抽出
head(health_change_score) #個々人の潜在得点を確認（LC列が差得点）

#潜在差得点の列をInputDataの列に追加
InputData <- bind_cols(InputData, health_change_score)
names(InputData) #列追加されたか確認


#潜在差得点の基礎統計量
health_change_score_discriptive <- 
  InputData %>% 
  dplyr::summarise(n = n (), #グループの人数を出力
                   health.lcs.mean = mean (LC), #差得点の平均
                   health1.lcs.sd = sd (LC)) #差得点のSD
health_change_score_discriptive　#出力（平均はモデル推定値の切片と同じになっている）

#潜在差得点のヒストグラム
png("figure/healthchange_histogram.png", width = 600, height = 400)
health_change_score_histogram <- ggplot(data = InputData, mapping = aes(x = LC, colour = "black")) + 
  geom_histogram(binwidth = 0.2, colour="black") + guides(fill = "none") #視覚化
health_change_score_histogram + theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    axis.title = element_text(size = 16), 
    axis.text = element_text(size = 16)) +labs(x = "精神的健康の潜在差得点", y = "度数")
dev.off()

##潜在差得点のシャピロ-ウィルク検定（正規性検定）
shapiro.test(InputData$LC)
#結果メモ：p < .05で正規分布ではないと判断。
#結果メモ：正規分布ではないという結果となった。

##2）HSCと学校環境のクラスタ抽出                  

#HSCのT1の下位尺度得点のみのデータセットを作成
HSC.data <- InputData %>% select(eoe_mean_T1, lst_mean_T1, aes_mean_T1)

##HSCの潜在プロファイル分析##
#tidyLPAパッケージで実行可能。使用方法はhttps://cran.r-project.org/web/packages/tidyLPA/vignettes/Introduction_to_tidyLPA.html

#install.packages("devtools")
#devtools::install_github("jrosen48/tidyLPA") #開発版を使用

library(tidyLPA)

#適切なプロファイル数を検討比較
compare_solutions(HSC.data, eoe_mean_T1, lst_mean_T1, aes_mean_T1)
  #結果メモ：出力された図によれば、最も明瞭な変化の推移を示しているのは、異平均、等分散、共分散0を仮定したmodel1のプロファイル数4でBICが最も低い
  #結果メモ：モデル4と5がないのはデフォルトらしい。Mplusなら推定できるとのこと（上記URL）。

#推奨されたmodel3プロファイル数3の適合度を算出
hsc.profile <- estimate_profiles(HSC.data,
                                 eoe_mean_T1, lst_mean_T1, aes_mean_T1,
                                 model = 3,
                                 n_profiles = 3,
                                 return_orig_df = TRUE)

png("figure/hsc_profile_rowscore.png", width = 600, height = 400)
plot_profiles(hsc.profile) #作図（素点）
dev.off()

png("figure/hsc_profile_centered_score.png", width = 600, height = 400)
plot_profiles(hsc.profile, to_center = TRUE, to_scale = TRUE) #作図（標準化得点）
dev.off()

  #結果メモ：クラスタ1が全体の40%でHSC群に該当すると思われる。やや割合が多い？
　#結果メモ：クラスタ2は低HSC群、クラスタ3は平均群。

#ほかのプロファイル数のBICとentropyも算出する（作図用）
#プロファイル数1
hsc.profile1 <- estimate_profiles(HSC.data,
                                 eoe_mean_T1, lst_mean_T1, aes_mean_T1,
                                 model = 3,
                                 n_profiles = 1,
                                 return_orig_df = TRUE)

#プロファイル数2
hsc.profile2 <- estimate_profiles(HSC.data,
                                  eoe_mean_T1, lst_mean_T1, aes_mean_T1,
                                  model = 3,
                                  n_profiles = 2,
                                  return_orig_df = TRUE)

#プロファイル数4
hsc.profile4 <- estimate_profiles(HSC.data,
                                  eoe_mean_T1, lst_mean_T1, aes_mean_T1,
                                  model = 3,
                                  n_profiles = 4,
                                  return_orig_df = TRUE)

#プロファイル数5
hsc.profile5 <- estimate_profiles(HSC.data,
                                  eoe_mean_T1, lst_mean_T1, aes_mean_T1,
                                  model = 3,
                                  n_profiles = 5,
                                  return_orig_df = TRUE)

#プロファイル数6
hsc.profile6 <- estimate_profiles(HSC.data,
                                  eoe_mean_T1, lst_mean_T1, aes_mean_T1,
                                  model = 3,
                                  n_profiles = 6,
                                  return_orig_df = TRUE)

#所属クラス列をdataの列に追加
hsc.profile <- hsc.profile %>% select(-eoe_mean_T1, -lst_mean_T1, -aes_mean_T1, -posterior_prob)
names(hsc.profile)
InputData <- bind_cols(InputData, hsc.profile)
names(InputData) #列追加されたか確認
head(InputData) #先頭行確認


##学校環境変化のクラスタ分析##　★分析したけどうまく分類されなかった

#学校環境変化尺度のT2の項目のみのデータセットを作成
#environment.data <- data.for.ANOVA %>%
#  select(starts_with("environment"))
#head(environment.data) #先頭行確認

#Gap統計量を算出し、最適なクラスタ数を決定 ★分析したけどうまく分類されなかった
#library(cluster)
#cluster.number <- clusGap(environment.data, kmeans, K.max = 10, B = 2000, verbose = interactive())
#cluster.number #結果出力 #クラスタ数は1が適切らしい
#plot(cluster.number) #Gap統計量の視覚化
  #結果メモ: クラスタ数1では意味がない


##学校環境変化尺度の潜在プロファイル分析## ★分析したけどうまく分類されなかった
#tidyLPAパッケージで実行可能。使用方法はhttps://cran.r-project.org/web/packages/tidyLPA/vignettes/Introduction_to_tidyLPA.html

#install.packages("devtools")
#devtools::install_github("jrosen48/tidyLPA") #開発版を使用

#library(tidyLPA)

#適切なプロファイル数を検討比較
#compare_solutions(environment.data, environment1_T2, environment2_T2, environment3_T2, environment4_T2, environment5_T2, environment6_T2, environment8_T2, environment11_T2)
  #結果メモ：7クラスタでBIC最小っぽい。
  #結果メモ：7クラスタは多すぎるww
  #結果メモ：BICの減衰過程は4クラスタで落ち着いているから、4クラスタで解釈
  #結果メモ：仮説（よい変化、変化しない、わるい変化）にあわせて、クラスタを抽出。4クラスタの特徴は仮説検討に適切だった。

#推奨されたmodel1プロファイル数4の適合度を算出
#environment.profile <- estimate_profiles(environment.data,
#                                         environment1_T2, environment2_T2, environment3_T2, environment4_T2, environment5_T2, environment6_T2, environment8_T2, environment11_T2,
#                                         model = 1,
#                                         n_profiles = 4,
#                                        return_orig_df = TRUE)
#
#plot_profiles(environment.profile) #作図（素点）
#plot_profiles(environment.profile, to_center = TRUE, to_scale = TRUE) #作図（標準化得点）

#所属クラス列をdata.for.ANOVAの列に追加
#environment.profile <- as.data.frame(environment.profile)
#data.for.ANOVA <- data.for.ANOVA %>% 
#  dplyr::mutate(environment.profile$profile)
#names(data.for.ANOVA) #列追加されたか確認
#head(data.for.ANOVA) #先頭行確認


#HSCプロファイルと学校環境プロファイルのクロス集計
#addmargins(table(hsc.profile$profile, environment.profile$profile))

##HSCと環境の統合LPA分析## ★分析したけど解釈できなかった！！
#LPA.data <- 
#  InputData %>% 
#  drop_na() %>% 
#  select("eoe_mean_T1", "lst_mean_T1", "aes_mean_T1", "environment_mean_afterEFA_T2") 
#head(LPA.data) 
#compare_solutions(LPA.data, eoe_mean_T1, lst_mean_T1, aes_mean_T1, environment_mean_afterEFA_T2)
#latent.profile <- estimate_profiles(LPA.data,
#                                 eoe_mean_T1, lst_mean_T1, aes_mean_T1, environment_mean_afterEFA_T2,
#                                 model = 3,
#                                 n_profiles = 3,
#                                 return_orig_df = TRUE)
#plot_profiles(latent.profile) #作図（素点）
#plot_profiles(latent.profile, to_center = TRUE, to_scale = TRUE) #作図（標準化得点）



##学校環境変化のカテゴリ分け##

#環境ポジティブ変化群 → 下位尺度平均値が4（どちらでもない）よりも＋1/2SD以上の生徒
#環境ポジティブ無変化群 → 下位尺度平均値が4（どちらでもない）よりも－1/2SD～＋1/2SDの生徒
#環境ネガティブ変化群 → 下位尺度平均値が4（どちらでもない）よりも－1/2SD以上の生徒
#下位尺度の1SDは0.89、1/2SDは0.445
#環境ポジティブ変化群 4+0.445 = 4.445以上の生徒
#環境ポジティブ無変化 3.555～4.445の生徒
#環境ネガティブ変化群 4-0.445 = 3.555以下の生徒

#カテゴリ分け（詳細はhttps://suryu.me/post/dplyr_recode/を参考にした）
#data.for.ANOVA <- data.for.ANOVA %>% mutate(
#  environment.group = case_when( #mutate内でcase_whenを使うときは、列指定の際に.$をつけること
#    .$environment_mean_afterEFA_T2 >= 4.445 ~ "positive", #環境ポジティブ変化群 4+0.445 = 4.445以上の生徒
#    .$environment_mean_afterEFA_T2 > 3.555 & .$environment_mean_afterEFA_T2 < 4.455 ~ "non", #環境ポジティブ無変化 3.555～4.445の生徒
#    .$environment_mean_afterEFA_T2 <= 3.555 ~ "negative")) #環境ネガティブ変化群 4-0.445 = 3.555以下の生徒
#head(data.for.ANOVA)   

#ついでにHSCプロファイルも名前変更
InputData <- InputData %>%
  mutate(hsc.profile = recode(profile,
                            "1" = "medium",
                            "2" = "low",
                            "3" = "hsc")) %>% 
  select(-profile)
head(InputData$hsc.profile)   
names(InputData)


#HSCグループごとのHSCSの平均値、SD
discriptive_HSCS_by_profile <- 
  InputData %>%
  dplyr::group_by(hsc.profile) %>% #性別でグルーピング
  dplyr::summarise(n = n (), #グループの人数を出力
                   hsc_T1_mean = mean (hsc_mean_T1), #hsc_T1の平均
                   hsc_T1_sd = sd (hsc_mean_T1), #hcs_T1のSD
                   eoe_T1_mean = mean (eoe_mean_T1), #eoe_T1の平均
                   eoe_T1_sd = sd (eoe_mean_T1), #eoe_T1のSD
                   lst_T1_mean = mean (lst_mean_T1), #lst_T1の平均
                   lst_T1_sd = sd (lst_mean_T1), #lst_T1のSD
                   aes_T1_mean = mean (aes_mean_T1), #aes_T1の平均
                   aes_T1_sd = sd (aes_mean_T1)) #aes_T1のSD
discriptive_HSCS_by_profile

#HSCグループごとの学校環境と精神的健康変化の平均値、SD

#分位点回帰分析に必要なデータセットを作成する（欠損値削除必要）
data <- 
  InputData %>% 
  select("environment_mean_afterEFA_T2", "eoe_mean_T1", "lst_mean_T1", "aes_mean_T1", "LC", "hsc.profile") 
data <- data %>% drop_na() #欠損値削除
names(data) #変数名確認 
head(data) #先頭行確認

#記述統計量算出byグループ
discriptive_by_hsc_profile <- 
  data %>% 
  dplyr::group_by(hsc.profile) %>% #hsc.profileでグルーピング
  dplyr::summarise(n = n (), #グループの人数を出力
                   environment_mean = mean (environment_mean_afterEFA_T2), #environment_mean_afterEFA_T2の平均
                   environment_sd = sd (environment_mean_afterEFA_T2), #environment_mean_afterEFA_T2のSD
                   health_improvement = mean (LC), #精神的健康変化の平均
                   health_sd = sd (LC)) #精神的健康変化のSD
discriptive_by_hsc_profile

#HSCプロファイルと学校環境プロファイルのクロス集計 ★人数が少なすぎるセルがあるので不可
#data.for.ANOVA %>% 
#  group_by(profile, environment.group) %>%
#  tally %>%
#  spread(environment.group, n)


##HSCグループによる学校環境変化知覚の平均値差（ANOVA）
anova(lm(data$environment_mean_afterEFA_T2~data$hsc.profile)) #ANOVA
TukeyHSD(aov(data$environment_mean_afterEFA_T2~data$hsc.profile)) #post hoc analysis

##HSCグループによる精神的健康変化の平均値差（ANOVA）
anova(lm(data$LC~data$hsc.profile)) #ANOVA
TukeyHSD(aov(data$LC~data$hsc.profile)) #post hoc analysis

##HSCグループごとに分位点回帰（感度分析としての位置づけにする？？？）
library(quantreg)

#データのグループ化
hsc.student <- data %>% filter(hsc.profile == "hsc") #hscのグループ
low.student <- data %>% filter(hsc.profile == "low") #低hscのグループ
medium.student <- data %>% filter(hsc.profile == "medium") #中hscのグループ

names(hsc.student)

#分位点回帰（4分位）#リサンプリング数2000, wild法によるブートストラッピング

#HSCグループ
fit.hsc <- rq(LC ~ environment_mean_afterEFA_T2, data = hsc.student, tau = seq(0, 1, 0.25))
summary(fit.hsc, se = "boot", R = 2000, bsmethod= "wild")

#低HSCグループ
fit.low <- rq(LC ~ environment_mean_afterEFA_T2, data = low.student, tau = seq(0, 1 , 0.25))
summary(fit.low, se = "boot", R = 2000, bsmethod= "wild")

#中HSCグループ
fit.medium <- rq(LC ~ environment_mean_afterEFA_T2, data = medium.student, tau = seq(0, 1 , 0.25))
summary(fit.medium, se = "boot", R = 2000, bsmethod= "wild")


#平均単回帰

#HSCグループ
lm.hsc <- lm(LC ~ environment_mean_afterEFA_T2, data = hsc.student)
summary(lm.hsc)

#低HSCグループ
lm.low <- lm(LC ~ environment_mean_afterEFA_T2, data = low.student)
summary(lm.low)
  #結果メモ：すべて非有意

#中HSCグループ
lm.medium <- lm(LC ~ environment_mean_afterEFA_T2, data = medium.student)
summary(lm.medium)

#各HSCグループの相関係数
cor.test(hsc.student$environment_mean_afterEFA_T2, hsc.student$LC, method = "pearson") #HSCグループ
cor.test(low.student$environment_mean_afterEFA_T2, low.student$LC, method = "pearson") #低HSCグループ
cor.test(medium.student$environment_mean_afterEFA_T2, medium.student$LC, method = "pearson") #中HSCグループ

#各HSCグループの散布図と回帰直線
png("figure/hsc_plot1.png", width = 600, height = 400)
plot1 <- ggplot(data = data, aes(x = environment_mean_afterEFA_T2, y = LC, group = factor(hsc.profile), colour = factor(hsc.profile))) + 
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic(base_size = 14, base_family = "serif") +
  labs(y = "mental health improvement", colour = "profiles")
plot1 <- plot1 + labs(x = "perceived school environment change (1 = negative change, 4 = no change, 7 = positive change)") #1枚で作図
  #メモ；xラベルがなぜか変化しないので、くどいけど2行で作図。これだと変化する。
print(plot1)
dev.off()


png("figure/hsc_plot2.png", width = 800, height = 400)
plot2 <- ggplot(data = data, aes(x = environment_mean_afterEFA_T2, y = LC)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ hsc.profile) #並べて作図
plot2 <- plot2 + theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12), 
    axis.text.x = element_text(size = 12), 
    legend.title = element_text(size = 9)) +labs(x = "perceived school environment change (1 = negative change, 4 = no change, 7 = positive change)", 
    y = "mental health improvement") #並べて作図
print(plot2)
dev.off()


#（5）論文投稿に必要な階層的重回帰分析====
#HSCS合計得点と学校環境変化得点をそれぞれ中心化する








###END ANALYSIS, GOOD JOB SHU!!
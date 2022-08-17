#This is a code I used at the Data Science Hackathon in XCat where I got the second best prize out of 12 groups by myself and almost all paticipants were graduate students except for me.

#This code vedoids of the data from XCat due to the confidential reasons. Therefore, this code would not work properly without the data from them. It is just a portfolio of my Causal Inference skills

#Synthetic Control Method (SCM)

library(Synth)
library(tidyverse)
library(ggplot2)




DF_for_SCM <- read.table("ALL/evaluation_for_SCM.csv",
                         sep = ",",
                         header = T, 
                         stringsAsFactors = F, 
                         fileEncoding = "utf8")
head(DF_for_SCM)

str(DF_for_SCM)

summary(DF_for_SCM)

DF_for_SCM


CG = c(1.0,
       2.0,
       3.0,
       4.0,
       5.0,
       7.0,
       9.0,
       10.0,
       11.0,
       12.0,
       13.0,
       14.0,
       15.0,
       16.0,
       17.0,
       18.0,
       19.0,
       20.0,
       21.0,
       22.0,
       23.0,
       24.0,
       25.0,
       26.0,
       27.0,
       28.0,
       29.0,
       30.0,
       31.0,
       32.0,
       33.0,
       34.0,
       35.0,
       36.0,
       37.0,
       38.0,
       39.0,
       40.0,
       41.0,
       42.0,
       43.0)





# データの前処理
dataprep.out <- dataprep(
  foo = DF_for_SCM,
  predictors = c(
    
    
  ),
  predictors.op = "mean",　
  time.predictors.prior = 2013:2019,       #介入前期間の指定
  special.predictors = list(             # Z_iの調整
    list(list('平日普通残業時間_法定内',  2017:2019, "mean"),
         list('平日普通残業時間_法定外',  2017:2019, "mean"),
         list('平日普通残業時間_計',  2017:2019, "mean"),
         list('平日深夜残業時間_法定内',  2017:2019, "mean"),
         list('平日深夜残業時間_法定外',  2017:2019, "mean"),
         list('平日深夜残業時間_計',  2017:2019, "mean"),
         list('平日残業時間_計',  2017:2019, "mean"),
         list('平日深夜時間',  2017:2019, "mean"),
         list('法定外休日通常時間',  2017:2019, "mean"),
         list('法定外休日深夜時間',  2017:2019, "mean"),
         list('法定外休日時間_計',  2017:2019, "mean"),
         list('法定休日通常時間',  2017:2019, "mean"),
         list('法定休日深夜時間',  2017:2019, "mean"),
         list('残業_計',  2017:2019, "mean"),
         list('休暇時間(有給)',  2017:2019, "mean"),
         list('休暇時間(無給)',  2017:2019, "mean"),
         list('遅刻回数',  2017:2019, "mean"),
         list('遅刻時間(控除有)',  2017:2019, "mean"),
         list('遅刻時間(控除無)',  2017:2019, "mean"),
         list('早退回数',  2017:2019, "mean"),
         list('早退時間(控除有)',  2017:2019, "mean"),
         list('早退時間(控除無)',  2017:2019, "mean"),
         list('欠勤日数',  2017:2019, "mean"),
         list('欠勤時間',  2017:2019, "mean"),
         list('テレワーク勤務',  2017:2019, "mean"),
         
         list("評価", 2013:2019, "mean"), 
         
         list('出勤日数',  2017:2019, "mean"),
         list('実働時間',  2017:2019, "mean"),
         list('休憩時間',  2017:2019, "mean")
    ),
  ),
  dependent = "評価",                  # 目的変数の指定 
  unit.variable = "社員ID",            # 各地域を識別する変数の指定
  unit.names.variable = "社員番号",
  time.variable = "年度",                # 時刻を表す変数の指定
  treatment.identifier = 1000,             # 介入群の指定
  controls.identifier = CG,     # コントロール群の指定
  time.optimize.ssr = 2013:2019,         # 最適化する期間の指定
  time.plot = 2013:2021                  # グラフ化する際に使用する期間の指定
)



dataprep.out$X1

dataprep.out$Z1


#合成コントロール法の実行
synth.out <- synth(data.prep.obj = dataprep.out, method = "BFGS")


# Get result tables
synth.tables <- synth.tab(dataprep.res = dataprep.out,
                          synth.res = synth.out
                          )

print(synth.tables)


#結果の可視化
path.plot(
  synth.res = synth.out, 
  dataprep.res = dataprep.out,
  Ylab = "評価（低い方が高評価）",
  Xlab = "年度",
  Ylim = c(1, 4),
  Legend = c("介入群（テレワーク利用者）", "対照群（テレワーク非利用者）"), 
  Legend.position = "topleft", 
  Main="テレワークが評価（生産性）に与える影響についての合成コントロール法"
)
abline(v=2019.5, col="aquamarine4", lwd=2)
text(x=2019.5,y=1.3,"テレワーク導入", col="aquamarine4")


# 差の出力
gaps.plot(
  synth.res = synth.out, 
  dataprep.res = dataprep.out,
  Ylab = "評価の差（低い方が高評価）", 
  Xlab = "年度", 
  Main = "結果の差の出力"
)
abline(v=2019.5, col='aquamarine4', lwd=2)
abline(h=-0.59, col='black', lwd=2, lty=2)
text(x=2019.5,y=-0.9,"テレワーク導入", col="aquamarine4")
text(x=2013.3,y=-0.63,"-0.59", col="black")




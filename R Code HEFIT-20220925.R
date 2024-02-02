## ======================================================================
#                                数据准备
## ======================================================================

# 读取问卷数据文件excel
## 获取包
## install.packages("openxlsx")
library(openxlsx)
HEFIT01 = read.xlsx("Sample Data_817_20220925.xlsx", "数据表1-fit匹配值")
## 如果只有一个sheet，那么最后面的sheet也可以不写。

## 也可以用下面这个包
## library(readxl)
## HEFIT01 = read_excel("4_源数据分析_20220714.xlsx")

# 查看样本
View(HEFIT01)
names(HEFIT01)

#设置名义变量（连续变量不用设置）
HEFIT01$Subject = factor(HEFIT01$Subject)
HEFIT01$Sex = factor(HEFIT01$Sex)
HEFIT01$AgeGroup = factor(HEFIT01$AgeGroup)
HEFIT01$Area = factor(HEFIT01$Area)
HEFIT01$AreaGroup = factor(HEFIT01$AreaGroup)
HEFIT01$CommunityYearGroup = factor(HEFIT01$CommunityYearGroup)
HEFIT01$CommunityGroup = factor(HEFIT01$CommunityGroup)
HEFIT01$IfMultistorey = factor(HEFIT01$IfMultistorey)
HEFIT01$F.IBFD = factor(HEFIT01$F.IBFD)
HEFIT01$D1.FI = factor(HEFIT01$D1.FI)
HEFIT01$D.IfFall = factor(HEFIT01$D.IfFall)
HEFIT01$IFMDD = factor(HEFIT01$IFMDD)

## ======================================================================
#                             样本特征描述
## ======================================================================

# 获取包
library(psych)
library(plyr)
library(coin)

# 描述样本特征（名义变量）的频次及在样本中占的比例
## 获取Sex、Housing type、Falls、Building periods
table(HEFIT01$Sex)
table(HEFIT01$Sex)/length(HEFIT01$Sex)
table(HEFIT01$IfMultistorey)
table(HEFIT01$IfMultistorey)/length(HEFIT01$IfMultistorey)
table(HEFIT01$D.IfFall)
table(HEFIT01$D.IfFall)/length(HEFIT01$D.IfFall)
table(HEFIT01$CommunityGroup)
table(HEFIT01$CommunityGroup)/length(HEFIT01$CommunityGroup)

### 获取Age
describe(data.frame(HEFIT01[,c(3)])) 

## 获取AgeGroup
ddply(HEFIT01, ~ AgeGroup, function(data) describe(data$Age)) # 数值

## ======================================================================
#                             相关性分析
## ======================================================================

# 描述变量之间的spearman相关系数及相应p值（包含人口特征变量）
library(psych)
data_corr = corr.test(HEFIT01[,c(32, 52, 54:64, 66, 68:74, 77:83, 85:88, 90:91, 94:96, 
                              98:100, 102:105, 107:110, 112:114, 116:122, 
                              124:126)], method = "spearman")

# 输出相关系数矩阵的下三角部分，保留两位小数
lowerMat(data_corr$r, digits = 2)
# 输出相关系数显著性检验的p值
lowerMat(data_corr$p, digits = 4)

## ======================================================================
#                             信度分析
## ======================================================================

# 调用包
library(psych)
## ABC+跌倒第一题，所有小题信度，看raw_alpha
print(alpha(HEFIT01[,c(52, 54:64, 66, 68:74, 77:83, 85:88, 90:91, 94:96, 
                       98:100, 102:105, 107:110, 112:114, 116:122, 
                       124:126, 136)]), digits = 3)
## 小区环境可及性部分小题信度，看raw_alpha
print(alpha(HEFIT01[,c(52, 54:64, 66, 68:74)]), digits = 3)
## 楼道和电梯可及性部分小题信度，看raw_alpha
print(alpha(HEFIT01[,c(77:83, 85:88, 90:91)]), digits = 3)
## 室内环境可及性部分小题信度，看raw_alpha
print(alpha(HEFIT01[,c(94:96, 98:100, 102:105, 107:110, 112:114, 116:122, 
                          124:126)]), digits = 3)

## ======================================================================
#                   效度分析（验证性因子分析CFA）
## ======================================================================

##使用第一种维度进行效度分析
# 调用包
library(lavaan)
## 定义模型
### the public environment, the corridor environment, the indoor environment
CFAmodel = "PEscore =~ A16 + A1 + A2 + A3 + A4 + A5 + A6 + A7 + A8 + A10 + A11
                        + A12 + A20 + A25 + A26 + A27 + A28 + C85 + C86 + C87
            CEscore =~ B1 + B2 + B3 + B5 + B6 + B9 + B11 + B12 + B14 + B16 + B17
                         + B43 + B44
            IEscore =~ C1 + C3 + C4 + C79 + C81 + C83 + C8 + C9 + C14 + C15 + 
                        C20 + C21 + C22 + C26 + C38 + C40 + C42 + C44 + C45 + 
                        C46 + C63 + C64 + C70 + C71 + C57 + C59 + C61"
CFAmodel.fit = cfa(CFAmodel, data = HEFIT01)
summary(CFAmodel.fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
### 结果：P值<0.01，与原模型有较大差异（原假设为无明显差异）
###       CFI和TLI都<0.9，RMSEA>0.05，拟合的效果不是很好。

##使用第二种维度进行效度分析
# 调用包
library(lavaan)
## 定义模型
### the public environment, the corridor environment, the indoor environment
CFAmodel = "PEscore1 =~ A16
            PEscore2 =~ A1 + A2 + A3 + A4 + A5 + A6 + A7 + A8 + A10 + A11
                        + A12
            PEscore3 =~ A20
            PEscore4 =~ A25 + A26 + A27 + A28 + C85 + C86 + C87
            CEscore1 =~ B1 + B2 + B3 + B5 + B6 + B9 + B11
            CEscore2 =~ B12 + B14 + B16 + B17
            CEscore3 =~ B43 + B44
            IEscore1 =~ C1 + C3 + C4
            IEscore2 =~ C79 + C81 + C83
            IEscore3 =~ C8 + C9 + C14 + C15
            IEscore4 =~ C20 + C21 + C22 + C26
            IEscore5 =~ C38 + C40 + C42
            IEscore6 =~ C44 + C45 + C46 + C63 + C64 + C70 + C71
            IEscore7 =~ C57 + C59 + C61
            Fall1 =~ D1.FINum"
CFAmodel.fit = cfa(CFAmodel, data = HEFIT01)
summary(CFAmodel.fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
### 结果：P值<0.01，与原模型有较大差异（原假设为无明显差异）
###       CFI和TLI都<0.9，RMSEA>0.05，拟合的效果不是很好。

## ======================================================================
#                 （全样本）可及性最差的环境障碍
## ======================================================================

## 统计可及性分数进行排序
library(psych)
describe(HEFIT01[,c(52, 54:64, 66, 68:74, 77:83, 85:88, 90:91, 94:96, 
                       98:100, 102:105, 107:110, 112:114, 116:122, 
                       124:126)])

## ======================================================================
#                （不同年龄）可及性最差的环境障碍
## ======================================================================

#  ------------------------------分组统计--------------------------------

## 拆出一个只有65-74的样本集
HEFIT02_age65 <- subset(HEFIT01,AgeGroup=='65-74')
View(HEFIT02_age65)
names(HEFIT02_age65)
## 统计可及性分数进行排序
library(psych)
describe(HEFIT02_age65[,c(52, 54:64, 66, 68:74, 77:83, 85:88, 90:91, 94:96, 
                    98:100, 102:105, 107:110, 112:114, 116:122, 
                    124:126)])

## 拆出一个只有75-84的样本集
HEFIT02_age75 <- subset(HEFIT01,AgeGroup=='75-84')
View(HEFIT02_age75)
names(HEFIT02_age75)
## 统计可及性分数进行排序
library(psych)
describe(HEFIT02_age75[,c(52, 54:64, 66, 68:74, 77:83, 85:88, 90:91, 94:96, 
                          98:100, 102:105, 107:110, 112:114, 116:122, 
                          124:126)])

## 拆出一个只有85-的样本集
HEFIT02_age85 <- subset(HEFIT01,AgeGroup=='85-')
View(HEFIT02_age85)
names(HEFIT02_age85)
## 统计可及性分数进行排序
library(psych)
describe(HEFIT02_age85[,c(52, 54:64, 66, 68:74, 77:83, 85:88, 90:91, 94:96, 
                          98:100, 102:105, 107:110, 112:114, 116:122, 
                          124:126)])

# ----------------------------HEfitAll总分组间差异--------------------------

## HEfitAll总分是否有年龄上的显著差异
### 获取包
library(plyr)
library(coin)
### AgeGroup分组获取HEfitAll统计
ddply(HEFIT01, ~ AgeGroup, function(data) summary(data$HEfitAll)) # 数值
shapiro.test(HEFIT01$HEfitAll) # p-value<0.01，未通过正态性检验
m1 = aov(HEfitAll ~ AgeGroup, data=HEFIT01)
shapiro.test(residuals(m1))  # p-value<0.01，残差也未通过正态性检验
### 组间差异是否显著检验，由于有3个组，且非正态，采用Kruskal-Wallis test
# 整体是否差异显著
library(coin)
kruskal_test(HEfitAll ~ AgeGroup, data=HEFIT01, distribution="asymptotic") 
# 配对检验
# manual post hoc Mann-Whitney U pairwise comparisons
# note: wilcox_test we used above doesn't take two data vectors, so use wilcox.test
age65.age75 = wilcox.test(HEFIT01[HEFIT01$AgeGroup == "65-74",]$HEfitAll, 
                    HEFIT01[HEFIT01$AgeGroup == "75-84",]$HEfitAll, exact=FALSE)
age65.age85 = wilcox.test(HEFIT01[HEFIT01$AgeGroup == "65-74",]$HEfitAll, 
                    HEFIT01[HEFIT01$AgeGroup == "85-",]$HEfitAll, exact=FALSE)
age75.age85 = wilcox.test(HEFIT01[HEFIT01$AgeGroup == "75-84",]$HEfitAll, 
                    HEFIT01[HEFIT01$AgeGroup == "85-",]$HEfitAll, exact=FALSE)
p.adjust(c(age65.age75$p.value, age65.age85$p.value, age75.age85$p.value), method="holm")

## ======================================================================
#                （是否有跌倒史）可及性最差的环境障碍
## ======================================================================

#  ------------------------------分组统计--------------------------------

## 拆出一个只有fallYes的样本集
HEFIT02_fallYes <- subset(HEFIT01,D.IfFall=='有跌倒')
View(HEFIT02_fallYes)
names(HEFIT02_fallYes)
## 统计可及性分数进行排序
library(psych)
describe(HEFIT02_fallYes[,c(52, 54:64, 66, 68:74, 77:83, 85:88, 90:91, 94:96, 
                          98:100, 102:105, 107:110, 112:114, 116:122, 
                          124:126)])

## 拆出一个只有fallNo的样本集
HEFIT02_fallNo <- subset(HEFIT01,D.IfFall=='无跌倒')
View(HEFIT02_fallNo)
names(HEFIT02_fallNo)
## 统计可及性分数进行排序
library(psych)
describe(HEFIT02_fallNo[,c(52, 54:64, 66, 68:74, 77:83, 85:88, 90:91, 94:96, 
                          98:100, 102:105, 107:110, 112:114, 116:122, 
                          124:126)])

# ----------------------------HEfitAll总分组间差异--------------------------

## HEfitAll总分是否有跌倒与否上的显著差异
### 获取包
library(plyr)
library(coin)
### D.IfFall分组获取HEfitAll统计
ddply(HEFIT01, ~ D.IfFall, function(data) summary(data$HEfitAll)) # 数值
shapiro.test(HEFIT01$HEfitAll) # p-value<0.01，未通过正态性检验
m1 = aov(HEfitAll ~ D.IfFall, data=HEFIT01)
shapiro.test(residuals(m1))  # p-value<0.01，残差也未通过正态性检验
### 组间差异是否显著检验，由于有2个组，且非正态，采用Mann-Whitney U test
wilcox_test(HEfitAll ~ D.IfFall, data=HEFIT01,paired=FALSE) # 差异显著
wilcox_test(C21 ~ D.IfFall, data=HEFIT01,paired=FALSE) # C21差异显著

## ======================================================================
#                （不同住房类型）可及性最差的环境障碍
## ======================================================================

#  ------------------------------分组统计--------------------------------

## 拆出一个只有MultistoreyNo的样本集
HEFIT02_MultistoreyNo <- subset(HEFIT01,IfMultistorey=='否')
View(HEFIT02_MultistoreyNo)
names(HEFIT02_MultistoreyNo)
## 统计可及性分数进行排序
library(psych)
describe(HEFIT02_MultistoreyNo[,c(52, 54:64, 66, 68:74, 77:83, 85:88, 90:91, 94:96, 
                            98:100, 102:105, 107:110, 112:114, 116:122, 
                            124:126)])

## 拆出一个只有MultistoreyYes的样本集
HEFIT02_MultistoreyYes <- subset(HEFIT01,IfMultistorey=='是')
View(HEFIT02_MultistoreyYes)
names(HEFIT02_MultistoreyYes)
## 统计可及性分数进行排序
library(psych)
describe(HEFIT02_MultistoreyYes[,c(52, 54:64, 66, 68:74, 77:83, 85:88, 90:91, 94:96, 
                           98:100, 102:105, 107:110, 112:114, 116:122, 
                           124:126)])

# ----------------------------HEfitAll总分组间差异--------------------------

## HEfitAll总分是否有住房类型上的显著差异
### 获取包
library(plyr)
library(coin)
### IfMultistorey分组获取HEfitAll统计
ddply(HEFIT01, ~ IfMultistorey, function(data) summary(data$HEfitAll)) # 数值
shapiro.test(HEFIT01$HEfitAll) # p-value<0.01，未通过正态性检验
m1 = aov(HEfitAll ~ IfMultistorey, data=HEFIT01)
shapiro.test(residuals(m1))  # p-value<0.01，残差也未通过正态性检验
### 组间差异是否显著检验，由于有2个组，且非正态，采用Mann-Whitney U test
wilcox_test(HEfitAll ~ IfMultistorey, data=HEFIT01,paired=FALSE) # 差异不显著

## ======================================================================
#                 （不同建造期）可及性最差的环境障碍
## ======================================================================

#  ------------------------------分组统计--------------------------------

## 拆出一个只有-2002的样本集
HEFIT02_BuildingOld <- subset(HEFIT01,CommunityGroup=='老旧小区')
View(HEFIT02_BuildingOld)
names(HEFIT02_BuildingOld)
## 统计可及性分数进行排序
library(psych)
describe(HEFIT02_BuildingOld[,c(52, 54:64, 66, 68:74, 77:83, 85:88, 90:91, 94:96, 
                                  98:100, 102:105, 107:110, 112:114, 116:122, 
                                  124:126)])

## 拆出一个只有2003-的样本集
HEFIT02_BuildingNew <- subset(HEFIT01,CommunityGroup=='较新小区')
View(HEFIT02_BuildingNew)
names(HEFIT02_BuildingNew)
## 统计可及性分数进行排序
library(psych)
describe(HEFIT02_BuildingNew[,c(52, 54:64, 66, 68:74, 77:83, 85:88, 90:91, 94:96, 
                                98:100, 102:105, 107:110, 112:114, 116:122, 
                                124:126)])

# ----------------------------HEfitAll总分组间差异--------------------------

## HEfitAll总分是否有建造期上的显著差异
### 获取包
library(plyr)
library(coin)
### IfMultistorey分组获取HEfitAll统计
ddply(HEFIT01, ~ CommunityGroup, function(data) summary(data$HEfitAll)) # 数值
shapiro.test(HEFIT01$HEfitAll) # p-value<0.01，未通过正态性检验
m1 = aov(HEfitAll ~ CommunityGroup, data=HEFIT01)
shapiro.test(residuals(m1))  # p-value<0.01，残差也未通过正态性检验
### 组间差异是否显著检验，由于有2个组，且非正态，采用Mann-Whitney U test
wilcox_test(HEfitAll ~ CommunityGroup, data=HEFIT01,paired=FALSE) # 差异不显著

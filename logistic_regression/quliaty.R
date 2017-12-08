quality <- read.csv("quality.csv")
str(quality)
table(quality$PoorCare)
library(caTools)
sessionInfo()
set.seed(88)
split <- sample.split(quality$PoorCare, SplitRatio = .75)
str(split)
split
qualityTrain <- subset(quality, split == TRUE)
qualityTest <- subset(quality, split == FALSE)

nrow(qualityTest)
nrow(qualityTrain)
QualityLog <- glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, family = "binomial")
QualityLog
summary(QualityLog)
predictTrain <- predict(QualityLog, type = "response")
predictTrain
summary(predictTrain)
tapply(predictTrain, qualityTrain$PoorCare, mean)
table(qualityTrain$PoorCare, predictTrain > 0.5)
table(qualityTrain$PoorCare, predictTrain > 0.7)
table(qualityTrain$PoorCare, predictTrain > 0.2)
library(ROCR)
ROCRpred <- predictTrain
ROCRpred <- prediction(predictTrain, qualityTrain$PoorCare)
ROCRpred
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
ROCRperf

plot(ROCRperf, colorize = TRUE)

plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2,1.7))

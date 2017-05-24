library(dataFun)
setwd("~/Documents/Kaggle/Russian Housing")

#Pulling in data
train_data <- read.csv("train.csv")
macro_data <- read.csv("macro.csv")

#Merge Data
training <- merge(x = train_data, y = macro_data, by = "timestamp", all.x = TRUE)
#Output Data for Future Reference
write.csv(training,"train_merged.csv")

prepIt(train_data, output_file = "DataPrep.html")
prepIt(macro_data, output_file = "MacroDataPrep.html")
prepIt(training, output_file = "FullPrep.html")

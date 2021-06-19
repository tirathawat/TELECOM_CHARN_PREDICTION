library(readr)
library(tidyverse)
library(corrplot)

# load data set
churn_bigml_80 <- read_csv("churn-bigml-80.csv")
churn_bigml_20 <- read_csv("churn-bigml-20.csv")
data <- rbind(churn_bigml_80, churn_bigml_20)

# summary data
summary(data)

# response value
data %>% 
  ggplot() +
  geom_bar(aes(Churn, fill = Churn))

table(data$Churn)

#===== distribution of numeric variable =====#

# select numeric column
num_data <- select(data, where(is.numeric))[ , c(-1,-2)]

# plot histogram of each column 
ggplot(gather(num_data), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

#
ggplot(num_data, aes(`Total day minutes`, fill = Churn)) + geom_histogram()

# box plot of total calls
boxplot_data_calls <- gather(data[ ,c(8 , 11 , 14, 17)], "Period", "Total calls", 1:4)

ggplot(boxplot_data_calls) +
  geom_boxplot(aes(`Total calls`, fill = `Period`))

# box plot of total mins
boxplot_data_mins <- gather(data[ ,c(7 , 10 , 13, 16)], "Period", "Total mins", 1:4)

ggplot(boxplot_data_mins) +
  geom_boxplot(aes(`Total mins`, fill = `Period`))

# box plot with churn
ggplot(data) + geom_boxplot(aes(`Total day minutes`, fill = Churn))
ggplot(data) + geom_boxplot(aes(`Total day calls`, fill = Churn))
ggplot(data) + geom_boxplot(aes(`Total eve minutes`, fill = Churn))
ggplot(data) + geom_boxplot(aes(`Total eve calls`, fill = Churn))
ggplot(data) + geom_boxplot(aes(`Total night minutes`, fill = Churn))
ggplot(data) + geom_boxplot(aes(`Total night calls`, fill = Churn))
ggplot(data) + geom_boxplot(aes(`Total intl minutes`, fill = Churn))
ggplot(data) + geom_boxplot(aes(`Total intl calls`, fill = Churn))

#===== relation of numerical variable with churn =====#

# correlation plot
cor <- cor(num_data)
corrplot(cor, 
         type = "upper", 
         order = "hclust", 
         tl.col = "black", 
         tl.srt = 45,
         )

# correlation plot show relation between minutes and charge
ggplot(num_data, aes(`Total day minutes`, `Total day charge`)) + geom_point()

#===== distribution of categorical variable =====#

# distribution for one variable
ggplot(data) +
  geom_bar(aes(`International plan`, fill = `International plan`))

ggplot(data) +
  geom_bar(aes(`Voice mail plan`, fill = `Voice mail plan`))

ggplot(data) +
  geom_bar(aes(`State`, fill = `State`))

#===== relation of categorical variable with churn =====#

# relation categorical with churn

ggplot(data) +
  geom_bar(aes(`International plan`, fill = `Churn`), position = 'fill')

ggplot(data) +
  geom_bar(aes(`International plan`, fill = `Churn`), position = 'dodge')

ggplot(data) +
  geom_bar(aes(`Voice mail plan`, fill = `Churn`), position = 'fill')

ggplot(data) +
  geom_bar(aes(`Voice mail plan`, fill = `Churn`), position = 'dodge')

ggplot(data) +
  geom_bar(aes(`State`, fill = `Churn`), position = 'fill')

ggplot(data) +
  geom_bar(aes(`State`, fill = `Churn`), position = 'dodge')

ggplot(data) +
  geom_bar(aes(`Customer service calls`, fill = `Churn`), position = 'fill')

ggplot(data) +
  geom_bar(aes(`Customer service calls`, fill = `Churn`), position = 'dodge')

# cross-tab with churn

table(data$`International plan`, data$Churn)
table(data$`State`, data$Churn)
table(data$`Voice mail plan`, data$Churn)



















#1
su <- read.delim("C:\\Users\\ivy-0\\Desktop\\UM\\587Data Mining Algorithms and Applications\\Assignments\\HW1\\Su_raw_matrix.txt")
mean_su <- mean(su$Liver_2.CEL)
sd_su <- sd(su$Liver_2.CEL)
col_means <- colMeans(su)
col_sums <- colSums(su)
cat("Mean of Liver_2.CEL: ", mean_su, "\n")
cat("Standard Deviation of Liver_2.CEL: ", sd_su, "\n")
cat("Column Means: ", col_means, "\n")
cat("Column Sums: ", col_sums, "\n")

#2
set.seed(666) 
data_a <- rnorm(10000, mean = 0, sd = 0.2)
set.seed(666) 
data_b <- rnorm(10000, mean = 0, sd = 0.5)
library(ggplot2)


hist(data_a, xlim = c(-5, 5), main = "Histogram (mean=0, sd=0.2)", xlab = "Values", ylab = "Frequency", col = "blue")

hist(data_b, xlim = c(-5, 5), main = "Histogram (mean=0, sd=0.5)", xlab = "Values", ylab = "Frequency", col = "red")


#3
dat <- data.frame(cond = factor(rep(c("A","B"), each=200)),
                  rating = c(rnorm(200),rnorm(200, mean=.8)))
#3b-3e
ggplot(dat, aes(x=rating, fill=cond)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")
ggplot(dat, aes(x=rating, fill=cond)) +
  geom_histogram(binwidth=.5, position="dodge")
ggplot(dat, aes(x=rating, colour=cond)) +
  geom_density()
ggplot(dat, aes(x=rating, fill=cond)) +
  geom_density(alpha=.3)

#3f

diabetes <- read.csv("C:\\Users\\ivy-0\\Desktop\\UM\\587Data Mining Algorithms and Applications\\Assignments\\HW1\\diabetes_train.csv")

ggplot(diabetes, aes(x=mass, fill=class)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")

ggplot(diabetes, aes(x=mass, fill=class)) +
  geom_histogram(binwidth=.5, position="dodge")
ggplot(diabetes, aes(x=mass, fill=class)) +
  geom_density()
ggplot(diabetes, aes(x=mass, fill=class)) +
  geom_density(alpha=.3)



#4
passengers <- read.csv("C:\\Users\\ivy-0\\Desktop\\UM\\587Data Mining Algorithms and Applications\\Assignments\\HW1\\titanic.csv")
library(tidyr)
library(dplyr)
#Removing Empty Spaces and Summarizing
passengers %>% drop_na() %>% summary()
#Finding All Male Passengers
male_passengers <- passengers %>% filter(Sex == "male")
#Sorting by Ticket Price, Highest First
passengers_sorted <- passengers %>% arrange(desc(Fare))
#Adding Family Size Information
passengers_with_famsize <- passengers %>% mutate(FamSize = Parch + SibSp)
#Average Fare and Survival Count by Gender
summary_by_sex <- passengers %>% group_by(Sex) %>% summarise(meanFare = mean(Fare), numSurv = sum(Survived))



#5
diabetes <- read.csv("C:\\Users\\ivy-0\\Desktop\\UM\\587Data Mining Algorithms and Applications\\Assignments\\HW1\\diabetes_train.csv")
skin_percentiles <- quantile(diabetes$skin, c(0.1, 0.3, 0.5, 0.6))
print(skin_percentiles)

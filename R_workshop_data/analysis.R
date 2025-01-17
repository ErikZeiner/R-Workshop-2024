l_bio <- read.csv('learner_biodata.csv')
l_bio <- read.csv2('learner_biodata.csv')
head(l_bio)

l_bio_BrE <- l_bio[ l_bio$BrE_score > 0 ,]
l_bio_AmE <- l_bio[l_bio$AmE_score > 0,]

nrow(l_bio) - (nrow(l_bio_AmE) + nrow(l_bio_BrE))

head(l_bio[(l_bio$BrE_score > 0 & l_bio$AmE_score > 0),])

head(l_bio[(l_bio$BrE_score == 0 & l_bio$AmE_score == 0),])


l_data <- read.csv2('laterals_data_learners.csv')

l_data_BrE <- l_data[l_data$subject %in% l_bio_BrE$subject,]
l_data_AmE <- l_data[l_data$subject %in% l_bio_AmE$subject,]

nrow(l_data) - nrow(l_data_BrE) - nrow(l_data_AmE)

table(l_data$subject)


l_data_BrE_pre <- l_data_BrE[l_data_BrE$context == 1,]
l_data_BrE_non <- l_data_BrE[l_data_BrE$context == 0,]

l_data_AmE_pre <- l_data_AmE[l_data_AmE$context == 1,]
l_data_AmE_non <- l_data_AmE[l_data_AmE$context == 0,]

plot(l_data_BrE_pre$FAR, l_data_BrE_pre$diff.bk)
plot(l_data_AmE_pre$FAR,l_data_AmE_pre$diff.bk)


plot(l_data_BrE_pre$FAR,l_data_BrE_pre$diff.bk, ylim=c(0,10))
plot(l_data_AmE_pre$FAR,l_data_AmE_pre$diff.bk, ylim=c(0,10))

par(mfrow = c(1,2))

plot(l_data_BrE_non$FAR,l_data_BrE_non$diff.bk, ylim=c(0,10))
plot(l_data_AmE_non$FAR,l_data_AmE_non$diff.bk, ylim=c(0,10))

par(mfrow=c(2,2))
plot(l_data_BrE_pre$FAR,l_data_BrE_pre$diff.bk, ylim=c(0,10))
plot(l_data_AmE_pre$FAR,l_data_AmE_pre$diff.bk, ylim=c(0,10))
plot(l_data_BrE_non$FAR,l_data_BrE_non$diff.bk, ylim=c(0,10))
plot(l_data_AmE_non$FAR,l_data_AmE_non$diff.bk, ylim=c(0,10))


n_bio <- read.csv2('ns_biodata.csv')

n_bio_BrE <- n_bio[n_bio$variety == 'BrE',]
n_bio_AmE <- n_bio[n_bio$variety == 'AmE',]


nrow(n_bio) - nrow(n_bio_BrE) - nrow(n_bio_AmE)


n_data <- read.csv2('laterals_data_ns.csv')
nrow(n_data)


n_data_BrE <- n_data[n_data$subject %in% n_bio_BrE$subject,]
n_data_AmE <- n_data[n_data$subject %in% n_bio_AmE$subject,]

n_data_BrE_pre <- n_data_BrE[n_data_BrE$context == 1,]
n_data_BrE_non <- n_data_BrE[n_data_BrE$context == 0,]

n_data_AmE_pre <- n_data_AmE[n_data_AmE$context == 1,]
n_data_AmE_non <- n_data_AmE[n_data_AmE$context == 0,]

nrow(n_data) - nrow(n_data_BrE_pre) - nrow(n_data_BrE_non) - nrow(n_data_AmE_pre) - nrow(n_data_AmE_non)

plot(n_data_BrE_pre$diff.bk)
plot(n_data_BrE_non$diff.bk)
plot(n_data_AmE_pre$diff.bk)
plot(n_data_AmE_non$diff.bk)

par(mfrow = c(2,2))
# average value
plot(l_data_BrE_pre$FAR,l_data_BrE_pre$diff.bk, ylim=c(0,10))
abline(h=mean(as.numeric(n_data_BrE_pre$diff.bk)))
plot(l_data_AmE_pre$FAR,l_data_AmE_pre$diff.bk, ylim=c(0,10))
abline(h=mean(as.numeric(n_data_AmE_pre$diff.bk)))
plot(l_data_BrE_non$FAR,l_data_BrE_non$diff.bk, ylim=c(0,10))
abline(h=mean(as.numeric(n_data_BrE_non$diff.bk)))
plot(l_data_AmE_non$FAR,l_data_AmE_non$diff.bk, ylim=c(0,10))
abline(h=mean(as.numeric(n_data_AmE_non$diff.bk)))

par(mfrow=c(1,1))

options(repr.plot.width=4, repr.plot.height=2)
plot(n_data_BrE_non$F2.Hz,n_data_BrE_non$F1.Hz, xlim=c(3500,500), ylim=c(1200,200), col = "red", pch = 16, xlab = "F2", ylab = "F1")
points(n_data_BrE_pre$F2.Hz,n_data_BrE_pre$F1.Hz, col = "blue", pch = 17)
title("Formant values for non-vocalic and pre-vocalic laterals")
legend("bottomleft", legend = c("pre-vocalic", "non-vocalic"), col = c("red", "blue"), pch = c(16,17))


mean(as.numeric(n_data_BrE_non$F1.Hz))
mean(as.numeric(n_data_BrE_non$F2.Hz))

mean(as.numeric(n_data_BrE_pre$F1.Hz))
mean(as.numeric(n_data_BrE_pre$F2.Hz))

# And now for something completely different.

n_samples <- 100
data <- data.frame(value = c(rnorm(n_samples,0,1),rnorm(n_samples,0,3),rnorm(n_samples,3,1)), category = c("A","B","C"))
head(data)
boxplot(data$value)
# What is a boxplot - five number summary - the minimum, the maximum, the sample median, and the first and third quartiles.
boxplot(data$value~data$category, yaxp = c(-10,10, 20))
max(data$value)
min(data$value)

# boxplot is like looking at a histogram from the top
plot(data[data$category == "A",]$value)
par(mfrow=c(3,1))
hist(data[data$category == "A",]$value)
plot(density(data[data$category == "A",]$value))
boxplot(data[data$category == "A",]$value, horizontal = TRUE)
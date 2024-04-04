iris
class(iris)
View(iris)
nrow(iris)
ncol(iris)
summary(iris)
iris[ , c("Sepal.Length","Species")]
iris[c(100:150),]
mean(iris$Sepal.Length)
quantile(iris$Petal.Width, probs = seq(from = 0.1, to = 0.9, by =0.1))
dfManga <- read.csv("L:/BUT/SD/Promo 2023/elitherland/R/manga.csv", header = TRUE, sep = ",", dec = ".")
View(dfManga)
View(dfAnime)
dim(dfManga)
mean(dfManga$Score)
quantile(dfManga$Score, probs = seq(from = 0.1, to = 0.9, by = 0.1))
extraction1 = subset(dfManga, Score > 9)
nrow(extraction1)
effectifRating <- table(dfManga$Ranked)
print(effectifRating)
length(effectifRating)
prop.table(effectifRating)
extraction3 <- subset(dfManga, Rating == "R - 17+ (violence & profanity)" &
                        Score >= 8)
nrow(extraction3)
dfManga <- dfManga[ , c("Title","Score","Vote","Ranked")]
dfManga$Type <- "Manga"

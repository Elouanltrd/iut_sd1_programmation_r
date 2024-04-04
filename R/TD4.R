df = read.csv("L:/BUT/SD/Promo 2023/elitherland/R/velov.csv", header = TRUE, dec=",",sep=";")
summary(df)
class(df$status)
class(df$CodePostal)
df$status = as.factor(df$status)
df$CodePostal = as.factor(df$CodePostal)
df$bornes = ifelse(df$capacity != (df$bikes + df$stands), "KO" , "OK")

hist(x = df$capacity, main = "Distribution de \n la capacité des stations")
hist(x = df$capacity, main = "Distribution de \n la capacité des stations",breaks = 6)
hist(x = df$capacity, main = "Distribution de \n la capacité des stations",breaks = 6,col = "red")
hist(x = df$capacity, main = "Distribution de \n la capacité des stations",breaks = 6,col = "red",xlab="Capacity")
abline(h = 100, col = "blue", lty = 2)

hist(x = df$capacity, main = "Distribution de \n la capacité des stations",col = "red",probability = TRUE,xlab = "Capacity")
lines(density(df$capacity),lty = 2,col = "blue",lwd = 4)
hist(x = df$capacity, main = "Distribution de \n la capacité des stations",col = "red",probability = TRUE,xlab = "Capacity",ylim = c(0,0.08))
lines(density(df$capacity),lty = 2,col = "blue",lwd = 4)

boxplot(x = df$capacity, main = "Boxplot de \n la capacité des stations")
boxplot(x = df$capacity, main = "Boxplot de \n la capacité des stations",horizontal = TRUE)
boxplot(x = df$capacity, main = "Boxplot de \n la capacité des stations",horizontal = FALSE,outline = FALSE)
points(moy, col = "red", pch = 15, cex = 2)

par(mfrow=c(1,2)) 

df7 = subset(df, CodePostal == "69007")
boxplot(x = df7$bikes, main = "Boxplot nb vélos \n 69007",ylim = c(0,40))

df8 = subset(df, CodePostal == "69008")
boxplot(x = df8$bikes, main = "Boxplot nb vélos \n 69008",ylim = c(0,40))

par(mfrow=c(1,1)) 

boxplot(formula = bikes ~ bonus,data = df, main = "Dispo vélos vs Stations Bonus")

means <- tapply(X = df$bikes, INDEX = df$bonus, FUN = function(X) mean(X))
print(means)

points(means, col = "red", pch = 19)


effectif = table(df$bonus)
barplot(height = effectif,main = "Répartition du nombre \n de station bonus")
barplot(height = effectif,main = "Répartition du nombre \n de station bonus",horiz = TRUE)

frequence = prop.table(effectif)
barplot(height = frequence,main = "Répartition en % du nombre \n de station bonus",horiz = TRUE)

effectif = table(df$banking, df$bonus)
print(effectif)
barplot(height = effectif,main = "Bonus vs Banking",xlab = "Station Bonus ?")


frequence = prop.table(x = effectif)
barplot(height = frequence,main = "Bonus vs Banking",xlab = "Station Bonus ?",col = c("red","green"))

legend_labels <- colnames(frequence)

legend(x = "topright", legend = legend_labels, fill  = c("red","green"))

print(frequence)







plot(x = df$stands, y = df$capacity,main = "Place disponible vs Capacité")

plot(x = df$stands, y = df$capacity,main = "Place disponible vs Capacité",xlim = c(0,60),ylim = c(0,60),pch=19)

df$bornes = as.factor(df$bornes)
plot(x = df$stands, y = df$capacity,
     main = "Place disponible vs Capacité",
     xlim = c(0,60),
     ylim = c(0,60),
     col = df$bornes,
     pch=19)

# Ajouter une légende
legend("topright", legend = levels(df$bornes),
       col = palette(), pch = 19)


myColors <- c("red", "blue", "green")  
# Ajoutez plus de couleurs si nécessaire avec le code HTML des couleurs à la place des noms

# Tracer le graphique
plot(x = df$stands, y = df$capacity,
     main = "Place disponible vs Capacité",
     xlim = c(0, 60),
     ylim = c(0, 60),
     col = myColors[df$bornes],
     pch = 19)

# Ajouter une légende
legend("topright", legend = levels(df$bornes),
       col = myColors, pch = 19)

moy_stands = mean(df$stands)
moy_capacity = mean(df$capacity)
points(x = moy_stands,y = moy_capacity, 
       pch = 15,
       col = myColors[3],
       cex = 2)
# Librairies nécessaires
library(leaflet)
library(dplyr)
library(ggplot2)

# Créer une carte Leaflet
maCarte <- leaflet(df) %>% 
  addTiles() %>% 
  addMarkers(~position_longitude, 
             ~position_latitude, 
             popup = ~address)

# Afficher la carte
maCarte






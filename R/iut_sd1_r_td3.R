library(readxl)
pokemon = read_excel(path = "L:/BUT/SD/Promo 2023/elitherland/R/pokemon.xlsx", sheet = "pokemon")
dim(pokemon)
ncol(pokemon)
nrow(pokemon)
summary(pokemon)

as.factor(pokemon$generation)
as.factor(pokemon$is_legendary)
as.factor(pokemon$type)
summary(pokemon)

med = median(pokemon$attack)
pokemon$group_attack = ifelse(test = pokemon$attack > med, yes = "attack+", no = "attack-")
pokemon$group_attack = as.factor(pokemon_group_attack)
summary(pokemon$group_attack)

pokemon$water_fire = ifelse(pokemon$type %in% c("fire","water"),"yes","no")
pokemon$water_fire <-as.factor(pokemon$water_fire)
summary(pokemon$water_fire)

best_attack = quantile(pokemon$attack, probs = 0.75)
best_defense = quantile(pokemon$defense, probs = 0.75)
best_speed = quantile(pokemon$speed, probs = 0.75)
pokemon$best = ifelse(pokemon$attack > best_attack & pokemon$defense > best_defense & pokemon$speed > best_speed, "yes", "no")
pokemon$best = as.factor(pokemon$best)
summary(pokemon$best)

requete = subset(pokemon, is.na(pokemon$weight_kg))
View(requete)                 
                 
requete = subset(pokemon, !is.na(pokemon$weight_kg))
View(requete)                 

med_weight = median(pokemon$weight_kg,na.rm = TRUE)
med_height = median(pokemon$height_m,na.rm = TRUE)
weight_kgNa = ifelse(is.na(pokemon$weight_kg),yes = med_weight,no = pokemon$weight_kg)
height_mNA = ifelse(is.na(pokemon$height_m),yes = med_height, no = pokemon$height_m)

weight_group = cut(pokemon$weight_kg, breaks = 3, labels = c("Leger", "Moyen", "Lourd"))
summary(weight_group)

pokemon$height_m_group = cut(pokemon$height_m,breaks = c(0,1,2,3,max(pokemon$height_m,na.rm = TRUE)))
summary(pokemon$height_m_group)

pokemon$defense_group = cut(pokemon$defense,breaks = quantile(pokemon$defense,na.rm = TRUE),include.lowest = TRUE)
summary(pokemon$defense_group)

aggregate(x = attack ~ type, data = pokemon, FUN = function(x) mean(x))

aggregate(x = attack ~ generation + type, data = pokemon, FUN = function(x) median(x))

aggregate(x = pokemon$pokedex_number ~ type, data = pokemon, FUN = function(x) length(x))

aggregate(speed ~ generation + type,data = pokemon, FUN = function(x) c(moy = mean(x),med = median(x),eff = length(x)))




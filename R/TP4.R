salaire_net_cadre = function(salaire_brut) {
  salaire_net_avant_impot = salaire_brut * 0.75
  return(salaire_net_avant_impot) }
  #Test de la fonction
  salaire_net_cadre(salaire_brut = 3000)
  
  
salaire_net_cadre = function(salaire_brut=2500) {
  salaire_net_avant_impot = salaire_brut * 0.75
  return(salaire_net_avant_impot) }
  #Test de la fonction
  salaire_net_cadre()  
  
  
  
salaire_net_cadre = function(salaire_brut,temps_de_travail) {
  salaire_net_avant_impot = salaire_brut * 0.75 * temps_de_travail
  return(salaire_net_avant_impot) }
  #Test de la fonction
  salaire_net_cadre(salaire_brut = 3000,temps_de_travail = 0.8)   

  
  
  salaire_net_cadre = function(salaire_brut = 2500,temps_travail = 1) {
    
    if (!is.numeric(salaire_brut)) {
      return("Erreur :  le salaire brut doit être une valeur numérique")
    }
    
    if (is.numeric(temps_travail) & (temps_travail >= 0) & (temps_travail <= 1)) {
      return("Erreur :  le temps de travail doit être une valeur numérique entre 0 et 1")
    }
    
    salaire_net_avant_impot = salaire_brut * 0.75 * temps_travail
    return(salaire_net_avant_impot) 
  }
  #Test de la fonction
  salaire_net_cadre(salaire_brut = 2000, temps_travail = "100%")
  salaire_net_cadre(salaire_brut = 2000, temps_travail = 0.8)
  salaire_net_cadre(salaire_brut = 2000, temps_travail = 100)

  
salaire_net = function(salaire_brut = 2500,temps_travail = 1, statut) {
    
    if (!is.numeric(salaire_brut)) {
      return("Erreur :  le salaire brut doit être une valeur numérique")
    }
    
    if (is.numeric(temps_travail) & (temps_travail >= 0) & (temps_travail <= 1)) {
      return("Erreur :  le temps de travail doit être une valeur numérique entre 0 et 1")
    }
    
    if (!statut %in% c("cadre","non cadre")) {
      return("Erreur :  le statut doit être cadre ou non cadre")
    }
    
    if (statut == "cadre") {
      salaire_net_avant_impot = salaire_brut * temps_travail * 0.75
    } else {
      salaire_net_avant_impot = salaire_brut * temps_travail * 0.78
    }
    
    return(salaire_net_avant_impot) 
  }
  #Test de la fonction
  salaire_net(salaire_brut = 2000, statut = "cadre")
  salaire_net(salaire_brut = 2000, statut = "non cadre")
  salaire_net(salaire_brut = 2000, statut = "technicien")  
  salaire_net = function(salaire_brut = 2500,temps_travail = 1, statut) {
    
    if (!is.numeric(salaire_brut)) {
      return("Erreur :  le salaire brut doit être une valeur numérique")
    }
    
    if (is.numeric(temps_travail) & (temps_travail >= 0) & (temps_travail <= 1)) {
      return("Erreur :  le temps de travail doit être une valeur numérique entre 0 et 1")
    }
    
    if (!statut %in% c("cadre","non cadre")) {
      return("Erreur :  le statut doit être cadre ou non cadre")
    }
    
    if (statut == "cadre") {
      salaire_net_avant_impot = salaire_brut * temps_travail * 0.75
    } else {
      salaire_net_avant_impot = salaire_brut * temps_travail * 0.78
    }
    
    if (salaire_net_avant_impot <= 1591) {
      salaire_net_apres_impot <- salaire_net_avant_impot
    } else if (salaire_net_avant_impot <= 2006) {
      salaire_net_apres_impot <- salaire_net_avant_impot * (1 - 0.029)
    } else if (salaire_net_avant_impot <= 3476) {
      salaire_net_apres_impot <- salaire_net_avant_impot * (1 - 0.099)
    } else if (salaire_net_avant_impot <= 8557) {
      salaire_net_apres_impot <- salaire_net_avant_impot * (1 - 0.20)
    } else {
      salaire_net_apres_impot <- salaire_net_avant_impot * (1 - 0.43)
    }
    
    return(salaire_net_apres_impot) 
  }  
salaire_net()  


shifumi <- function() {
  # Demander à l'utilisateur de saisir une valeur
  choix_utilisateur <- readline(prompt = "Choisissez entre pierre, papier ou ciseaux : ")
  
  # Vérifier si l'utilisateur a saisi une valeur valide
  if (choix_utilisateur %in% c("pierre", "papier", "ciseaux")) {
    # Simuler un choix aléatoire pour l'ordinateur
    choix_ordi <- sample(c("pierre", "papier", "ciseaux"), 1)
    
    # Afficher les choix de l'utilisateur et de l'ordinateur
    cat("Votre choix :", choix_utilisateur, "\n")
    cat("Choix de l'ordinateur :", choix_ordi, "\n")
    
    # Retourner le résultat du jeu
    if (choix_utilisateur == choix_ordi) {
      return("Égalité !")
    } else if ((choix_utilisateur == "pierre" & choix_ordi == "ciseaux") |
               (choix_utilisateur == "papier" & choix_ordi == "pierre") |
               (choix_utilisateur == "ciseaux" & choix_ordi == "papier")) {
      return("Vous avez gagné !")
    } else {
      return("L'ordinateur a gagné !")
    }
  } else {
    return("Valeur invalide. Veuillez choisir entre pierre, papier ou ciseaux.")
  }
}

#Test de la fonction
shifumi()

resultat = 0
for (element in c(1,2,3,4,5)) {
  resultat = resultat +  element
  print(paste("le resultat est : ",resultat))
}


element = 1
resultat = 0
while (resultat <= 50) {
  resultat = resultat +  element
  print(paste("le resultat est : ",resultat))
  print(paste("le programme s'est arrêté à la valeur : ", element))
  element = element + 1
}



##################################### TP2 ACP ###############################################

########################## Analyse des PVs de délibérations #################################


# ****************** Nom et Prénom(s) Étudiant 1: ABOUD Ibrahim. ****************************

# ****************** Nom et Prénom(s) Étudiant 2: BOUYAKOUB Rayane. *************************

# ******************************** Option: SID ***********************************************

# ******************************** Groupe: 2SD1 **********************************************




####################### Partie 1: On considère tous les étudiants ###################


# Installation des packages nécessaires.
install.packages("readxl")
install.packages("dplyr")
install.packages("corrplot")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("ggplot2")


# Importation des packages.
library(readxl)
library(dplyr)
library(corrplot)
library(FactoMineR)
library(ggplot2)
library(factoextra)



pv <- "PV_1CS_2021_ANNUEL.xlsx" # Le chemin du fichier qui contient le PV de délibération de la promotion 1CS 2021.
affectation_specs <- "LISTE_Affectations_2022.xlsx" # Le chemin du fichier qui contient l'affectation des étudiants de la promotion 1CS 2021 aux 4 spécialités (SID, SIQ, SIL, SIT)


# On combine les 4 feuilles de calcul dans un seul tableau.
sheet_names <- excel_sheets(affectation_specs)
list_of_dfs <- lapply(sheet_names, function(sheet) {
	read_excel(affectation_specs, sheet = sheet)
})
specs <- bind_rows(list_of_dfs)



# On effectue une jointure externe entre le PV de délibération et le tableau contenant l'affectation des étudiants aux 4 spécialités.
notes_etudiants <- read_excel(pv)
# 'all.y' pour inclure tous les étudiants, y compris les redoublants qui n'ont pas été affectés à une spécialité. by='Matricule' pour spécifier l'attribut utilisé pour effectuer la jointure.
data <- merge(specs, notes_etudiants, by="Matricule", all.y = TRUE) 
# Restituer l'ordre initial (selon le rang annuel)
data <- data[order(data$Rang_annuel), ] 
rownames(data) <- NULL 



colnames(data)
# [1] "Matricule"     "Spécialité"    "Groupe_S1"     "SYS1"          "RES1"          "UEF1.1.1"      "ANUM"          "RO"            "ORG"           "UEM1.1"        "LANG1"         "UET1.1"        "IGL"          
# [14] "THP"           "UEF1.1.2"      "Ne_S1"         "Rang_S1"       "Moy_S1"        "Moy_rachatS1"  "Crd_S1"        "Groupe_S2"     "MCSI"          "BDD"           "UEF1.2.2"      "SEC"           "CPROJ"        
# [27] "PROJ"          "UEM1.2"        "LANG2"         "UET1.2"        "ARCH"          "SYS2"          "RES2"          "UEF1.2.1"      "Ne_S2"         "Rang_S2"       "Moy_S2"        "Moy_rachatS2"  "Crd_S2"       
# [40] "Rang_annuel"   "Moy_annuelle"  "Moy_rachat"    "Crd_annuel"    "Decision_jury"

# On supprime les colonnes non pertinentes pour l'analyse.
cleaned_data <- data[, -c(1, 3, 6, 10, 12, 15, 16, 17, 18, 19, 20, 21, 24, 28, 30, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44)]



data_to_analyze <- cleaned_data[, -c(1)] # On supprime la colonne 'Spécialité', car on n'a pas besoin de cette dernière dans la première partie.
# Visualisation de la matrice de corrélation.
corr_matrix <- cor(data_to_analyze)
corr_matrix
corrplot(corr_matrix, type='upper')


# Effectuer l'ACP.
pca_result <- PCA(data_to_analyze, graph = FALSE)


# Étude des valeurs propres.
pca_result$eig
fviz_eig(pca_result, addlabels=TRUE)


# Étude des variables
fviz_cos2(pca_result, choice="var", axes=1:2)
corrplot(get_pca_var(pca_result)$cos2, is.corr=FALSE)
fviz_pca_var(pca_result, col.var="black", repel = TRUE)
fviz_pca_var(pca_result, col.var ="cos2", gradient.cols=c("#00AFBB","#E7B800","#FC4E07"), repel = TRUE)


# Signification des axes
pca_result$var$coord # Pour avoir la projection des variables
pca_result$var$contrib # Pour avoir les contributions absolues des variables aux différents axes.
# Visualiser la contribution absolue des variables au premier axe.
fviz_contrib(pca_result, choice="var", axes=1)
# Visualiser la contribution absolue des variables au deuxième axe.
fviz_contrib(pca_result, choice="var", axes=2)



#Étude des individus

# On cherche les individus qui contribuent le plus dans la construction de l'axe 1.
contrib_axis_1 <- pca_result$ind$contrib[, 1]
fviz_pca_ind(pca_result, col.ind=contrib_axis_1, gradient.cols=c("#00AFBB","#E7B800","#FC4E07"), repel = TRUE) + labs(color = "Contrib to Dim1")
fviz_contrib(pca_result, choice="ind", axes=1, top = 40) #On limite le nombre d'individus à 40 pour avoir une visualisation lisible.

# On cherche les individus qui contribuent le plus dans la construction de l'axe 2.
contrib_axis_2 <- pca_result$ind$contrib[, 2]
fviz_pca_ind(pca_result, col.ind=contrib_axis_2, gradient.cols=c("#00AFBB","#E7B800","#FC4E07"), repel = TRUE) + labs(color = "Contrib to Dim2")
fviz_contrib(pca_result, choice="ind", axes=2, top = 50) #On limite le nombre d'individus à 40 pour avoir une visualisation lisible.


# Qualité de représentations des individus
fviz_pca_biplot(pca_result, repel=TRUE, col.ind ="cos2", gradient.cols=c("#00AFBB","#E7B800","#FC4E07"), col.var="#2E9FDF")
fviz_pca_ind(pca_result, col.ind ="cos2", gradient.cols=c("#00AFBB","#E7B800","#FC4E07"), repel = TRUE)
fviz_cos2(pca_result, choice="ind", axes=1:2, top = 50)


# Biplot Individus-Variables.
fviz_pca_biplot(pca_result, repel=TRUE, col.ind ="#696969", col.var="#2E9FDF")



#################################### Partie 2 #################################### 


# Supprimer les individus redoublants, en situation d'abandon ou en congés académique qui n'ont pas une spécialité attribuée.
data_to_analyze <- cleaned_data[-c(156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166),]
data_to_analyze

# Matrice de corrélation.
corr_matrix <- cor(data_to_analyze[, -c(1)])
corr_matrix
corrplot(corr_matrix, type='upper')


# Effectuer l'ACP.
pca_result <- PCA(data_to_analyze[,-1], graph = FALSE)


# Étude des valeurs propres.
pca_result$eig
fviz_eig(pca_result, addlabels = TRUE)


# Étude des variables
fviz_cos2(pca_result, choice="var", axes=1:2)
corrplot(get_pca_var(pca_result)$cos2, is.corr=FALSE)
fviz_pca_var(pca_result, col.var="black", repel = TRUE)
fviz_pca_var(pca_result, col.var ="cos2", gradient.cols=c("#00AFBB","#E7B800","#FC4E07"), repel = TRUE)



# Signification des axes
pca_result$var$coord # Pour avoir la projection des variables
pca_result$var$contrib # Pour avoir les contributions absolues des variables aux différents axes.
# Visualiser la contribution absolue des variables au premier axe.
fviz_contrib(pca_result, choice="var", axes=1)
# Visualiser la contribution absolue des variables au deuxième axe.
fviz_contrib(pca_result, choice="var", axes=2)



# Biplot Individus-Variables.
fviz_pca_biplot(pca_result, repel=TRUE, col.ind ="#696969", col.var="#2E9FDF")




#Étude des individus

# On cherche les individus qui contribuent le plus dans la construction de l'axe 1.
contrib_axis_1 <- pca_result$ind$contrib[, 1]
fviz_pca_ind(pca_result, col.ind=contrib_axis_1, gradient.cols=c("#00AFBB","#E7B800","#FC4E07"), repel = TRUE) + labs(color = "Contrib to Dim1")
fviz_contrib(pca_result, choice="ind", axes=1, top = 50) #On limite le nombre d'individus à 70 pour avoir une visualisation lisible.



# On cherche les individus qui contribuent le plus dans la construction de l'axe 2.
contrib_axis_2 <- pca_result$ind$contrib[, 2]
fviz_pca_ind(pca_result, col.ind=contrib_axis_2, gradient.cols=c("#00AFBB","#E7B800","#FC4E07"), repel = TRUE) + labs(color = "Contrib to Dim2")
fviz_contrib(pca_result, choice="ind", axes=2, top = 50) #On limite le nombre d'individus à 40 pour avoir une visualisation lisible.


# Qualité de représentations des individus
fviz_pca_biplot(pca_result, repel=TRUE, col.ind ="cos2", gradient.cols=c("#00AFBB","#E7B800","#FC4E07"), col.var="#2E9FDF")
fviz_pca_ind(pca_result, col.ind ="cos2", gradient.cols=c("#00AFBB","#E7B800","#FC4E07"), repel = TRUE)
fviz_cos2(pca_result, choice="ind", axes=1:2, top = 50)


# Projections des nouvelles individus
bouyakoub <- data.frame(
	Spécialité = '2SD',
	SYS1 = 14.85,
	RES1 = 16.00,
	ANUM = 17.49,
	RO =  13.94,
	ORG =  13.25,
	LANG1 = 16.82,
	IGL = 16.36,
	THP = 14.70,
	MCSI = 11.47,
	BDD = 17.34,
	SEC = 17.68,
	CPROJ = 16.12,
	PROJ = 15.78,
	LANG2 = 16.50,
	ARCH =  17.56,
	SYS2 = 18.45,
	RES2 = 18.35
)

aboud <- data.frame(
	Spécialité = '2SD',
	SYS1 = 16.40,
	RES1 = 16.62,
	ANUM = 16.18,
	RO =  17.10,
	ORG =  12.62,
	LANG1 = 16.82,
	IGL = 14.96,
	THP = 18.10,
	MCSI = 13.90,
	BDD = 12.88,
	SEC = 13.81,
	CPROJ = 13.12,
	PROJ = 15.78,
	LANG2 = 16.84,
	ARCH =  18.98,
	SYS2 = 16.50,
	RES2 = 16.58	
)

#dyna <- data.frame(
#	Spécialité = '2SD',
#	SYS1 = 11.9,
#	RES1 = 15.25,
#	ANUM = 13.39,
#	RO =  11.95,
#	ORG =  12.44,
#	LANG1 = 13.06,
#	IGL = 14.14,
#	THP = 10.8,
#	MCSI = 11.89,
#	BDD = 13.62,
#	SEC = 13.69,
#	CPROJ = 12.62,
#	PROJ = 14.48,
#	LANG2 = 14.91,
#	ARCH =  14.44,
#	SYS2 = 13.05,
#	RES2 = 15.95
#)

#mellissa <- data.frame(
#	Spécialité = '2SD',
#	SYS1 = 10.1,
#	RES1 = 10,
#	ANUM = 14.42,
#	RO =  11.06,
#	ORG =  11.38,
#	LANG1 = 12.01,
#	IGL = 12.06,
#	THP = 9.4,
#	MCSI = 12.6,
#	BDD = 12.83,
#	SEC = 9.25,
#	CPROJ = 11.49,
#	PROJ = 14.48,
#	LANG2 = 12.99,
#	ARCH =  14.58,
#	SYS2 = 11.88,
#	RES2 = 15.4
#)


#rahou <- data.frame(
#	Spécialité = '2SD',
#	SYS1 = 9.8,
#	RES1 = 9,
#	ANUM = 12.16,
#	RO =  5.65,
#	ORG =  11.38,
#	LANG1 = 13.48,
#	IGL = 11.47,
#	THP = 7.9,
#	MCSI = 12.23,
#	BDD = 13.88,
#	SEC = 12.75,
#	CPROJ = 12.29,
#	PROJ = 15.59,
#	LANG2 = 15.82,
#	ARCH =  9.09,
#	SYS2 = 8.1,
#	RES2 = 9.4
#)


#data_to_analyze <- rbind(data_to_analyze, dyna)
#data_to_analyze <- rbind(data_to_analyze, mellissa)
#data_to_analyze <- rbind(data_to_analyze, rahou)
#data_to_analyze

# Visualisation du nuage.
data_to_analyze <- rbind(data_to_analyze, bouyakoub)
data_to_analyze <- rbind(data_to_analyze, aboud)
data_to_analyze
pca_result <- PCA(data_to_analyze[,-1], graph = FALSE, ind.sup = 156:157)
fviz_pca_biplot(pca_result, repel=TRUE, col.ind ="#696969", col.var="#2E9FDF")

# Qualité de représentation des individus supplémentaires.
pca_result$ind.sup$cos2

# Attribuer à chaque spécialité une couleur bien particulière.
active_data <- data_to_analyze[1:155, ]
majors <- as.factor(active_data$Spécialité)
fviz_pca_biplot(pca_result,
                col.ind = majors,                
                label = "var",                          
                legend.title = "Spécialités",
                col.var = "grey",
                repel = TRUE)



# Visualiser uniquement les personnes de la spécialité SID.
target_major <- "2SD"
filtered_colors <- ifelse(majors == target_major, "red", "transparent")
fviz_pca_biplot(pca_result,
                col.ind = filtered_colors,  
                label = "var",              
                col.var = "grey",         
                repel = TRUE) +
  scale_color_manual(values = c("red", "transparent"))  + theme(legend.position = "none")



# Visualiser uniquement les personnes de la spécialité SIL avec les individus supplémentaires.
target_major <- "2SL"
filtered_colors <- ifelse(majors == target_major, "green", "transparent")
fviz_pca_biplot(pca_result,
                col.ind = filtered_colors,  
                label = "var",              
                col.var = "grey",          
                repel = TRUE) +
  scale_color_manual(values = c("green", "transparent"))  + theme(legend.position = "none")


# Visualiser uniquement les personnes de la spécialité SIQ avec les individus supplémentaires.
target_major <- "2SQ"
filtered_colors <- ifelse(majors == target_major, "orange", "transparent")
fviz_pca_biplot(pca_result,
                col.ind = filtered_colors,  
                label = "var",            
                col.var = "grey",       
                repel = TRUE) +
  scale_color_manual(values = c("orange", "transparent"))  + theme(legend.position = "none")



# Visualiser uniquement les personnes de la spécialité SIT avec les individus supplémentaires.
target_major <- "2ST"
filtered_colors <- ifelse(majors == target_major, "purple", "transparent")
fviz_pca_biplot(pca_result,
                col.ind = filtered_colors,   # Colors for individuals
                label = "var",              # Display variable names
                col.var = "grey",           # Color for variables
                repel = TRUE) +
  scale_color_manual(values = c("purple", "transparent"))  + theme(legend.position = "none")






############################################## Projection sur la 1CP ##############################################

pv <- "PV_1CP_2021_annuel.xlsx" # Le chemin du fichier qui contient le PV de délibération de la promotion 1CP 2021.
data <- read_excel(pv)
colnames(data)

#[1] "Matricule"     "Groupe_S1"     "SYST1"         "ALSDS"         "ARCH1"         "UEF1"          "ANAL1"         "ALG1"          "ELECT"         "UEF2"          "TEE"           "UET1"          "BW"           
#[14] "UED1"          "Ne_S1"         "Rang_S1"       "Moy_S1"        "Moy_rachatS1"  "Crd_S1"        "Groupe_S2"     "ANAL2"         "ALG2"          "UEF4"          "TEO"           "ANG1"          "UET2"         
#[27] "MECA"          "ELEF1"         "UEM1"          "ALSDD"         "SYST2"         "UEF3"          "Ne_S2"         "Rang_S2"       "Moy_S2"        "Moy_rachatS2"  "Crd_S2"        "Rang_annuel"   "Moy_annuelle" 
#[40] "Moy_rachat"    "Crd_annuel"    "Decision_jury"

# Supprimer les colonnes non pertinentes et les outliers.
data <- data[, -c(1, 2, 6, 10, 12, 14, 15, 16, 17, 18, 19, 20, 23, 26, 29, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42)]
data <- data[1:248, ]


# Effectuer l'ACP.
pca_result <- PCA(data, graph = FALSE)

# Signification des axes
fviz_pca_var(pca_result, col.var="black", repel = TRUE)
# Visualiser la contribution absolue des variables au premier axe.
fviz_contrib(pca_result, choice="var", axes=1)
# Visualiser la contribution absolue des variables au deuxième axe.
fviz_contrib(pca_result, choice="var", axes=2)


# Représentation variables-individus
fviz_pca_biplot(
  pca_result,
  geom = "point",           
  repel = TRUE,
  col.var = 'purple',         
) +
  # Attribuer une autre couleur et un label aux deux étudiants BOUYAKOUB et ABOUD.
  geom_point(data = data.frame(pca_result$ind$coord)[c(1, 3), ],
             aes(x = Dim.1, y = Dim.2),
             color = "red", size = 2) +
  geom_text(data = data.frame(pca_result$ind$coord)[c(1, 3), ],
            aes(x = Dim.1, y = Dim.2, label = c("BOUYAKOUB", "ABOUD")),
            color = "red", vjust = -1, size = 3)


############################################## Projection sur la 2CP ##############################################


pv <- "PV_2CP_2021_ANNUEL.xlsx" # Le chemin du fichier qui contient le PV de délibération de la promotion 1CP 2021.
data <- read_excel(pv)
colnames(data)
# [1] "Matricule"                                   "Groupe_S1"                                  
# [3] "Structure Fichiers et Structures de Données" "Architecture des ordinateurs 2"             
# [5] "UEF5"                                        "Analyse 3"                                  
# [7] "Algèbre 3"                                   "UEF6"                                       
# [9] "Electronique fondamentale 2"                 "Probabilités et statistiques"               
# [11] "UEM2"                                        "Anglais 2"                                  
# [13] "UET3"                                        "Economie"                                   
# [15] "UED2"                                        "Ne_S1"                                      
# [17] "Rang_S1"                                     "Moy_S1"                                     
# [19] "Moy_rachatS1"                                "Crd_S1"                                     
# [21] "Groupe_S2"                                   "Programmation Orientée Objet"               
# [23] "Introduction aux systèmes d'information"     "UEF7"                                       
# [25] "Projet Pluridisciplinaire"                   "UEM3"                                       
# [27] "Probabilités et Statistiques 2"              "UEM4"                                       
# [29] "Anglais 3"                                   "UET4"                                       
# [31] "Analyse 4"                                   "Logique Mathématique"                       
# [33] "Optique et Ondes éléctromagnétiques"         "UEF8"                                       
# [35] "Ne_S2"                                       "Rang_S2"                                    
# [37] "Moy_S2"                                      "Moy_rachatS2"                               
# [39] "Crd_S2"                                      "Rang_annuel"                                
# [41] "Moy_annuelle"                                "Moy_rachat"                                 
# [43] "Crd_annuel"                                  "Decision_jury"  


data <- data[, -c(1, 2, 5, 8, 11, 13, 15, 16, 17, 18, 19, 20, 21, 24, 26, 28, 30, 34, 35, 36, 37, 38, 39, 40, 41, 42 ,43, 44)]

colnames(data)

# Utiliser des abbréviations pour avoir une meilleure représentation.

colnames(data) <- c('SFSD', 'ARCH2','ANA3', 'ALG3', 'ELEF2', 'PRST1', 'ANG2', 'ECON', 'POO', 'SINF', 'PRJP', 'PRST2', 'ANG3', 'ANA4', 'LOGM', 'OOE')

colnames(data)

# On ne garde que les étudiants allant de 1 jusqu'à 207.
data <- data[1:207, ]

# On rajoute les individus supplémentaires
bouyakoub_2CP <- data.frame(
	SFSD= 15.57,
	ARCH2= 17.11,
	ANA3= 16.59,
	ALG3= 17.67,
	ELEF2= 17.91,
	PRST1= 18.13,
	ANG2= 14.98,
	ECON= 17.75,
	POO= 16.40,
	SINF=16.96,
	PRJP=18.50,
	PRST2=18.88,
	ANG3=15.33,
	ANA4=17.68,
	LOGM=14.30,
	OOE=15.47 
)
aboud_2CP <- data.frame(
	SFSD=16.35,
	ARCH2=16.28,
	ANA3=16.29,
	ALG3=14.00,
	ELEF2=16.50,
	PRST1=17.25,
	ANG2=14.82,
	ECON=14.88,
	POO=16.11,
	SINF=14.66,
	PRJP=18.50,
	PRST2=16.75,
	ANG3=15.75,
	ANA4=18.26,
	LOGM=14.85,
	OOE=16.25 
)

data <- rbind(data, bouyakoub_2CP)
data <- rbind(data, aboud_2CP)
print(data, n=250)

# Effectuer l'ACP.
# La commande ind.sup=208:209 permet de considérer les individus 208-209 (BOUYAKOUB et ABOUD) en tant qu'individus supplémentaires.
pca_result <- PCA(data, ind.sup=208:209, graph = FALSE)


# Signification des axes
fviz_pca_var(pca_result, col.var="black", repel = TRUE)
# Visualiser la contribution absolue des variables au premier axe.
fviz_contrib(pca_result, choice="var", axes=1)
# Visualiser la contribution absolue des variables au deuxième axe.
fviz_contrib(pca_result, choice="var", axes=2)


# Représentation variables-individus
fviz_pca_biplot(
  pca_result,
  geom = "point",           
  repel = TRUE,
  col.var = 'purple',         
) +
  # Attribuer une autre couleur et un label aux deux étudiants BOUYAKOUB et ABOUD.
  geom_point(data = data.frame(pca_result$ind.sup$coord)[c(1,2), ],
             aes(x = Dim.1, y = Dim.2),
             color = "red", size = 2) +
  geom_text(data = data.frame(pca_result$ind.sup$coord)[c(1,2), ],
            aes(x = Dim.1, y = Dim.2, label = c("BOUYAKOUB","ABOUD")),
            color = "red", vjust = -1, size = 3)
            
            
# Qualité de représentation des différents individus supplémentaires.
pca_result$ind.sup$cos2

#'
#'
#Packages utiles à charger une fois 
#install.packages("xlsx")
#install.packages("ggplot2")
#Chargement des bibliothèques utiles
library(xlsx)  #pour write.xlsx
library(ggplot2)  #pour les graphiques ggplot

#'
#'
#Installer la version stable
#install.package("googleAnalyticsR", dependencies = TRUE)  #version stable
#Installer la version en cours de développement dur Github
devtools::install_github("MarkEdmondson1234/googleAnalyticsR")  #Version en cours de dev 

#'
#'
#googleanaylticsR fournit par défaut un ID Client et un Code secret sinon vous pouvez utiliser les vôtres :
# ici les miens merci de prndre les votres de dtoutes façons ces codes seront supprimés
myclient_id <-  "507175436240-qb0pgbntdmvf32i1kni67u2o455t0rbh.apps.googleusercontent.com" 
myclient_secret <-   "Mi_JcEQRuBgOLHDj8MH_s0uw" 

options(googleAuthR.client_id = myclient_id )
options(googleAuthR.client_secret = myclient_secret ) 
library(googleAnalyticsR)  #mettre mes infos OAuth avant de recharger.

#'
#'
ga_auth() #connexion via son compte google (un navigateur va s'ouvrir)

#'
#' 
#liste des comptes dans Google Analyticspour récupérer le code viewID
account_list <- ga_account_list()
account_list  #Afficher la liste et repérez la vue du site que vous souhaitez.
ga_id <- account_list[41, 'viewId']  #Pour moi c'est la ligne 41 de mes vues, des sites que je gère dans Google Analytics

#'
#'
#Variables disponibles dans l'API GA Reporting 
meta <- google_analytics_meta()  #Recupération de la liste des variables
View(meta) #Affichage de la liste des Variables disponible dans l'API de Google Analytics.
#Sauvegarde en xlsx ... ça peut aider pour consulter les variables ailleurs.
write.xlsx(meta, file = "metaGA.xlsx")

#'
#'
#QQ dates 
yesterday <- Sys.Date() - 1
threeDaysAgo <- Sys.Date() - 3
ThirtyDaysAgo <- Sys.Date() - 30
SixMonthsAgo <- Sys.Date()-183
OneYearAgo <- Sys.Date()-365
FiveYearsAgo <- Sys.Date()-1827

#'
#'
#Traffic en sessions sur 30 jours avec courbe de tendance
galastthirtydays <- google_analytics(ga_id, date_range = c(ThirtyDaysAgo , yesterday), metrics = c("sessions"), 
                                         dimensions = c("date"))

str(galastthirtydays) #pour voir ce que l'on a
#Graphique 
galastthirtydays %>%
  ggplot(aes(x = date,y = sessions) ) + 
  geom_point() + 
  geom_line() +
  geom_smooth(method = "loess") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
#'
#'
#Jours les plus actifs dans l'année
gadow <- google_analytics(ga_id, 
                           date_range = c(OneYearAgo , yesterday), 
                           metrics = c("sessions"), 
                           dimensions = c("dayOfWeekName","date"))
gadow %>%  
  ggplot(aes(x = dayOfWeekName, y = sessions)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#on vire les valeurs extrêmes pour y voir plus clair
gadow[gadow$sessions < 50,] %>%  
  ggplot(aes(x = dayOfWeekName, y = sessions)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
#'
#'
#Sessions sur les 5 années précédentess
ga5years <- google_analytics(ga_id, 
                          date_range = c(FiveYearsAgo, yesterday),
                          metrics = c("sessions"),
                          dimensions = c("date"), 
                          max=2000)

#on ajoute l'année à notre jeu de données calculé à partir de la date.
ga5years$year<-format(ga5years$date,"%Y")

#Graphique avec des années de différentes couleurs.
ggplot(ga5years, aes(x=date, y=sessions, color=year)) + 
  geom_line() 
  
#'
#'
#Type de device en fonction du continent
gadevice <- google_analytics(ga_id, 
                           date_range = c(OneYearAgo , yesterday),
                           metrics = c("sessions"),
                           dimensions = c("date","continent", "deviceCategory"), 
                           anti_sample = TRUE,
                           max=5000)


ggplot(gadevice[gadevice$continent != "(not set)",], aes(x=continent)) + 
  geom_bar(aes(fill=deviceCategory), position="fill") 
  
  

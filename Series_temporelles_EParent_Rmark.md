Reproductibilité d'un dossier RMark
================
Elizabeth Parent
10 mai 2019

#Présentation du devoir

**Téléchargement des librairies**
``` {r}
library("tidyverse")
library("forecast")
library("lubridate")
```

**Téléchargement des documents du projet "Series temporelles et reproductibilite"**
``` {r}
hawai <- read_csv("hawai.csv")
glimpse(hawai) #Pour vérifier si les dates sont détectées. Ici ce n'est pas détecté.
```
**Exploration du fichier**

*Vérifier si le fichier comporte des données manquantes*
``` {r}
is.na(hawai)
which(is.na(hawai)) #Il n'y a pas de données manquantes

hawai %>%
  ggplot(aes(x = time, y = CO2)) +
  geom_line() #Aucune donnée manquante observée sur ce graphique
```

*Explorer le fichier avec des statistiques de base*
``` {r}
summary(hawai)
ggplot(data = hawai, mapping = aes(x = time, y = CO2)) +
  geom_point() #C'est une droite.
haw_aov <- aov(CO2 ~ time, hawai)
summary(haw_aov) #La p-value montre des effets significatifs.
modlin_hw <- lm(CO2 ~ time, data = hawai)
summary(modlin_hw) #La p-value montre des effets significatifs.
res_df <- data.frame(CO2 = hawai$CO2,  
                    residus_lm = residuals(modlin_hw),
                    residus_calcul = hawai$time - predict(modlin_hw))

ggplot(res_df, aes(x = CO2, y = residus_lm)) +
  geom_point() +
  labs(x = "CO2", y = "Résidus") +
  geom_hline(yintercept = 0, col = "red", size = 1) #Les résidus ont une structure particulière.
```

*Test de normalité*
``` {r}
shapiro.test(res_df$residus_lm)
```
L'hypothèse nulle que la distribution est normale est rejetée au seuil 0.05.
Ici, il y a normalité. 

Confirmer la normalité avec un histogramme
``` {r}
ggplot(res_df, aes(x = residus_lm)) +
  geom_histogram(binwidth = 2, color = "white") +
  labs(x = "Residual")
```

*Lire la date en format date et organiser le tableau pour une meilleure visualisation*
``` {r}
date <- date_decimal(hawai$time, tz = "UTC")
date_decimal(1958.16666666667)
date_1 <- as.Date(date, "%m/%d/%y", tz ="UTC") 

hawai_date <- hawai %>%
  mutate(Year = date %>% year(),
         Month = date %>% month(),
         Day = date %>% day(),
         YearMonthDay = ymd(paste0(Year, "-", Month, "-", Day))) %>%
  group_by(YearMonthDay) %>%
  select(., time, -Year, -Month, -Day, YearMonthDay, CO2)
hawai_date <- hawai_date[,c("time", "YearMonthDay", "CO2")]
```

**Créer une série temporelle à partir du CO2**
``` {r}
hawai_ts2 <- ts(hawai_date %>% ungroup() %>% select(CO2), start = c(1958, 1), frequency = c(526/(2002-1958)))
``` 

**Séparer en parties d'entraînement (70% du lot)**
``` {r}
Emission_ts <- hawai_ts2[, 1] 
Emission_train <- window(Emission_ts, start = c(1958), end = c(1988.4)) 
``` 

**Créer un modèle ETS sur les données d'entraînement**
``` {r}
Emission_model <- ets(Emission_train)
autoplot(Emission_model)
```
**Prédire les données de test**
``` {r}
Emission_fc <- Emission_model %>% forecast()
accuracy(Emission_fc, Emission_ts)
autoplot(Emission_fc)
``` 

**Effectuer une analyse des résidus**
``` {r}
checkresiduals(Emission_model)
``` 
**Commenter le modèle**

Fiabilité: Il est peu probable que les résidus aient été générés par un bruit 
blanc puisque la p-valeur est sous le seuil 0.05. Donc, le modèle semble fiable.

Amélioration: Les prédictions montrent une large étendue de possibilités de prédictions.
Il pourrait y avoir un prétraitement des données avec transformation 
logarithmique, mais ça ne devrait pas tant changer le modèle de prédiction.
En fait, des covariables pourraient apporter des informations au modèle pour
l'améliorer. Par exemple, la température, les activités volcaniques ou la
densité de population.

#lien github
https://github.com/eliparent/2019_reproduc/blob/master/Series_temporelles_EParent_Rmark.md

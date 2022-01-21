# Préparatifs
library(tidyverse)
gapminder <- read_csv("data/gapminder_data.csv")

## Défi 1

# Écrivez une commande qui produit un data frame (un `tibble`) avec les valeurs
# pour l'**Afrique seulement** et qui ne montre que le contenu des colonnes
# `lifeExp`, `country` et `year`. Combien de rangées le data frame contient-il?

# Solution
gapminder %>%
  filter(continent == "Africa") %>%
  select(lifeExp, country, year)

## Défi 2

# Calculez l'espérance de vie moyenne par pays. Essayez ensuite de trouver lequel a
# l'espérance de vie la plus **longue**.

# Solution 1
gapminder %>%
  group_by(country) %>%
  summarize(lifeExp_mean = mean(lifeExp)) %>%
  filter(lifeExp_mean == max(lifeExp_mean))

# Solution 2
gapminder %>%
  group_by(country) %>%
  summarize(lifeExp_mean = mean(lifeExp)) %>%
  arrange(desc(lifeExp_mean)) %>%
  head(1)

### Défi 3

# Réparez le jeu de données suivant et calculez l'espérance de vie **médiane**
# par année

# Charger le nouveau jeu de données
url_defi <- "https://raw.githubusercontent.com/gabrieldansereau/RanDonnees2022-atelier-manipulation-R/main/gapminder_messy_challenges.csv"
gapminder_defi <- read_csv(url_defi)
head(gapminder_defi)

# Réponse
gapminder_defi_clean <- gapminder_defi %>%
  slice(-1) %>%
  rename(year = X2,
         lifeExp = `life _Exp`) %>%
  mutate(lifeExp = as.numeric(lifeExp))

gapminder_defi_clean %>%
  group_by(year) %>%
  summarize(lifeExp_med = median(lifeExp))

### Défi 4

# Reprenez votre jeu de données gapminder_defi du Défi 3
# En quelle année l'espérance de vie a-t-elle été la plus élevée au Canada?
# Quel était le nombre d'habitant cette même année?

# Voici les commandes pour le reproduire au besoin
url_defi <- "https://raw.githubusercontent.com/gabrieldansereau/RanDonnees2022-atelier-manipulation-R/main/gapminder_messy_challenges.csv"
gapminder_defi_clean <- read_csv(url_defi) %>%
  slice(-1) %>%
  rename(year = ...2,
         lifeExp = `life _Exp`) %>%
  mutate(lifeExp = as.numeric(lifeExp))

## Solution
# Regardons d'abord les données pour le Canada seulement
gapminder_defi_clean %>%
  filter(country == "Canada")

# Y'a-t-il des doublons ou des erreurs au niveau des années?
gapminder_defi_clean %>%
  count(year)

# Y'a-t-il des valeurs aberrantes pour l'espérance de vie?
gapminder_defi_clean %>%
  ggplot(aes(x = year, y = lifeExp)) +
  geom_point()

# Corrigeons les erreurs
gapminder_canada <- gapminder_defi_clean %>%
  filter(country == "Canada") %>%
  mutate(lifeExp = replace(lifeExp, lifeExp == 80653, 80.653)) %>%
  distinct()

# Vérifions maintenant en quelle année l'espérance de vie était la plus élevée
# et extrayons la population cette même année
reponse <- gapminder_canada %>%
  filter(lifeExp == max(lifeExp, na.rm = TRUE)) %>%
  pull(pop, year)
reponse

# À noter qu'il s'agit ici d'un vector nommé (named vector) qui ne contient
# qu'une seule valeur.
length(reponse) # 1 seule valeur
names(reponse) # l'annee est le nom
reponse[[1]] # la population est la valeur

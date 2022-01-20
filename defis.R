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

gapminder %>%
  group_by(country) %>%
  summarize(lifeExp_mean = mean(lifeExp)) %>%
  filter(lifeExp_mean == max(lifeExp_mean))

gapminder %>%
  group_by(country) %>%
  summarize(lifeExp_mean = mean(lifeExp)) %>%
  arrange(desc(lifeExp_mean)) %>%
  head()

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

gapminder_defi_clean %>%
  filter(country == "Canada")

gapminder_defi_clean %>%
  count(year)

gapminder_defi_clean %>%
  ggplot(aes(x = year, y = lifeExp)) +
  geom_point()

gapminder_canada <- gapminder_defi_clean %>%
  filter(country == "Canada") %>%
  mutate(lifeExp = replace(lifeExp, lifeExp == 80653, 80.653)) %>%
  distinct()

gapminder_canada %>%
  filter(lifeExp == max(lifeExp, na.rm = TRUE)) %>%
  pull(lifeExp, pop)

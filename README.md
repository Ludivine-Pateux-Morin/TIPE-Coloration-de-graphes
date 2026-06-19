# TIPE-Coloration-de-graphes
Projet de TIPE en informatique, effectué en MP pendant l'année 2025-2026. C'est un projet de groupe, dont les membres sont :
- PATEUX--MORIN Ludivine
- AUDREN Tudwal
- SUARD Philippe

# Présentation générale
TIPE portant sur la coloration de graphes et l'étude de différents algorithmes heuristiques ou exacts pour colorer un graphe, dans le but d'utiliser le moins de couleurs possible.

Une présentation détaillée se trouve dans le fichier pdf "MCOT.pdf", fichier demandé par les examinateurs du concours.

Le diaporama "diapo.pdf" est celui présenté à l'épreuve.

# Etapes pour re-générer les résultats du TIPE

Différentes étapes sont nécessaires pour refaire les tests du TIPE :
- étape 1 : générer des graphes
- étape 2 : appliquer les algorithmes sur les graphes
- étape 3 : analyser les résultats



## Etape 1 : générer des bases de données de graphes

Pour faire des tests, des bases de données de différents graphes sont d'abord créées sous la forme de fichier ocaml du type "donneesX.ml".
Pour générer une telle base, utiliser le code source du fichier genere_graphes.ml, puis lancer la commande :

``` ocaml
fonction_finale l_couples_1 5 "donnees1.ml"
```

## Etape 2 : Générer des résultats de coloration

On applique chacun des algorithmes étudiés sur les graphes de la base de données, pour calculer leur temps d'exécution et le nombre de couleurs utilisées dans la coloration renvoyée dans un fichier de données du type "resultats_X.csv".
Pour générer un tel fichier, copier le contenu du fichier "algos.ml" contenant les codes des différents algorithmes heuristiques dans le fichier "genere_base_de_donnees.ml", puis dans ce dernier fichier lancer la commande :

``` ocaml
genere_csv "resultats_1.csv" "donnees1.ml" l_couples_1 mes_algos
```

Pour générer un fichier avec uniquement l'algorithme exact Branch-and-Bound, lancer la commande :

``` ocaml
genere_csv "resultats_bb_1.csv" "donnees1.ml" l_couples_1 mes_algos
```

## Etape 3 : Analyser les résultats obtenus

On analyse les résultats obtenus en générant plusieurs graphiques en python.
Pour générer de tels graphiques, utiliser le code source du fichier "courbes1.py". Modifier les lignes 4 et 5 du code pour personnaliser le chemin vers les 2 fichiers nécessaires.
Pour obtenir des graphes, lancer une des commandes au choix :

``` python
comp_nc_t(20,'n','wp','d','df','b')

comp_nc_d(6,'n','wp','d','df','b')
```

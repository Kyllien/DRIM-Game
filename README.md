# DRIM-Game
Tout le code et les documents nécessaires à la résolution du DRIM Game 2021.
 
Le sujet est le suivant : "Détermination d’un modèle de projection de matrice de migration dans un portefeuille de crédits."

L’objectif du sujet est de déterminer une méthode de projection alternative de matrice de migration et d’en évaluer la capacité prédictive au travers d’un backtesting.

3 méthodes sont utlisées afin de faire la prédiction des matrices PIT : 
 - méthode classique : modèle de Merton Vasicek avec prédiction sur le taux de défaut obtenus à l'aide des matrices empiriques
 - méthode alternative : création d'un index de crédit pour chaque ligne (classe) de chaque matrice PIT, et prediction de ces credit index afin d'obtenir les matrices PIT demandées.
 - méthode alternative : prédiction des matrices PIT à partir d'une modelisation et prédiction de chaque cellule des matrices PIT cumulées empiriques, obtention de 100 modèles afin de faire la prévision des 100 cellules. Utilisation des matrices cumulées pour faire la prédiction car il faut garder la propriété que la somme de chaque ligne est égale à 1, et donc pas besoin de faire la prédiction de la première colonne, c'est à dire de toute les probabilités conditionnelles de migration en classe 1 car celle-ci est toujours égale à 1.

Les dossiers de previsions sont des fichiers R permettant la modélisation et la prediction du taux de défaut pour la méthode de Merton Vasicek, mais aussi pour les deux émthodes alternatives.

La modélisation GAM est utilisé pour faire de modélisation sur le taux de défaut pour la méthode de Merton Vasicek.

Le fichier python "MatriceGood" permet la création des différentes matrices PIT et TTC, et la gestion de la base brute.

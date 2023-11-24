## Membre du groupe
Ulysse  
Lucie  
Romain M  
Romain T  
Mathilde
## Objectif du Projet
Ce projet concerne la création d'un module dans le langage de programmation Haskell pour gérer des formules logiques. Une formule logique est une structure utilisée pour représenter une proposition qui peut être vraie ou fausse, comme dans la logique mathématique ou la programmation de la logique.
## Fichiers Principaux
- Formula : Il représente une formule logique, qui peut être une constante (vraie/fausse), une variable (qui peut prendre les valeurs vrai ou faux), ou des opérations composées comme NOT (négation), AND (et logique), OR (ou logique), etc.
- Literal : C'est une partie pour simplifier une formule, généralement une variable ou la négation d'une variable.
- NormalForm : Cela représente une formule sous une forme standard appelée forme normale conjonctive (FNC). En FNC, une formule est une conjonction (AND) de disjonctions (OR). C'est une manière standardisée de représenter des formules qui est particulièrement utile pour certains types d'analyse et d'algorithmes.
- Environment : C'est une correspondance entre les noms des variables et leurs valeurs de vérité (vrai/faux). Il est utilisé pour évaluer une formule dans un contexte spécifique.
## Fonctionnalités Principales
- Création de Formules : Vous pouvez créer des formules en utilisant des constantes, des variables et des opérations.
- Évaluation : Vous pouvez évaluer ce que vaut une formule (vrai/faux) dans un certain environnement (où les valeurs des variables sont définies).
- Équivalence : Vous pouvez vérifier si deux formules sont équivalentes (c'est-à-dire, ont les mêmes valeurs de vérité dans tous les environnements possibles).
- Simplification : Vous pouvez simplifier les formules pour obtenir une version plus simple qui est équivalente à la formule originale.
- Conversion et Analyse en Forme Normale : Vous pouvez convertir des formules en leur forme normale conjonctive (FNC) et effectuer des analyses sur elles dans ce format, comme appliquer l'algorithme de résolution.
## En Résumé
Ce module est destiné à être un outil pour travailler avec des formules logiques de manière abstraite. Il permet la manipulation et l'analyse de ces formules, offrant des fonctionnalités souvent nécessaires en logique mathématique, en informatique théorique, ou en intelligence artificielle, parmi d'autres domaines.


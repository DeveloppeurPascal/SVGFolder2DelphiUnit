# 20240723 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

* mise à jour de l'arborescence du dépôt de code (depuis le template)
* ajout de la dépendance envers les librairie (boite à outils Delphi)
* ajout de la dépendance envers la boite de dialogue "about"
* mise à jour des docs FR/EN
* renommage du projet
* mise à jour des informations de version du projet
* activation de la compilation du binaire universel pour macOS (x64+ARM)
* changement du commentaire en début de l'unité générée
* ajout de fichiers SVG de test
* ajout de l'indentation dans l'unité générée telle que l'aurait faite le formateur de code avec ses paramètres par défaut
* ajout de la directive TEXTBLOCK avant le remplissage du tableau afin d'avoir le bon CR/LF (celui par défaut) et surtout le formatage en XML du code importé le jour où ce sera implémenté dans un plugin ou directement l'IDE
* ajout d'une classe avec des Tag(s) (en variables de classe) dans le code généré pour accéder depuis un type aux codes sources des SVG et à la liste de leurs index (si quelqu'un préfère passer par là)

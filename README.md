# MasterECLAT
Code à trous en R pour le Master ECLAT
Le code lit un jeu de données netcdf d'une sortie de simulation CMIP6 du modèle de l'IPSL
Le but des calculs est de faire des analyses de GEV/GPD des données et de les tracer.
Les exemples de figures de diagnostics simples de visualisation de pdf sont donnés.

Le code R est fourni avec un fichier "caldat.R" contenant une routine qui transforme les jours depuis une date en yyy, mm, jj.
Deux fichiers de données netcdf (température: tas; précipitation: pr) sont fournis pour le TP.

On commence par lancer R et on ouvre le fichier TP_ECLAT_GEV.R dans un éditeur (comme emacs).
Dans l'environnement R, il faut installer les packages "ncdf4" et "extRemes". Ensuite, on "joue" avec les instructions du script TP_ECLAT_GEV.R

Code distributé pour le Master ECLAT de l'UVSQ/Université Paris Saclay, par Pascal Yiou (LSCE, Janvier 2021).
Le code est réalisé sous licence (gratuite) CECILL. Il est distribué "tel quel" sans garantie de fonctionnement. Il peut être re-distribué à toute fin d'éducation ou de recherche. Toute utilisation commerciale est interdite. Merci de contacter Pascal Yiou (pascal.yiou at lsce.ipsl.fr).

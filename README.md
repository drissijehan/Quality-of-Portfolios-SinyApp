# Quality-of-Portfolios-SinyApp

J'ai programmé un algorithme qui sert à calculer le taux de défaut annuel de la banque selon quatre définitions. Ensuite, en se basant sur cet algorithme, j'ai programmé cette application Shiny, qui sert à calculer et visualiser l’évolution du taux de défaut annuel et faire des filtrations par année, résultat brut, avec ou sans bilan et par secteur.

                             
Classifier les clients en des clients sains et d’autres mauvais selon trois définitions:
        Retard de paiement :
                 Les clients à crédits dont le retard <90 jours sont considérés comme des clients sains. Ceux dont le retard dépasse les                    90 jours sont considérés comme des mauvais clients. 
        Problèmes judiciaires :
                 Les clients avec des problèmes judiciaires sont considérés comme des mauvais clients par contre le reste des clients 
                 sont qualifiés comme des bons clients. 
        Classe des portefeuilles :
                 Les clients avec des portefeuilles de classe 1 et 2 sont considérés comme des clients sains. Les autres clients classés 3                  et 4 sont des mauvais clients. 
                 La banque centrale distinguent 4 classes basées sur  des critères bien définis: 
                     Classe 1 : Actifs nécessitant un suivi particulier  
                     Classe 2 : Actifs incertains 
                     Classe 3 : Actifs préoccupants 
                     Classe 3 : Actifs préoccupants 


                     Taux de défaut (année N) = (défauts (année N+1)) / (observations (année N)) 

Observations : Les clients considérés comme sains pendant l’année N.
Défauts : Les clients considérés comme sains pendant l’année N et devenus mauvais pendant l’année N+1.

Un utilisateur peut utiliser facilement l'application grâce à sa simplicité de fonctionnement:
Télécharger la base, l'utilisateur peut choisir les 13 colonnes nécessaires afin de minimiser le temps de fonctionnement le plus possible. (ne dépasse pas 1 minute).
Choisir la définition sur laquelle se base le calcul et choisir les filtrations nécessaires.
L’utilisateur peut changer soit la définition, soit les filtrations à tous moment et visualiser directement les changement sans aucun besoin d’actualiser ou avoir un problème de fonctionnement.

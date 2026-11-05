# -*- coding: utf-8 -*-
"""
Created on Mon Dec 16 16:53:10 2024

@author: ACER
"""

# lecture du fichier groupe-13A_data
f = open('groupe-13A_data.txt', mode='r', encoding='utf-8')
a = f.read()  
f.close()
# l'entête du fichier
colonnes = 'Référence;Titre;Année;Durée (min);Nombre de votes;Score;Restriction;Description\n'
# Initialisation des listes pour stocker les informations de chaque film
Film = []
Référence = []
Titre = []
Année = []
Durée = []
Votes = []
Score = []
Restriction = []
Description = []

# séparation des films avec @@
Films = a.split('@@')

# Initialisation d'un compteur de film
num_film = 1

# Traitement de chaque film
for film_data in Films:
    #séparation des colonnes 
    elements = film_data.split(',')
    #print(elements)
    # Initialisation des variables
    titre = ""
    restriction = ""
    score = ""
    nbvotes = ""
    year = ""
    duration = ""
    desc = ""
#traitement des éléments de chaque film    
    for p in elements:
        nom_col = p.split('=')  
        #print(nom_col)
        if len(nom_col) == 2:         
            nom = nom_col[0].strip()  
            contenu = nom_col[1].strip()
            #print(nom)
            #print(contenu)
            if nom == 'title':
                titre = contenu.title()  
            elif nom == 'restriction':
                if contenu:  
                    restriction = contenu.title()  
                else:
                    restriction = ""   
            elif nom == 'score':
                score = float(contenu)
            elif nom == 'nbvotes':
                nbvotes = int(float(contenu))
            elif nom == 'year':
                year = int(float(contenu))
            elif nom == 'duration':
                if contenu:
                    duration = int(float(contenu))
                else: 
                    duration = ""
            elif nom == 'desc':
                desc = contenu

    # Référence
    ref = "F-" + "13A" + str(num_film) + "-" + str(year)  
    #print(ref)

    # ajout des infos, Si une information est vide dans le fichier de données reçu, cette information devra rester vide dans le
#fichier CSV résultat.

    Référence.append(ref)
    Titre.append(titre)
    if year:  
        Année.append(int(float(year)))  # Conversion en entier
    else:
        Année.append("")  
    if duration:  
        Durée.append(int(float(duration)))  # Conversion en entier
    else:
        Durée.append("")  
    if nbvotes:  
        Votes.append(int(nbvotes))  # Conversion en entier 
    else:
        Votes.append("")  
    if score:  
        Score.append(float(score))  # Conversion en réel 
    else:
        Score.append("") 
    if restriction:
        Restriction.append(restriction)
    else:
        Restriction.append("")
    if desc:
        Description.append(desc)
    else:
        Description.append("")

    #boucler les lignes et conversion des variables en str
    ligne = ref + ";" + titre + ";" + str(year) + ";" + str(duration) + ";" + str(nbvotes) + ";" + str(score) + ";" + restriction + ";" + desc
    Film.append(ligne)
    #print(ligne)
    
    num_film += 1

# resultat final
resultat1 = colonnes + "\n".join(Film)
print(resultat1)
# Sauvegarder le fichier CSV avec l'encodage sp√©cifi√©
f = open('groupe-13A_data.csv', 'w', encoding='cp1252')
f.write(resultat1)
f.close

#fichier 2
f = open('groupe-13A_data.csv', mode='r', encoding='cp1252')
données = f.read()  
f.close()

# Initialisation des variables pour stocker les films et les scores
films = []
Score = []

# enlever la premiere ligne et récupérer les films
ligne1 = données.split('\n')  # Diviser le contenu du fichier en lignes
for i in range(1, len(ligne1)):  
    film = ligne1[i]
    films.append(film)  # Ajouter chaque film dans films

    # scores
    elements = film.split(';')  # séparation des colonnes par ;
    if len(elements) >= 6:  # Vérifier qu'il y a suffisamment de colonnes dans le fichier csv, au moins jusqu a colonne score
        score = elements[5]  # Le score est dans la 6ème colonne
        if len(score) > 0:  # Si le score n'est pas vide
            Score.append(float(score))  # Ajouter le score à la liste
#print(elements)
# Calcul des statistiques
nombre_de_films = len(Score)
#print(nombre_de_films)
if len(Score)  > 0:  # Si la liste des scores n'est pas vide
    score_minimum = min(Score)
    #print(score_minimum)
    score_maximum = max(Score)
    #print(score_maximum)
    score_moyen = sum(Score) / nombre_de_films
    score_moyen = round(score_moyen, 2)
    #print(score_moyen)
else:#s il n y a pas de valeurs
    score_minimum = score_maximum = score_moyen = 0

# Création du texte à écrire dans le fichier texte
resultat2 = "Nom du fichier : groupe-13A_info.txt\n" + "Nombre de films : " + str(nombre_de_films) + "\nScore minimum : " + str(score_minimum) + "\nScore maximum : " + str(score_maximum) + "\nScore moyen : " + str(score_moyen) + "\n"

# Sauvegarde du fichier texte
f = open('groupe-13A_info.txt', 'w', encoding='utf-8')
f.write(resultat2)
f.close()


print(resultat2)

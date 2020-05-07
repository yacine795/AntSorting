;;#######################################################################################################
;; Création des différentes familles d'agents qui composent l'environnement.
;;## Fourmis : qui font trier les différents types de nourriture.
;;## nCentre : la position moyenne pour chaque type de nourriture, nécessaire pour l'arrêt automatique du
;; programme après le tri.
;;=======================================================================================================
breed[fourmis fourmi]
breed[nCentres nCentre]

globals [colorTab maxCountPatch]                   ;; tableau des nourritures disponnibles dans l'environnement

fourmis-own[tab charge?]             ;; tab : un tableau de taille "m" qui defini la capacité de mémorisation des fourmis
                                     ;; charge? : variable booléenne qui renseigne l'état de chaque fourmi .

nCentres-own[typeC]                  ;; Type Centre : chaque centre est défini par la couleur de la nourriture qu’il représente
;;#######################################################################################################





;;#######################################################################################################
;; Méthode setup : Efface le monde et les valeurs des variables de patches et des variable de tortues   |
;; pour chaque simulation, puis initialiser les variables de patches et des variable de fourmis aux     |
;; valeurs initiales par défaut                                                                         |
;;=======================================================================================================
to setup                                                                                               ;|
  resize-world (0) w (0) h                                                   ;; redimensionner le monde
  let v (w * h)                                                                                        ;|
  ifelse(v > 10000)[set-patch-size 1.7][set-patch-size 7]                                              ;|
  ca                                                                                                   ;|
  ask patches[                                                                                         ;|
    if (random-float(v / (typeNourriture * nbrNourritureParType)) < 1)[      ;; {(1/((w*h)/(typeN*nbrNpT)))} des patches du WORLD vont changés de couleur
      set pcolor (random typeNourriture) * 30 + red                          ;; chaque {(1/(typeNourriture)} des patches seront coloriés de couleur différentes
    ]                                                                                                  ;|
  ]                                                                                                    ;|
  set colorTab (remove-duplicates([pcolor] of patches with [pcolor != 0]))   ;; ajouter les differents types de nourritures disponible
  let res []                                                                          ;|
  foreach colorTab [ a ->                                                    ;; pour chaque type de nourriture ::
    create-nCentres 1 [                                                      ;; création du barycentre
      set typeC a                                                                                      ;|
      set hidden? true                                                                                 ;|
      set color (white)                                                                                ;|
      set shape "circle"                                                                               ;|
    ]
    set res fput (count patches with [pcolor = a]) res
  ]
  set maxCountPatch (circle-container (max res))
  print (word res" : rayon min = "maxCountPatch)
  create-fourmis nbrFourmis [                                                ;; création des fourmis
    set tab []                                                               ;; initialisation de la table de memoire de chaque fourmi
    set charge? false                                                                                  ;|
    setxy random-xcor random-ycor                                                                      ;|
    set color white                                                                                    ;|
    set shape "bug"                                                                                    ;|
  ]                                                                                                    ;|
  reset-ticks                                                                                          ;|
  screenshot                                                                                           ;|
end                                                                                                    ;|
;;#######################################################################################################





;;#######################################################################################################
;; Méthode go : Lancer la simulation en appelant les différentes fonctions définies dans le programme   |
;;=======================================================================================================
to go                                                                                                  ;|
  ask fourmis [                                                                                        ;|
    ifelse AntShow?[st][ht]                            ;; afficher ou pas les fourmis(st: show-turtle ; ht: hide-turtle)
    move                                               ;; faire deplacer les fourmis sur les patches
    ifelse (not charge?)[pickup][putdown]              ;; si la fourmi n'est pas chargée de nourriture elle la "ramasse" sinon elle la "pose"
  ]                                                                                                    ;|
  tick                                                                                                 ;|
  if (ticks = 60000)[screenshot]                       ;; {
  if (ticks = 225000)[screenshot]                      ;; prendre des captures aux instants X
  if (ticks = 1660000)[screenshot]                     ;; }
  ifelse(StopCondition?)[                              ;; vérifier l'état du switch "StopCondition?"
    ask nCentres [                                     ;; afficher les barycentres de chaque groupe de nourriture
      st                                                                                               ;|
      set label typeC                                                                                  ;|
      set label-color (typeC - 3)                                                                      ;|
    ]                                                                                                  ;|
    if (stop-condition)[                               ;; vérification de la condition d'arrêt si elle est vraie :
      clear-output                                                                                     ;|
      output                                                                                           ;|
      screenshot                                                                                       ;|
      ;export-plot (word "./Exp/" "EXPERIENCE_80x49" "/plot"ticks".csv")      ;; exporter les valeurs de l'expérience dans un fichier CSV
      stop                                                                    ;; arrêt de l'expérience
    ]                                                                                                  ;|
  ][ask nCentres [ht]]                                                                                 ;|
end                                                                                                    ;|
;;#######################################################################################################




;;#######################################################################################################
;; Méthode screenshot : permet d'enregistrer une image de la vue dans un fichier PNG.                   |
;;=======================================================================================================
to screenshot                                                                                          ;|
  export-view (word "./Exp/" "test" "/image"ticks".png")                                               ;|
end                                                                                                    ;|
;;#######################################################################################################





;;#######################################################################################################
;; Méthode output : permet d'afficher une boite de message avec la couleur et la distance moyenne du    |
;; "barycentre" pour chaque type de nourriture. Et le nombre de nourriture récolter dans un rayon défini|
;; du barycentre.                                                                                       |
;;=======================================================================================================
to output                                                                                              ;|
  foreach colorTab [                                                                                   ;|
    a ->
    ask (ncentres with [typeC = a])[
      ask patches with [pcolor = a] in-radius (maxCountPatch + 5)[
        set plabel "#"                                                                                 ;|
      ]
    ]
    output-print "-------------------------------------"                                               ;|
    output-print (word "mean couleur "a" : ("mean [distance one-of nCentres with [typeC = a]] of patches with [pcolor = a]")");|
    let x item 0 ([count patches with [pcolor = a] in-radius (maxCountPatch + 5)] of ncentres with [typeC = a])
    let totalP ((count fourmis with [color = a]) + (count patches with [pcolor = a]))
    output-type (word "Nourriture totale de couleur "a" : " totalP)                                    ;|
    output-print ""                                                                                    ;|
    output-type (word "Fourmis encore chargé avec " a ": " count fourmis with [color = a])             ;|
    output-print ""                                                                                    ;|
    output-type (word "Nourriture bien triée de " a ": " x "(")                                        ;|
    output-type (word int((x / totalP)* 100) "%)")                                                     ;|
    output-print ""                                                                                    ;|
    output-print "-------------------------------------"                                               ;|
  ]                                                                                                    ;|
end                                                                                                    ;|
;;#######################################################################################################





;;#######################################################################################################
;; Méthode report stop-condition : retourne vrai si tout a été trier dans l'espace, faut sinon.         |
;;=======================================================================================================
to-report stop-condition                                                                               ;|
  let condition? false
  let res []
  set-current-plot "meanNourriture"
  ask nCentres[                                                                   ;; pour chaque type de centre :
    let mycolor typeC
    let x (mean [distance myself] of patches with [pcolor = mycolor])
    move-to mean_nc typeC
    set res fput (x < (maxCountPatch)) res                                        ;; on calcule la valeur de verité de [la moyenne des distance par rapport au barycentre]
                                                                                  ;; qui est inferieur [au rayon du cercle pouvant contenir toute la nourriture] + une valeur d'erreur
    set-plot-pen-mode 2 set-plot-pen-color typeC plot x plot-pen-up               ;; on plot le mean de chaque nourriture sur le graphique
  ]
  plot-pen-down set-plot-pen-color 8 plot maxCountPatch
  set condition? (reduce and res)                                                 ;; on reduit avec un "and" toute les valeurs obtenues dans "res"
                                                                                  ;; afin d'obtenir la valeur de vérité de tout le tableau
  report condition?
end                                                                                                    ;|
;;#######################################################################################################





;;#######################################################################################################
;; Méthode move : permet de faire avancer les fourmis sur les patches à condition que le patch soit libre,
;; chaque fois que la fourmi avance, elle ajoute le type de patch à sa mémoire.                         |
;;=======================================================================================================
to move                                                                                                ;|
  rt random 45 lt random 45                                                                            ;|
  ifelse (can-move? 1)[                                                                                ;|
    ifelse ([pcolor] of patch-ahead 1 = 0 and (not charge?))[
      fd 2 repeat 2[memoire]][fd 1 memoire]                    ;; chaque fois que la fourmi avance elle fait appel a la fonction
  ][rt 180]                                                                                            ;|
end                                                                                                    ;|
;;#######################################################################################################





;;#######################################################################################################
;; Méthode memoire : chaque fourmis dispose d'un tableau de mémoire de taille "m". Si sa mémoire est    |
;; plaine alors elle oublie le premier objet qu'elle a rencontrée et le remplace par ce dernier, sinon  |
;; elle l'ajoute directement dans ça mémoire.                                                           |
;;=======================================================================================================
to memoire                                                                                             ;|
  if (length tab >= m)[                  ;; si la mémoire plaine :
    set tab but-last tab                 ;; effacer le premier objet rencontrer (dernier du tableau)
  ]                                                                                                    ;|
  set tab fput pcolor tab                ;; ajouter l'objet courant à la mémoire (premier du tableau)
end                                                                                                    ;|
;;#######################################################################################################





;;#######################################################################################################
;; Méthode report sameInTable : elle compte le nombre d'objets de meme type avec un objet qu'une fourmi |
;; vient de trouver avec tous les objets qu'elle a en mémoire.                                          |
;; A : couleur de l'objet rencontré.                                                                    |
;; B : liste des objets présent dans sa mémoire.                                                        |
;;=======================================================================================================
to-report sameInTable [A B]                                                                            ;|
  report length(filter [i -> i = A] B)                                                                 ;|
end                                                                                                    ;|
;;#######################################################################################################




;;#######################################################################################################
;; Méthode report diffInTable : elle compte le nombre d'objets different de l'objet qu'une fourmi vient |
;; de trouver avec tous les objets qu'elle a en mémoire.                                                |
;; A : couleur de l'objet rencontré.                                                                    |
;; B : liste des objets présent dans sa mémoire.                                                        |
;;=======================================================================================================
to-report diffInTable [A B]                                                                            ;|
  report length(filter [i -> ((i != A) and (i != 0))] B)                                               ;|
end                                                                                                    ;|
;;#######################################################################################################




;;#######################################################################################################
;; Methode putdown : la fonction permettant aux fourmis de deposer la nourriture dans un tas.           |
;; la fonction à un caractère probabiliste, qui dépend de la densité du tas, plus la densité est grande,|
;; plus la probabilité de depposer la nourriture est grande.                                            |                                                                                  |
;;=======================================================================================================
to putdown                                                                                             ;|
  if (pcolor = 0)[                                                                                     ;|
    let f (precision (((sameInTable color tab) + (err * (diffInTable color tab))) / (length tab)) 2)   ;|
    if (random-float 1.0 < ((f / (k- + f)) ^ 2))[                                                      ;|
      set pcolor color set color white set charge? false                                               ;|
    ]                                                                                                  ;|
  ]                                                                                                    ;|
end                                                                                                    ;|
;;#######################################################################################################




;;#######################################################################################################
;; Methode pickup : la fonction permettant aux fourmis de prendre et deplacer la nourriture.            |
;; la fonction à un caractère probabiliste, qui dépend de la densité du tas, plus la densité est faible,|
;; plus la probabilité de ramasser la nourriture est grande.                                            |
;;=======================================================================================================
to pickup                                                                                              ;|
  if ((pcolor != 0) and (length tab > 0))[                                                             ;|
    let f (precision (((sameInTable color tab) + (err * (diffInTable color tab))) / (length tab)) 2)   ;|
    if (random-float 1.0 < ((k+ / (k+ + f)) ^ 2))[                                                     ;|
      set color pcolor set pcolor 0 set charge? true set plabel ""                                     ;|
    ]                                                                                                  ;|
  ]                                                                                                    ;|
end                                                                                                    ;|
;;#######################################################################################################




;;#######################################################################################################
;; Methode report mean_nc : pour chaque type de nourriture, elle retourn la distances moyenne entre tous|
;; les patches de meme couleur par rapport à leurs barycentre.                                          |
;; [nc] : Un paramètre passé à la fonction qui est la couleur de la nourriture.                         |
;;=======================================================================================================
to-report mean_nc [nc]                                                                                 ;|
  let xmean mean [pxcor] of patches with [pcolor = nc]                                                 ;|
  let ymean mean [pycor] of patches with [pcolor = nc]                                                 ;|
  let centre-geographique patch xmean ymean                                                            ;|
  report centre-geographique                                                                           ;|
end                                                                                                    ;|
;;#######################################################################################################




;;#######################################################################################################
;; Méthode report circle-container : Calcule le plus petit rayon du cercle pouvant contenir un nombre   |
;; "X" de patches.                                                                                      |
;; [x] : Un paramètre passé à la fonction qui est le nombre de nourriture (patches) d'une même couleur. |
;;=======================================================================================================
to-report circle-container [x]                                                                         ;|
  let i 1                                                                                              ;|
  while [x > 0] [                                                                                      ;|
    set x (x - 8 * i)                                                                                  ;|
    set i (i + 1)                                                                                      ;|
  ]                                                                                                    ;|
  report i                                                                                             ;|
end                                                                                                    ;|
;;#######################################################################################################
@#$#@#$#@
GRAPHICS-WINDOW
205
20
780
379
-1
-1
7.0
1
10
1
1
1
0
0
0
1
0
80
0
49
1
1
1
ticks
30.0

BUTTON
530
425
605
458
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
15
205
187
238
typeNourriture
typeNourriture
1
5
2.0
1
1
NIL
HORIZONTAL

SLIDER
15
170
187
203
nbrFourmis
nbrFourmis
0
500
50.0
10
1
NIL
HORIZONTAL

SLIDER
15
240
187
273
nbrNourritureParType
nbrNourritureParType
1
1500
300.0
50
1
NIL
HORIZONTAL

SLIDER
15
325
187
358
m
m
0
100
15.0
1
1
NIL
HORIZONTAL

SLIDER
15
430
187
463
err
err
0
1
0.0
.1
1
NIL
HORIZONTAL

SLIDER
15
395
187
428
k+
k+
0.1
1
0.2
.1
1
NIL
HORIZONTAL

SLIDER
15
360
187
393
k-
k-
0.1
1
0.1
.1
1
NIL
HORIZONTAL

INPUTBOX
15
50
93
110
w
80.0
1
0
Number

INPUTBOX
109
50
187
110
h
49.0
1
0
Number

TEXTBOX
97
70
112
90
X\n
16
0.0
1

SWITCH
210
425
330
458
AntShow?
AntShow?
1
1
-1000

BUTTON
615
425
690
458
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
345
425
465
458
StopCondition?
StopCondition?
0
1
-1000

PLOT
795
40
1160
255
meanNourriture
NIL
NIL
0.0
0.0
0.0
0.0
true
false
"" ""
PENS
"pen-0" 1.0 0 -16448764 true "" ""

BUTTON
700
425
777
458
Capture
screenshot
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

OUTPUT
825
280
1140
455
11

TEXTBOX
10
295
200
321
PARAMÈTRAGE DE L'ENVIRONNEMENT
11
0.0
1

TEXTBOX
25
145
190
171
PARAMÈTRES D'INITIALISATION\n
11
0.0
1

TEXTBOX
35
30
185
48
DIMENSIONS DE L'UNIVERS 
11
0.0
1

TEXTBOX
620
410
685
428
SIMULATION
11
0.0
1

TEXTBOX
530
410
610
428
INITIALISATION
11
0.0
1

TEXTBOX
825
25
1135
51
MOYENNE DES POSITIONS DE CHAQUE TYPE DE NOURRITURES
11
130.0
1

TEXTBOX
880
265
1080
291
RÉSULTATS À LA FIN DE LA SIMULATION
11
0.0
1

TEXTBOX
355
410
480
436
CONDITION D'ARRÊT
11
0.0
1

TEXTBOX
210
410
335
428
AFFICHER LES FOURMIS
11
0.0
1

@#$#@#$#@
# **TRI COLLECTIF**

## Introduction


L'intelligence collective désigne la capacité d'une communauté à faire converger intelligence et connaissances pour avancer vers un but commun. Elle résulte de la qualité des interactions entre ses membres (ou agents).

<center>
<img src="./info/Intelligence-collective.png">
</center>

Nous pouvons prendre comme exemple le comportement des fourmis qui inspire toujours de les informaticiens, plusieurs algorithmes issus de la modélisation de colonies de fourmis sont appliqués au domaine de l'informatique.

<br>
<center>
<img src="./info/fourmis1.png">
<img src="./info/fourmis1.jpg">
</center>

Parmis ces algorithme on peut citer le tri collectif ([Collective Sorting][1]), Optimisation des colonies de fourmis ([Ant Colony Optimization][2]), raccourcis auto-organisés([Self-organized Shortcuts in the Argentine Ant][3]) ...

## Tri Collectif

<center>
<img src="./info/tc.png">
</center>

Cependant, Le phénomène du tri du couvain chez certaines espèces de fourmis est connu comme un des principaux exemples de la capacité des insectes sociaux à résoudre des problèmes à l’échelle collective.


# **Manuel d’utilisation de l'application**

## Composants de l’interface utilisateur

### Les BUTTONS

<center>
<img src="./info/interface/buttons.png">
</center>

#### setup : 
> Bouton pour charger l'environement (patches et tortues).

#### go : 
> Bouton qui permet d'executer la simulation.

#### capture : 
> Bouton qui permet d'enregistrer une capture de la vue dans un fichier PNG.


### Les INPUTS

<center>
<img src="./info/interface/inputs.png">
</center>

#### w (width)
> passer au modèle la largeur de la fenêtre de simulation.

#### h (heigh)
> passer au modèle la hauteur de la fenêtre de simulation. 


### Les SLIDERS

#### Les PARAMÈTRES D'INITIALISATION
<center>
<img src="./info/interface/paramsInit.png">
</center>

#### nbrFourmis
> Sélectionner le nombre de fourmis qu'on veut utiliser dans la simulation.


#### typeNourriture
> Sélectionner le nombre de type de nourritures à utiliser.

#### nbrNourritureParType
> Sélectionner le nombre d'element pour chaque type de nourritures.

#### Les PARAMÈTRES DE L'ENVIRONNEMENT

<center>
<img src="./info/interface/paramsEnv.png">
</center>

#### m (mémoire)
> Définir une taille pour la mémoire des fourmis.

#### k- et k+
> Régler la valeur des deux constante k- et k+.

#### err (erreur)
> Régler la valeur du degré d’erreur que peuvent faire les fourmis.

### Le GRAPHIQUE

<center>
<img src="./info/interface/plot.png">
</center>

#### meanNourriture
> Graphique pour afficher la distance entre les barycentres de chaque type de norriture par rapport a une valeur **'circle-container nbrNourritureParType'**.


### Les OUTPUTS

<center>
<img src="./info/interface/outputs.png">
</center>

> Une zone permettant d'afficher à la fin de l'execution, pour chaque type de nourriture :
  	- La moyenne des distances entre les objets de cette nourriture et son barycentre.
	- Le nombre totale d'objet de cette nourriture.
	- Le nombre de fourmis qui transporte encore des objets de cette nourriture.
	- Le nombre et le pourcentage de nourriture bien triés.











## **Comment utiliser l’application**

**1**. Saisir les dimensions de la fenêtre (**w**:width et **h**:heigh).
 
**2**. Régler le nombre de fourmis **nbrFourmis**, le nombre de type de nourriture **typeNourriture**, et le nombre d'éléments par nourriture **nbrNourriturePartype** à créer.

**3**. Paramétrer **m** la taille de la mémoire de chaque fourmi, la valeur de **k-** qui vas permettre aux fourmis après le calcule d'une certaine probabilité, de déposer la nourriture dans un tas. La valeur de **k+** qui vas permettre aux fourmis après le calcule d'une certaine probabilité, de ramasser la nourriture.

**4**. Initialiser le monde avec ces paramètres grâce au bouton **setup**.

**5**. Lancer la simulation avec le bouton **go**.

**6**. Tout au long de la simulation il est possible de :
	- "afficher" ou "cacher" les *fourmis* dans la fenêtre.
	- enregistrer une *capture* de la fenêtre a tout instant.
	- "activer" ou "désactiver" la **méthode de contrôle d'arrêt**.


**NB:** 
```
la méthode *méthode de contrôle d'arrêt* peut être désactiver afin d'améliorer la vitesse de la simulation.
```

## EXPLICATION DES MÉTHODES

### circle-container [x]
<center>
<img src="./info/methode/circle-container.png">
</center>


Cette méthode est appelée par deux autres méthodes qui sont **```stop-condition```** et **```output```**. Elle permet de calculer le rayon du plus petit cercle pouvant contenir un nombre de patches déjà défini. Comme nous pouvons le voir sur la figure ci-dessus, à chaque pas qu'on s’éloigne du centre on remarque que le nombre de patches augmente de 8. Nous avons donc creer la methode **```circle-container```** qui permet de trouver le rayon le plus petite pouvant contenir **```nbrNourritureParType```**.
Le principe donc et de calculer d'abord la moyenne des distances de tous les éléments d'une même nourriture avec le barycentre, lors de la formation d'un grand tas, la majorité des points seront prochent du barycentre ce qui fait baisser la moyenne. Si elle descend sous la taille du rayon **```circle-container nbrNourritureParType```**, alors la majorité des objets se trouve dans un meme tas.



Distance moyenne du barycentre :
*distance 1*-	(8*1)/8 = **1**    				<= rayon(1)
*distance 2*-	[(8 * 1)+(16 *2)]/24 = 40/24 = **1,66**   	<  rayon(2)
*distance 3*-	[40 + (24 * 3)]/48 = 112/48 = **2,33**    	<  rayon(3
*distance 4*-	[112 + (32 * 4)]/80 = 240/80 = **3**        	<  rayon(4)
*distance 5*-	[240 + (40 * 5)]/120 = 440/120 = **3,66**	<  rayon(5)


## Projet SMA :
Dans le cadre d'un projet pour le module  "Systèmes multi-agents".

### Ce travail a été réalisé par :
<center>
	<b>Yacine HADJAR</b>
	<br>
   	<b>Imad BOUZGOU</b>
</center>

[1]: https://www.researchgate.net/publication/235362107_The_dynamics_of_collective_sorting_robot-like_ants_and_antlike_robots

[2]: https://readera.org/read/15012067
[3]: http://homepages.ulb.ac.be/~jldeneub/images/Deneubourgetal1990.pdf
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="test_1n" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>export-view (word "./Exp/" "test"  "/image"ticks".png")</final>
    <timeLimit steps="4000000"/>
    <exitCondition>stop-condition = true</exitCondition>
    <metric>mean [distance nCentre 0] of patches with [pcolor = 15]</metric>
    <enumeratedValueSet variable="AntShow?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="typeNourriture">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="err">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k-">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k+">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nbrFourmis">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="h">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="w">
      <value value="290"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nbrNourritureParType">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="m">
      <value value="50"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment2" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>stop-condition = true</exitCondition>
    <metric>mean [distance nCentre 0] of patches with [pcolor = [label] of nCentre 0]</metric>
    <metric>mean [distance nCentre 1] of patches with [pcolor = [label] of nCentre 1]</metric>
    <enumeratedValueSet variable="AntShow?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="typeNourriture">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="err">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="k-" first="0.1" step="0.1" last="0.9"/>
    <steppedValueSet variable="k+" first="0.1" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="h">
      <value value="49"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="w">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nbrNourritureParType">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="m">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="StopCondition?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nbrFourmis">
      <value value="50"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@

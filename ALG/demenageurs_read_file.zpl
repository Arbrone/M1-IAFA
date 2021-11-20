# Une instance des déménageurs simple à 24 objets numérotés de 1 à 24

param fichier := "data_demenageurs_moyen.txt";
param capacite := read fichier as "1n" skip 1 use 1;
param NO := read fichier as "2n" skip 1 use 1;

set Objets := { 1 to NO by 1 } ;
set Boites := { 1 to NO by 1 } ;

param t[Objets] := read fichier as "1n" skip 2 use NO;

var x[Objets*Boites] binary;
var y[Boites] binary;

minimize valeur : sum<j> in Boites: y[j];

subto capacite_boite : forall <j> in Boites: sum<i> in Objets: x[i,j] * t[i] <= capacite;
subto boite_non_vide : forall <j> in Boites: forall <i> in Boites : y[j] >= x[i,j];
subto unique_boite   : forall <i> in Objets: sum<j> in Boites: x[i,j]==1;
# subto domain : forall<i> in Objets : 0 <= x[i] <= 1 ;

# Une instance des déménageurs simple à 24 objets numérotés de 1 à 24

param capacite := 9;
set Objets := { 1 to 24 by 1 } ;
set Boites := { 1 to 24 by 1 } ;
param t[Objets] :=
	<1> 6, <2> 6, <3> 5, <4> 5, <5> 5, <6> 4, <7> 4, <8> 4, <9> 4, <10> 2, <11> 2, <12>2, <13>2, <14> 3, <15> 3, <16> 7, <17> 7, <18> 5, <19> 5, <20> 8, <21> 8, <22> 4, <23> 4, <24> 5;

var x[Objets*Boites] binary;
var y[Boites] binary;

minimize valeur : sum<j> in Boites: y[j];

subto capacite_boite : forall <j> in Boites: sum<i> in Objets: x[i,j] * t[i] <= capacite;
subto boite_non_vide : forall <j> in Boites: forall <i> in Boites : y[j] >= x[i,j];
subto unique_boite   : forall <i> in Objets: sum<j> in Boites: x[i,j]==1;
# subto domain : forall<i> in Objets : 0 <= x[i] <= 1 ;

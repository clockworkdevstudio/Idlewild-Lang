; This program demonstrates the use of subroutines

creature$ = Input("Type a carnivorous creature to get a vegetarian creature! ")
Gosub vegetarianCreature
Goto skip

.vegetarianCreature
Print "Fruit-" + creature$
Return

.skip
End

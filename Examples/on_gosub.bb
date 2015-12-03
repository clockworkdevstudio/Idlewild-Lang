.start

user$ = Input("Type a number between 1 and 3: ")

If Asc(user$) < Asc("0") OR Asc(user$) > Asc("9") Then Goto start

num% = user$

If num% < 1 OR num% > 3 Then Goto start

On (num% - 1) Gosub gold, frankincense, myrrh

End

.gold
Print "Gold."
Return

.frankincense

Print "Frankincense."
Return

.myrrh

Print "Myrrh."
Return

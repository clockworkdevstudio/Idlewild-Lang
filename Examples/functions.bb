Const nine# = 9.0
Const five# = 5.0

Print ""
.start
user$ = Input("Farenheit <-> Celsius converter. Enter a decimal value followed by F (for Farenheit) or C (for Celsius) to convert between the units. ")

units$ = Right(user$,1)

If units$ = "F" OR units$ = "f" Then Goto c
If units$ = "C" OR units$ = "c" Then Goto f

Goto start

.f
Print Farenheit(Left(user$,Len(user$) - 1)) + "F"
End

.c
Print Celsius(Left(user$,Len(user$) - 1)) + "C"
End

Function Farenheit(n#)
	Return n# * nine# / five# + 32.0
End Function

Function Celsius(n#)
	Return (n# - 32) * (five# / nine#)
End Function 

; This program demonstrates the use of FOR... NEXT loops

Print ""
Print "Multiplication tables from 2 to 12:"
Print ""

For i% = 2 To 12
	For j% = 2 To 12
                z% = i% * j%
		Print i% + " * " + j% + " = " + z%
	Next
Next

; This program reads four strings from stdin into an array, then prints them out

Dim a$(2,2)

For i = 0 To 1
	For j = 0 To 1
		a$(i,j) = Input("> ")
	Next
Next

For i = 0 To 1
	For j = 0 To 1
		Print a$(i,j)
	Next
Next

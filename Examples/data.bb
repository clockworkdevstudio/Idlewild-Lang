Restore dataLine1

For i = 0 To 3
    Read word$
    Write word$
    Write " "
Next

Print ""

Restore dataLine2

For i = 0 To 4
    Read word$
    Write word$
    Write " "
Next

Print ""

Restore dataNumeric
Read integer%
Read floating

Print "Some sacred numbers:"
Print integer%
Print floating

.dataLine2
Data "Buried","in","an","angel's","breast."
.dataLine1
Data "Love","my","eternal","mystery"
.dataNumeric
Data 7,12.0

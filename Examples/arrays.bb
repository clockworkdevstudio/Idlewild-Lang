; Arrays example

Dim powers%(32)

For i% = 0 To 31
    powers%(i%) = 2 ^ i
Next

For i% = 0 To 31
    Print "2 ^ " + i% + " = " + powers%(i%)
Next

.enterPassword

password$ = Input("Enter your password: ")

If password$ = "orange" Then Gosub freedom
If authorised% Then Goto endProgram

failures% = failures% + 1
If failures% > 2 Then Goto badNews

Goto enterPassword

.freedom
authorised% = 1
Print "You are now free. Use your freedom wisely."
Return

.badNews
Print "You have entered the wrong password 3 times; the Minister of the Inferior has been informed."

.endProgram
End 0

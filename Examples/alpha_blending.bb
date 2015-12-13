Graphics 640,480,32,1
Origin 320,240
ClsColor 0,0,0

Local a# = 0.0,k# = 0.0
Local d

While Not KeyDown(1)
	Cls

	a# = GetAlpha()
	If a# >= 1.0 Or a# <= 0.0
		d = Not d
	End If

	If d
		SetAlpha a# - 0.005
	Else
		SetAlpha a# + 0.005
	End If

	k# = GetAlpha()
	SetAlpha 1
	Color 0,0,255
	Oval -128,-128,256,256,True
	SetAlpha k#

	Color 255,0,0
	Rect(-128,-128,256,256,True)

	o# = GetOrientation()
	SetOrientation(o# + 1)
	Flip
Wend

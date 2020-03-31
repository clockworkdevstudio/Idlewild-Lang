; Program demonstrates the use of 3D acceleration (transparency, scaling, rotation) to create a flame effect.

Const FLAME_RADIUS = 16
Const FLAME_CREATE_RATE = 5
Const SPEED# = 5.0
Const MAX_NUM_FLAMES = 250
Const WINDOWED = True

Global NUM_FLAMES = 0
Global TIMER,PINGS,i

Type Flame
	Field x#,y#
	Field intensity#
	Field transparency#
	Field scale#
	Field orientation#
	Field speed
	Field direction
End Type

SeedRnd Millisecs()

Graphics 800,600,0, WINDOWED

Global GRAPHICS_WIDTH = GraphicsWidth()
Global GRAPHICS_HEIGHT = GraphicsHeight()

SetBuffer BackBuffer()

TIMER = CreateTimer(30)
	
While Not KeyHit(1)
	PINGS = WaitTimer(TIMER)
	For i = 0 To PINGS
		Cls
		UpdateFire()
		UpdateFlames()
		DrawFlames()
		Flip
	Next
Wend

Function UpdateFire()
	Local f1.Flame,f2.Flame
	Local i = 0

	If NUM_FLAMES > MAX_NUM_FLAMES
		f1 = First Flame
		While i < FLAME_CREATE_RATE And f1 <> Null
			f2 = After f1
			
			Delete f1
			NUM_FLAMES = NUM_FLAMES - 1
			f1 = f2
			i = i + 1
		Wend
	EndIf

	For i = 1 To FLAME_CREATE_RATE
		CreateFlame(Rnd(0.0,GRAPHICS_WIDTH),Rnd(0.0,GRAPHICS_HEIGHT))
	Next
	
End Function

Function CreateFlame.Flame(x#,y#)
	Local f.Flame
	
	f = New Flame
	
	f\x# = x#
	f\y# = y#
	
	f\intensity# = Rnd(0.0,360.0)
	f\transparency# = Rnd(0.0,360.0)
	f\scale# = Rnd(0.0,360.0)
	f\orientation# = Rnd(0.0,360.0)
	f\speed = Rand(1,4)
	f\direction = Rand(0,2)

	NUM_FLAMES = NUM_FLAMES + 1
	
	Return f
	
End Function

Function UpdateFlames()
	Local f.Flame
	
	For f = Each Flame
		f\intensity# = (f\intensity# + SPEED#) Mod 360.0
		f\transparency# = (f\transparency# + SPEED#) Mod 360.0
		f\scale# = (f\scale# + SPEED#) Mod 360.0
		
		If f\direction = 0
			f\orientation# = (f\orientation# + SPEED#) Mod 360.0
		Else
			f\orientation# = (f\orientation# - SPEED#)
			If f\orientation# < 0.0
				f\orientation# = 360.0 + f\orientation#
			End If
		End If
		
		f\y# = f\y# - f\speed#
		
	Next
	
End Function

Function DrawFlames()
	Local alpha#,scale_x#,scale_y#,orientation#
	Local f.Flame
	
	alpha# = GetAlpha()
	scale_x# = GetScaleX()
	scale_y# = GetScaleY()
	orientation# = GetOrientation()
	
	For f = Each Flame
		SetAlpha Abs(Cos(f\transparency#))
		SetScale Abs(Cos(f\scale#)),Abs(Cos(f\scale#))
		SetOrientation f\orientation#		
		Color 255,216 * Abs(Cos(f\intensity)),0
		Rect f\x#,f\y#,2 * FLAME_RADIUS,2 * FLAME_RADIUS,True
	Next

	SetAlpha alpha#
	SetScale scale_x#, scale_y#
	SetOrientation orientation#
	
End Function

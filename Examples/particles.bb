Const WINDOWED = True

NUM_MODES = CountGfxModes()

Graphics 800,600,0, WINDOWED

Global GRAPHICS_WIDTH = GraphicsWidth()
Global GRAPHICS_HEIGHT = GraphicsHeight()

Const NUM_PARTICLES = 1
Const PARTICLE_LIFESPAN = 120
Const PARTICLE_VELOCITY = 3

Function f.T()

End Function

Type Particle
	Field x#,y#
	Field r#,g#,b#
	Field a#
	Field f
End Type

Origin GraphicsWidth() / 2,GraphicsHeight() / 2

Timer = CreateTimer(30)

While Not KeyHit(1)
	Frames = WaitTimer(Timer)
	For i = 1 To Frames
		Cls
		;If Rand(0,5) = 0
			CreateParticles
		;End If
		UpdateParticles
		DrawParticles
		Flip
	Next
Wend

While Not KeyHit(1)
	Frames = WaitTimer(Timer)
	For i = 1 To Frames
		Cls
		;If Rand(0,5) = 0
			;CreateParticles
		;End If
		UpdateParticles
		DrawParticles
		Flip
	Next
Wend

Function CreateParticles()
	Local p.Particle
	For i = 0 To NUM_PARTICLES - 1
		p = New Particle
		p\x# = 0
		p\y# = 0
		p\r# = Rnd(0.0,1.0)
		p\g# = Rnd(0.0,1.0)
		p\b# = Rnd(0.0,1.0)
		p\a# = Rnd(0.0,360.0)
		p\f = PARTICLE_LIFESPAN
	Next
End Function

Function UpdateParticles()
	Local p.Particle
	For p = Each Particle
		If p\f = 0
			Delete p
		Else
			p\f = p\f - 1
			p\x# = p\x# + PARTICLE_VELOCITY * Cos(p\a#)
			p\y# = p\y# + PARTICLE_VELOCITY * Sin(p\a#)
		End If
	Next
End Function

Function DrawParticles()
	Local p.Particle
	For p = Each Particle
		Color 255 * p\r#, 255 * p\g#, 255 * p\b#
		Oval p\x# - 4, p\y# - 4, 8, 8, True
	Next
End Function

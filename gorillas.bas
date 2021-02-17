' Monkey Type Game

' Setup Screen and graphics
Screen 13: Cls

Const x_max = 320
Const y_max = 200

' TODO: make this adjustable
Const y_grav = 1
' wind is another TODO

' User defined values
velocity = 0
angle = 0


' Creating standin monkey sprite in program (we can load in sprites in future)
Circle (4, 3), 4, 4
Paint (4, 3), 12, 4
Dim banana%(37)
Get (0, 0)-(8, 7), banana%()
Circle (4, 3), 4, 4
Paint (4, 3), 12, 4
Dim testObj%(37)
Get (0, 0)-(8, 7), testObj%()

' Monkey box (until sprite replaces)
Line (60, 60)-(80, 80), 15, BF
Dim monkey%(400)
Get (60, 60)-(80, 80), monkey%()

Cls: Paint (160, 100), 100

' monkey spawning
rilla_y = 100
rilla_w = 20
rilla_h = rilla_w
Put (40, 100), monkey%()
Put (240, 100), monkey%()

shooter = -1 ' -1 is true for some reason

Do:
    Locate 1, 8: Input angle: Locate 2, 8: Input velocity
    GoSub Launch_Banana
    shooter = Not shooter
Loop While InKey$ <> "q"


Launch_Banana:
old_x = 80
If shooter Then old_x = 40 Else old_x = 240
old_y = 100
x = old_x
y = old_y
multiplier = 1
' attempt to normalize angle to point towards other rilla
If shooter Then multiplier = 180 Else multiplier = 0
Print multiplier
dy = y_component(angle + multiplier, velocity)
dx = x_component(angle + multiplier, velocity)

Put (old_x, old_y), banana%(), Xor

Do
    ' clear  old banana
    Put (old_x, old_y), banana%(), Xor

    x = x + dx
    y = y + dy

    old_x = x
    old_y = y

    dy = dy + y_grav


    If x + 4 > x_max Or x < 0 Or y + 4 > y_max Or y < 0 Then Exit Do
    ' new banan
    Put (x, y), banana%(), Xor

    ' Check collision (with non shooting gorilla)
    rilla_x = 0
    If shooter Then rilla_x = 240 Else rilla_x = 40
    Locate 3, 8: Print is_collision(x, x + 4, y, y + 3, rilla_x, rilla_x + rilla_w, rilla_y, rilla_y + rilla_h)

    ' delay a bit
    Delay_Framerate
Loop While old_x + 4 < x_max And old_y + 4 < y_max And old_x > 0 And old_y > 0

Return

' shamelessly from https://balau82.wordpress.com/2015/01/18/nostalgia-trip-qbasic-game-programming/
Sub Delay_Framerate
    Static lasttimer As Single 'The value is retained between calls.
    Dim nexttimer As Single
    Dim maxfps As Single
    maxfps = 10
    nexttimer = lasttimer + 1! / maxfps
    Do While Timer < nexttimer
    Loop
    lasttimer = Timer
End Sub

Function x_component (angle, vel)
    x_component = Cos(angle) * vel
End Function

Function y_component (angle, vel)
    y_component = Sin(angle) * vel
End Function

' Rectangle bounding box collision detection
Function is_collision (x1, x2, y1, y2, xx1, xx2, yy1, yy2)
    is_collision = Not (yy2 < y1 Or yy1 > y2 Or xx2 < x1 Or xx1 > x2)
End Function

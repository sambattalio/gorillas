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
Cls: Paint (160, 100), 100

Put (50, 100), testObj%(), Xor
Do:
    Locate 1, 8: Input angle: Locate 2, 8: Input velocity

    GoSub Launch_Banana
Loop While InKey$ <> "q"


Launch_Banana:
old_x = 80
old_y = 100
x = old_x
y = old_y
dy = y_component(angle, velocity)
dx = x_component(angle, velocity)

Put (old_x, old_y), banana%(), Xor

Do
    ' clear  old banana
    Put (old_x, old_y), banana%(), Xor

    x = x + dx
    y = y + dy

    old_x = x
    old_y = y

    dy = dy + y_grav


    If x > x_max Or x < 0 Or y > y_max Or y < 0 Then Exit Do
    ' new banan
    Put (x, y), banana%(), Xor

    ' Check collision
    Locate 3, 8: Print is_collision(x, x + 4, y, y + 3, 50, 50 + 4, 100, 100 + 3)

    ' delay a bit
    Delay_Framerate
Loop While old_x < x_max And old_y < y_max And old_x > 0 And old_y > 0

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

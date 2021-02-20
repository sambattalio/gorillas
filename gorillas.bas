' Monkey Type Game

' Setup Screen and graphics
Screen 13: Cls
Cls: Paint (160, 100), 100

Const x_max = 320
Const y_max = 200

' TODO: make this adjustable
Const y_grav = 1
' wind is another TODO

' User defined values
velocity = 0
angle = 0

' Creating standin banana sprite in program (we can load in sprites in future)
Cls
'attempting to put in background


' Monke sprite
'$INCLUDE: 'GORILLA.QBD'

GORILLAW = 32 'Sprite width
GORILLAH = 32 'Sprite height
For Y = 1 To GORILLAH ' For each row; image height
    For X = 1 To GORILLAW ' For each column; image width
        Read DotColor
        If DotColor <> 15 Then 'We can then use this IF-THEN statement to
            PSet (X, Y), DotColor 'make COLOR 00 as a transparent color.
        End If
    Next X:
Next Y

Dim monkey%(GORILLAW * GORILLAH)
Get (1, 1)-(GORILLAW, GORILLAH), monkey%()
Cls

'$INCLUDE: 'BANANA.QBD'
BANANAW = 8
BANANAH = 8
Cls
For Y = 1 To BANANAW ' For each row; image height
    For X = 1 To BANANAH ' For each column; image width
        Read DotColor
        If DotColor <> 15 Then 'We can then use this IF-THEN statement to
            PSet (X, Y), DotColor 'make COLOR 00 as a transparent color.
        End If
    Next X
Next Y

Dim banana%(BANANAH * BANANAW)
Get (1, 1)-(BANANAW, BANANAH), banana%()

Cls
'$INCLUDE: 'BACKGROU.QBD'
For Y = 1 To 200 ' For each row; image height
    For X = 1 To 320 ' For each column; image width
        Read DotColor
        If DotColor <> 15 Then 'We can then use this IF-THEN statement to
            PSet (X, Y), DotColor 'make COLOR 00 as a transparent color.
        End If
    Next X
Next Y


' test banana

' monkey spawning
rilla_y = 100
rilla_w = 20
rilla_h = rilla_w

rilla1_x = 40
rilla1_y = 62

rilla2_x = 225
rilla2_y = 90

Put (rilla1_x, rilla1_y), monkey%()
Put (rilla2_x, rilla2_y), monkey%()

shooter = -1 ' -1 is true for some reason
player1_score = 0
player2_score = 0

' MAIN GAME LOOP
Do:
    Locate 1, 1: Print player1_score: Locate 1, 38: Print player2_score
    Locate 1, 8: Input angle: Locate 2, 8: Input velocity

    angle = angle * 3.14159 / 180
    GoSub Launch_Banana
    shooter = Not shooter
Loop While InKey$ <> "q"


' LAUNCH SUBROUTINE
Launch_Banana:
old_x = 80
If shooter Then old_x = rilla1_x Else old_x = rilla2_x
old_y = 100
If shooter Then old_y = rilla1_y Else old_y = rilla2_y
X = old_x
Y = old_y
multiplier = 1
' attempt to normalize angle to point towards other rilla
If shooter Then multiplier = 0 Else multiplier = 3.14159
dy = y_component(angle + multiplier, velocity)
dx = x_component(angle + multiplier, velocity)

Put (old_x, old_y), banana%(), Xor

Do
    ' clear  old banana
    Put (old_x, old_y), banana%(), Xor

    X = X + dx
    Y = Y + dy

    old_x = X
    old_y = Y

    dy = dy + y_grav


    If X + 4 + 2 > x_max Or X < 0 Or Y + 4 + 2 > y_max Or Y < 0 Then Exit Do
    ' new banan
    Put (X, Y), banana%(), Xor

    ' Check collision (with non shooting gorilla)
    rilla_x = 0
    If shooter Then rilla_x = rilla2_x Else rilla_x = rilla1_x
    if shooter then rilla_y = rilla2_y Else rilla_y = rilla1_y
    ' Check if it hit the opposing player and if so update the score accordingly
    If is_collision(X, X + BANANAW, Y, Y + BANANAH, rilla_x, rilla_x + rilla_w, rilla_y, rilla_y + rilla_h) Then
        If shooter Then player1_score = player1_score + 1 Else player2_score = player2_score + 1
        Return
    End If

    ' add collision detection of buildings
    ' first one is 30x80 on bottom left
    If is_collision(X, X + BANANAW, Y, Y + BANANAH, 0, 30, y_max - 80, y_max) Then
        Return
    End If

    If is_collision(X, X + BANANAW, Y, Y + BANANAH, 31, 90, y_max - 107, y_max) Then
        Return
    End If

    If is_collision(X, X + BANANAW, Y, Y + BANANAH, 91, 120, y_max - 77, y_max) Then
        Return
    End If

    If is_collision(X, X + BANANAW, Y, Y + BANANAH, 121, 150, y_max - 115, y_max) Then
        Return
    End If

    If is_collision(X, X + BANANAW, Y, Y + BANANAH, 151, 180, y_max - 125, y_max) Then
        Return
    End If

    If is_collision(X, X + BANANAW, Y, Y + BANANAH, 181, 210, y_max - 84, y_max) Then
        Return
    End If

    If is_collision(X, X + BANANAW, Y, Y + BANANAH, 211, 240, y_max - 80, y_max) Then
        Return
    End If


    If is_collision(X, X + BANANAW, Y, Y + BANANAH, 241, 270, y_max - 90, y_max) Then
        Return
    End If


    If is_collision(X, X + BANANAW, Y, Y + BANANAH, 271, x_max, y_max - 103, y_max) Then
        Return
    End If









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

' Helper functions
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

' Monkey Type Game

' Setup Screen and graphics
Screen 13: Cls
Cls: Paint (160, 100), 100

Const x_max = 320
Const y_max = 200

Const y_grav = 1
Const MAX_HITBOXES = 420
wind = 0

' Create initial hitboxes for buildings
Type hitbox_bounds
    x1 As Integer
    x2 As Integer
    y1 As Integer
    y2 As Integer
End Type
Common num_hitboxes As Integer
num_hitboxes = 9
Dim building_bounds(MAX_HITBOXES) As hitbox_bounds

' UGLY CODE don't look
Dim building1 As hitbox_bounds
building1.x1 = 0
building1.x2 = 32
building1.y1 = y_max - 80
building1.y2 = y_max
building_bounds(1) = building1
 
Dim building2 As hitbox_bounds
building2.x1 = 33
building2.x2 = 95
building2.y1 = y_max - 107
building2.y2 = y_max
building_bounds(2) = building2


Dim building3 As hitbox_bounds
building3.x1 = 96
building3.x2 = 131
building3.y1 = y_max - 77
building3.y2 = y_max
building_bounds(3) = building3

Dim building4 As hitbox_bounds
building4.x1 = 132
building4.x2 = 162
building4.y1 = y_max - 115
building4.y2 = y_max
building_bounds(4) = building4

Dim building5 As hitbox_bounds
building5.x1 = 163
building5.x2 = 196
building5.y1 = y_max - 125
building5.y2 = y_max
building_bounds(5) = building5

Dim building6 As hitbox_bounds
building6.x1 = 197
building6.x2 = 227
building6.y1 = y_max - 84
building6.y2 = y_max
building_bounds(6) = building6

Dim building7 As hitbox_bounds
building7.x1 = 228
building7.x2 = 261
building7.y1 = y_max - 80
building7.y2 = y_max
building_bounds(7) = building7

Dim building8 As hitbox_bounds
building8.x1 = 262
building8.x2 = 295
building8.y1 = y_max - 90
building8.y2 = y_max
building_bounds(8) = building8

Dim building9 As hitbox_bounds
building9.x1 = 296
building9.x2 = x_max
building9.y1 = y_max - 103
building9.y2 = y_max
building_bounds(9) = building9

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
Randomize Timer
' MAIN GAME LOOP
Do:
    Locate 1, 1: Print player1_score: Locate 1, 38: Print player2_score
    wind = Int(Rnd * 7) - 3
    Locate 1, 25: Print "Wind: ": Locate 1, 29: Print wind
    Locate 1, 8: Print "Angle: ": Locate 2, 8: Print "Velocity: "
    Locate 1, 17: Input angle: Locate 2, 17: Input velocity


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
addition = 1
' attempt to normalize angle to point towards other rilla
If shooter Then
    multiplier = -1
    addition = 0
Else
    multiplier = 1
    addition = 3.14159
End If
dy = y_component(multiplier * (angle + addition), velocity)
dx = x_component(multiplier * (angle + addition), velocity)

Put (old_x, old_y), banana%(), Xor

Do
    ' clear  old banana
    Put (old_x, old_y), banana%(), Xor

    X = X + dx + wind
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
    If shooter Then rilla_y = rilla2_y Else rilla_y = rilla1_y
    ' Check if it hit the opposing player and if so update the score accordingly
    If is_collision(X, X + BANANAW, Y, Y + BANANAH, rilla_x, rilla_x + rilla_w, rilla_y, rilla_y + rilla_h) Then
        If shooter Then player1_score = player1_score + 1 Else player2_score = player2_score + 1
        Return
    End If





    For I = 1 To num_hitboxes
        If is_collision(X, X + BANANAW, Y, Y + BANANAH, building_bounds(I).x1, building_bounds(I).x2, building_bounds(I).y1, building_bounds(I).y2) Then
            ' BOOM make hitbox for banan
            Dim banana_hitbox As hitbox_bounds
            banana_hitbox.x1 = X
            banana_hitbox.x2 = X + BANANAW
            banana_hitbox.y1 = Y
            banana_hitbox.y2 = Y + BANANAH
            num_hitboxes = explode_hitboxes(building_bounds(), I, banana_hitbox, num_hitboxes)
            ' clean last banana?
            LINE(X, Y)-(X+BANANAW, Y+BANANAH), 0, BF
            Return
        End If
    Next I




    ' delay a bit
    Delay_Framerate
Loop While old_x + 4 < x_max And old_y + 4 < y_max And old_x > 0 And old_y > 0

Return

Function explode_hitboxes (hitbox_array() As hitbox_bounds, building_i, banana_hitbox As hitbox_bounds, num_hitboxes)
    ' Case 1, on left edge of building
    ' cut off rectangle from banana up on left hash and
    ' form 2 new hitboxes for building
    Dim new_box As hitbox_bounds
    If banana_hitbox.x1 < hitbox_array(building_i).x1 Then
        y2_divot = max(banana_hitbox.y2, hitbox_array(building_i).y1 + (banana_hitbox.y2 - banana_hitbox.y1))
        x2_divot = hitbox_array(building_i).x1 + (banana_hitbox.x2 - banana_hitbox.x1)

        old_x1 = hitbox_array(building_i).x1
        hitbox_array(building_i).x1 = x2_divot + 1

        ' add new hitbox
        new_box.x1 = old_x1
        new_box.x2 = x2_divot
        new_box.y1 = y2_divot
        new_box.y2 = hitbox_array(building_i).y2
        num_hitboxes = num_hitboxes + 1
        hitbox_array(num_hitboxes) = new_box
        Line (new_box.x1, hitbox_array(building_i).y1)-(new_box.x2, new_box.y1), 0, BF
        explode_hitboxes = num_hitboxes
        Exit Function
    End If
    ' Case 2, right edge
    If banana_hitbox.x2 > hitbox_array(building_i).x2 Then
        y2_divot = max(banana_hitbox.y2, hitbox_array(building_i).y1 + (banana_hitbox.y2 - banana_hitbox.y1))
        x2_divot = hitbox_array(building_i).x2 - (banana_hitbox.x2 - banana_hitbox.x1)

        old_x2 = hitbox_array(building_i).x2
        hitbox_array(building_i).x2 = x2_divot - 1

        ' add new hitbox
        new_box.x1 = x2_divot
        new_box.x2 = old_x2
        new_box.y1 = y2_divot
        new_box.y2 = hitbox_array(building_i).y2
        num_hitboxes = num_hitboxes + 1
        hitbox_array(num_hitboxes) = new_box
        Line (new_box.x1, hitbox_array(building_i).y1)-(new_box.x2, new_box.y1), 0, BF
        explode_hitboxes = num_hitboxes
        Exit Function
    End If
    ' Case 3, in da middle ,split into 3 rectangles and just yeet a rect out the middle bits
    If banana_hitbox.x1 > hitbox_array(building_i).x1 And banana_hitbox.x2 < hitbox_array(building_i).x2 Then
        y2_divot = hitbox_array(building_i).y1 + (banana_hitbox.y2 - banana_hitbox.y1)
        x1_divot = banana_hitbox.x1
        x2_divot = banana_hitbox.x2
        y1_divot = hitbox_array(building_i).y1

        ' modify one hitbox in place -- just lower y value
        hitbox_array(building_i).y1 = y2_divot + 1

        ' create left hitbox
        new_box.x1 = hitbox_array(building_i).x1
        new_box.x2 = x1_divot - 1
        new_box.y1 = y1_divot
        new_box.y2 = y2_divot

        ' right hitbox
        Dim new_box2 As hitbox_bounds
        new_box2.x1 = x2_divot + 1
        new_box2.x2 = hitbox_array(building_i).x2
        new_box2.y1 = y1_divot
        new_box2.y2 = y2_divot

        ' add new ones in
        num_hitboxes = num_hitboxes + 1
        hitbox_array(num_hitboxes) = new_box
 
        num_hitboxes = num_hitboxes + 1
        hitbox_array(num_hitboxes) = new_box2
        Line (x1_divot, y1_divot)-(x2_divot, y2_divot), 0, BF
        explode_hitboxes = num_hitboxes
        Exit Function
    End If
End Function

' shamelessly from https://balau82.wordpress.com/2015/01/18/nostalgia-trip-qbasic-game-programming/
Sub Delay_Framerate
    Static lasttimer As Single 'The value is retained between calls.
    Dim nexttimer As Single
    Dim maxfps As Single
    maxfps = 20
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

Function min (a, b)
    smallest = -1
    If a < b Then smallest = a Else smallest = b
    min = smallest
End Function

Function max (a, b)
    biggest = -1
    If a > b Then biggest = a Else biggest = b
    max = biggest
End Function

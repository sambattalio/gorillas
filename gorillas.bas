' Monkey Type Game

' Setup Screen and graphics
SCREEN 13: CLS
CLS: PAINT (160, 100), 100

CONST x_max = 320
CONST y_max = 200

CONST y_grav = 1
CONST MAX_HITBOXES = 420
wind = 0

' Create initial hitboxes for buildings
TYPE hitbox_bounds
    x1 AS INTEGER
    x2 AS INTEGER
    y1 AS INTEGER
    y2 AS INTEGER
END TYPE
COMMON num_hitboxes AS INTEGER
num_hitboxes = 9
DIM building_bounds(MAX_HITBOXES) AS hitbox_bounds

' UGLY CODE don't look
DIM building1 AS hitbox_bounds
building1.x1 = 0
building1.x2 = 32
building1.y1 = y_max - 80
building1.y2 = y_max
building_bounds(1) = building1
 
DIM building2 AS hitbox_bounds
building2.x1 = 33
building2.x2 = 95
building2.y1 = y_max - 107
building2.y2 = y_max
building_bounds(2) = building2


DIM building3 AS hitbox_bounds
building3.x1 = 96
building3.x2 = 131
building3.y1 = y_max - 77
building3.y2 = y_max
building_bounds(3) = building3

DIM building4 AS hitbox_bounds
building4.x1 = 132
building4.x2 = 162
building4.y1 = y_max - 115
building4.y2 = y_max
building_bounds(4) = building4

DIM building5 AS hitbox_bounds
building5.x1 = 163
building5.x2 = 196
building5.y1 = y_max - 125
building5.y2 = y_max
building_bounds(5) = building5

DIM building6 AS hitbox_bounds
building6.x1 = 197
building6.x2 = 227
building6.y1 = y_max - 84
building6.y2 = y_max
building_bounds(6) = building6

DIM building7 AS hitbox_bounds
building7.x1 = 228
building7.x2 = 261
building7.y1 = y_max - 80
building7.y2 = y_max
building_bounds(7) = building7

DIM building8 AS hitbox_bounds
building8.x1 = 262
building8.x2 = 295
building8.y1 = y_max - 90
building8.y2 = y_max
building_bounds(8) = building8

DIM building9 AS hitbox_bounds
building9.x1 = 296
building9.x2 = x_max
building9.y1 = y_max - 103
building9.y2 = y_max
building_bounds(9) = building9

' User defined values
velocity = 0
angle = 0

' Creating standin banana sprite in program (we can load in sprites in future)
CLS
'attempting to put in background

' Monke sprite
'$INCLUDE: 'GORILLA.QBD'

GORILLAW = 32 'Sprite width
GORILLAH = 32 'Sprite height
FOR Y = 1 TO GORILLAH ' For each row; image height
    FOR X = 1 TO GORILLAW ' For each column; image width
        READ DotColor
        IF DotColor <> 15 THEN 'We can then use this IF-THEN statement to
            PSET (X, Y), DotColor 'make COLOR 00 as a transparent color.
        END IF
    NEXT X:
NEXT Y

DIM monkey%(GORILLAW * GORILLAH)
GET (1, 1)-(GORILLAW, GORILLAH), monkey%()
CLS

'$INCLUDE: 'BANANA.QBD'
BANANAW = 8
BANANAH = 8
CLS
FOR Y = 1 TO BANANAW ' For each row; image height
    FOR X = 1 TO BANANAH ' For each column; image width
        READ DotColor
        IF DotColor <> 15 THEN 'We can then use this IF-THEN statement to
            PSET (X, Y), DotColor 'make COLOR 00 as a transparent color.
        END IF
    NEXT X
NEXT Y

DIM banana%(135)
GET (1, 1)-(BANANAW, BANANAH), banana%()

'$INCLUDE: 'BANANA_9.QBD'
CLS
FOR Y = 1 TO BANANAW ' For each row; image height
    FOR X = 1 TO BANANAH ' For each column; image width
        READ DotColor
        IF DotColor <> 15 THEN 'We can then use this IF-THEN statement to
            PSET (X, Y), DotColor 'make COLOR 00 as a transparent color.
        END IF
    NEXT X
NEXT Y

GET (1, 1)-(BANANAW, BANANAH), banana%(34)
CLS

'$INCLUDE: 'BANANA_1.QBD'
CLS
FOR Y = 1 TO BANANAW ' For each row; image height
    FOR X = 1 TO BANANAH ' For each column; image width
        READ DotColor
        IF DotColor <> 15 THEN 'We can then use this IF-THEN statement to
            PSET (X, Y), DotColor 'make COLOR 00 as a transparent color.
        END IF
    NEXT X
NEXT Y

GET (1, 1)-(BANANAW, BANANAH), banana%(68)
CLS

'$INCLUDE: 'BANANA_2.QBD'
CLS
FOR Y = 1 TO BANANAW ' For each row; image height
    FOR X = 1 TO BANANAH ' For each column; image width
        READ DotColor
        IF DotColor <> 15 THEN 'We can then use this IF-THEN statement to
            PSET (X, Y), DotColor 'make COLOR 00 as a transparent color.
        END IF
    NEXT X
NEXT Y

GET (1, 1)-(BANANAW, BANANAH), banana%(102)
CLS


'$INCLUDE: 'BACKGROU.QBD'
FOR Y = 1 TO 200 ' For each row; image height
    FOR X = 1 TO 320 ' For each column; image width
        READ DotColor
        IF DotColor <> 15 THEN 'We can then use this IF-THEN statement to
            PSET (X, Y), DotColor 'make COLOR 00 as a transparent color.
        END IF
    NEXT X
NEXT Y


' test banana

' monkey spawning
rilla_y = 100
rilla_w = 20
rilla_h = rilla_w

rilla1_x = 40
rilla1_y = 62

rilla2_x = 225
rilla2_y = 90

PUT (rilla1_x, rilla1_y), monkey%()
PUT (rilla2_x, rilla2_y), monkey%()

shooter = -1 ' -1 is true for some reason
player1_score = 0
player2_score = 0
RANDOMIZE TIMER
' MAIN GAME LOOP
DO:
    LOCATE 1, 1: PRINT player1_score: LOCATE 1, 38: PRINT player2_score
    wind = INT(RND * 7) - 3
    LOCATE 1, 25: PRINT "Wind: ": LOCATE 1, 29: PRINT wind
    LOCATE 1, 8: PRINT "Angle: ": LOCATE 2, 8: PRINT "Velocity: "
    LOCATE 1, 17: INPUT angle: LOCATE 2, 17: INPUT velocity


    angle = angle * 3.14159 / 180
    GOSUB Launch_Banana
    shooter = NOT shooter

    'Monkey dance

    IF player1_score = 3 THEN
        GOSUB Monkey_Dance
    ELSEIF player2_score = 3 THEN
        GOSUB Monkey_Dance
    END IF

LOOP WHILE INKEY$ <> "q"

' LAUNCH SUBROUTINE
Launch_Banana:
old_x = 80
IF shooter THEN old_x = rilla1_x ELSE old_x = rilla2_x
old_y = 100
IF shooter THEN old_y = rilla1_y ELSE old_y = rilla2_y
X = old_x
Y = old_y
multiplier = 1
addition = 1
' attempt to normalize angle to point towards other rilla
IF shooter THEN
    multiplier = -1
    addition = 0
ELSE
    multiplier = 1
    addition = 3.14159
END IF
dy = y_component(multiplier * (angle + addition), velocity)
dx = x_component(multiplier * (angle + addition), velocity)

PUT (old_x, old_y), banana%(), XOR
PUT (old_x, old_y), banana%(34), XOR
PUT (old_x, old_y), banana%(68), XOR
PUT (old_x, old_y), banana%(102), XOR

DO
    ' clear  old banana
    PUT (old_x, old_y), banana%(), XOR
    PUT (old_x, old_y), banana%(34), XOR
    PUT (old_x, old_y), banana%(68), XOR
    PUT (old_x, old_y), banana%(102), XOR

    X = X + dx + wind
    Y = Y + dy

    old_x = X
    old_y = Y

    dy = dy + y_grav

    IF X + 4 + 2 > x_max OR X < 0 OR Y + 4 + 2 > y_max OR Y < 0 THEN EXIT DO
    ' new banana
    FOR spritenum% = 0 TO 3
        Delay_Framerate
        PUT (X, Y), banana%(spritenum% * 34), XOR
    NEXT spritenum%

    ' Check collision (with non shooting gorilla)
    rilla_x = 0
    IF shooter THEN rilla_x = rilla2_x ELSE rilla_x = rilla1_x
    IF shooter THEN rilla_y = rilla2_y ELSE rilla_y = rilla1_y
    ' Check if it hit the opposing player and if so update the score accordingly
    IF is_collision(X, X + BANANAW, Y, Y + BANANAH, rilla_x, rilla_x + rilla_w, rilla_y, rilla_y + rilla_h) THEN
        IF shooter THEN player1_score = player1_score + 1 ELSE player2_score = player2_score + 1
        RETURN
    END IF


    FOR I = 1 TO num_hitboxes
        IF is_collision(X, X + BANANAW, Y, Y + BANANAH, building_bounds(I).x1, building_bounds(I).x2, building_bounds(I).y1, building_bounds(I).y2) THEN
            ' BOOM make hitbox for banan
            DIM banana_hitbox AS hitbox_bounds
            banana_hitbox.x1 = X
            banana_hitbox.x2 = X + BANANAW
            banana_hitbox.y1 = Y
            banana_hitbox.y2 = Y + BANANAH
            num_hitboxes = explode_hitboxes(building_bounds(), I, banana_hitbox, num_hitboxes)
            ' clean last banana?
            LINE (X, Y)-(X + BANANAW, Y + BANANAH), 0, BF
            RETURN
        END IF
    NEXT I




    ' delay a bit
    Delay_Framerate
LOOP WHILE old_x + 4 < x_max AND old_y + 4 < y_max AND old_x > 0 AND old_y > 0

RETURN

Monkey_Dance:
DIM monkey_dance%(4099)

'Monke left
'$INCLUDE: 'GORILLA_LEFT.QBD'

FOR Y = 1 TO GORILLAH ' For each row; image height
    FOR X = 1 TO GORILLAW ' For each column; image width
        READ DotColor
        IF DotColor <> 15 THEN 'We can then use this IF-THEN statement to
            PSET (X, Y), DotColor 'make COLOR 00 as a transparent color.
        END IF
    NEXT X:
NEXT Y

GET (1, 1)-(GORILLAW, GORILLAH), monkey_dance%()
CLS

'Monke right
'$INCLUDE: 'GORILLA_RIGHT.QBD'

FOR Y = 1 TO GORILLAH ' For each row; image height
    FOR X = 1 TO GORILLAW ' For each column; image width
        READ DotColor
        IF DotColor <> 15 THEN 'We can then use this IF-THEN statement to
            PSET (X, Y), DotColor 'make COLOR 00 as a transparent color.
        END IF
    NEXT X:
NEXT Y

GET (1, 1)-(GORILLAW, GORILLAH), monkey_dance%(2050)
CLS

DO:
    FOR spritenum% = 0 TO 1
        Delay_Framerate
        PUT (100, 100), monkey_dance%(spritenum% * 2050), PSET
        Delay_Framerate
    NEXT spritenum%
LOOP WHILE INKEY$ <> "q"
SYSTEM 1
RETURN

FUNCTION explode_hitboxes (hitbox_array() AS hitbox_bounds, building_i, banana_hitbox AS hitbox_bounds, num_hitboxes)
    ' Case 1, on left edge of building
    ' cut off rectangle from banana up on left hash and
    ' form 2 new hitboxes for building
    DIM new_box AS hitbox_bounds
    IF banana_hitbox.x1 < hitbox_array(building_i).x1 THEN
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
        LINE (new_box.x1, hitbox_array(building_i).y1)-(new_box.x2, new_box.y1), 0, BF
        explode_hitboxes = num_hitboxes
        EXIT FUNCTION
    END IF
    ' Case 2, right edge
    IF banana_hitbox.x2 > hitbox_array(building_i).x2 THEN
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
        LINE (new_box.x1, hitbox_array(building_i).y1)-(new_box.x2, new_box.y1), 0, BF
        explode_hitboxes = num_hitboxes
        EXIT FUNCTION
    END IF
    ' Case 3, in da middle ,split into 3 rectangles and just yeet a rect out the middle bits
    IF banana_hitbox.x1 > hitbox_array(building_i).x1 AND banana_hitbox.x2 < hitbox_array(building_i).x2 THEN
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
        DIM new_box2 AS hitbox_bounds
        new_box2.x1 = x2_divot + 1
        new_box2.x2 = hitbox_array(building_i).x2
        new_box2.y1 = y1_divot
        new_box2.y2 = y2_divot

        ' add new ones in
        num_hitboxes = num_hitboxes + 1
        hitbox_array(num_hitboxes) = new_box
 
        num_hitboxes = num_hitboxes + 1
        hitbox_array(num_hitboxes) = new_box2
        LINE (x1_divot, y1_divot)-(x2_divot, y2_divot), 0, BF
        explode_hitboxes = num_hitboxes
        EXIT FUNCTION
    END IF
END FUNCTION

' shamelessly from https://balau82.wordpress.com/2015/01/18/nostalgia-trip-qbasic-game-programming/
SUB Delay_Framerate
    STATIC lasttimer AS SINGLE 'The value is retained between calls.
    DIM nexttimer AS SINGLE
    DIM maxfps AS SINGLE
    maxfps = 10
    nexttimer = lasttimer + 1! / maxfps
    DO WHILE TIMER < nexttimer
    LOOP
    lasttimer = TIMER
END SUB

' Helper functions
FUNCTION x_component (angle, vel)
    x_component = COS(angle) * vel
END FUNCTION

FUNCTION y_component (angle, vel)
    y_component = SIN(angle) * vel
END FUNCTION

' Rectangle bounding box collision detection
FUNCTION is_collision (x1, x2, y1, y2, xx1, xx2, yy1, yy2)
    is_collision = NOT (yy2 < y1 OR yy1 > y2 OR xx2 < x1 OR xx1 > x2)
END FUNCTION

FUNCTION min (a, b)
    smallest = -1
    IF a < b THEN smallest = a ELSE smallest = b
    min = smallest
END FUNCTION

FUNCTION max (a, b)
    biggest = -1
    IF a > b THEN biggest = a ELSE biggest = b
    max = biggest
END FUNCTION

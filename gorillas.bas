' Monkey Type Game

' Setup Screen and graphics
SCREEN 13: CLS
CLS: PAINT (160, 100), 100

CONST x_max = 320
CONST y_max = 200

' TODO: make this adjustable
CONST y_grav = 1
' wind is another TODO

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

DIM banana%(BANANAH * BANANAW)
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

DIM rotated_banana%(BANANAH * BANANAW)
GET (1, 1)-(BANANAW, BANANAH), rotated_banana%()


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

' MAIN GAME LOOP
DO:
    LOCATE 1, 1: PRINT player1_score: LOCATE 1, 38: PRINT player2_score
    LOCATE 1, 8: INPUT angle: LOCATE 2, 8: INPUT velocity

    angle = angle * 3.14159 / 180
    GOSUB Launch_Banana
    shooter = NOT shooter

    'Monkey dance

    IF player1_score = 3 THEN
        GOSUB Monkey_Dance
    ELSEIF player2_score = 1 THEN
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
' attempt to normalize angle to point towards other rilla
IF shooter THEN multiplier = 0 ELSE multiplier = 3.14159
dy = y_component(angle + multiplier, velocity)
dx = x_component(angle + multiplier, velocity)

PUT (old_x, old_y), banana%(), XOR
PUT (old_x, old_y), rotated_banana%(), XOR

DO
    ' clear  old banana
    PUT (old_x, old_y), banana%(), XOR
    PUT (old_x, old_y), rotated_banana%(), XOR

    X = X + dx
    Y = Y + dy

    old_x = X
    old_y = Y

    dy = dy + y_grav


    IF X + 4 + 2 > x_max OR X < 0 OR Y + 4 + 2 > y_max OR Y < 0 THEN EXIT DO
    ' new banana
    Delay_Framerate
    PUT (X, Y), banana%(), XOR
    Delay_Framerate
    PUT (X, Y), rotated_banana%(), XOR

    ' Check collision (with non shooting gorilla)
    rilla_x = 0
    IF shooter THEN rilla_x = rilla2_x ELSE rilla_x = rilla1_x
    IF shooter THEN rilla_y = rilla2_y ELSE rilla_y = rilla1_y
    ' Check if it hit the opposing player and if so update the score accordingly
    IF is_collision(X, X + BANANAW, Y, Y + BANANAH, rilla_x, rilla_x + rilla_w, rilla_y, rilla_y + rilla_h) THEN
        IF shooter THEN player1_score = player1_score + 1 ELSE player2_score = player2_score + 1
        RETURN
    END IF

    ' add collision detection of buildings
    ' first one is 30x80 on bottom left
    IF is_collision(X, X + BANANAW, Y, Y + BANANAH, 0, 30, y_max - 80, y_max) THEN
        RETURN
    END IF

    IF is_collision(X, X + BANANAW, Y, Y + BANANAH, 31, 90, y_max - 107, y_max) THEN
        RETURN
    END IF

    IF is_collision(X, X + BANANAW, Y, Y + BANANAH, 91, 120, y_max - 77, y_max) THEN
        RETURN
    END IF

    IF is_collision(X, X + BANANAW, Y, Y + BANANAH, 121, 150, y_max - 115, y_max) THEN
        RETURN
    END IF

    IF is_collision(X, X + BANANAW, Y, Y + BANANAH, 151, 180, y_max - 125, y_max) THEN
        RETURN
    END IF

    IF is_collision(X, X + BANANAW, Y, Y + BANANAH, 181, 210, y_max - 84, y_max) THEN
        RETURN
    END IF

    IF is_collision(X, X + BANANAW, Y, Y + BANANAH, 211, 240, y_max - 80, y_max) THEN
        RETURN
    END IF


    IF is_collision(X, X + BANANAW, Y, Y + BANANAH, 241, 270, y_max - 90, y_max) THEN
        RETURN
    END IF


    IF is_collision(X, X + BANANAW, Y, Y + BANANAH, 271, x_max, y_max - 103, y_max) THEN
        RETURN
    END IF









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
RETURN

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

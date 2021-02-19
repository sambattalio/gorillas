' Monkey Type Game

' Setup Screen and graphics
SCREEN 13: CLS

CONST x_max = 320
CONST y_max = 200

' TODO: make this adjustable
CONST y_grav = 1
' wind is another TODO

' User defined values
velocity = 0
angle = 0

' Creating standin banana sprite in program (we can load in sprites in future)
CIRCLE (4, 3), 4, 4
PAINT (4, 3), 12, 4
DIM banana%(37)
GET (0, 0)-(8, 7), banana%()
CIRCLE (4, 3), 4, 4
PAINT (4, 3), 12, 4
DIM testObj%(37)
GET (0, 0)-(8, 7), testObj%()

'attempting to put in background


' Monke sprite 
'$INCLUDE: 'GORILLA.QBD'

SCREEN 13 'Use SCREEN number you picked
CLS
GORILLAW = 32 'Sprite width
GORILLAH = 32 'Sprite height
FOR Y = 1 TO GORILLAH ' For each row; image height
    FOR X = 1 TO GORILLAW ' For each column; image width
        READ DotColor
        IF DotColor > 0 THEN 'We can then use this IF-THEN statement to
            PSET (X, Y), DotColor 'make COLOR 00 as a transparent color.
        END IF
    NEXT X
NEXT Y

DIM monkey%(GORILLAW * GORILLAH)
GET (1, 1)-(GORILLAW, GORILLAH), monkey%()
CLS

CLS: PAINT (160, 100), 100

' test banana

' monkey spawning
rilla_y = 100
rilla_w = 20
rilla_h = rilla_w
PUT (40, 100), monkey%()
PUT (240, 100), monkey%()

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
LOOP WHILE INKEY$ <> "q"


' LAUNCH SUBROUTINE
Launch_Banana:
old_x = 80
IF shooter THEN old_x = 40 ELSE old_x = 240
old_y = 100
X = old_x
Y = old_y
multiplier = 1
' attempt to normalize angle to point towards other rilla
IF shooter THEN multiplier = 0 ELSE multiplier = 3.14159
dy = y_component(angle + multiplier, velocity)
dx = x_component(angle + multiplier, velocity)

PUT (old_x, old_y), banana%(), XOR

DO
    ' clear  old banana
    PUT (old_x, old_y), banana%(), XOR

    X = X + dx
    Y = Y + dy

    old_x = X
    old_y = Y

    dy = dy + y_grav


    IF X + 4 + 2 > x_max OR X < 0 OR Y + 4 + 2 > y_max OR Y < 0 THEN EXIT DO
    ' new banan
    PUT (X, Y), banana%(), XOR

    ' Check collision (with non shooting gorilla)
    rilla_x = 0
    IF shooter THEN rilla_x = 240 ELSE rilla_x = 40
    ' Check if it hit the opposing player and if so update the score accordingly
    IF is_collision(X, X + 4, Y, Y + 3, rilla_x, rilla_x + rilla_w, rilla_y, rilla_y + rilla_h) THEN
        IF shooter THEN player1_score = player1_score + 1 ELSE player2_score = player2_score + 1
        RETURN
    END IF

    ' delay a bit
    Delay_Framerate
LOOP WHILE old_x + 4 < x_max AND old_y + 4 < y_max AND old_x > 0 AND old_y > 0

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

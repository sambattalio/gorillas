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


' Creating standin monkey sprite in program (we can load in sprites in future)
CIRCLE (4, 3), 4, 4
PAINT (4, 3), 12, 4
DIM banana%(37)
GET (0, 0)-(8, 7), banana%()
CLS: PAINT (160, 100), 100


DO:
    LOCATE 1, 8: INPUT angle: LOCATE 2, 8: INPUT velocity
    GOSUB Launch_Banana
LOOP WHILE INKEY$ <> "q"


SUB Launch_Banana
    old_x = 80
    old_y = 100
    x = old_x
    y = old_y
    dy = y_component(angle, velocity)
    dx = x_component(angle, velocity)


    DO
        ' clear  old banana
        PUT (old_x, old_y), banana%(), XOR

        x = x + dx
        y = y + dy

        old_x = x
        old_y = y

        dy = dy + y_grav


        IF x > x_max OR x < 0 OR y > y_max OR y < 0 THEN EXIT DO
        ' new banan
        PUT (x, y), banana%(), XOR

        ' delay a bit
        Delay_Framerate

    LOOP WHILE old_x < x_max AND old_y < y_max AND old_x > 0 AND old_y > 0

END SUB

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

FUNCTION x_component (angle, vel)
    x_component = COS(angle) * vel
END FUNCTION

FUNCTION y_component (angle, vel)
    y_component = SIN(angle) * vel
END FUNCTION

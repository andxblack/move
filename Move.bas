'Move!! version 4.2 by The Rizzler
'Mouse routeines from "The Revolutionary Guide to Qbasic", Wrow Press
'MAKE SURE DIRCTORY$ IS SET TO THE DIRECTORY WHICH MOVE.ZIP WAS UNZIPPED TO
'FOLLOW THE LINE DOWN TO FIND DIRECTORY$--\
					 'Ý
					 'Ý
DIM SHARED a%(34)                        'Ý
DIM dir1.1(404)                          'Ý
DIM dir1.2(404)                          'Ý
DIM dir1.3(404)                          'Ý
DIM dir5.1(404)                          'Ý
DIM dir5.2(404)                          'Ý
DIM dir5.3(404)                          'Ý
DIM dir3.1(404)                          'Ý
DIM dir3.2(404)                          'Ý
DIM dir3.3(404)                          'Ý
DIM dir7.1(404)                          'Ý
DIM dir7.2(404)                          'Ý
DIM dir7.3(404)                          'Ý
DIM mon(4824)                            'Ý
					 'Ý
DECLARE SUB MouseInit ()                 'Ý
DECLARE SUB MouseReset ()                'Ý
DECLARE SUB MouseHide ()                 'Ý
DECLARE SUB MouseShow ()                 'Ý
DECLARE SUB boundry.coord (curx%, curY%) 'Ý
DECLARE SUB load.cells ()                'Ý
DECLARE SUB intro ()                     'Ý
DECLARE FUNCTION mousex% ()              'Ý
DECLARE FUNCTION MouseY% ()              'Ý
DECLARE FUNCTION mousebuttons% ()        'Ý
					 'Ý
DIRECTORY$ = "C:\QBASIC45\" '<<-----------/ eg "C:\QBASIC\MOVE\"

DEF SEG = VARSEG(a%(0))
FOR i% = 0 TO 63
  READ d%
  POKE VARPTR(a%(0)) + i%, d%
NEXT i%
DEF SEG
DATA 00,00
DATA 00,00
DATA 00,00
'    Mashine codes        :        Assembler commands
DATA &HB8,00,00           :       'mov    ax,000h
DATA &HCD,&H33            :       'int    33h
DATA &H3D,&HFF,&HFF       :       'cmp    ax,0FFFFh
DATA &H75,&H0D            :       'jne    @@notPresent
DATA &H0E                 :       'push   cs
DATA &H07                 :       'pop    es
DATA &HBA,&H24,&H00       :       'mov    dx,offset MouseHandler
DATA &HB9,&HFF,&HFF       :       'mov    cx,0FFFFh
DATA &HB8,&H0C,&H00       :       'mov    ax,000Ch
DATA &HCD,&H33            :       'int    33h
			  : '@@NotPresent:
DATA &HCB                 :       'ret
DATA &HB8,00,00           :       'mov    ax,0000h
DATA &HCD,&H33            :       'int    33h
DATA &HCB                 :       'ret
DATA &H2E,&H89,&H0E,00,00 :       'mov    [cs:mouseX],cx
DATA &H2E,&H89,&H16,02,00 :       'mov    [cs:mouseY],dx
DATA &H2E,&H89,&H1E,04,00 :       'mov    [cs:mouseButtons],bx
DATA &HCB                 :       'ret
DATA &HB8,01,00           :       'mov    ax,0001h
DATA &HCD,&H33            :       'int    33h
DATA &HCB                 :       'ret
DATA &HB8,02,00           :       'mov    ax,0002h
DATA &HCD,&H33            :       'int    33h
DATA &HCB                 :       'ret
TOTALDIRECTORY$ = DIRECTORY$ + "MONSTER1.ANM"
SCREEN 9, 0, 0, 1
CLS
DEF SEG = VARSEG(mon(0))
BLOAD TOTALDIRECTORY$, 0
DEF SEG

PUT (1, 1), mon
GET (0, 0)-(19, 33), dir5.1
GET (20, 0)-(39, 33), dir5.2
GET (40, 0)-(59, 33), dir5.3
GET (60, 0)-(79, 33), dir1.1
GET (80, 0)-(99, 33), dir1.2
GET (100, 0)-(119, 33), dir1.3
GET (120, 0)-(139, 33), dir3.1
GET (140, 0)-(159, 33), dir3.2
GET (160, 0)-(179, 33), dir3.3
GET (180, 0)-(199, 33), dir7.1
GET (200, 0)-(219, 33), dir7.2
GET (220, 0)-(239, 33), dir7.3

CLS
SCREEN 9

FOR y = 0 TO 13
FOR x = 0 TO 7
READ a$
PSET (x, y), VAL("&h" + a$)
NEXT x
NEXT y

DATA F,0,0,0,0,0,0,0
DATA F,F,0,0,0,0,0,0
DATA F,F,F,0,0,0,0,0
DATA F,F,F,F,0,0,0,0
DATA F,F,F,F,F,0,0,0
DATA F,F,F,F,F,F,0,0
DATA F,F,F,F,F,F,F,0
DATA F,F,F,F,F,F,F,F
DATA F,F,0,F,F,0,0,0
DATA F,0,0,F,F,0,0,0
DATA 0,0,0,0,F,F,0,0
DATA 0,0,0,0,F,F,0,0
DATA 0,0,0,0,0,F,F,0
DATA 0,0,0,0,0,F,F,0
DIM cur%(8, 14)
GET (0, 0)-(8, 14), cur%
CLS

CALL MouseInit
CALL intro

SCREEN 9, 0, 0, 1: CLS
points% = 0
move.pos.x = 100
move.pos.y = 100
start.x% = 100
start.y% = 100
MOVE$ = ""
col% = 0
mouse.counter$ = "go"
DO

IF mouse.counter$ = "go" THEN LET mouse.counter% = 1: mouse.counter$ = "move"
IF mouse.counter$ = "move" THEN mouse.counter% = mouse.counter% + 1
IF mouse.counter% > 5 THEN LET mouse.counter$ = "ok"

IF MOVE$ = "go" AND smaller$ = "x" THEN GOSUB xsmall
IF MOVE$ = "go" AND smaller$ = "y" THEN GOSUB ysmall

IF dir = 1 AND move.pos.y > end.pos.y THEN MOVE$ = "stop"
IF dir = 2 AND move.pos.y > end.pos.y THEN MOVE$ = "stop"
IF dir = 8 AND move.pos.y > end.pos.y THEN MOVE$ = "stop"
IF dir = 4 AND move.pos.y < end.pos.y THEN MOVE$ = "stop"
IF dir = 5 AND move.pos.y < end.pos.y THEN MOVE$ = "stop"
IF dir = 6 AND move.pos.y < end.pos.y THEN MOVE$ = "stop"
IF dir = 3 AND move.pos.x > end.pos.x THEN MOVE$ = "stop"
IF dir = 7 AND move.pos.x < end.pos.x THEN MOVE$ = "stop"

IF move.pos.x < 15 THEN MOVE$ = "stop"
IF move.pos.x > 622 THEN MOVE$ = "stop"
IF move.pos.y < 20 THEN MOVE$ = "stop"
IF move.pos.y > 320 THEN MOVE$ = "stop"
      
IF MOVE$ = "stop" THEN
start.x% = move.pos.x
start.y% = move.pos.y
points% = 0
col% = 0
END IF

CLS
LOCATE 1, 1: PRINT "Move!!   v4.2                                                 Press Esc to Quit"

IF mousebuttons% = 1 AND points% = 0 THEN end.x% = mousex%: end.y% = MouseY%: points% = 1: mouse.counter$ = "go": GOSUB parimeters
IF mouse.counter$ = "ok" AND mousebuttons% = 1 AND points% = 1 THEN
	start.x% = move.pos.x
	start.y% = move.pos.y
	end.x% = curx%
	end.y% = curY%
	points% = 1
	mouse.counter$ = "go"
	col% = 14
	GOSUB parimeters
END IF
IF mousebuttons% = 2 THEN MOVE$ = "stop"

PSET (end.x%, end.y%), col%
GOSUB animate
CALL boundry.coord(curx%, curY%)
PUT (curx%, curY%), cur%, XOR

PCOPY 0, 1
LOOP UNTIL INKEY$ = CHR$(27)
CALL MouseReset
END

parimeters:

x.along% = end.x% - start.x%
y.along% = end.y% - start.y%

IF SGN(x.along%) = -1 THEN x.along% = x.along% * -1
IF SGN(y.along%) = -1 THEN y.along% = y.along% * -1

IF x.along% < y.along% THEN smaller$ = "x"
IF y.along% < x.along% THEN smaller$ = "y"

IF smaller$ = "x" THEN fraction = x.along% / y.along%
IF smaller$ = "y" THEN fraction = y.along% / x.along%

IF start.x% < end.x% AND start.y% < end.y% THEN dir = 2
IF start.y% = end.y% AND start.x% < end.x% THEN dir = 3
IF start.x% < end.x% AND start.y% > end.y% THEN dir = 4
IF start.x% > end.x% AND start.y% > end.y% THEN dir = 6
IF start.y% = end.y% AND start.x% > end.x% THEN dir = 7
IF start.x% > end.x% AND start.y% < end.y% THEN dir = 8
IF start.x% = end.x% AND start.y% < end.y% THEN dir = 1
IF start.x% = end.x% AND start.y% > end.y% THEN dir = 2

move.pos.x = start.x%
move.pos.y = start.y%
end.pos.y = end.y%
end.pos.x = end.x%

MOVE$ = "go"
points% = 1
col% = 14
RETURN

xsmall:
IF dir = 4 THEN
move.pos.x = move.pos.x + fraction
move.pos.y = move.pos.y - 1
END IF

IF dir = 2 THEN
move.pos.x = move.pos.x + fraction
move.pos.y = move.pos.y + 1
END IF

IF dir = 6 THEN
move.pos.x = move.pos.x - fraction
move.pos.y = move.pos.y - 1
END IF

IF dir = 8 THEN
move.pos.x = move.pos.x - fraction
move.pos.y = move.pos.y + 1
END IF

IF dir = 7 THEN move.pos.x = move.pos.x - 1
IF dir = 1 THEN move.pos.y = move.pos.y + 1
IF dir = 5 THEN move.pos.y = move.pos.y - 1
IF dir = 3 THEN move.pos.x = move.pos.x + 1
col% = 14
RETURN


ysmall:
IF dir = 4 THEN
move.pos.x = move.pos.x + 1
move.pos.y = move.pos.y - fraction
END IF

IF dir = 2 THEN
move.pos.x = move.pos.x + 1
move.pos.y = move.pos.y + fraction
END IF

IF dir = 6 THEN
move.pos.x = move.pos.x - 1
move.pos.y = move.pos.y - fraction
END IF

IF dir = 8 THEN
move.pos.x = move.pos.x - 1
move.pos.y = move.pos.y + fraction
END IF

IF dir = 7 THEN move.pos.x = move.pos.x - 1
IF dir = 1 THEN move.pos.y = move.pos.y + 1
IF dir = 5 THEN move.pos.y = move.pos.y - 1
IF dir = 3 THEN move.pos.x = move.pos.x + 1
col% = 14
RETURN

animate:
SELECT CASE MOVE$
CASE IS = "", "stop"
PUT (move.pos.x - 9, move.pos.y - 16), dir5.1, XOR
CASE IS = "go"
IF smaller$ = "y" THEN frame.counter% = frame.counter% + 1
IF smaller$ = "y" AND frame.counter% > 15 THEN frame.counter% = 1

IF smaller$ = "x" THEN frame.counter% = frame.counter% + 1
IF smaller$ = "x" AND frame.counter% > 10 THEN frame.counter% = 1

IF dir = 1 AND frame.counter% > 0 AND frame.counter% <= 5 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir1.2, XOR
IF dir = 1 AND frame.counter% > 5 AND frame.counter% <= 10 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir1.1, XOR

IF dir = 2 AND smaller$ = "x" AND frame.counter% > 0 AND frame.counter% <= 5 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir5.2, XOR
IF dir = 2 AND smaller$ = "x" AND frame.counter% > 5 AND frame.counter% <= 10 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir5.3, XOR

IF dir = 2 AND smaller$ = "y" AND frame.counter% > 0 AND frame.counter% <= 5 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir3.2, XOR
IF dir = 2 AND smaller$ = "y" AND frame.counter% > 5 AND frame.counter% <= 10 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir3.1, XOR
IF dir = 2 AND smaller$ = "y" AND frame.counter% > 10 AND frame.counter% <= 15 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir3.3, XOR

IF dir = 3 AND smaller$ = "x" AND frame.counter% > 0 AND frame.counter% <= 5 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir1.2, XOR
IF dir = 3 AND smaller$ = "x" AND frame.counter% > 5 AND frame.counter% <= 10 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir1.3, XOR

IF dir = 3 AND smaller$ = "y" AND frame.counter% > 0 AND frame.counter% <= 5 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir7.2, XOR
IF dir = 3 AND smaller$ = "y" AND frame.counter% > 5 AND frame.counter% <= 10 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir7.1, XOR
IF dir = 3 AND smaller$ = "y" AND frame.counter% > 10 AND frame.counter% <= 15 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir7.3, XOR

IF dir = 5 AND frame.counter% > 0 AND frame.counter% <= 5 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir1.2, XOR
IF dir = 5 AND frame.counter% > 5 AND frame.counter% <= 10 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir1.1, XOR
IF dir = 5 AND frame.counter% > 10 AND frame.counter% <= 15 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir1.3, XOR

IF dir = 4 AND smaller$ = "x" AND frame.counter% > 0 AND frame.counter% <= 5 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir1.2, XOR
IF dir = 4 AND smaller$ = "x" AND frame.counter% > 5 AND frame.counter% <= 10 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir1.3, XOR

IF dir = 4 AND smaller$ = "y" AND frame.counter% > 0 AND frame.counter% <= 5 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir3.2, XOR
IF dir = 4 AND smaller$ = "y" AND frame.counter% > 5 AND frame.counter% <= 10 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir3.1, XOR
IF dir = 4 AND smaller$ = "y" AND frame.counter% > 10 AND frame.counter% <= 15 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir3.3, XOR

IF dir = 6 AND smaller$ = "x" AND frame.counter% > 0 AND frame.counter% <= 5 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir1.2, XOR
IF dir = 6 AND smaller$ = "x" AND frame.counter% > 5 AND frame.counter% <= 10 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir1.3, XOR

IF dir = 6 AND smaller$ = "y" AND frame.counter% > 0 AND frame.counter% <= 5 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir7.2, XOR
IF dir = 6 AND smaller$ = "y" AND frame.counter% > 5 AND frame.counter% <= 10 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir7.1, XOR
IF dir = 6 AND smaller$ = "y" AND frame.counter% > 10 AND frame.counter% <= 15 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir7.3, XOR

IF dir = 7 AND frame.counter% > 0 AND frame.counter% <= 5 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir7.2, XOR
IF dir = 7 AND frame.counter% > 5 AND frame.counter% <= 10 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir7.1, XOR
IF dir = 7 AND frame.counter% > 10 AND frame.counter% <= 15 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir7.3, XOR

IF dir = 8 AND smaller$ = "x" AND frame.counter% > 0 AND frame.counter% <= 5 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir5.2, XOR
IF dir = 8 AND smaller$ = "x" AND frame.counter% > 5 AND frame.counter% <= 10 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir5.3, XOR

IF dir = 8 AND smaller$ = "y" AND frame.counter% > 0 AND frame.counter% <= 5 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir7.2, XOR
IF dir = 8 AND smaller$ = "y" AND frame.counter% > 5 AND frame.counter% <= 10 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir7.1, XOR
IF dir = 8 AND smaller$ = "y" AND frame.counter% > 10 AND frame.counter% <= 15 THEN PUT (move.pos.x - 9, move.pos.y - 16), dir7.3, XOR

END SELECT
CIRCLE (move.pos.x, move.pos.y + 15), 18, 4, 2, 1, .3
RETURN

SUB boundry.coord (curx%, curY%)

'-----------------MOUSE POINTER------------------------
curx% = mousex%
IF mousex% > 630 THEN curx% = 630
'-----------------
curY% = MouseY%
IF curY% > 334 THEN curY% = 334
'---------------------------------------------------------

END SUB

SUB check.point (dir, move.pos.y, move.pos.x, MOVE$)
END SUB

SUB intro
SCREEN 9, 0, 1, 1
LOCATE 1, 36: PRINT "Move!!"
LOCATE 3, 32: PRINT "by The Rizzler"
LOCATE 7, 17: PRINT "Click where you want the shaggy monster to go!"
LOCATE 9, 30: PRINT "Press Esc to exit"
DO
u = u + 1
IF u = 16 THEN u = 1
COLOR u, 0: LOCATE 20, 22: PRINT "     Press any key to continue"
FOR p = 1 TO 25000
NEXT p
LOOP UNTIL INKEY$ <> ""
 
END SUB

SUB load.cells



END SUB

FUNCTION mousebuttons%
  mousebuttons% = a%(2)
END FUNCTION

SUB MouseHide
  DEF SEG = VARSEG(a%(0))
  CALL ABSOLUTE(VARPTR(a%(0)) + &H3A)
  DEF SEG
END SUB

SUB MouseInit
  DEF SEG = VARSEG(a%(0))
  CALL ABSOLUTE(VARPTR(a%(0)) + 6)
  DEF SEG
END SUB

SUB MouseReset
  DEF SEG = VARSEG(a%(0))
  CALL ABSOLUTE(VARPTR(a%(1)) + &H1E)
  DEF SEG
END SUB

SUB MouseShow
  DEF SEG = VARSEG(a%(0))
  CALL ABSOLUTE(VARPTR(a%(0)) + &H34)
  DEF SEG
END SUB

FUNCTION mousex%
  mousex% = a%(0)
END FUNCTION

FUNCTION MouseY%
  MouseY% = a%(1)
END FUNCTION


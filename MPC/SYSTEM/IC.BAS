DEFINT A-Z: DEFLNG L
DECLARE SUB KeyHit (f$)
DECLARE SUB OpenFiles ()
'$INCLUDE: '..\Include\Library.DEC'
'$INCLUDE: '..\Include\Material.REC'

	DIM SHARED Mat AS Material
	DIM SHARED Mnx AS MatIndex
	
	ON ERROR GOTO Errors
	KEY OFF
	KEY 1, "1"
	KEY 2, "2"
	KEY 3, "3"
	KEY 4, "4"
	KEY 5, "5"
	KEY 6, "6"
	KEY 7, "7"
	KEY 8, "8"
	KEY 9, "9"
	KEY 10, "0"

   'screen initialize
	WIDTH 80, 50: SCREEN 0, 0, 1, 1
	PCOPY 0, 1:   SCREEN 0, 0, 0, 0
	CALL MhOpenScreenLib("MPC.SLB" + CHR$(0), e)
	CALL MhShadowOn(0, 8)
	CALL MhBlinkOff


	OpenFiles
	FOR x = 1 TO LOF(2) \ LEN(Mnx)
	GET #2, x, Mnx
	IF Mnx.Typ = "þþþ" THEN Top = x - 1: EXIT FOR
	NEXT

	CALL MhDisplayByName(0, 0, "IN-Count", e)
	COLOR 16, 3
	n = 0: FOR r = 5 TO 24: n = n + 1: GOSUB MatLin: NEXT
	n = 1: r = 5: LOCATE r
950 COLOR 15, 1: GOSUB MatLin
	Picture r, 36, 11, Qty$
951 EntryN r, 36, 11, 2, e, b, Qty$: IF e GOTO Terminate
	IF b = 0 THEN
		EntryN r, 52, 7, 3, e, b, Mat.Ref: IF e GOTO Terminate ELSE IF b THEN 951
	END IF
	Mat.Mbq = VAL(Qty$) * 100
	Mat.Mbp = VAL(Qty$) * VAL(Mat.Ref) * 100
	Mat.Ybq = VAL(Qty$) * 100
	Mat.Ybp = VAL(Qty$) * VAL(Mat.Ref) * 100
	PUT #1, Mnx.Num, Mat
	COLOR 16, 3: GOSUB MatLin
	IF b THEN
	   n = n - 1: IF n < 1 THEN n = 1: GOTO 950
	   r = r - 1: IF r < 5 THEN CALL BkScroll(2, 5, 70, 24, 1): r = 5
	ELSE
	   n = n + 1: IF n > Top THEN n = Top: GOTO 950
	   r = r + 1: IF r > 24 THEN CALL scroll(2, 5, 70, 24, 1): r = 24
	END IF
	GOTO 950
MatLin:
	GET #2, n, Mnx
	GET #1, Mnx.Num, Mat
	LOCATE r, 2
	PRINT USING "&³&³########.## &³ &³########.## "; Mat.Typ + Mat.Num; Mat.Nam; Mat.Ybq / 100; Mat.Con; Mat.Ref; Mat.Ybp / 100
	RETURN
Errors: '====================================================================
	WinMssg "Error:" + STR$(ERR) + "  Line:" + STR$(ERL)
	f$ = INPUT$(1)
Terminate:
	CLOSE

DEFSNG A-Z
SUB KeyHit (f$)
	DO: f$ = INKEY$: LOOP WHILE f$ > ""
	DO: f$ = INKEY$: LOOP WHILE f$ = ""
	f$ = UCASE$(f$)
END SUB

SUB OpenFiles ()
	OPEN "..\Data\Material.MAS" FOR RANDOM AS #1 LEN = LEN(Mat)
	OPEN "..\Data\Material.INX" FOR RANDOM AS #2 LEN = LEN(Mnx)
END SUB

SUB WinMssg (f$)
	SCREEN , , 1, 1
	PCOPY 0, 1
	COLOR 14, 4: LOCATE 12
	LOCATE , 36: PRINT " ÉÍ"; STRING$(LEN(f$), "Í"); "Í» "
	LOCATE , 36: PRINT " º "; f$; " º "
	LOCATE , 36: PRINT " ÈÍ"; STRING$(LEN(f$), "Í"); "Í¼ "
	COLOR 12, 0: LOCATE 13
	LOCATE , 35: PRINT "Û"
	LOCATE , 35: PRINT "Û"
	LOCATE , 35: PRINT "ßßß"; STRING$(LEN(f$) + 2, "ß")
	SOUND 770, .1: SOUND 970, .2: KeyHit c$
	SCREEN , , 0, 0
END SUB


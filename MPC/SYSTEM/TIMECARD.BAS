DEFINT A-Z
DECLARE FUNCTION Confirm% (f$)
DECLARE SUB StatLin (f$)
DECLARE SUB KeyPres (f$)
DECLARE SUB Picture (r%, c%, s%, f$)
DECLARE SUB EntryN (r%, c%, s%, d%, e%, b%, f$)
DECLARE SUB qPrint (s$, BYVAL r, BYVAL c, BYVAL p, BYVAL f)
    
    DIM Clk(1, 6, 3) AS STRING * 4 'hours in and out
    DIM Hrs(1, 9)    AS STRING * 6 'total hours
    DIM Tot(2)       AS STRING * 6 'tot, reg, ovt
    
    DIM SHARED Total(2)       AS INTEGER
    DIM SHARED Hours(1, 2)    AS INTEGER
    DIM SHARED Decim(1, 6, 3) AS INTEGER

    IF COMMAND$ = "W" THEN Payroll$ = "W" ELSE Payroll$ = "B"
    ON ERROR GOTO Errors
    CLS : WIDTH 80, 50

    CALL MhOpenScreenLib("TimeCard.SLB" + CHR$(0), e)
    CALL MhBlinkOff
    CALL MhShadowOn(0, 8)
    
200 IF Payroll$ = "W" THEN
       t = 0: CALL MhDisplayByName(0, 0, "PR-Card1W", e)
    ELSE
       t = 1: CALL MhDisplayByName(0, 0, "PR-Card2W", e)
    END IF
    
    w = 0
    d = 0
    h = 0
202 r = 21 + 2 * d
    c = 11 + 5 * h + 34 * w
    EntryN r, c, 4, 0, e, b, Clk(w, d, h): IF e THEN END
    IF b THEN
       h = h - 1: IF h < 0 THEN h = 3: d = d - 1: IF d < 0 THEN d = 6: w = w - 1: IF w < 0 THEN w = t
       GOTO 202
    END IF

    IF MID$(Clk(w, d, h), 1, 2) > "12" THEN BEEP: GOTO 202
    IF MID$(Clk(w, d, h), 3, 2) > "59" THEN BEEP: GOTO 202
    Decim(w, d, h) = 100 * VAL(MID$(Clk(w, d, h), 1, 2)) + VAL(MID$(Clk(w, d, h), 3, 2)) / .6

    SELECT CASE h
    CASE 1
         IF Clk(w, d, 1) > "   1" AND Clk(w, d, 0) < "   1" THEN BEEP: GOTO 202
    CASE 2
         IF Clk(w, d, 2) > "   1" AND Clk(w, d, 1) < "   1" THEN BEEP: GOTO 202
    CASE 3
         IF Clk(w, d, 3) > "   1" AND Clk(w, d, 0) < "   1" THEN BEEP: GOTO 202
         IF Clk(w, d, 3) < "   1" AND Clk(w, d, 2) > "   1" THEN BEEP: GOTO 202
         Hours(w, 0) = Hours(w, 0) - 100 * VAL(Hrs(w, d))
         IF Decim(w, d, 1) < Decim(w, d, 0) THEN Bef = 1200 + Decim(w, d, 1) - Decim(w, d, 0) ELSE Bef = Decim(w, d, 1) - Decim(w, d, 0)
         IF Decim(w, d, 3) < Decim(w, d, 2) THEN Aft = 1200 + Decim(w, d, 3) - Decim(w, d, 2) ELSE Aft = Decim(w, d, 3) - Decim(w, d, 2)
         COLOR 16, 3: LOCATE r, c + 5, 0: PRINT USING "###.##"; (Bef + Aft) / 100;
         Picture r, c + 5, 6, Hrs(w, d)
         Hours(w, 0) = Hours(w, 0) + 100 * VAL(Hrs(w, d))
         IF Hours(w, 0) > 4000 THEN
            Hours(w, 1) = 4000
            Hours(w, 2) = Hours(w, 0) - 4000
         ELSE
            Hours(w, 1) = Hours(w, 0)
            Hours(w, 2) = 0
         END IF
         Total(0) = Hours(0, 0) + Hours(1, 0)
         Total(1) = Hours(0, 1) + Hours(1, 1)
         Total(2) = Hours(0, 2) + Hours(1, 2)
         COLOR 4, 7
         LOCATE 35, c + 5: PRINT USING "###.##"; Hours(w, 0) / 100;
         LOCATE 37, c + 5: PRINT USING "###.##"; Hours(w, 1) / 100;
         LOCATE 39, c + 5: PRINT USING "###.##"; Hours(w, 2) / 100;
         Picture 35, c + 5, 6, Hrs(w, 7)
         Picture 37, c + 5, 6, Hrs(w, 8)
         Picture 39, c + 5, 6, Hrs(w, 9)
         IF t THEN
            LOCATE 9, 65:  PRINT USING "###.##"; Total(0) / 100;
            LOCATE 10, 65: PRINT USING "###.##"; Total(1) / 100;
            LOCATE 11, 65: PRINT USING "###.##"; Total(2) / 100;
         END IF
    END SELECT
    h = h + 1
    IF h > 3 THEN
       h = 0
       d = d + 1
       IF d > 6 THEN
          d = 0
          w = w + 1
          IF w > t THEN
             IF Confirm("NEXT TimeCard?") THEN
                ERASE Clk, Hrs, Tot, Total, Hours, Decim
                GOTO 200
             ELSE
                w = t
                d = 6
                h = 3
             END IF
          END IF
       END IF
    END IF
    GOTO 202

Errors:

FUNCTION Confirm (f$)
    r = 11
    PCOPY 0, 3
    COLOR 17, 6: LOCATE r + 1, 40: PRINT STRING$(15 + LEN(f$), " ");
    COLOR 17, 6: LOCATE r + 2, 40: PRINT "  Press * to "; f$; "  ";
    COLOR 8, 0: Picture r + 2, POS(0), 1, c$: PRINT c$;
    COLOR 17, 6: LOCATE r + 3, 40: PRINT STRING$(15 + LEN(f$), " ");
    COLOR 8, 0: Picture r + 3, POS(0), 1, c$: PRINT c$;
    COLOR 8, 0: Picture r + 4, 41, 15 + LEN(f$), c$: LOCATE r + 4, 41: PRINT c$;
    SOUND 350, .5: KeyPres f$
    IF f$ = "*" THEN Confirm = 1: SOUND 1200, .3: SOUND 1000, .2
    PCOPY 3, 0
END FUNCTION

SUB KeyPres (f$)
    DO: LOOP WHILE INKEY$ > ""
    DO: f$ = INKEY$: LOOP WHILE f$ = ""
    f$ = UCASE$(f$)
END SUB


DEFINT A-Z

'$INCLUDE: '..\Include\Config.dec'

OPEN "..\DataFile\Activity.Mas" FOR RANDOM AS #1 LEN = LEN(Act)
w$ = "#######"
DIM Bor(5, 5) AS STRING * 7
WIDTH 80, 25
CLS : COLOR 15, 7
     LOCATE 2, 1
     PRINT "+------------+---------+----------+----------+----------+----------+----------+"
     PRINT "|BOR         |    0    |    DAY   |   WEEK   |  MONTH   |   YEAR   |   TOTAL  |"
     PRINT "+------------+---------+----------+----------+----------+----------+----------+"
     PRINT "|OUTS        |         |          |          |          |          |          |"
     PRINT "|INS         |         |          |          |          |          |          |"
     PRINT "|PAID-OUT    |         |          |          |          |          |          |"
     PRINT "|CHARG-OFF   |         |          |          |          |          |          |"
     PRINT "+------------+---------+----------+----------+----------+----------+----------+"
     PRINT "|CHANGE      |         |          |          |          |          |          |"
     PRINT "|BEGINNING   |         |          |          |          |          |          |"
     PRINT "|ENDING BOR  |         |          |          |          |          |          |"
     PRINT "+------------+---------+----------+----------+----------+----------+----------+"

    CALL MhDisplayByName(0, 0, "Activity0", e)
    GET #1, 1, Act
    FOR x = 0 TO 5
    LOCATE 5, 15 + x * 11:  PRINT USING w$; Act.Bor(1, x); : Picture 5, 15 + x * 11, 7, Bor(1, x)
    LOCATE 6, 15 + x * 11:  PRINT USING w$; Act.Bor(0, x); : Picture 6, 15 + x * 11, 7, Bor(0, x)
    LOCATE 7, 15 + x * 11:  PRINT USING w$; Act.Bor(2, x); : Picture 7, 15 + x * 11, 7, Bor(2, x)
    LOCATE 8, 15 + x * 11:  PRINT USING w$; Act.Bor(3, x); : Picture 8, 15 + x * 11, 7, Bor(3, x)
    LOCATE 10, 15 + x * 11: PRINT USING w$; Act.Bor(1, x) - Act.Bor(0, x) - Act.Bor(2, x) - Act.Bor(3, x);
    LOCATE 11, 15 + x * 11: PRINT USING w$; Act.Bor(4, x); : Picture 11, 15 + x * 11, 7, Bor(4, x)
    LOCATE 12, 15 + x * 11: PRINT USING w$; Act.Bor(4, x) + Act.Bor(1, x) - Act.Bor(0, x) - Act.Bor(2, x) - Act.Bor(3, x);
    NEXT

20  FOR x = 1 TO 5
21  EntryN 5, 15 + x * 11, 7, 0, e, b, Bor(1, x): IF e THEN END ELSE IF b THEN x = x - 1: IF x < 1 THEN 20 ELSE 25
22  EntryN 6, 15 + x * 11, 7, 0, e, b, Bor(0, x): IF e THEN END ELSE IF b THEN 21
23  EntryN 7, 15 + x * 11, 7, 0, e, b, Bor(2, x): IF e THEN END ELSE IF b THEN 22
24  EntryN 8, 15 + x * 11, 7, 0, e, b, Bor(3, x): IF e THEN END ELSE IF b THEN 23
    LOCATE 10, 15 + x * 11: PRINT USING w$; VAL(Bor(1, x)) - VAL(Bor(0, x)) - VAL(Bor(2, x)) - VAL(Bor(3, x));
25  EntryN 11, 15 + x * 11, 7, 0, e, b, Bor(4, x): IF e THEN END ELSE IF b THEN 24
    LOCATE 12, 15 + x * 11: PRINT USING w$; VAL(Bor(4, x)) + VAL(Bor(1, x)) - VAL(Bor(0, x)) - VAL(Bor(2, x)) - VAL(Bor(3, x));
28  NEXT
    IF Confirm(1, "save BOR") THEN
       FOR x = 0 TO 5
           Act.Bor(1, x) = VAL(Bor(1, x))
           Act.Bor(0, x) = VAL(Bor(0, x))
           Act.Bor(2, x) = VAL(Bor(2, x))
           Act.Bor(3, x) = VAL(Bor(3, x))
           Act.Bor(4, x) = VAL(Bor(4, x))
       NEXT
       PUT #1, 1, Act
    ELSE
        x = 5: GOTO 25
    END IF
CLOSE

FUNCTION Confirm (g, f$)
    r = 13
    PCOPY 0, 1
    COLOR 17, 6: LOCATE r + 1, 40: PRINT STRING$(15 + LEN(f$), " ");
    COLOR 17, 6: LOCATE r + 2, 40: PRINT "  Press * to "; f$; "  ";
    COLOR 8, 0: Picture r + 2, POS(0), 1, c$: PRINT c$;
    COLOR 17, 6: LOCATE r + 3, 40: PRINT STRING$(15 + LEN(f$), " ");
    COLOR 8, 0: Picture r + 3, POS(0), 1, c$: PRINT c$;
    COLOR 8, 0: Picture r + 4, 41, 15 + LEN(f$), c$: LOCATE r + 4, 41: PRINT c$;
    SOUND 350, .5: KeyPres f$
    IF f$ = "*" THEN Confirm = 1: SOUND 1200, .3: SOUND 1000, .2
    PCOPY 1, 0
END FUNCTION

DEFSNG A-Z
SUB KeyPres (f$)
    DO: f$ = INKEY$: LOOP WHILE f$ = ""
    f$ = UCASE$(f$)
END SUB


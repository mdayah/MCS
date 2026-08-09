DEFINT A-Z
DECLARE SUB KeyPres (f$)
DECLARE SUB Picture (r, c, s, f$)
DECLARE SUB WinMssg (f$)
DECLARE FUNCTION Confirm% (g%, f$)

DECLARE SUB scroll (r%, c%, t%, b%, l%)
DECLARE SUB BkScroll (r%, c%, t%, b%, l%)
DECLARE SUB PrtSc (BYVAL e%)
DECLARE SUB qPrint (s$, BYVAL r%, BYVAL c%, BYVAL p%, BYVAL f%)

TYPE PayIn
     SSn AS STRING * 11
     Nam AS STRING * 32
     Dat AS STRING * 8
     Amt AS STRING * 11
     Crl AS STRING * 2
END TYPE
TYPE NatIn
     SSn AS STRING * 11
     Nam AS STRING * 32
     Dat AS STRING * 8
     Num AS STRING * 7
     Amt AS STRING * 11
     Crl AS STRING * 2
END TYPE
TYPE PayOut
     Rec AS STRING * 2
     Bla AS STRING * 3
     Id1 AS STRING * 15
     Id2 AS STRING * 15
     Id3 AS STRING * 9
     Uc1 AS STRING * 6
     Uc2 AS STRING * 6
     Uc3 AS STRING * 6
     Amt AS STRING * 11
     Dat AS STRING * 8
     Fil AS STRING * 19
     Crl AS STRING * 2
END TYPE

DIM Pi AS PayIn
DIM Ni AS NatIn
DIM Po AS PayOut

    SCREEN 0, 0, 0, 0: WIDTH 80, 50
    SCREEN 0, 0, 1, 1: PCOPY 0, 1
    SCREEN 0, 0, 0, 0

    CALL MhOpenScreenLib("CNB.SLB" + CHR$(0), e)
    CALL MhBlinkOff
    CALL MhShadowOn(0, 8)

    y& = SETMEM(-16384)   ' SETMEM in MS-DOS session under
                          ' Windows
    ON ERROR GOTO Errors
    CALL MhDisplayByName(0, 0, "Logo", e)
    WinMssg "Press key to start"
'============================================================================
    OPEN "A:\Payroll.TXT" FOR RANDOM AS #1 LEN = LEN(Pi)
    Top = LOF(1) \ LEN(Pi)
    IF Top = 0 THEN
       WinMssg "PAYROLL.TXT file is empty"
    ELSE
       CLOSE #2: OPEN "A:\Payroll.out" FOR OUTPUT AS #2
       CLOSE #2: OPEN "A:\Payroll.out" FOR RANDOM AS #2 LEN = LEN(Po)
       CALL MhDisplayByName(0, 0, "List", e)
       qPrint "PAYROLL AMOUNT", 8, 7, 0, 1
       FOR x = 1 TO Top
           GET #1, x, Pi
           Po.Rec = "AL"
           Po.Bla = SPACE$(3)
           Po.Id1 = MID$(Pi.SSn, 1, 3) + MID$(Pi.SSn, 5, 2) + MID$(Pi.SSn, 8, 4)
           Po.Id2 = SPACE$(15)
           Po.Id3 = SPACE$(9)
           Po.Uc1 = SPACE$(6)
           Po.Uc2 = SPACE$(6)
           Po.Uc3 = SPACE$(6)
           Po.Amt = Pi.Amt
           Po.Dat = Pi.Dat
           Po.Fil = SPACE$(19)
           Po.Crl = CHR$(13) + CHR$(10)
           PUT #2, x, Po
           n% = n% + 1
           t# = t# + VAL(Po.Amt)
           CALL scroll(7, 10, 74, 40, 1)
           qPrint Pi.SSn + "ณ", 40, 7, 0, 1
           qPrint Pi.Nam + "ณ", 40, 19, 0, 1
           qPrint MID$(Pi.Dat, 5, 2) + "-" + MID$(Pi.Dat, 7, 2) + "-" + MID$(Pi.Dat, 1, 4) + "ณ", 40, 52, 0, 1
           qPrint Pi.Amt, 40, 63, 0, 1
           COLOR 14, 1: LOCATE 44, 7: PRINT USING "Record Number: #####"; n%;
       NEXT
       COLOR 17, 11: LOCATE 42, 63: PRINT USING "#######,.##"; t#;
    END IF
    CLOSE
    WinMssg "Press key for next file"
'============================================================================
    n% = 0
    t# = 0
    OPEN "A:\1stNat.TXT" FOR RANDOM AS #1 LEN = LEN(Ni)
    Top = LOF(1) \ LEN(Ni)
    IF Top = 0 THEN
       WinMssg "1stNat.TXT file is empty"
    ELSE
       CLOSE #2: OPEN "A:\1stNat.out" FOR OUTPUT AS #2
       CLOSE #2: OPEN "A:\1stNat.out" FOR RANDOM AS #2 LEN = LEN(Po)
       CALL MhDisplayByName(0, 0, "List", e)
       qPrint "FIRST NATIONAL AMOUNT", 8, 7, 0, 1
       FOR x = 1 TO Top
           GET #1, x, Ni
           Po.Rec = "AL"
           Po.Bla = SPACE$(3)
           Po.Id1 = MID$(Ni.SSn, 1, 3) + MID$(Ni.SSn, 5, 2) + MID$(Ni.SSn, 8, 4)
           Po.Id2 = SPACE$(15)
           Po.Id3 = SPACE$(9)
           Po.Uc1 = SPACE$(6)
           Po.Uc2 = SPACE$(6)
           Po.Uc3 = SPACE$(6)
           Po.Amt = Ni.Amt
           Po.Dat = Ni.Dat
           Po.Fil = SPACE$(19)
           Po.Crl = CHR$(13) + CHR$(10)
           PUT #2, x, Po
           n% = n% + 1
           t# = t# + VAL(Po.Amt)
           CALL scroll(7, 10, 74, 40, 1)
           qPrint Ni.SSn + "ณ", 40, 7, 0, 1
           qPrint Ni.Nam + "ณ", 40, 19, 0, 1
           qPrint MID$(Ni.Dat, 5, 2) + "-" + MID$(Ni.Dat, 7, 2) + "-" + MID$(Ni.Dat, 1, 4) + "ณ", 40, 52, 0, 1
           qPrint Ni.Amt, 40, 63, 0, 1
           COLOR 14, 1: LOCATE 44, 7: PRINT USING "Record Number: #####"; n%;
       NEXT
       COLOR 17, 11: LOCATE 42, 63: PRINT USING "#######,.##"; t#;
    END IF
    CLOSE
    WinMssg "Press key for next file"
'============================================================================
    n% = 0
    t# = 0
    OPEN "A:\League.TXT" FOR RANDOM AS #1 LEN = LEN(Ni)
    Top = LOF(1) \ LEN(Ni)
    IF Top = 0 THEN
       WinMssg "LEAGUE.TXT file is empty"
    ELSE
       CLOSE #2: OPEN "A:\League.out" FOR OUTPUT AS #2
       CLOSE #2: OPEN "A:\League.out" FOR RANDOM AS #2 LEN = LEN(Po)
       CALL MhDisplayByName(0, 0, "List", e)
       qPrint "LEAGUE CREDIT UNION AMOUNT", 8, 7, 0, 1
       FOR x = 1 TO Top
           GET #1, x, Ni
           Po.Rec = "AL"
           Po.Bla = SPACE$(3)
           Po.Id1 = MID$(Ni.SSn, 1, 3) + MID$(Ni.SSn, 5, 2) + MID$(Ni.SSn, 8, 4)
           Po.Id2 = SPACE$(15)
           Po.Id3 = SPACE$(9)
           Po.Uc1 = SPACE$(6)
           Po.Uc2 = SPACE$(6)
           Po.Uc3 = SPACE$(6)
           Po.Amt = Ni.Amt
           Po.Dat = Ni.Dat
           Po.Fil = SPACE$(19)
           Po.Crl = CHR$(13) + CHR$(10)
           PUT #2, x, Po
           n% = n% + 1
           t# = t# + VAL(Po.Amt)
           CALL scroll(7, 10, 74, 40, 1)
           qPrint Ni.SSn + "ณ", 40, 7, 0, 1
           qPrint Ni.Nam + "ณ", 40, 19, 0, 1
           qPrint MID$(Ni.Dat, 5, 2) + "-" + MID$(Ni.Dat, 7, 2) + "-" + MID$(Ni.Dat, 1, 4) + "ณ", 40, 52, 0, 1
           qPrint Ni.Amt, 40, 63, 0, 1
           COLOR 14, 1: LOCATE 44, 7: PRINT USING "Record Number: #####"; n%;
       NEXT
       COLOR 17, 11: LOCATE 42, 63: PRINT USING "#######,.##"; t#;
    END IF
    CLOSE
    WinMssg "Finished"
    END
Errors: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    PCOPY 0, 3: CALL MhDisplayByName(0, 0, "Error", e)
    SELECT CASE ERR
           CASE ELSE: Er$ = ERROR$(ERR)
    END SELECT
    qPrint STR$(ERR) + STR$(ERDEV AND &HFF) + STR$((ERDEV AND &HFF00) \ 256), 32, 38, 0, 1
    qPrint Er$, 33, 38, 0, 1
    qPrint ERDEV$, 35, 38, 0, 1
    SOUND 50, 3: KeyPres f$
    IF f$ = CHR$(27) THEN CLOSE : END
    PCOPY 3, 0
    RESUME

FUNCTION Confirm (g, f$)
    r = 30
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

DEFINT A-Z
SUB WinMssg (f$)
    r = 16
    PCOPY 0, 3
    COLOR 17, 7
    LOCATE r + 1, 31: PRINT STRING$(4 + LEN(f$), " ");
    LOCATE r + 2, 31: PRINT "  "; f$; "  ";
    LOCATE r + 3, 31: PRINT STRING$(4 + LEN(f$), " ");
    COLOR 8, 0
    Picture r + 2, 30, 1, b$: LOCATE r + 2, 30: PRINT b$;
    Picture r + 3, 30, 1, b$: LOCATE r + 3, 30: PRINT b$;
    Picture r + 4, 30, 4 + LEN(f$), b$: LOCATE r + 4, 30: PRINT b$;
    SOUND 350, 3: KeyPres f$
    PCOPY 3, 0
END SUB


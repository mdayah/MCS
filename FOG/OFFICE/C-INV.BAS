DECLARE FUNCTION FindCod% (f$)

REM $INCLUDE: '..\Include\Inventor.OFF'
REM $INCLUDE: '..\Include\Codes.Rec'
REM $INCLUDE: '..\Include\Stores.Rec'
TYPE OldInv
    Sto AS STRING * 2
    Typ AS STRING * 1
    Cod AS STRING * 4
    Bra AS STRING * 3
    Des AS STRING * 24
    Sta AS STRING * 1
    Ctr AS STRING * 2
    Mdl AS STRING * 15
    Ser AS STRING * 15
    Pdt AS STRING * 10
    Prc AS STRING * 7
    Dlt AS STRING * 10
    Tim AS STRING * 2
    Tfr AS STRING * 2
    Cus AS STRING * 6
    Dis AS STRING * 10
    Del AS STRING * 1
    Tto AS STRING * 2
    Num AS INTEGER
    Fil AS STRING * 6
    Mdr AS LONG: Ydr AS LONG: Tdr AS LONG 'D-Rented
    Mds AS LONG: Yds AS LONG: Tds AS LONG 'D-Repairs
    Min AS LONG: Yin AS LONG: Tin AS LONG '$-Revenues
    Mde AS LONG: Yde AS LONG: Tde AS LONG '$-Depreciation
    Mre AS LONG: Yre AS LONG: Tre AS LONG '$-Repairs
END TYPE


    DIM Old AS OldInv
    DIM Inv AS Inventory
    DIM Sto AS Stores
    DIM SHARED Cod AS Codes

    '*0. Idle           0*  0. Idle
    '*1. On Rent        2*  1. In-Service
    '*2. In Service     1*  2. On-Rent
    '*3. On Rent (S)    5*  3. On-Rent/AS
    '*4. Loaner         3*  4. On-Rent/IS
    '*5. On Rent (AS)       5. On-Rent/AR
    '*6. Cash Sale          6. On-Rent/IC
    '*7. Charge off         7. On-Rent/L
    '*8. Paid Out       4*  8. Loaner
    '9. E R A S E D     6*  9. Cash-Sale
    '                   8* 10. Paid-Out
    '                   7* 11. Charge-Off
    '                      12. Charge-Off/L
    '                      13. Charge-Off/S
    '                      14. Charge-Off/G
    '                      15. Transferred
    CLS
    ON ERROR GOTO Errors
    KILL "..\OfData\Inventor.MAS"

    OPEN "..\ofData\Inventor.OLD" FOR RANDOM AS #1 LEN = LEN(Old)
    OPEN "..\OfData\Inventor.MAS" FOR ISAM Inventory "Inventory" AS #2
    OPEN "..\Master\Codes   .Mas" FOR RANDOM AS #3 LEN = LEN(Cod)
    OPEN "..\Master\Stores  .Mas" FOR RANDOM AS #4 LEN = LEN(Sto)
    CREATEINDEX 2, "Sto", 1, "Sto", "Typ", "Grp", "Cod", "Mdl", "Ser"
    CREATEINDEX 2, "Agr", 0, "Sto", "Agr"
    CREATEINDEX 2, "Ser", 1, "Ser"

    SETINDEX 2, "Sto"

    LOCATE 7, 33: PRINT LOF(1) \ LEN(Old);
    FOR x = 1 TO LOF(1) \ LEN(Old)
    IF INKEY$ = CHR$(27) THEN CLOSE : END
    GET #1, x, Old
         IF FindCod(Old.Cod) = 0 THEN STOP
         IF Old.Sto > " 0" THEN GET #4, VAL(Old.Sto), Sto
         SELECT CASE VAL(Old.Sta)
                CASE 0: ns = 0
                CASE 1: ns = 2
                CASE 2: ns = 1
                CASE 3: ns = 4
                CASE 4: ns = 8
                CASE 5: ns = 3
                CASE 6: ns = 9
                CASE 7: ns = 11
                CASE 8: ns = 10
                CASE 9: STOP
         END SELECT
         Inv.Sto = Sto.Num
         Inv.Typ = Cod.Typ
         Inv.Grp = Cod.Grp
         Inv.Ini = "   "
         Inv.Cod = Old.Cod
         Inv.Bra = Old.Bra
         Inv.Sta = ns
         Inv.Tim = VAL(Old.Tim)
         Inv.Ctr = Old.Ctr
         Inv.Mdl = LEFT$(Old.Mdl, 10)
         Inv.Ser = LEFT$(Old.Ser, 10)
         Inv.Dis = Old.Dis
         Inv.Dtp = Old.Pdt
         Inv.Dtr = SPACE$(10)
         Inv.Dta = Old.Dlt
         Inv.Prc = Old.Prc
         Inv.Agr = Old.Cus
         Inv.Del = Old.Del
         Inv.Tfr = Old.Tfr
         Inv.Tto = Old.Tto
         Inv.Pon = SPACE$(10)
         Inv.Act = "  "
'         Inv.Par = Old.Par
'         Inv.Lab = Old.Lab
         Inv.Rat = Cod.Rat
'         Inv.Ovr = Old.Ovr

'         Inv.Rmk = Old.Rmk

'         Inv.Mdi = Old.Mdi: Inv.Ydi = Old.Ydi: Inv.Tdi = Old.Tdi
'         Inv.Mdr = Old.Mdr: Inv.Ydr = Old.Ydr: Inv.Tdr = Old.Tdr
'         Inv.Mas = Old.Mas: Inv.Yas = Old.Yas: Inv.Tas = Old.Tas
'         Inv.Mds = Old.Mds: Inv.Yds = Old.Yds: Inv.Tds = Old.Tds
'         Inv.Mar = Old.Mar: Inv.Yar = Old.Yar: Inv.Tar = Old.Tar
'         Inv.Min = Old.Min: Inv.Yin = Old.Yin: Inv.Tin = Old.Tin
'         Inv.Mde = Old.Mde: Inv.Yde = Old.Yde: Inv.Tde = Old.Tde
'         Inv.Mre = Old.Mre: Inv.Yre = Old.Yre: Inv.Tre = Old.Tre
    LOCATE 7, 43: PRINT x;
    INSERT #2, Inv
    NEXT

Errors:
    LPRINT Inv.Ser
    RESUME NEXT

DEFINT A-Z
FUNCTION FindCod (f$)
   b = 1: t = LOF(3) \ LEN(Cod)
   DO UNTIL (t < b)
      m = (t + b) \ 2: GET #3, m, Cod
      IF f$ = Cod.Cod THEN FindCod = 1: EXIT DO
      IF f$ > Cod.Cod THEN b = m + 1 ELSE t = m - 1
   LOOP
END FUNCTION


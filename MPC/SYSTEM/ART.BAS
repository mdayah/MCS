DEFINT A-Z: DEFLNG L: DEFDBL T

DECLARE SUB Picture (r, c, s, f$)
DECLARE SUB EntryD (r, c, e, b, f$)
DECLARE SUB EntryU (r, c, s, e, b, f$)
DECLARE SUB EntryS (r, c, s, e, b, f$)
DECLARE SUB EntryN (r, c, s, d, e, b, f$)

DECLARE SUB Heading ()
DECLARE SUB HRegister ()
DECLARE SUB HChecks ()
DECLARE SUB Printing ()
DECLARE SUB TimeDate ()
DECLARE SUB KeyHit (f$)
DECLARE SUB HitKey (f$)
DECLARE SUB WinMssg (f$)
DECLARE SUB OpenFiles ()
DECLARE SUB Monitor ()
DECLARE SUB InvSort ()
DECLARE SUB InvDisp ()
DECLARE SUB RctPost ()
DECLARE SUB InvPost (c$, o$)
DECLARE SUB InvFind (n$, f, e)
DECLARE SUB Complete (x, e)
DECLARE SUB TestCus ()
DECLARE SUB mTestInx ()
DECLARE SUB mTestInv ()

DECLARE FUNCTION Confirm ()
DECLARE FUNCTION CusFind (f$)
DECLARE FUNCTION NumFind (f&)

REM $INCLUDE: '..\Include\Library.Dec'
REM $INCLUDE: '..\Include\Customer.Rec'
REM $INCLUDE: '..\Include\Accounts.Rec'
REM $INCLUDE: '..\Include\SInvoice.Rec'
REM $INCLUDE: '..\Include\Receipts.Rec'
REM $INCLUDE: '..\Include\Material.REC'

    DIM SHARED Cus AS Customer
    DIM SHARED Cnx AS CusIndex
    DIM SHARED Inv AS Invoices
    DIM SHARED Inx AS InvIndex
    DIM SHARED Acc AS Accounts
    DIM SHARED Rct AS Receipts
    DIM SHARED Mat AS Material
    DIM SHARED Mnx AS MatIndex

'    DIM SHARED Day(12) AS INTEGER
    DIM SHARED LineNum AS INTEGER
    DIM SHARED PageNum AS INTEGER
    DIM SHARED Segment AS INTEGER
    DIM SHARED Sort    AS INTEGER
    DIM SHARED Top     AS INTEGER
    DIM SHARED Bot     AS INTEGER
 
    DIM Ini(20) AS INTEGER
    DIM InN(20) AS STRING * 8
    DIM Due(20) AS STRING * 8
    DIM Rcv(20) AS STRING * 8

    DIM Itm(0 TO 14) AS STRING * 3
    DIM Qty(0 TO 14) AS STRING * 8
    DIM Cog(0 TO 14) AS STRING * 8
    DIM oCo(0 TO 14) AS STRING * 8
    DIM Prc(0 TO 14) AS STRING * 8
   '------------------------------------------------------------------------
   'routine to remove invoices with zero net
   '------------------------------------------------------------------------
'   CLS
'    OpenFiles
'    OPEN "..\Data\Invoices.TIM" FOR RANDOM AS #13 LEN = LEN(Inv)
'    OPEN "..\Data\Invoices.TIN" FOR RANDOM AS #14 LEN = LEN(Inx)
'
'    e = LOF(7) \ LEN(Inx)
'    FOR x = 1 TO e
'    GET #7, x, Inx
'    GET #4, Inx.Num, Inv
'    SELECT CASE Inv.Num
'    CASE "      34": PRINT Inv.Num
'    CASE "      35": PRINT Inv.Num
'    CASE "    1156": PRINT Inv.Num
'    CASE "  427254": PRINT Inv.Num
'    CASE "    7052": PRINT Inv.Num
'    CASE "     332": PRINT Inv.Num
'    CASE "    2567": PRINT Inv.Num
'    CASE "   18824": PRINT Inv.Num
'    CASE ELSE
'       y = y + 1
'       Inx.Num = y
'       PUT #13, , Inv
'       PUT #14, , Inx
'    END SELECT
'    NEXT
'
'    CLOSE
'
'    KILL "..\Data\Invoices.MAS"
'    KILL "..\Data\Invoices.INX"
'    KILL "..\Data\Invoices.CUS"
'    KILL "..\Data\Invoices.INV"
'    NAME "..\Data\Invoices.TIM" AS "..\Data\Invoices.MAS"
'    NAME "..\Data\Invoices.TIN" AS "..\Data\Invoices.INX"
'    Sort = 1
'    InvSort
'
'    STOP
'    GOTO 10

   '------------------------------------------------------------------------
'   FOR y = 1 TO 12: READ Day(y): NEXT
    KEY OFF: KEY 1, "1": KEY 2, "2": KEY 3, "3": KEY 4, "4": KEY 5, "5"
             KEY 6, "6": KEY 7, "7": KEY 8, "8": KEY 9, "9": KEY 10, "0"
    ON ERROR GOTO Errors
    Monitor
    CLS

10  CLOSE : WIDTH 80, 25: DEF SEG = Segment: BLOAD "Utility.Scr", 0: DEF SEG
    HitKey f$: f = VAL(f$)
    IF f$ = "0" THEN f = 10
    OpenFiles
    IF f$ = CHR$(27) GOTO Terminate
    ON f GOTO Cancel, Purge, PurgeChk, ReCost, TestInv, TestInx
    GOTO 10
TestInx: 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    CALL mTestInx
    GOTO 10
TestInv: 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    CALL mTestInv
    GOTO 10
Cancel: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    WIDTH 80, 43: DEF SEG = Segment: BLOAD "Cancel.SCR", 0: DEF SEG
    c$ = "          "
400 EntryS 11, 31, 10, e, b, c$: IF e THEN 10 ELSE IF b THEN 400
    FOR x = 1 TO LOF(9) \ LEN(Rct)
    GET #9, x, Rct
    IF c$ = Rct.Chk THEN
       ERASE InN: z = 1
       LOCATE 14: COLOR 14, 4
       FOR y = 1 TO 20
       InN(y) = MID$(Rct.Inv, z, 8)
       Rcv(y) = MID$(Rct.Inv, z + 8, 8)
       LOCATE , 47: PRINT InN(y);
       LOCATE , 58: PRINT Rcv(y)
       z = z + 16
       NEXT
     
       IF CusFind(Rct.Cus) = 0 THEN ERROR 200
       GET #1, Cnx.Num, Cus: IF Cus.Nam < "!" THEN 400
       LOCATE 13, 31: PRINT Rct.Dat
       LOCATE 14, 31: PRINT Rct.Amt

       LOCATE 19, 30: PRINT Rct.Cus
       LOCATE 21, 15: PRINT Cus.Nam
       LOCATE 22, 15: PRINT Cus.Man
       LOCATE 23, 15: PRINT Cus.Add
       LOCATE 24, 15: PRINT Cus.Str
       LOCATE 25, 15: PRINT Cus.Cty; " "; Cus.Sta; " "; Cus.Zip
      
       FOR y = 1 TO 20              'check if invoice was purged
       IF InN(y) > "        " THEN  'terminate if it was
          IF NumFind(VAL(InN(y))) = 0 THEN
             WinMssg "Sorry invoice was purged"
             WinMssg "Searching for next check number"
             GOTO 15
          END IF
       END IF
       NEXT

       IF Confirm THEN
          FOR y = 1 TO 20
          IF NumFind(VAL(InN(y))) THEN
             GET #4, Inx.Num, Inv
             GET #5, Inx.Num, Inx
             IF Rcv(y) = Inv.Pmt THEN
                Inv.Pdt = "**-**-****"
                Inv.Chk = "*Not Paid*"
                Inv.Pmt = "    0.00"
             ELSE
                LOCATE 42, 1
                PRINT USING "#####.##"; VAL(Inv.Pmt) - VAL(Rcv(y));
                Picture 42, 1, 8, Inv.Pmt
             END IF
             Inx.Mrk = " "
             PUT #4, Inx.Num, Inv '----------------------------<INVOICES.DAT>
             PUT #5, Inx.Num, Inx '----------------------------<INVOICES.INX>
          END IF
          NEXT
          Cus.Mpd = Cus.Mpd - VAL(Rct.Amt) * 100
          Cus.Ypd = Cus.Ypd - VAL(Rct.Amt) * 100
          PUT #1, Cnx.Num, Cus '-------------------------------<CUSTOMER.DTA>

          RctPost

          Rct.Amt = "          "
          Rct.Cus = Cus.Cod
          Rct.Inv = SPACE$(360)
          PUT #9, LOC(9), Rct '--------------------------------<INVOICES.CHK>
          Sort = 1
          EXIT FOR
       END IF
    END IF
15  NEXT
    GOTO 10
Purge: 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    TestCus
    y = 0: d$ = "": T1 = 0: T2 = 0: T3 = 0: T4 = 0: T5 = 0: T6 = 0
    LOCATE 23, 20: PRINT "Purge paid invoices up to date:          "
    LOCATE 24, 20: PRINT "Be sure to backup before purging invoices";
    EntryD 23, 51, e, b, d$: IF e OR b THEN 10
    d$ = RIGHT$(d$, 2) + LEFT$(d$, 2) + MID$(d$, 4, 2)
    IF LEFT$(d$, 1) = "0" THEN d$ = "A" + RIGHT$(d$, 5)
    OPEN "..\Data\Invoices.TIM" FOR RANDOM AS #13 LEN = LEN(Inv)
    OPEN "..\Data\Invoices.TIN" FOR RANDOM AS #14 LEN = LEN(Inx)
    PageNum = 0: HRegister: Printing
    e = LOF(7) \ LEN(Inx)
    FOR x = 1 TO e
    IF INKEY$ = CHR$(27) THEN 10
    Complete x, e
    GET #7, x, Inx
    GET #4, Inx.Num, Inv
    i$ = Inx.Dat
    IF LEFT$(i$, 1) = "0" THEN i$ = "A" + RIGHT$(i$, 5)
    IF i$ > d$ THEN
        GOSUB 300
    ELSE
       IF CusFind(Inv.Cus) = 0 THEN PRINT Inv.Cus: ERROR 200
       GET #1, Cnx.Num, Cus
       IF Inx.Mrk = "O" AND Inv.Net = Inv.Pmt THEN
          GOSUB 301
       ELSE
          GOSUB 300
       END IF
    END IF
    NEXT
    LPRINT : LPRINT TAB(82); : LPRINT USING "#######.##"; T1; T2; T1 - T2; T3; T4; T5; T6; T5 - T6
    CLOSE
    KILL "..\Data\Invoices.MAS"
    KILL "..\Data\Invoices.INX"
    KILL "..\Data\Invoices.CUS"
    KILL "..\Data\Invoices.INV"
    NAME "..\Data\Invoices.TIM" AS "..\Data\Invoices.MAS"
    NAME "..\Data\Invoices.TIN" AS "..\Data\Invoices.INX"
    Sort = 1
    InvSort
    GOTO 10

300 COLOR 3
       y = y + 1
       Inx.Num = y
       PUT #13, , Inv
       PUT #14, , Inx
    RETURN
301 COLOR 11
    LineNum = LineNum + 1: IF LineNum > 60 THEN HRegister
    LPRINT USING "& "; Inv.Num; Cnx.Nam; Inv.Bdt; Inv.Ddt; Inv.Pdt; Inv.Chk;
    LPRINT USING " &  & ######.##  &  &  &"; Inv.Grs; Inv.Crd; VAL(Inv.Grs) - VAL(Inv.Crd); Inv.Tax; Inv.Frt; Inv.Net;
    LPRINT USING " ######.##"; VAL(Inv.Pmt); VAL(Inv.Net) - VAL(Inv.Pmt)
    T1 = T1 + VAL(Inv.Grs): T2 = T2 + VAL(Inv.Crd): T3 = T3 + VAL(Inv.Tax)
    T4 = T4 + VAL(Inv.Frt): T5 = T5 + VAL(Inv.Net): T6 = T6 + VAL(Inv.Pmt)
    RETURN
PurgeChk: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    s = 0: d$ = ""
    LOCATE 23, 20: PRINT "Purge check register up to date:          ";
    LOCATE 24, 20: PRINT " Be sure to backup before purging checks. ";
    EntryD 23, 52, e, b, d$: IF e OR b THEN 10
    d$ = RIGHT$(d$, 4) + LEFT$(d$, 2) + MID$(d$, 4, 2)
    OPEN "..\Data\Invoices.TCK" FOR RANDOM AS #13 LEN = LEN(Rct)
    PageNum = 0: HChecks
    e = LOF(9) \ LEN(Rct)
    FOR x = 1 TO e
    IF INKEY$ = CHR$(27) THEN
       CLOSE : KILL "..\Data\Invoices.TCK"
       GOTO 10
    END IF
    GET #9, x, Rct
    f$ = RIGHT$(Rct.Dat, 4) + LEFT$(Rct.Dat, 2) + MID$(Rct.Dat, 4, 2)
    IF f$ > d$ THEN
       s = s + 1
       PUT #13, s, Rct
    ELSE
       IF CusFind(Rct.Cus) THEN GET #1, Cnx.Num, Cus
       LPRINT USING "& "; Rct.Dat; Rct.Chk; Rct.Amt; Cus.Nam; RTRIM$(Rct.Inv)
       LineNum = LineNum + 1: IF LineNum > 60 THEN HChecks
    END IF
    NEXT
    CLOSE
    KILL "..\Data\Invoices.Chk"
    NAME "..\Data\Invoices.TCK" AS "..\Data\Invoices.Chk"
    LOCATE 24, 20: PRINT USING "   Total check receipts purged:  ######   "; e - s;
    KeyHit f$
    GOTO 10
ReCost: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    Mon$ = LEFT$(DATE$, 2)
    LOCATE 23, 20: PRINT "   Recost invoices for the month of:     "
    LOCATE 24, 20: PRINT "  Backup before starting this routine.   ";
    EntryS 23, 57, 2, e, b, Mon$: IF e OR b THEN 10
    FOR xInv = 1 TO LOF(4) \ LEN(Inv)
    IF INKEY$ = CHR$(27) THEN EXIT FOR
    GET #4, xInv, Inv
    IF LEFT$(Inv.Bdt, 2) = Mon$ THEN
       ERASE Itm, Qty, Cog, oCo, Prc: T1 = 0: T2 = 0: CLS
       LOCATE 2, 21: PRINT USING "Sequence Number [#####]"; xInv
       LOCATE 3, 21: PRINT USING " Invoice Number [&]  Date [&]"; Inv.Num; Inv.Bdt
       LOCATE 4, 21: PRINT USING "Customer Number [&]"; Inv.Cus;
       FOR x = 0 TO 14
           Itm(x) = MID$(Inv.Dst, 1 + 27 * x, 3): IF Itm(x) < "  1" THEN EXIT FOR
           Qty(x) = MID$(Inv.Dst, 4 + 27 * x, 8)
           oCo(x) = MID$(Inv.Dst, 12 + 27 * x, 8)
           Prc(x) = MID$(Inv.Dst, 20 + 27 * x, 8)
           GET #10, VAL(Itm(x)), Mat
           Lqt = Mat.Ybq + Mat.Ypq
           Lpr = Mat.Ybp + Mat.Ypp
           IF Lqt = 0 OR Lpr = 0 THEN Tup = VAL(Mat.Ref) ELSE Tup = Lpr / Lqt
           Lq = VAL(Qty(x)) * 100
           LOCATE x + 6, 21
           PRINT USING "& & & #####.## &"; Itm(x); Qty(x); oCo(x); Lq * Tup / 100; Prc(x)
           Picture x + 6, 43, 8, Cog(x)
           T1 = T1 + VAL(oCo(x))
           T2 = T2 + VAL(Cog(x))
       NEXT
       LOCATE 22, 34: PRINT USING "#####.##"; T1; : Picture 22, 34, 8, oCs$
       LOCATE 22, 43: PRINT USING "#####.##"; T2; : Picture 22, 43, 8, Cst$

       f$ = "": FOR x = 0 TO 14
       IF Itm(x) > "  0" THEN
           GET #10, VAL(Itm(x)), Mat
           f$ = f$ + Itm(x) + Qty(x) + Cog(x) + Prc(x)
           Mat.Msc = Mat.Msc + (VAL(Cog(x)) * 100) - (VAL(oCo(x)) * 100)
           Mat.Ysc = Mat.Ysc + (VAL(Cog(x)) * 100) - (VAL(oCo(x)) * 100)
           PUT #10, VAL(Itm(x)), Mat '--------------update inventory--------
       ELSE
           EXIT FOR
       END IF
       NEXT
       Inv.Dst = f$
       PUT #4, xInv, Inv '----------------------------save invoice--------------
       IF CusFind(Inv.Cus) = 0 THEN ERROR 200
       GET #1, Cnx.Num, Cus
       Cus.Mco = Cus.Mco + (VAL(Cst$) * 100) - (VAL(oCs$) * 100)
       Cus.Yco = Cus.Yco + (VAL(Cst$) * 100) - (VAL(oCs$) * 100)
       PUT #1, Cnx.Num, Cus '----------------------update customer-----------

       InvPost Cst$, oCs$

    END IF
    NEXT
    GOTO 10
Errors: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    SELECT CASE ERR
           CASE 70:   WinMssg "Permission denied"
           CASE 200:  WinMssg "Unable to find customer code"
           CASE ELSE: WinMssg "Error:" + STR$(ERR) + "  Line:" + STR$(ERL)
    END SELECT
    RESUME 10
Terminate: 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
    IF Sort THEN InvSort
    CLOSE
    RUN "ARP"
DATA 31%,28%,31%,30%,31%,30%,31%,31%,30%,31%,30%,31%

DEFINT L, T
SUB Complete (x, T)
    p& = 51 * (x / T)
    p& = p& + 15 - POS(0)
    PRINT STRING$(p&, "Û");
END SUB

DEFSNG A-B, D-Z
FUNCTION Confirm ()
    SCREEN , , 1, 1
    PCOPY 0, 1
    COLOR 15, 1: LOCATE 35
    LOCATE , 21: PRINT "                      "
    LOCATE , 21: PRINT " Press [*] to confirm "
    LOCATE , 21: PRINT "                      "
    COLOR 11, 0: LOCATE 36
    LOCATE , 20: PRINT "Û"
    LOCATE , 20: PRINT "Û"
    LOCATE , 20: PRINT "ÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛ"
    SOUND 1000, .2: SOUND 1100, .4
    KeyHit f$: IF f$ = "*" THEN Confirm = 1
    SCREEN , , 0, 0
END FUNCTION

DEFINT A-B, D-Z
FUNCTION CusFind (f$)
   b = 1: T = LOF(3) \ LEN(Cnx)
   DO UNTIL (T < b)
      m = (T + b) \ 2: GET #3, m, Cnx
      IF f$ = Cnx.Cod THEN CusFind = 1: EXIT DO
      IF f$ > Cnx.Cod THEN b = m + 1 ELSE T = m - 1
   LOOP
END FUNCTION

DEFSNG A-Z
SUB HChecks ()
     Heading
     LPRINT TAB(72); "PURGED RECEIPTS": LPRINT CHR$(27); "P"; CHR$(15)
     LPRINT "             Check      Check   "
     LPRINT "Date Paid    Number     Amount          Customer Name         Invoices Paid With This Check"
     LPRINT "========== ========== ========== ============================ ========================================================================================================================================================================="
END SUB

DEFINT A-Z
SUB Heading ()
    LPRINT CHR$(12); CHR$(27); "@"; : WIDTH "LPT1:", 255: LPRINT CHR$(27); "M"
    PageNum = PageNum + 1: LineNum = 13
    LPRINT "Date: "; DATE$; TAB(61); "       McKENRY PRODUCE COMPANY        "; TAB(153); "Page: "; PageNum
    LPRINT "Time: "; TIME$; TAB(61); "717 Willow Avenue, Knoxville, TN 37915"
    LPRINT :         LPRINT TAB(61); "         ACCOUNTS RECEIVABLE          ": LPRINT
END SUB

DEFSNG A-Z
SUB HitKey (f$)
    DO: f$ = INKEY$: LOOP WHILE f$ <> ""
    DO: f$ = INKEY$: TimeDate: LOOP WHILE f$ = ""
END SUB

SUB HRegister ()
    Heading
    LPRINT TAB(71); "PURGED  INVOICES": LPRINT
    LPRINT "INVOICE                                INVOICE                           CHECK      GROSS                NET      SALES     OTHER     TOTAL    AMOUNT    AMOUNT  "
    LPRINT "NUMBER         CUSTOMER NAME             DATE     DUE DATE  DATE PAID    NUMBER     SALES    CREDITS    SALES      TAX     CHARGES   CHARGES    PAID       DUE   "
    LPRINT "======== ============================ ========== ========== ========== ========== ========= ========= ========= ========= ========= ========= ========= ========="
END SUB

DEFINT A-K, M-S, U-Z
DEFLNG L
DEFDBL T
SUB InvDisp ()
    COLOR 14, 0
    GET #4, Inx.Num, Inv
    IF CusFind(Inv.Cus) = 0 THEN ERROR 200
    GET #1, Cnx.Num, Cus
    LOCATE 9, 3:   PRINT Inv.Num
    LOCATE 11, 12: PRINT Cus.Cod
    LOCATE , 2:    PRINT Cus.Nam
    LOCATE , 2:    PRINT Cus.Man
    LOCATE , 2:    PRINT Cus.Add
    LOCATE , 2:    PRINT Cus.Str
    LOCATE , 2:    PRINT Cus.Cty; Cus.Sta; " "; Cus.Zip

    LOCATE 9, 46:  PRINT Inv.Ref
    LOCATE 10, 46: PRINT Inv.Man
    LOCATE 11, 46: PRINT Inv.Bdt
    LOCATE 12, 46: PRINT Inv.Ddt
    LOCATE 14, 46: PRINT Inv.Pdt
    LOCATE 15, 46: PRINT Inv.Chk
    LOCATE 16, 48: PRINT Inv.Pmt

    LOCATE 9, 72:  PRINT Inv.Grs
    LOCATE 10, 72: PRINT Inv.Crd: COLOR 15, 3
    LOCATE 12, 72: PRINT USING "#####.##"; VAL(Inv.Grs) - VAL(Inv.Crd); : COLOR 14, 0
    LOCATE 13, 72: PRINT Inv.Tax
    LOCATE 14, 72: PRINT Inv.Frt
    LOCATE 16, 72: PRINT Inv.Net
    oCu$ = Cus.Cod
    Pmt$ = Inv.Pmt
    Grs$ = Inv.Grs
    Crd$ = Inv.Crd
    Tax$ = Inv.Tax
    Frt$ = Inv.Frt
    Net$ = Inv.Net
''''AccDist
    LOCATE 23
'    FOR x = 0 TO 14
'    IF VAL(oIt(x)) > 0 THEN
'       GET #10, VAL(oIt(x)), Mat
'       COLOR 14, 0: LOCATE , 2:  PRINT Mat.Num;
'       COLOR 10, 0: LOCATE , 6:  PRINT Mat.Nam;
'       COLOR 10, 0: LOCATE , 44: PRINT Mat.Mes;
'       COLOR 14, 0: LOCATE , 35: PRINT oQt(x);
'       COLOR 10, 0: LOCATE , 47: PRINT oCo(x);
'       COLOR 10, 0: LOCATE , 56: PRINT USING "####.##"; VAL(Mat.Prc);
'       COLOR 10, 0: LOCATE , 64: PRINT USING "####.##"; VAL(oPr(x)) / VAL(oQt(x));
'       COLOR 14, 0: LOCATE , 72: PRINT oPr(x)
'       Old(VAL(oIt(x)), 1) = VAL(oQt(x)) * 100
'       Old(VAL(oIt(x)), 2) = VAL(oCo(x)) * 100
'       T1 = T1 + VAL(oCo(x))
'       T2 = T2 + VAL(oPr(x))
'    ELSE
'       COLOR 3, 0: LOCATE , 2: PRINT "   ณ                            ณ        ณ  ณ        ณ       ณ       ณ        "
'    END IF
'    NEXT
    COLOR 15, 1
    LOCATE 39, 47: PRINT USING "#####.##"; T1; : Picture 39, 47, 8, Cst$: oCs$ = Cst$
    LOCATE 39, 72: PRINT USING "#####.##"; T2; : Picture 39, 72, 8, Amt$: oAm$ = Amt$
END SUB

DEFINT L, T
SUB InvFind (n$, f, e)
   f = 0: e = 0
   b = 1: T = LOF(6) \ LEN(Inx)
   DO UNTIL (T < b)
      m = (T + b) \ 2: GET #6, m, Inx
      IF n$ > Inx.Cus THEN b = m + 1 ELSE T = m - 1
   LOOP
   DO WHILE n$ > Inx.Cus AND NOT EOF(6)
      GET #6, , Inx
   LOOP
   IF n$ = Inx.Cus THEN
      f = LOC(6): e = f
      DO WHILE NOT EOF(6)
         GET #6, , Inx
         IF n$ = Inx.Cus THEN e = LOC(6) ELSE EXIT DO
      LOOP
   END IF
END SUB

DEFSNG A-Z
SUB InvPost (Cst$, oCs$)
    ON LOCAL ERROR GOTO ErInv

    LOCK #12, 60 : GET #12, 60, Acc
    Acc.Mdb = Acc.Mdb + (VAL(Cst$) * 100) - (VAL(oCs$) * 100)
    Acc.Ydb = Acc.Ydb + (VAL(Cst$) * 100) - (VAL(oCs$) * 100)
    PUT #12, 60, Acc: UNLOCK #12, 60 '----------debit cost of goods-------

    LOCK #12, 9 : GET #12, 9, Acc
    Acc.Mcr = Acc.Mcr + (VAL(Cst$) * 100) - (VAL(oCs$) * 100)
    Acc.Ycr = Acc.Ycr + (VAL(Cst$) * 100) - (VAL(oCs$) * 100)
    PUT #12, 9, Acc: UNLOCK #12, 9 '------------credit inventory----------
EXIT SUB

ErInv:
    IF ERR = 70 THEN
       DO:
            WHILE Tries < 10
                Tries = Tries + 1
                RESUME
            WEND
            WinMssg "Account in use, try again?"
            Tries = 0
       LOOP
    ELSE
       ON LOCAL ERROR GOTO 0
       ERROR ERR
    END IF
END SUB

SUB InvSort ()
    CLOSE
    IF Sort THEN
       COLOR 20, 0: LOCATE 23, 35: PRINT "Sorting...."
       Sort = 0
       SHELL "AC.COM ..\Data\Invoices.INX ..\Data\Invoices.CUS /L18 /+1,4 /+9,1 /+8,1 /+7,1 /+6,1 /Q"
       SHELL "AC.COM ..\Data\Invoices.INX ..\Data\Invoices.INV /L18 /+9,1 /+8,1 /+7,1 /+6,1 /Q"
    END IF
END SUB

SUB KeyHit (f$)
    DO: LOOP WHILE INKEY$ <> ""
    DO: f$ = INKEY$: LOOP WHILE f$ = ""
    f$ = UCASE$(f$)
END SUB

SUB Monitor ()
    DEF SEG = 0
       Segment = &HB800
    DEF SEG
END SUB

DEFINT A-S, U-Z
DEFLNG T
SUB mTestInv ()
    WIDTH 80, 50
    COLOR 11, 1
    CLS
    COLOR 15, 4
    LOCATE 50, 1: PRINT " Checking Customer's Balances    ณCustomersณ Invoice ณ  + OR -  ณCreditsณ      ";
    VIEW PRINT 1 TO 49
    COLOR 11, 1
    LOCATE 49, 1
    FOR x = 1 TO LOF(3) \ LEN(Cnx)
        GET #3, x, Cnx
        GET #1, Cnx.Num, Cus
        tCus = (Cus.Ybb + Cus.Ygr - Cus.Ycr + Cus.Ytx + Cus.Yfr + Cus.Chg - Cus.Ypd)
        tDue = 0
        InvFind Cus.Cod, b, e
        IF b THEN
           FOR i = b TO e
               GET #6, i, Inx
               GET #4, Inx.Num, Inv
               tDue = tDue + 100 * VAL(Inv.Net) - 100 * VAL(Inv.Pmt)
           NEXT
        END IF
        PRINT
        PRINT USING "& &ณ######.##ณ######.##ณ#######.##ณ####.##ณ"; Cus.Cod; Cus.Nam; tCus / 100; tDue / 100; (tCus - tDue) / 100; Cus.Crd / 100;
        IF tDue <> tCus THEN PRINT "ERORR"; : KeyHit f$
        ttDue = ttDue + tDue
        ttCus = ttCus + tCus
        ttCrd = ttCrd + Cus.Crd
    NEXT
    PRINT TAB(28); "Total ณ";
    PRINT USING "######.##ณ######.##ณ#######.##ณ####.##ณ"; ttCus / 100; ttDue / 100; (ttCus - ttDue) / 100; ttCrd / 100;
    KeyHit f$
END SUB

DEFINT T
SUB mTestInx ()
    WIDTH 80, 50
    COLOR 11, 1
    CLS
    COLOR 15, 4
    LOCATE 50, 1: PRINT " Checking Invoices Master  ณ      Invoices Index       ณ         Errors         ";
    VIEW PRINT 1 TO 49
    COLOR 11, 1
    LOCATE 49, 1
    FOR x = 1 TO LOF(4) \ LEN(Inv)
    GET #4, x, Inv
    GET #5, x, Inx
    d$ = RIGHT$(Inv.Bdt, 2) + LEFT$(Inv.Bdt, 2) + MID$(Inv.Bdt, 4, 2)
    PRINT USING "##### & & & ณ"; x; Inv.Cus; Inv.Num; d$;
    PRINT USING "##### & ######## & ณ"; Inx.Num; Inx.Cus; Inx.Inv; Inx.Dat;
    IF Inv.Cus <> Inx.Cus THEN e = 1: Sort = 1
    IF Inv.Man <> Inx.Man THEN e = 1: Sort = 1
    IF VAL(Inv.Num) <> Inx.Inv THEN e = 1: Sort = 1
    IF d$ <> Inx.Dat THEN e = 1: Sort = 1
    IF x <> Inx.Num THEN e = 1: Sort = 1
    IF e THEN
       PRINT "   ERROR press any key";
       e = 0: KeyHit f$
       Inx.Cus = Inv.Cus
       Inx.Man = Inv.Man
       Inx.Inv = VAL(Inv.Num)
       Inx.Dat = d$
       Inx.Mrk = " "
       Inx.Num = x
       PUT #5, x, Inx
    END IF
    PRINT
    NEXT
    InvSort
END SUB

FUNCTION NumFind (f&)
   b = 1: T = LOF(7) \ LEN(Inx)
   DO UNTIL (T < b)
      m = (T + b) \ 2: GET #7, m, Inx
      IF f& = Inx.Inv THEN NumFind = 1: EXIT DO
      IF f& > Inx.Inv THEN b = m + 1 ELSE T = m - 1
   LOOP
END FUNCTION

DEFSNG A-Z
SUB OpenFiles ()
    InvSort
    OPEN "..\Data\Customer.MAS" FOR RANDOM LOCK READ WRITE AS #1 LEN = LEN(Cus)
    OPEN "..\Data\Customer.INX" FOR RANDOM LOCK READ WRITE AS #2 LEN = LEN(Cnx)
    OPEN "..\Data\Customer.NAM" FOR RANDOM LOCK READ WRITE AS #3 LEN = LEN(Cnx)
 
    OPEN "..\Data\Invoices.MAS" FOR RANDOM LOCK READ WRITE AS #4 LEN = LEN(Inv)
    OPEN "..\Data\Invoices.INX" FOR RANDOM LOCK READ WRITE AS #5 LEN = LEN(Inx)
    OPEN "..\Data\Invoices.CUS" FOR RANDOM LOCK READ WRITE AS #6 LEN = LEN(Inx)
    OPEN "..\Data\Invoices.INV" FOR RANDOM LOCK READ WRITE AS #7 LEN = LEN(Inx)
    OPEN "..\Data\Invoices.Chk" FOR RANDOM LOCK READ WRITE AS #9 LEN = LEN(Rct)
  
    OPEN "..\Data\Material.MAS" FOR RANDOM LOCK READ WRITE AS #10 LEN = LEN(Mat)
    OPEN "..\Data\Material.INX" FOR RANDOM LOCK READ WRITE AS #11 LEN = LEN(Mnx)
    OPEN "..\Data\Accounts.MAS" FOR RANDOM SHARED AS #12 LEN = LEN(Acc)
END SUB

SUB Printing ()
    WIDTH 80, 50: CLS : COLOR 14, 5: LOCATE 18
    LOCATE , 11: PRINT "                                                           "
    LOCATE , 11: PRINT " ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออป "
    LOCATE , 11: PRINT " บ  0    10   20   30   40   50   60   70   80   90  100 บ "
    LOCATE , 11: PRINT " บ  รยยยยลยยยยลยยยยลยยยยลยยยยลยยยยลยยยยลยยยยลยยยยลยยยยด  บ "
    LOCATE , 11: PRINT " บ                                                       บ "
    LOCATE , 11: PRINT " บ  ณ    ณ    ณ    ณ    ณ    ณ    ณ    ณ    ณ    ณ    ณ  บ "
    LOCATE , 11: PRINT " ศออฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯฯออผ "
    LOCATE , 11: PRINT "            Press [ESC] to terminate printing              "
    LOCATE 22, 15: COLOR 11
END SUB

SUB RctPost ()
    ON LOCAL ERROR GOTO ErRct

    LOCK #12, 1 : GET #12, 1, Acc
    Acc.Mdb = Acc.Mdb - VAL(Rct.Amt) * 100
    Acc.Ydb = Acc.Ydb - VAL(Rct.Amt) * 100
    PUT #12, 1, Acc: UNLOCK #12, 1 '---------------------<Debit Cash>

    LOCK #12, 4 : GET #12, 4, Acc
    Acc.Mcr = Acc.Mcr - VAL(Rct.Amt) * 100
    Acc.Ycr = Acc.Ycr - VAL(Rct.Amt) * 100
    PUT #12, 4, Acc: UNLOCK #12, 4 '---------------------<Credit A/R>

EXIT SUB

ErRct:
    IF ERR = 70 THEN
       DO:
            WHILE Tries < 10
                Tries = Tries + 1
                RESUME
            WEND
            WinMssg "Account in use, try again?"
            Tries = 0
       LOOP
    ELSE
       ON LOCAL ERROR GOTO 0
       ERROR ERR
    END IF
END SUB

DEFINT A-Z
SUB TestCus ()
    CLS : Printing
    LOCATE 26, 11: PRINT "             Testing Customers before Purging              ";
    LOCATE 22, 15: COLOR 11
    e = LOF(7) \ LEN(Inx)
    FOR x = 1 TO e
    IF INKEY$ = CHR$(27) THEN EXIT SUB
    Complete x, e
    GET #7, x, Inx
    IF CusFind(Inx.Cus) = 0 THEN LPRINT USING "Invoice No. ########    missing Customer: &"; Inx.Inv; Inx.Cus
    NEXT
    CLS
END SUB

DEFSNG A-Z
SUB TimeDate ()
    COLOR 0, 7
    LOCATE 10, 22, 0: PRINT " "; DATE$; " "
    T$ = " " + LEFT$(TIME$, 2)
    IF TIME$ > "11:59:59" THEN f$ = "pm " ELSE f$ = "am "
    IF TIME$ > "12:59:59" THEN T$ = " " + STR$(VAL(T$) - 12)
    LOCATE 10, 47: PRINT T$; MID$(TIME$, 3, 6) + f$
END SUB

SUB WinMssg (f$)
    SCREEN , , 1, 1
    PCOPY 0, 1
    COLOR 14, 4: LOCATE 12
    LOCATE , 31: PRINT "ษอ"; STRING$(LEN(f$), "อ"); "อป"
    LOCATE , 31: PRINT "บ "; f$; " บ"
    LOCATE , 31: PRINT "ศอ"; STRING$(LEN(f$), "อ"); "อผ"
    COLOR 12, 0: LOCATE 13
    LOCATE , 30: PRINT "Û"
    LOCATE , 30: PRINT "Û"
    LOCATE , 30: PRINT "ÛÛÛÛ"; STRING$(LEN(f$), "Û")
    SOUND 660, .1: SOUND 770, .1: SLEEP 2
    SCREEN , , 0, 0
END SUB


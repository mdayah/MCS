DEFINT A-Z: DEFLNG L: DEFDBL T

DECLARE SUB Heading ()
DECLARE SUB HSales ()
DECLARE SUB HChecks ()
DECLARE SUB HRegister ()
DECLARE SUB HSalesSum ()
DECLARE SUB HCusBuy ()
DECLARE SUB Printing ()
DECLARE SUB Complete (x, e)
DECLARE SUB TimeDate ()
DECLARE SUB KeyHit (f$)
DECLARE SUB HitKey (f$)

DECLARE SUB WinMat (f$)
DECLARE SUB WinCus (f$)
DECLARE SUB WinMssg (f$)
DECLARE SUB mReceipts ()
DECLARE SUB mSales ()
DECLARE SUB OpenFiles ()
DECLARE SUB Monitor ()
DECLARE SUB AccDist ()

DECLARE SUB InvFind (n$, f, e)
DECLARE SUB OnHand (r)

DECLARE FUNCTION CusFind (f$)
DECLARE FUNCTION SalesCus ()
DECLARE FUNCTION SalesInx (s, d$)

REM $INCLUDE: '..\Include\Library.Dec'
REM $INCLUDE: '..\Include\Customer.Rec'
REM $INCLUDE: '..\Include\SInvoice.Rec'
REM $INCLUDE: '..\Include\SalesReg.Rec'
REM $INCLUDE: '..\Include\SalesCus.Rec'
REM $INCLUDE: '..\Include\Receipts.Rec'
REM $INCLUDE: '..\Include\Material.REC'

	DIM SHARED Cus AS Customer
	DIM SHARED Cnx AS CusIndex
	DIM SHARED Inv AS Invoices
	DIM SHARED Inx AS InvIndex
	DIM SHARED Sal AS SalesReg
	DIM SHARED Sac AS SalesCus
	DIM SHARED Rct AS Receipts
	DIM SHARED Mat AS Material
	DIM SHARED Mnx AS MatIndex
	DIM SHARED LineNum AS INTEGER
	DIM SHARED PageNum AS INTEGER
	DIM SHARED Segment AS INTEGER
  
	DIM SHARED Lqt, Lpr, Tup
	DIM SHARED Itm(0 TO 15) AS STRING * 3
	DIM SHARED Qty(0 TO 15) AS STRING * 8
	DIM SHARED Cog(0 TO 15) AS STRING * 8
	DIM SHARED Prc(0 TO 15) AS STRING * 8
   
	ON ERROR GOTO Errors
	
KEY OFF: KEY 1, "1": KEY 2, "2": KEY 3, "3": KEY 4, "4": KEY 5, "5"
		 KEY 6, "6": KEY 7, "7": KEY 8, "8": KEY 9, "9": KEY 10, "0"
	Monitor
	CLS

10  CLOSE : WIDTH 80, 25: DEF SEG = Segment: BLOAD "DailyRpt.Scr", 0: DEF SEG
	HitKey f$: f = VAL(f$)
	IF f$ = "0" THEN f = 10
	OpenFiles
	IF f$ = CHR$(27) GOTO Terminate
	ON f GOTO InvReg, SalReg, SalSum, SalCus, CusBuy, RctReg, Lables, SalRpt, RctRpt, IndexCards
	GOTO 10
InvReg: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	T1 = 0: T2 = 0: T3 = 0: T4 = 0: T5 = 0: T6 = 0: d$ = DATE$
	COLOR 14, 0: LOCATE 23, 20: PRINT USING " Daily Invoice Register for: &  "; DATE$
300 EntryD 23, 49, e, b, d$: IF e THEN 10 ELSE IF b THEN 300
	d$ = RIGHT$(d$, 2) + LEFT$(d$, 2) + MID$(d$, 4, 2)
	PageNum = 0: HRegister: Printing
	e = LOF(7) \ LEN(Inx)
	FOR x = 1 TO e
	IF INKEY$ = CHR$(27) THEN 10
	Complete x, e
	COLOR 3
	GET #7, x, Inx
	IF d$ = Inx.Dat THEN
	   COLOR 11
	   GET #4, Inx.Num, Inv
	   IF Inv.Ref <> "Dummy    " THEN
		  IF CusFind(Inv.Cus) = 0 THEN ERROR 200
		  LineNum = LineNum + 1: IF LineNum > 60 THEN HRegister
		  LPRINT USING "& "; Inv.Num; Cnx.Nam; Inv.Bdt; Inv.Ddt; Inv.Pdt; Inv.Chk;
		  LPRINT USING " &  & ######.##  &  &  &"; Inv.Grs; Inv.Crd; VAL(Inv.Grs) - VAL(Inv.Crd); Inv.Tax; Inv.Frt; Inv.Net;
		  LPRINT USING " ######.##"; VAL(Inv.Pmt); VAL(Inv.Net) - VAL(Inv.Pmt)
		  T1 = T1 + VAL(Inv.Grs): T2 = T2 + VAL(Inv.Crd): T3 = T3 + VAL(Inv.Tax)
		  T4 = T4 + VAL(Inv.Frt): T5 = T5 + VAL(Inv.Net): T6 = T6 + VAL(Inv.Pmt)
	   END IF
	END IF
	NEXT
	LPRINT : LPRINT TAB(82); : LPRINT USING "#######.##"; T1; T2; T1 - T2; T3; T4; T5; T6; T5 - T6
	GOTO 10
CusBuy: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	COLOR 14, 0: LOCATE 23, 25: PRINT "Creating Sales File for 2 Months";
	IF SalesCus() GOTO 10
	OpenFiles
	CLOSE #9: OPEN "..\Data\Invoices.SAL" FOR RANDOM AS #9 LEN = LEN(Sac)
	Printing
	HCusBuy
	GET #9, 1, Sac
	c$ = Sac.Cus
	i$ = Sac.Itm
	b$ = MID$(Sac.Dat, 5, 2) + "-" + RIGHT$(Sac.Dat, 2) + "-" + LEFT$(Sac.Dat, 4)
	l$ = MID$(Sac.Dat, 5, 2) + "-" + RIGHT$(Sac.Dat, 2) + "-" + LEFT$(Sac.Dat, 4)
	IF CusFind(c$) = 0 THEN ERROR 200
	LPRINT USING "&  "; Cnx.Cod; Cnx.Nam;
	e = LOF(9) \ LEN(Sac)
	FOR x = 1 TO e
		IF INKEY$ = CHR$(27) THEN 10
		Complete x, e
		GET #9, x, Sac
		IF c$ <> Sac.Cus THEN
		   GOSUB 401
		   c$ = Sac.Cus
		   IF CusFind(c$) = 0 THEN ERROR 200
		   LPRINT USING "&  "; Cnx.Cod; Cnx.Nam;
		END IF
		IF i$ <> Sac.Itm THEN GOSUB 401
		Ln = Ln + 1: Lq = Lq + VAL(Sac.Qty)
		l$ = MID$(Sac.Dat, 5, 2) + "-" + RIGHT$(Sac.Dat, 2) + "-" + LEFT$(Sac.Dat, 4)
	NEXT
	GOTO 10

401 GET #10, VAL(i$), Mat
	Ld = DateDif(b$, l$)
	LPRINT TAB(37);
	LPRINT USING "&  "; Mat.Typ + Mat.Num; Mat.Nam;
	LPRINT USING "##### &     "; Lq; Mat.Mes;
	LPRINT USING "#####  "; Ln; Ld; Lq / Ln;
	IF Ld = 0 THEN
	   LPRINT USING "& one time"; Mat.Mes
	ELSE
	   LPRINT USING "& every ##.## Days"; Mat.Mes; Ld / Ln
	END IF
	LineNum = LineNum + 1: IF LineNum > 60 THEN HCusBuy
	Lq = 0: Ln = 0: Ld = 0: i$ = Sac.Itm
	b$ = MID$(Sac.Dat, 5, 2) + "-" + RIGHT$(Sac.Dat, 2) + "-" + LEFT$(Sac.Dat, 4)
	RETURN
SalCus: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	s = 2: GOTO 400
SalSum: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	s = 0: GOTO 400
SalReg: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	s = 1
400 T1 = 0: T2 = 0: T3 = 0: T4 = 0: T5 = 0: T6 = 0: d$ = DATE$
	COLOR 14, 0: LOCATE 23, 20: PRINT USING "   Daily Sales Register for: &  "; DATE$
405 EntryD 23, 49, e, b, d$: IF e THEN 10 ELSE IF b THEN 405
	d$ = RIGHT$(d$, 2) + LEFT$(d$, 2) + MID$(d$, 4, 2)
	IF SalesInx(s, d$) GOTO 10
	OpenFiles

	PageNum = 0: IF s THEN HSales ELSE HSalesSum
	Printing

	GET #9, 1, Sal
	p$ = Sal.Cod
	i$ = Sal.Itm
	c$ = Sal.Cus
	e = LOF(9) \ LEN(Sal)
	FOR x = 1 TO e
	IF INKEY$ = CHR$(27) THEN 10
	Complete x, e
	GET #9, x, Sal
	SELECT CASE s
	CASE 0: IF i$ <> Sal.Itm THEN GOSUB 410
	CASE 1: IF p$ <> Sal.Cod THEN GOSUB 410
			LineNum = LineNum + 1
			IF LineNum > 60 THEN IF s THEN HSales ELSE HSalesSum
			GET #10, VAL(Sal.Itm), Mat
			LPRINT USING "&  "; Sal.Typ + Sal.Itm; Sal.Mat;
			LPRINT USING " #####.## & "; VAL(Sal.Qty) / VAL(Mat.Fra); Mat.Con;
			LPRINT USING "#######.##  "; VAL(Sal.Cog); VAL(Sal.Cog) / VAL(Sal.Qty); VAL(Sal.Prc);
			LPRINT USING "#######.##  "; VAL(Sal.Prc) / VAL(Sal.Qty); VAL(Sal.Prc) - VAL(Sal.Cog);
			LPRINT USING "  &"; Sal.Inv; Sal.Cod; Sal.Cus
	CASE 2: IF p$ <> Sal.Cod THEN GOSUB 410
	END SELECT
	
	T1 = T1 + VAL(Sal.Qty)
	T2 = T2 + VAL(Sal.Cog)
	T3 = T3 + VAL(Sal.Prc)
	NEXT

	GOSUB 410
	IF s = 0 OR s = 2 THEN LPRINT
	LPRINT TAB(37);
	LPRINT USING "#######.##   "; T4;
	IF T4 = 0 THEN
	   LPRINT USING "  #######.##"; T5; T5; T6; T6; T6 - T5;
	ELSE
	   LPRINT USING "  #######.##"; T5; T5 / T4; T6; T6 / T4; T6 - T5;
	END IF
	LPRINT USING "    ###.##%"; 100 * (T6 - T5) / T6
	GOTO 10
410 SELECT CASE s
		   CASE 0: GET #10, VAL(i$), Mat
				   LPRINT Mat.Typ; Mat.Num; "  "; Mat.Nam;
		   CASE 1: LPRINT : LineNum = LineNum + 2
		   CASE 2:
	END SELECT
	LPRINT TAB(37);
	LPRINT USING "#######.## &"; T1 / VAL(Mat.Fra);
	IF s THEN LPRINT "  ";  ELSE LPRINT Mat.Con;
	IF T1 = 0 THEN
	   LPRINT USING "  #######.##"; T2; 0; T3; 0; T3 - T2;
	   LPRINT USING "    ###.##%"; 0;
	ELSE
	   LPRINT USING "  #######.##"; T2; T2 / T1; T3; T3 / T1; T3 - T2;
	   LPRINT USING "    ###.##%"; 100 * (T3 - T2) / T3;
	END IF
	IF s THEN
	   IF s = 1 THEN LPRINT : LPRINT  ELSE LPRINT USING "  &"; p$; c$
	ELSE
	   LPRINT
	END IF
	LineNum = LineNum + 1: IF LineNum > 60 THEN IF s THEN HSales ELSE HSalesSum
	T4 = T4 + T1: T1 = 0
	T5 = T5 + T2: T2 = 0
	T6 = T6 + T3: T3 = 0
	p$ = Sal.Cod
	i$ = Sal.Itm
	c$ = Sal.Cus
	RETURN
RctReg: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	T1 = 0: d$ = DATE$
	COLOR 14, 0: LOCATE 23, 20: PRINT USING " Check Receipts Register for: & "; DATE$
425 EntryD 23, 50, e, b, d$: IF e THEN 10 ELSE IF b THEN 425
	PageNum = 0: HChecks: Printing
	e = LOF(8) \ LEN(Rct)
	FOR x = 1 TO e
	Complete x, e
	COLOR 3
	GET #8, x, Rct
	IF d$ = Rct.Dat THEN
	   COLOR 11
	   IF INKEY$ = CHR$(27) THEN 10
	   IF CusFind(Rct.Cus) = 0 THEN ERROR 200
	   r = 1
	   l = LEN(RTRIM$(Rct.Inv))
	   LPRINT USING "& "; Rct.Dat; Rct.Chk; Rct.Amt; Cnx.Nam;
	   IF l = 0 THEN LPRINT : LineNum = LineNum + 1: IF LineNum > 63 THEN HChecks
	   DO WHILE l > 1
		  LPRINT TAB(63); MID$(Rct.Inv, r, 96)
		  l = l - 96
		  r = r + 96
		  LineNum = LineNum + 1: IF LineNum > 63 THEN HChecks
	   LOOP
	   T1 = T1 + VAL(Rct.Amt)
	END IF
	NEXT
	LPRINT
	LPRINT TAB(20);
	LPRINT USING "##########.##"; T1
	GOTO 10
SalRpt: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	mSales
	GOTO 10
RctRpt: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	mReceipts
	GOTO 10
Lables: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	Printing
	LPRINT CHR$(27); "@"; CHR$(27); "E";
	e = LOF(3) \ LEN(Cnx)
	FOR x = 1 TO e
	IF INKEY$ = CHR$(27) THEN EXIT FOR
	Complete x, e
	GET #3, x, Cnx
	IF Cnx.Cod <> "þþþþ" THEN
	   GET #1, Cnx.Num, Cus
	   LPRINT Cus.Nam; "  "; Cus.Cod
	   LPRINT Cus.Add
	   LPRINT Cus.Str
	   LPRINT RTRIM$(Cus.Cty); ", "; Cus.Sta; " "; Cus.Zip
	   LPRINT
	   LPRINT
	END IF
	NEXT
	GOTO 10
IndexCards: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	Printing
	LPRINT CHR$(27); "@"; CHR$(27); "E";
	e = LOF(3) \ LEN(Cnx)
	FOR x = 1 TO e
	IF INKEY$ = CHR$(27) THEN EXIT FOR
	Complete x, e
	GET #3, x, Cnx
	IF Cnx.Cod <> "þþþþ" THEN
	   GET #1, Cnx.Num, Cus
			LPRINT CHR$(27); "-1";
	   LPRINT " "; Cus.Cod; "  "; Cus.Nam
			LPRINT CHR$(27); "-0";
	   LPRINT " "; "Tax ID: "; Cus.Tax; "   Date: "; Cus.Dat
			LPRINT CHR$(27); "-1";
	   LPRINT " "; "Billing Address   "; Cus.Tel
			LPRINT CHR$(27); "-0";
	   LPRINT " "; Cus.Man
	   LPRINT " "; Cus.Add
	   LPRINT " "; Cus.Str
			LPRINT CHR$(27); "-1";
	   LPRINT " "; RTRIM$(Cus.Cty); " "; Cus.Sta; " "; Cus.Zip
	   LPRINT " "; "Shipping Address  "; Cus.Ste
			LPRINT CHR$(27); "-0";
	   LPRINT " "; Cus.Sco
	   LPRINT " "; Cus.Sad
	   LPRINT " "; Cus.Ssr
	   LPRINT " "; RTRIM$(Cus.Sct); " "; Cus.Sst; " "; Cus.Szi
	   LPRINT
	END IF
	NEXT
	GOTO 10
Errors: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	SELECT CASE ERR
	CASE 200:  WinMssg "Unable to find customer code"
	CASE ELSE: WinMssg "Error:" + STR$(ERR) + "  Line:" + STR$(ERL)
	END SELECT
	KeyHit f$
Terminate: 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	CLOSE
	RUN "ARP"
	

DATA 31%,28%,31%,30%,31%,30%,31%,31%,30%,31%,30%,31%

DEFINT L, T
SUB AccDist ()
	ERASE Itm, Qty, Cog, Prc
	FOR x = 0 TO 14
		Itm(x) = MID$(Inv.Dst, 1 + 27 * x, 3)
		Qty(x) = MID$(Inv.Dst, 4 + 27 * x, 8)
		Cog(x) = MID$(Inv.Dst, 12 + 27 * x, 8)
		Prc(x) = MID$(Inv.Dst, 20 + 27 * x, 8)
		IF Itm(x) < "  0" THEN EXIT FOR
	NEXT
END SUB

SUB Complete (x, e)
	p& = 51 * (x / e)
	p& = p& + 15 - POS(0)
	PRINT STRING$(p&, "Û");
END SUB

FUNCTION CusFind (f$)
   b = 1: t = LOF(3) \ LEN(Cnx)
   DO UNTIL (t < b)
	  m = (t + b) \ 2: GET #3, m, Cnx
	  IF f$ = Cnx.Cod THEN CusFind = 1: EXIT DO
	  IF f$ > Cnx.Cod THEN b = m + 1 ELSE t = m - 1
   LOOP
END FUNCTION

DEFSNG A-Z
SUB HChecks ()
	 Heading
	 LPRINT TAB(72); "CHECK  REGISTER"
	 LPRINT
	 LPRINT "             Check      Check   "
	 LPRINT "Date Paid    Number     Amount          Customer Name         Invoices Paid With This Check"
	 LPRINT "========== ========== ========== ============================ "; STRING$(96, "=")
END SUB

SUB HCusBuy ()
	Heading
	LPRINT TAB(68); "CUSTOMER BUYING FRQUENCY": LPRINT
	LPRINT "CUS                                                                    QUANTITY    NUMBER   ELAPSED"
	LPRINT "NUM         CUSTOMER NAME            ITEM      IVENTORY DESCRIPTION    PURCHASED  OF TIMES   DAYS    FREQUENCY"
	LPRINT "====  ============================  ======  ========================== =========  ========  =======  =============================="
END SUB

SUB Heading ()
	LPRINT CHR$(12); CHR$(27); "@"; : WIDTH "LPT1:", 255: LPRINT CHR$(27); "M"
	PageNum = PageNum + 1: LineNum = 13
	LPRINT "Date: "; DATE$; TAB(61); "       McKENRY PRODUCE COMPANY        "; TAB(153); "Page: "; PageNum
	LPRINT "Time: "; TIME$; TAB(61); "717 Willow Avenue, Knoxville, TN 37915"
	LPRINT :         LPRINT TAB(61); "         ACCOUNTS RECEIVABLE          ": LPRINT
END SUB

SUB HitKey (f$)
	DO: f$ = INKEY$: LOOP WHILE f$ <> ""
	DO: f$ = INKEY$: TimeDate: LOOP WHILE f$ = ""
END SUB

SUB HRegister ()
	Heading
	LPRINT TAB(71); "INVOICE REGISTER": LPRINT
	LPRINT "INVOICE                                INVOICE                           CHECK      GROSS                NET      SALES     OTHER     TOTAL    AMOUNT    AMOUNT  "
	LPRINT "NUMBER         CUSTOMER NAME             DATE     DUE DATE  DATE PAID    NUMBER     SALES    CREDITS    SALES      TAX     CHARGES   CHARGES    PAID       DUE   "
	LPRINT "======== ============================ ========== ========== ========== ========== ========= ========= ========= ========= ========= ========= ========= ========="
END SUB

SUB HSales ()
	Heading
	LPRINT TAB(71); " SALES REGISTER ": LPRINT CHR$(27); "P";
	LPRINT
	LPRINT " ITEM                                               COST OF       UNIT       SALE        UNIT        GROSS      PROFIT    CUST                              "
	LPRINT "NUMBER     INVENTORY DESCRIPTION       QUANTITY      GOODS        COST       PRICE       PRICE       PROFIT     MARGIN    NUMB         CUSTOMER NAME        "
	LPRINT "======  ===========================  ============  ==========  ==========  ==========  ==========  ==========  =========  ====  ============================"
END SUB

SUB HSalesSum ()
	Heading
	LPRINT TAB(68); " INVENTORY DAILY SALES ": LPRINT CHR$(27); "P";
	LPRINT
	LPRINT " ITEM                                               COST OF       UNIT       SALE        UNIT        GROSS      PROFIT  "
	LPRINT "NUMBER     INVENTORY DESCRIPTION       QUANTITY      GOODS        COST       PRICE       PRICE       PROFIT     MARGIN  "
	LPRINT "======  ===========================  ============  ==========  ==========  ==========  ==========  ==========  ========="
END SUB

DEFINT A-K, M-S, U-Z
DEFLNG L
DEFDBL T
SUB IndexCards ()
END SUB

DEFINT L, T
SUB InvFind (n$, f, e)
   f = 0: e = 0
   b = 1: t = LOF(6) \ LEN(Inx)
   DO UNTIL (t < b)
	  m = (t + b) \ 2: GET #6, m, Inx
	  IF n$ > Inx.Cus THEN b = m + 1 ELSE t = m - 1
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
DEFDBL T
SUB mReceipts ()
	'total receipts between 2 dates
	CLS
	LOCATE 5, 10: PRINT "Check receipts total from date:";
	LOCATE 6, 10: PRINT "                       to date:";
603 EntryD 5, 43, e, b, f$: IF e THEN EXIT SUB ELSE IF b THEN 603
604 EntryD 6, 43, e, b, t$: IF e THEN EXIT SUB ELSE IF b THEN 603

	f$ = RIGHT$(f$, 4) + LEFT$(f$, 2) + MID$(f$, 4, 2)
	t$ = RIGHT$(t$, 4) + LEFT$(t$, 2) + MID$(t$, 4, 2)
	LOCATE 8, 10: PRINT "Please wait, to cancel press ESC";
	COLOR 1, 7

	e = LOF(8) \ LEN(Rct)
	FOR x = 1 TO e
	GET #8, x, Rct
	d$ = RIGHT$(Rct.Dat, 4) + LEFT$(Rct.Dat, 2) + MID$(Rct.Dat, 4, 2)
	IF d$ >= f$ AND d$ <= t$ THEN
	   IF INKEY$ = CHR$(27) THEN EXIT SUB
	   n = n + 1
	   Tr = Tr + VAL(Rct.Amt)
	END IF
	NEXT
	PRINT
	PRINT
	PRINT USING "  Total Number of Checks:  ##########,   "; n
	PRINT USING "    Total Check Receipts: $##########,.##"; Tr
	KeyHit f$
END SUB

DEFINT T
DEFLNG L
SUB mSales ()
	'total invoices between 2 dates
	CLS
	LOCATE 5, 10: PRINT "Sales total from date:";
	LOCATE 6, 10: PRINT "              to date:";
601 EntryD 5, 33, e, b, f$: IF e THEN EXIT SUB ELSE IF b THEN 601
602 EntryD 6, 33, e, b, t$: IF e THEN EXIT SUB ELSE IF b THEN 601

	f$ = RIGHT$(f$, 4) + LEFT$(f$, 2) + MID$(f$, 4, 2)
	t$ = RIGHT$(t$, 4) + LEFT$(t$, 2) + MID$(t$, 4, 2)
	LOCATE 8, 10: PRINT "Please wait, to cancel press ESC ";
	COLOR 1, 7
	e = LOF(4) \ LEN(Inv)
	FOR x = 1 TO e
	IF INKEY$ = CHR$(27) THEN EXIT SUB
	GET #4, x, Inv
	d$ = RIGHT$(Inv.Bdt, 4) + LEFT$(Inv.Bdt, 2) + MID$(Inv.Bdt, 4, 2)
	IF d$ >= f$ AND d$ <= t$ THEN
	   IF Inv.Chk = "Cash      " THEN
		  la = la + 100 * VAL(Inv.Net)
	   ELSE
		  lp = lp + 100 * VAL(Inv.Net)
	   END IF

	   AccDist
	   FOR y = 0 TO 14
	   IF Itm(y) > "  0" THEN
		  Sal.Cog = Cog(y)
		  Sal.Prc = Prc(y)
		  Lo = Lo + 100 * VAL(Cog(y))
	   ELSE EXIT FOR
	   END IF
	   NEXT

	   v = v + 1
	   lg = lg + 100 * VAL(Inv.Grs)
	   lc = lc + 100 * VAL(Inv.Crd)
	   lt = lt + 100 * VAL(Inv.Tax)
	   lf = lf + 100 * VAL(Inv.Frt)
	   Ln = Ln + 100 * VAL(Inv.Net)
	END IF
	NEXT
	LOCATE 12, 1
	PRINT USING " Number of Invoices    ########,"; v
	PRINT USING "   Gross sales   Total $########,.##"; lg / 100
	PRINT USING "   Credit        Total $########,.##"; lc / 100
	PRINT USING "   Sales Tax     Total $########,.##"; lt / 100
	PRINT USING "   Frieght       Total $########,.##"; lf / 100
	PRINT USING "   Net amount    Total $########,.##"; Ln / 100
	PRINT USING "   Cash Sales    Total $########,.##"; la / 100
	PRINT USING "   Credit Sales  Total $########,.##"; lp / 100
	PRINT USING "   Cost of Goods Total $########,.##"; Lo / 100
	KeyHit f$
END SUB

DEFSNG A-K, M-Q, S, U-Z
DEFDBL T
SUB OnHand (r)
	Lqt = Mat.Ybq + Mat.Ypq + Mat.Ymq
	Lqt = Lqt - Mat.Ypq - Mat.Ysq - Mat.Ywq - Mat.Ygq
	Lpr = Mat.Ybp + Mat.Ypp + Mat.Ymp
	Lpr = Lpr - Mat.Ypp - Mat.Ysc - Mat.Ywp - Mat.Ygp
	IF Lqt = 0 OR Lpr = 0 THEN Tup = VAL(Mat.Ref) ELSE Tup = Lpr / Lqt
END SUB

DEFSNG L, R, T
SUB OpenFiles ()
	OPEN "..\Data\Customer.MAS" FOR RANDOM LOCK READ WRITE AS #1 LEN = LEN(Cus)
	OPEN "..\Data\Customer.INX" FOR RANDOM LOCK READ WRITE AS #2 LEN = LEN(Cnx)
	OPEN "..\Data\Customer.NAM" FOR RANDOM LOCK READ WRITE AS #3 LEN = LEN(Cnx)
  
	OPEN "..\Data\Invoices.MAS" FOR RANDOM LOCK READ WRITE AS #4 LEN = LEN(Inv) 'Invoices Master
	OPEN "..\Data\Invoices.INX" FOR RANDOM LOCK READ WRITE AS #5 LEN = LEN(Inx) '
	OPEN "..\Data\Invoices.CUS" FOR RANDOM LOCK READ WRITE AS #6 LEN = LEN(Inx) 'Index by Customer
	OPEN "..\Data\Invoices.INV" FOR RANDOM LOCK READ WRITE AS #7 LEN = LEN(Inx) 'Index by invoice number
	OPEN "..\Data\Invoices.Chk" FOR RANDOM LOCK READ WRITE AS #8 LEN = LEN(Rct)
	OPEN "..\Data\Invoices.SAL" FOR RANDOM LOCK READ WRITE AS #9 LEN = LEN(Sal)

	OPEN "..\Data\Material.MAS" FOR RANDOM LOCK READ WRITE AS #10 LEN = LEN(Mat)
	OPEN "..\Data\Material.INX" FOR RANDOM LOCK READ WRITE AS #11 LEN = LEN(Mnx)
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

DEFINT A-Z
FUNCTION SalesCus ()
	CLOSE : OPEN "..\Data\Invoices.SAL" FOR OUTPUT AS #1
	CLOSE : OpenFiles
	CLOSE #9: OPEN "..\Data\Invoices.SAL" FOR RANDOM AS #9 LEN = LEN(Sac)
	dl$ = MonMath(DATE$, -2)
	dl$ = RIGHT$(dl$, 4) + LEFT$(dl$, 2) + MID$(dl$, 4, 2)
	e = LOF(7) \ LEN(Inx)
	FOR x = 1 TO e
	GET #6, x, Inx
	GET #4, Inx.Num, Inv
	di$ = RIGHT$(Inv.Bdt, 4) + LEFT$(Inv.Bdt, 2) + MID$(Inv.Bdt, 4, 2)
	IF INKEY$ = CHR$(27) THEN SalesCus = 1: EXIT FUNCTION
	IF di$ > dl$ THEN
	   AccDist
	   FOR y = 0 TO 14
	   IF Itm(y) > "  0" THEN
		  GET #10, VAL(Itm(y)), Mat
		  Sac.Typ = Mat.Typ
		  Sac.Itm = Itm(y)
		  Sac.Qty = Qty(y)
		  Sac.Cus = Inv.Cus
		  Sac.Dat = di$
		  PUT #9, , Sac
	   ELSE EXIT FOR
	   END IF
	   NEXT
	END IF
	NEXT
	IF LOC(9) = 0 THEN WinMssg "No sales for this date": SalesCus = 1: EXIT FUNCTION
	CLOSE
	COLOR 14, 0: LOCATE 23, 25: PRINT " Sorting Index File............ ";
	SHELL "AC.COM ..\Data\Invoices.SAL ..\Data\Invoices /L26 /+1,10 /+19,8 /Q"
	KILL "..\Data\Invoices.SAL"
	NAME "..\Data\Invoices" AS "..\Data\Invoices.SAL"
END FUNCTION

FUNCTION SalesInx (s, d$)
	CLOSE : OPEN "..\Data\Invoices.SAL" FOR OUTPUT AS #1
	CLOSE : OpenFiles
	e = LOF(7) \ LEN(Inx)
	FOR x = 1 TO e
	GET #7, x, Inx
	IF d$ = Inx.Dat THEN
	   GET #4, Inx.Num, Inv
	   IF INKEY$ = CHR$(27) THEN SalesInx = 1: EXIT FUNCTION
	   IF CusFind(Inv.Cus) = 0 THEN ERROR 200
	   AccDist
	   FOR y = 0 TO 14
	   IF Itm(y) > "  0" THEN
		  GET #10, VAL(Itm(y)), Mat
		  Sal.Typ = Mat.Typ
		  Sal.Itm = Itm(y)
		  Sal.Qty = Qty(y)
		  Sal.Cog = Cog(y)
		  Sal.Prc = Prc(y)
		  Sal.Mes = Mat.Mes
		  Sal.Mat = Mat.Nam
		  Sal.Cod = Cnx.Cod
		  Sal.Cus = Cnx.Nam
		  Sal.Dat = Inv.Bdt
		  Sal.Inv = Inv.Num
		  PUT #9, , Sal
	   ELSE EXIT FOR
	   END IF
	   NEXT
	END IF
	NEXT
	IF LOC(9) = 0 THEN WinMssg "No sales for this date": SalesInx = 1: EXIT FUNCTION
	CLOSE
	IF s THEN
	   SHELL "AC.COM ..\Data\Invoices.SAL ..\Data\Invoices /L110 /+7,4 /+1,6 /Q"
	ELSE
	   SHELL "AC.COM ..\Data\Invoices.SAL ..\Data\Invoices /L110 /+1,6 /+101,10 /Q"
	END IF
	KILL "..\Data\Invoices.SAL"
	NAME "..\Data\Invoices" AS "..\Data\Invoices.SAL"
END FUNCTION

DEFSNG A-Z
SUB TimeDate ()
 COLOR 0, 7
 LOCATE 10, 22, 0: PRINT " "; DATE$; " "
 t$ = " " + LEFT$(TIME$, 2)
 IF TIME$ > "11:59:59" THEN f$ = "pm " ELSE f$ = "am "
 IF TIME$ > "12:59:59" THEN t$ = " " + STR$(VAL(t$) - 12)
 LOCATE 10, 47: PRINT t$; MID$(TIME$, 3, 6) + f$
END SUB

DEFINT A-Z
SUB WinCus (f$)
	SCREEN , , 1, 1
	PCOPY 0, 1
	n = 1
	Top = LOF(3) \ LEN(Cnx)
	Bot = 1
	r = 7
	GOSUB C2
C1: GET #3, n, Cnx
	COLOR 15, 5: LOCATE r, 22: PRINT USING "&ณ&"; Cnx.Cod; Cnx.Nam
	KeyHit n$
	COLOR 11, 1: LOCATE r, 22: PRINT USING "&ณ&"; Cnx.Cod; Cnx.Nam
	SELECT CASE n$
	CASE CHR$(13)
		 f$ = Cnx.Cod
		 SCREEN , , 0, 0
		 COLOR 14, 0
		 EXIT SUB
	CASE CHR$(0) + "P", CHR$(0) + "M" 'Dn
		 IF n + 1 > Top GOTO C1 ELSE n = n + 1
		 IF r + 1 > 20 THEN CALL scroll(22, 7, 54, 20, 1) ELSE r = r + 1
	CASE CHR$(0) + "H", CHR$(0) + "K" 'Up
		 IF n - 1 < 1 GOTO C1 ELSE n = n - 1
		 IF r - 1 < 7 THEN CALL BkScroll(22, 7, 54, 20, 1) ELSE r = r - 1
	CASE CHR$(0) + "I" 'PgUp
		 Bot = n - r - 7: IF Bot < 1 THEN Bot = 1
		 GOSUB C2: n = Bot + r - 7
	CASE CHR$(0) + "Q" 'PgDn
		 Bot = 21 - r + n
		 IF Bot > Top - 13 THEN Bot = Top - 13: n = Top - 20 + r ELSE n = n + 14
		 GOSUB C2
	CASE CHR$(0) + "G" 'Home
		 Bot = 1: GOSUB C2
		 n = r - 6
	CASE CHR$(0) + "O" 'End
		 Bot = Top - 13: GOSUB C2
		 n = Top - 20 + r
	CASE ELSE
		 f$ = ""
		 SCREEN , , 0, 0
		 COLOR 14, 0
		 EXIT SUB
	END SELECT
	GOTO C1
 
C2: COLOR 11, 1: LOCATE 6, 20: PRINT " ษออออัออออออออออออออออออออออออออออป "
	FOR x = Bot TO Top: GET #3, x, Cnx
	LOCATE , 20: PRINT USING " บ&ณ&บ "; Cnx.Cod; Cnx.Nam
	IF CSRLIN > 20 THEN EXIT FOR
	NEXT
	LOCATE , 20: PRINT " ศออออฯออออออออออออออออออออออออออออผ ": COLOR 15
	LOCATE , 20: PRINT "  "; CHR$(17); "ู "; CHR$(24); " "; CHR$(25); "  PgUp  PgDn  Home  End  Esc "
	RETURN
END SUB

SUB WinMat (f$)
	SCREEN , , 1, 1
	PCOPY 0, 1
	n = 1
	Top = LOF(11) \ LEN(Mnx)
	Bot = 1
	r = 7
	GOSUB T2
T1: GET #11, n, Mnx
	COLOR 15, 5: LOCATE r, 22: PRINT USING "&###ณ&"; Mnx.Typ; Mnx.Num; Mnx.Nam
	KeyHit n$
	COLOR 11, 1: LOCATE r, 22: PRINT USING "&###ณ&"; Mnx.Typ; Mnx.Num; Mnx.Nam
	SELECT CASE n$
	CASE CHR$(13)
		 Picture r, 25, 3, f$
		 SCREEN , , 0, 0
		 COLOR 14, 0
		 EXIT SUB
	CASE CHR$(0) + "P", CHR$(0) + "M" 'Dn
		 IF n + 1 > Top GOTO T1 ELSE n = n + 1
		 IF r + 1 > 20 THEN CALL scroll(22, 7, 54, 20, 1) ELSE r = r + 1
	CASE CHR$(0) + "H", CHR$(0) + "K" 'Up
		 IF n - 1 < 1 GOTO T1 ELSE n = n - 1
		 IF r - 1 < 7 THEN CALL BkScroll(22, 7, 54, 20, 1) ELSE r = r - 1
	CASE CHR$(0) + "I" 'PgUp
		 Bot = n - r - 7: IF Bot < 1 THEN Bot = 1
		 GOSUB T2: n = Bot + r - 7
	CASE CHR$(0) + "Q" 'PgDn
		 Bot = 21 - r + n
		 IF Bot > Top - 13 THEN Bot = Top - 13: n = Top - 20 + r ELSE n = n + 14
		 GOSUB T2
	CASE CHR$(0) + "G" 'Home
		 Bot = 1: GOSUB T2
		 n = r - 6
	CASE CHR$(0) + "O" 'End
		 Bot = Top - 13: GOSUB T2
		 n = Top - 20 + r
	CASE ELSE
		 f$ = ""
		 SCREEN , , 0, 0
		 COLOR 14, 0
		 EXIT SUB
	END SELECT
	GOTO T1

T2: COLOR 11, 1: LOCATE 6, 20: PRINT " ษออออออัออออออออออออออออออออออออออป "
	FOR x = Bot TO Top: GET #11, x, Mnx
	LOCATE , 20: PRINT USING " บ&###ณ&บ "; Mnx.Typ; Mnx.Num; Mnx.Nam
	IF CSRLIN > 20 THEN EXIT FOR
	NEXT
	LOCATE , 20: PRINT " ศออออออฯออออออออออออออออออออออออออผ ": COLOR 15
	LOCATE , 20: PRINT "  "; CHR$(17); "ู "; CHR$(24); " "; CHR$(25); "  PgUp  PgDn  Home  End  Esc "
	RETURN
END SUB

DEFSNG A-Z
SUB WinMssg (f$)
	SCREEN , , 1, 1
	PCOPY 0, 1
	COLOR 14, 4: LOCATE 12
	LOCATE , 36: PRINT " ษอ"; STRING$(LEN(f$), "อ"); "อป "
	LOCATE , 36: PRINT " บ "; f$; " บ "
	LOCATE , 36: PRINT " ศอ"; STRING$(LEN(f$), "อ"); "อผ "
	COLOR 12, 0: LOCATE 13
	LOCATE , 35: PRINT "Û"
	LOCATE , 35: PRINT "Û"
	LOCATE , 35: PRINT "฿฿฿"; STRING$(LEN(f$) + 2, "฿")
	SOUND 660, .1: SOUND 770, .1: SLEEP 2
	SCREEN , , 0, 0
END SUB


DEFINT A-Z: DEFLNG L: DEFCUR T
DECLARE SUB HAging ()
DECLARE SUB Heading ()
DECLARE SUB HCusInv ()
DECLARE SUB HCusYear ()
DECLARE SUB HCusMonth ()
DECLARE SUB HInvoices ()
DECLARE SUB HStatement ()

DECLARE SUB KeyHit (f$)
DECLARE SUB HitKey (f$)
DECLARE SUB WinCus (f$)
DECLARE SUB WinMssg (f$)

DECLARE SUB mInvoices ()
DECLARE SUB AccDist (t@)
DECLARE SUB InvSort ()
DECLARE SUB Monitor ()
DECLARE SUB TimeDate ()
DECLARE SUB Printing ()
DECLARE SUB OpenFiles ()
DECLARE SUB Complete (x, p)
DECLARE SUB InvFind (n$, f, e)
DECLARE SUB Remarks (N1$, N2$, e)

DECLARE FUNCTION CusFind (f$)
REM $INCLUDE: '..\Include\Library.Dec'
REM $INCLUDE: '..\Include\Customer.Rec'
REM $INCLUDE: '..\Include\SInvoice.Rec'

	DIM SHARED Cus AS Customer
	DIM SHARED Cnx AS CusIndex
	DIM SHARED Inv AS Invoices
	DIM SHARED Inx AS InvIndex

	DIM SHARED Days(12) AS INTEGER
	DIM SHARED LineNum AS INTEGER
	DIM SHARED PageNum AS INTEGER
	DIM SHARED Segment AS INTEGER
	DIM SHARED Itm(0 TO 15) AS STRING * 3
	DIM SHARED Cog(0 TO 15) AS STRING * 8
	
	ON ERROR GOTO Errors
	FOR Y = 1 TO 12: READ Days(Y): NEXT

KEY OFF: KEY 1, "1": KEY 2, "2": KEY 3, "3": KEY 4, "4": KEY 5, "5"
		 KEY 6, "6": KEY 7, "7": KEY 8, "8": KEY 9, "9": KEY 10, "0"
	Monitor
	CLS
10  CLOSE : WIDTH 80, 25: DEF SEG = Segment: BLOAD "Reports.Scr", 0: DEF SEG
	HitKey f$: f = VAL(f$)
	IF f$ = "0" THEN f = 10
	OpenFiles
	IF f$ = CHR$(27) GOTO Terminate
	ON f GOTO Aging, CusInv, InvTyp, CusMon, CusYear, StaRpt, StaCus, StaTyp, InvList
	GOTO 10
InvList: 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	mInvoices
	GOTO 10
Aging: 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	PageNum = 0: HAging: Printing
	T1 = 0: T2 = 0: T3 = 0: T4 = 0: T5 = 0: T6 = 0: T7 = 0: T8 = 0: T9 = 0: T0 = 0: T10 = 0
	TA = 0: TB = 0: TC = 0: TD = 0: TE = 0: TF = 0: TG = 0: TH = 0: TI = 0: TJ = 0: TK = 0
	p = LOF(6) \ LEN(Inx)
	FOR x = 1 TO p
		IF INKEY$ = CHR$(27) THEN EXIT FOR
		Complete x, p: COLOR 11
		GET #6, x, Inx
		GET #4, Inx.Num, Inv
		IF Inv.Pmt <> Inv.Net THEN
		   COLOR 3
		   IF p$ <> Inv.Cus THEN
			  GOSUB 532
			  IF CusFind(Inv.Cus) = 0 THEN ERROR 200
			  LPRINT USING "& &"; Cnx.Cod; Cnx.Nam; : p$ = Inv.Cus
		   END IF
		   LPRINT TAB(35); Inv.Ddt; " "; Inx.Inv;
		   d = DateDif(DATE$, Inv.Ddt)
		   TT = VAL(Inv.Net) - VAL(Inv.Pmt)
		   IF d < -28 THEN Y = 56: T1 = T1 + TT:  GOTO 530
		   IF d < -21 THEN Y = 66: T2 = T2 + TT:  GOTO 530
		   IF d < -14 THEN Y = 76: T3 = T3 + TT:  GOTO 530
		   IF d < -7 THEN Y = 86:  T4 = T4 + TT:  GOTO 530
		   IF d < 0 THEN Y = 96:   T5 = T5 + TT:  GOTO 530
		   IF d = 0 THEN Y = 106:  T6 = T6 + TT:  GOTO 530
		   IF d > 28 THEN Y = 156: T10 = T10 + TT: GOTO 530
		   IF d > 21 THEN Y = 146: T0 = T0 + TT:  GOTO 530
		   IF d > 14 THEN Y = 136: T9 = T9 + TT:  GOTO 530
		   IF d > 7 THEN Y = 126:  T8 = T8 + TT:  GOTO 530
		   IF d > 0 THEN Y = 116:  T7 = T7 + TT
530        LPRINT TAB(Y); : LPRINT USING "#####.##"; TT
		   LineNum = LineNum + 1: IF LineNum > 60 THEN HAging
		END IF
	NEXT
	GOSUB 532: LPRINT : LPRINT TAB(55);
	LPRINT USING "######.## "; TA; TB; TC; TD; TE; TF; TG; TH; TI; TJ; TK
	GOTO 10
532 LPRINT CHR$(27); "G"
	LPRINT TAB(55);
	LPRINT USING "######.## "; T1; T2; T3; T4; T5; T6; T7; T8; T9; T0; T10;
	LPRINT CHR$(27); "H"
	TA = TA + T1: T1 = 0
	TB = TB + T2: T2 = 0
	TC = TC + T3: T3 = 0
	TD = TD + T4: T4 = 0
	TE = TE + T5: T5 = 0
	TF = TF + T6: T6 = 0
	TG = TG + T7: T7 = 0
	TH = TH + T8: T8 = 0
	TI = TI + T9: T9 = 0
	TJ = TJ + T0: T0 = 0
	TK = TK + T10: T10 = 0
	LineNum = LineNum + 3: IF LineNum > 60 THEN HAging
	RETURN
CusInv: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	PageNum = 0: HCusInv: Printing
	T0 = 0: T1 = 0: T2 = 0: T3 = 0: T4 = 0: T5 = 0
	e = LOF(6) \ LEN(Inx)
	FOR x = 1 TO e
	IF INKEY$ = CHR$(27) THEN EXIT FOR
	Complete x, e: COLOR 11
	GET #6, x, Inx
	GET #4, Inx.Num, Inv
	IF Inv.Net <> Inv.Pmt THEN
	   IF p$ <> Inv.Cus THEN GOSUB 300
	   T0 = T0 + VAL(Inv.Net)
	   T1 = T1 + VAL(Inv.Pmt)
	   T4 = T4 + VAL(Inv.Net) - VAL(Inv.Pmt)
	   COLOR 3
	   LPRINT TAB(41);
	   LPRINT USING "&   "; Inv.Num; Inv.Bdt; Inv.Ddt;
	   LPRINT USING "  &   "; Inv.Net; Inv.Pmt;
	   LPRINT USING "&   "; Inv.Pdt; Inv.Chk;
	   LPRINT USING "########,.##    ########,.##"; VAL(Inv.Net) - VAL(Inv.Pmt); T0 - T1
	   LineNum = LineNum + 1: IF LineNum > 60 THEN HCusInv
	END IF
	NEXT
	IF T0 THEN
	   LPRINT
	   LPRINT TAB(78);
	   LPRINT USING "#######.##   "; T0; T1
	   LPRINT
	END IF
	T2 = T2 + T0
	T3 = T3 + T1
	T5 = T2 - T3
	LPRINT TAB(78); :  LPRINT USING "#######.##   "; T2; T3;
	LPRINT TAB(126); : LPRINT USING "      #######.##"; T4; T5
	GOTO 10
300 IF T0 <> 0 THEN
	   LPRINT : LPRINT TAB(78);
	   LPRINT USING "#######.##   "; T0; T1
	   LPRINT
	   LineNum = LineNum + 3
	END IF
	p$ = Inv.Cus
	IF CusFind(Inx.Cus) = 0 THEN ERROR 200
	IF LineNum > 60 THEN HCusInv
	LPRINT USING "&   "; Cnx.Cod; Cnx.Nam;
	T2 = T2 + T0
	T3 = T3 + T1
	T5 = T2 - T3
	T0 = 0
	T1 = 0
	RETURN
InvTyp: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	CLS
	c$ = " "

	COLOR 14, 1: LOCATE 13, 17: PRINT " Print customers' open invoices by type:    "
400 EntryU 13, 58, 1, e, b, c$: IF e THEN 10 ELSE IF b THEN 400
	PageNum = 0: HCusInv: Printing
	T0 = 0: T1 = 0: T2 = 0: T3 = 0
	p = LOF(2) \ LEN(Cnx)
	FOR c = 1 TO p
		IF INKEY$ = CHR$(27) THEN EXIT FOR
		Complete c, p: COLOR 11
		GET #2, c, Cnx
		GET #1, Cnx.Num, Cus
		IF Cus.Typ = c$ THEN                     ' requested type?
		   IF Cus.Ynt + Cus.Ybb <> Cus.Ypd THEN
			  InvFind Cnx.Cod, b, e                 ' get customer invoices
			  IF e >= b THEN
				 GOSUB 410
				 FOR x = b TO e
					 GET #6, x, Inx
					 GET #4, Inx.Num, Inv
					 IF Inv.Net <> Inv.Pmt THEN
						T0 = T0 + VAL(Inv.Net)
						T1 = T1 + VAL(Inv.Pmt)
						COLOR 3
						LPRINT TAB(41);
						LPRINT USING "&   "; Inv.Num; Inv.Bdt; Inv.Ddt;
						LPRINT USING "  &   "; Inv.Net; Inv.Pmt;
						LPRINT USING "&   "; Inv.Pdt; Inv.Chk;
						LPRINT USING "########,.##    ########,.##"; VAL(Inv.Net) - VAL(Inv.Pmt); T0 - T1
						LineNum = LineNum + 1: IF LineNum > 60 THEN HCusInv
					 END IF
				 NEXT
			  END IF
		   END IF
		END IF
	NEXT
	LPRINT : LPRINT TAB(78);
	LPRINT USING "#######.##   "; T0; T1
	LPRINT
	LPRINT TAB(78); : LPRINT USING "#######.##   "; T2 + T0; T3 + T1
	GOTO 10
410 IF T0 THEN
	   LPRINT : LPRINT TAB(78);
	   LPRINT USING "#######.##   "; T0; T1
	   LPRINT : LineNum = LineNum + 3
	ELSE
	   LPRINT
	END IF
	IF LineNum > 60 THEN HCusInv
	LPRINT USING "&   "; Cnx.Cod; Cnx.Nam;
	T2 = T2 + T0: T0 = 0
	T3 = T3 + T1: T1 = 0
	RETURN
CusMon: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	PageNum = 0: HCusMonth: Printing
	L0 = 0: L1 = 0: L2 = 0: L3 = 0
	L4 = 0: L5 = 0: L6 = 0: L7 = 0
	e = LOF(2) \ LEN(Cnx)
	FOR x = 1 TO e
	IF INKEY$ = CHR$(27) THEN EXIT FOR
	Complete x, e
	GET #2, x, Cnx
	IF Cnx.Cod <> "þþþþ" THEN
	   GET #1, Cnx.Num, Cus
	   LineNum = LineNum + 1: IF LineNum > 60 THEN HCusMonth
	   LPRINT USING "& &"; Cus.Cod; LEFT$(Cus.Nam, 26);
	   LPRINT USING " #######.##"; Cus.Mgr / 100; Cus.Mcr / 100; Cus.Mtx / 100; Cus.Mfr / 100; Cus.Mnt / 100;
	   LPRINT USING " #######.##"; Cus.Mbb / 100; (Cus.Mbb + Cus.Mnt) / 100; Cus.Mpd / 100; (Cus.Mbb + Cus.Mnt - Cus.Mpd) / 100;
	   LPRINT USING " #######.##"; (Cus.Mgr - Cus.Mcr) / 100; Cus.Mco / 100; (Cus.Mgr - Cus.Mcr - Cus.Mco) / 100
	   L0 = L0 + Cus.Mgr
	   L1 = L1 + Cus.Mcr
	   L2 = L2 + Cus.Mtx
	   L3 = L3 + Cus.Mfr
	   L4 = L4 + Cus.Mnt
	   L5 = L5 + Cus.Mbb
	   L6 = L6 + Cus.Mpd
	   L7 = L7 + Cus.Mco
	END IF
	NEXT
	LPRINT : LPRINT TAB(32);
	LPRINT USING " #######.##"; L0 / 100; L1 / 100; L2 / 100; L3 / 100; L4 / 100; L5 / 100; (L4 + L5) / 100; L6 / 100; (L4 + L5 - L6) / 100;
	LPRINT USING " #######.##"; (L0 - L1) / 100; L7 / 100; (L0 - L1 - L7) / 100
	GOTO 10
CusYear: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	PageNum = 0: HCusYear: Printing
	L0 = 0: L1 = 0: L2 = 0: L3 = 0
	L4 = 0: L5 = 0: L6 = 0: L7 = 0
	e = LOF(2) \ LEN(Cnx)
	FOR x = 1 TO e
	IF INKEY$ = CHR$(27) THEN EXIT FOR
	Complete x, e
	GET #2, x, Cnx
	IF Cnx.Cod <> "þþþþ" THEN
	   GET #1, Cnx.Num, Cus
	   LineNum = LineNum + 1: IF LineNum > 60 THEN HCusYear
	   LPRINT USING "& &"; Cus.Cod; LEFT$(Cus.Nam, 26);
	   LPRINT USING " #######.##"; Cus.Ygr / 100; Cus.Ycr / 100; Cus.Ytx / 100; Cus.Yfr / 100; Cus.Ynt / 100;
	   LPRINT USING " #######.##"; Cus.Ybb / 100; (Cus.Ybb + Cus.Ynt) / 100; Cus.Ypd / 100; (Cus.Ybb + Cus.Ynt - Cus.Ypd) / 100;
	   LPRINT USING " #######.##"; (Cus.Ygr - Cus.Ycr) / 100; Cus.Yco / 100; (Cus.Ygr - Cus.Ycr - Cus.Yco) / 100
	   L0 = L0 + Cus.Ygr
	   L1 = L1 + Cus.Ycr
	   L2 = L2 + Cus.Ytx
	   L3 = L3 + Cus.Yfr
	   L4 = L4 + Cus.Ynt
	   L5 = L5 + Cus.Ybb
	   L6 = L6 + Cus.Ypd
	   L7 = L7 + Cus.Yco
	END IF
	NEXT
	LPRINT : LPRINT TAB(32);
	LPRINT USING " #######.##"; L0 / 100; L1 / 100; L2 / 100; L3 / 100; L4 / 100; L5 / 100; (L4 + L5) / 100; L6 / 100; (L4 + L5 - L6) / 100;
	LPRINT USING " #######.##"; (L0 - L1) / 100; L7 / 100; (L0 - L1 - L7) / 100
	GOTO 10
StaRpt: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	CLS
	c$ = "    "
	COLOR 14, 1: LOCATE 13, 20: PRINT "   Reprint Selected Statement for:       "
500 EntryU 13, 55, 4, e, b, c$: IF e THEN 10 ELSE IF b THEN 500
	IF CusFind(c$) = 0 THEN WinCus c$: IF c$ = "" THEN 500
	GET #1, Cnx.Num, Cus
	LOCATE 13, 55: PRINT Cus.Cod
	InvFind c$, b, e: p = e - b
	IF p < 0 THEN WinMssg "Customer has no invoices": GOTO 500
	Remarks N1$, N2$, q: IF q THEN 10
	Printing
	T0 = 0: T1 = 0
	PageNum = 0: HStatement
	FOR x = b TO e
		IF INKEY$ = CHR$(27) THEN EXIT FOR
		Complete x - b, p: COLOR 3
		GET #6, x, Inx
		IF Inx.Mrk = "R" OR Inx.Mrk = " " THEN
		   GET #4, Inx.Num, Inv
		   IF LineNum + 1 > 20 THEN
			  LPRINT
			  LPRINT " "; N1$; TAB(72);
			  LPRINT USING "######.##         ######.##"; T0 - T1; T0 - T1
			  LPRINT " "; N2$
			  LPRINT
			  LPRINT
			  LPRINT
			  LPRINT
			  HStatement
		   END IF
		   LineNum = LineNum + 1: COLOR 11
		   LPRINT USING " &"; Inv.Num; Inv.Bdt; Inv.Ddt; Inv.Net; Inv.Pmt; Inv.Pdt; Inv.Chk;
		   LPRINT USING " #####.##"; VAL(Inv.Net) - VAL(Inv.Pmt);
		   LPRINT USING " & #####.##"; Inv.Num; VAL(Inv.Net) - VAL(Inv.Pmt)
		   T0 = T0 + VAL(Inv.Net)
		   T1 = T1 + VAL(Inv.Pmt)
		END IF
	NEXT
	FOR n = LineNum TO 19: LPRINT : NEXT
	LPRINT
	LPRINT " "; N1$; TAB(72);
	LPRINT USING "######.##         ######.##"; T0 - T1; T0 - T1
	LPRINT " "; N2$
	LPRINT
	LPRINT
	LPRINT
	LPRINT
	GOTO 10
StaCus: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	CLS
	c$ = "    "
	COLOR 14, 1: LOCATE 13, 20: PRINT "    Print Selected Statement for:        "
600 EntryU 13, 54, 4, e, b, c$: IF e THEN 10 ELSE IF b THEN 600
	IF CusFind(c$) = 0 THEN WinCus c$: IF c$ = "" THEN 600
	GET #1, Cnx.Num, Cus
	LOCATE 13, 54: PRINT Cus.Cod
	InvFind c$, b, e: p = e - b
	IF p < 0 THEN WinMssg "Customer has no invoices": GOTO 600
	Remarks N1$, N2$, q: IF q THEN 10
	Printing
	T0 = 0: T1 = 0
	PageNum = 0: HStatement
	FOR x = b TO e
		IF INKEY$ = CHR$(27) THEN EXIT FOR
		Complete x - b, p: COLOR 3
		GET #6, x, Inx
		IF Inx.Mrk = " " THEN
		   GET #4, Inx.Num, Inv
		   IF LineNum + 1 > 20 THEN
			  LPRINT
			  LPRINT " "; N1$; TAB(72);
			  LPRINT USING "######.##         ######.##"; T0 - T1; T0 - T1
			  LPRINT " "; N2$
			  LPRINT
			  LPRINT
			  LPRINT
			  LPRINT
			  HStatement
		   END IF
		   LineNum = LineNum + 1: COLOR 11
		   LPRINT USING " &"; Inv.Num; Inv.Bdt; Inv.Ddt; Inv.Net; Inv.Pmt; Inv.Pdt; Inv.Chk;
		   LPRINT USING " #####.##"; VAL(Inv.Net) - VAL(Inv.Pmt);
		   LPRINT USING " & #####.##"; Inv.Num; VAL(Inv.Net) - VAL(Inv.Pmt)
		   T0 = T0 + VAL(Inv.Net)
		   T1 = T1 + VAL(Inv.Pmt)
		   IF Inv.Net = Inv.Pmt THEN       ' if invoice is paid
			  GET #5, Inx.Num, Inx         ' mark it with an R
			  Inx.Mrk = "R"                ' for recent invoice
			  PUT #5, Inx.Num, Inx         '
		   END IF                          '
		ELSE                               '
		   GET #5, Inx.Num, Inx            ' otherwise mark replace
		   Inx.Mrk = "O"                   ' the marker R with O
		   PUT #5, Inx.Num, Inx            ' for old invoice
		END IF
	NEXT
	FOR n = LineNum TO 19: LPRINT : NEXT
	LPRINT
	LPRINT " "; N1$; TAB(72);
	LPRINT USING "######.##         ######.##"; T0 - T1; T0 - T1
	LPRINT " "; N2$
	LPRINT
	LPRINT
	LPRINT
	LPRINT
	InvSort
	GOTO 10
StaTyp: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	CLS
	c$ = " "
	COLOR 14, 1: LOCATE 13, 20: PRINT " Print statements for customers type:    "
700 EntryU 13, 58, 1, e, b, c$: IF e THEN 10 ELSE IF b THEN 700
	Remarks N1$, N2$, q: IF q THEN 10
	Printing
	p = LOF(2) \ LEN(Cnx)
	FOR c = 1 TO p
		IF INKEY$ = CHR$(27) THEN EXIT FOR
		Complete c, p: COLOR 3
		GET #2, c, Cnx
		GET #1, Cnx.Num, Cus
		IF Cnx.Nam > "!" THEN                       ' active customer?
		   IF Cus.Typ = c$ THEN                     ' requested type?
			  InvFind Cnx.Cod, b, e                 ' get customer invoices
													' print if balance and invoices
			  IF e - b >= 0 THEN
				 IF (Cus.Ybb + Cus.Ynt - Cus.Ypd) <> 0 THEN
					T0 = 0: T1 = 0: COLOR 11           ' initialize
					PageNum = 0: HStatement            ' and start printing
					FOR x = b TO e                     ' check invoices
					GET #6, x, Inx                     '
					IF Inx.Mrk = " " THEN              ' if marker is null
					   GET #4, Inx.Num, Inv
					   IF LineNum + 1 > 20 THEN
						  LPRINT
						  LPRINT " "; N1$; TAB(72);
						  LPRINT USING "######.##         ######.##"; T0 - T1; T0 - T1
						  LPRINT " "; N2$
						  LPRINT
						  LPRINT
						  LPRINT
						  LPRINT
						  HStatement
					   END IF
					   LineNum = LineNum + 1
					   LPRINT USING " &"; Inv.Num; Inv.Bdt; Inv.Ddt; Inv.Net; Inv.Pmt; Inv.Pdt; Inv.Chk;
					   LPRINT USING " #####.##"; VAL(Inv.Net) - VAL(Inv.Pmt);
					   LPRINT USING " & #####.##"; Inv.Num; VAL(Inv.Net) - VAL(Inv.Pmt)
					   T0 = T0 + VAL(Inv.Net)          '
					   T1 = T1 + VAL(Inv.Pmt)          '
					   IF Inv.Net = Inv.Pmt THEN       ' if invoice is paid
						  GET #5, Inx.Num, Inx         ' mark it with an R
						  Inx.Mrk = "R"                ' for recent invoice
						  PUT #5, Inx.Num, Inx         '
					   END IF                          '
					ELSE                               '
					   GET #5, Inx.Num, Inx            ' otherwise mark replace
					   Inx.Mrk = "O"                   ' the marker R with O
					   PUT #5, Inx.Num, Inx            ' for old invoice
					END IF
					NEXT
					FOR n = LineNum TO 19: LPRINT : NEXT
					LPRINT
					LPRINT " "; N1$; TAB(72);
					LPRINT USING "######.##         ######.##"; T0 - T1; T0 - T1
					LPRINT " "; N2$
					LPRINT
					LPRINT
					LPRINT
					LPRINT
				 ELSE
					FOR x = b TO e                     ' check invoices
						GET #6, x, Inx                 '
						GET #4, Inx.Num, Inv
						IF Inv.Net = Inv.Pmt THEN      ' if invoice is paid
						   GET #5, Inx.Num, Inx        ' mark it with an R
						   IF Inx.Mrk = " " THEN       ' if marker is null
							  Inx.Mrk = "R"            ' for recent invoice
						   ELSE
							  Inx.Mrk = "O"            ' the marker R with O
						   END IF
						   PUT #5, Inx.Num, Inx         '
						END IF                          '
					NEXT
				 END IF
			  END IF
		   END IF
		END IF
	NEXT
	InvSort
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

SUB AccDist (t@)
	ERASE Cog
	t@ = 0
	FOR x = 0 TO 14
		Itm(x) = MID$(Inv.Dst, 1 + 27 * x, 3)
		IF Itm(x) < "  0" THEN EXIT FOR
		Cog(x) = MID$(Inv.Dst, 12 + 27 * x, 8)
		t@ = t@ + VAL(Cog(x))
	NEXT
END SUB

SUB Complete (x, e)
	p = 51 * ((x + 1) / (e + 1))
	p = p + 15 - POS(0)
	PRINT STRING$(p, "Û");
END SUB

FUNCTION CusFind (f$)
   b = 1: t = LOF(2) \ LEN(Cnx)
   DO UNTIL (t < b)
	  m = (t + b) \ 2: GET #2, m, Cnx
	  IF f$ = Cnx.Cod THEN CusFind = 1: EXIT DO
	  IF f$ > Cnx.Cod THEN b = m + 1 ELSE t = m - 1
   LOOP
END FUNCTION

DEFSNG A-Z
SUB HAging ()
	Heading
	LPRINT TAB(70); "AGING OF RECEIVABLES"
	LPRINT : LPRINT
	LPRINT "                                     Due     Invoice   _________________P A S T   D U E________________    Due     _______________D U E   L A T E R________________"
	LPRINT "Code       Customer Name             Date     Number   >28 Days  >21 Days  >14 Days   >7 Days   >1 Day    Today     >1 Day    >7 Days  >14 Days  >21 Days  >28 Days"
	LPRINT "==== ============================ ========== ========  ========  ========  ========  ========  ========  ========  ========  ========  ========  ========  ========"
END SUB

SUB HChecks ()
	 Heading
	 LPRINT TAB(72); "CHECK  REGISTER": LPRINT
	 LPRINT "             Check      Check   "
	 LPRINT "Date Paid    Number     Amount          Customer Name         Invoices Paid With This Check"
	 LPRINT "========== ========== ========== ============================ ============================================================================================================"
END SUB

SUB HCusInv ()
	Heading
	LPRINT TAB(69); "CUSTOMER OPEN INVOICES"
	LPRINT : LPRINT
	LPRINT "                                       Invoice      Invoice                   Invoice                                 Check                                  "
	LPRINT "Code         Customer Name              Number        Date       Due Date      Amount       Payment    Date Paid      Number      Amount Due     Balance Due "
	LPRINT "====   ============================   ==========   ==========   ==========   ==========   ==========   ==========   ==========   ============   ============="
END SUB

SUB HCusMonth ()
	Heading
	LPRINT TAB(68); "CUSTOMER LISTING [MONTH]"
	LPRINT : LPRINT
	LPRINT "                                  Gross     Credits     Sales       Other      Net     Beginning    Total      Total     Balance   ================================"
	LPRINT "Code       Customer Name          Sales     Returns      Tax       Charges    Amount    Balance      Due      Receipts     Due     Net Sales     Cost      Profit  "
	LPRINT "==== ========================== ========== ========== ========== ========== ========== ========== ========== ========== ========== ========== ========== =========="
END SUB

SUB HCusYear ()
	Heading
	LPRINT TAB(68); "CUSTOMER LISTING [YEAR]"
	LPRINT : LPRINT
	LPRINT "                                  Gross     Credits     Sales       Other      Net     Beginning    Total      Total     Balance   ================================"
	LPRINT "Code       Customer Name          Sales     Returns      Tax       Charges    Amount    Balance      Due      Receipts     Due     Net Sales     Cost      Profit  "
	LPRINT "==== ========================== ========== ========== ========== ========== ========== ========== ========== ========== ========== ========== ========== =========="
END SUB

SUB Heading ()
	LPRINT CHR$(12); CHR$(27); "@"; : WIDTH "LPT1:", 255: LPRINT CHR$(27); "M"
	PageNum = PageNum + 1: LineNum = 13
	LPRINT "Date: "; DATE$; TAB(61); "       McKENRY PRODUCE COMPANY        "; TAB(153); "Page: "; PageNum
	LPRINT "Time: "; TIME$; TAB(61); "717 Willow Avenue, Knoxville, TN 37915"
	LPRINT :         LPRINT TAB(61); "         ACCOUNTS RECEIVABLE          ": LPRINT
END SUB

SUB HInvoices ()
	Heading
	LPRINT TAB(71); "INVOICE REGISTER": LPRINT
	LPRINT "  INVOICE                                INVOICE     DATE       CHECK      GROSS                NET      SALES     OTHER     TOTAL    AMOUNT    Cost of    Gross"
	LPRINT "  NUMBER         CUSTOMER NAME             DATE      PAID       NUMBER     SALES    CREDITS    SALES      TAX     CHARGES   CHARGES    PAID      Goods     Profit"
	LPRINT "  ======== ============================ ========== ========== ========== ========= ========= ========= ========= ========= ========= ========= ========= ========="
END SUB

SUB HitKey (f$)
	DO: f$ = INKEY$: LOOP WHILE f$ <> ""
	DO: f$ = INKEY$: TimeDate: LOOP WHILE f$ = ""
END SUB

DEFINT A-Z
SUB HStatement ()
	LPRINT CHR$(27); "@";    'reset printer
	WIDTH "LPT1:", 255       'define width
	LPRINT CHR$(27); "M";    'elite print
	PageNum = PageNum + 1
	LineNum = 0
   
	LPRINT
	LPRINT
	LPRINT TAB(66); DATE$; TAB(89); DATE$
	LPRINT
	LPRINT TAB(66); Cus.Cod; TAB(89); Cus.Cod
	LPRINT
	LPRINT TAB(70); "Page:"; PageNum
	LPRINT TAB(10); Cus.Nam
	LPRINT TAB(10); Cus.Man
	LPRINT TAB(10); Cus.Add
	LPRINT TAB(10); Cus.Str
	LPRINT TAB(10); RTRIM$(Cus.Cty); ", "; Cus.Sta; " "; Cus.Zip
	LPRINT
	LPRINT
	LPRINT
END SUB

SUB InvFind (n$, f, e)
   f = 1: e = 0
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
SUB InvSort ()
	CLOSE
	SHELL "AC.COM ..\Data\Invoices.INX ..\Data\Invoices.CUS /L18 /+1,4 /+9,1 /+8,1 /+7,1 /+6,1 /Q"
	SHELL "AC.COM ..\Data\Invoices.INX ..\Data\Invoices.INV /L18 /+9,1 /+8,1 /+7,1 /+6,1 /Q"
END SUB

SUB KeyHit (f$)
	DO: LOOP WHILE INKEY$ <> ""
	DO: f$ = INKEY$: LOOP WHILE f$ = ""
	f$ = UCASE$(f$)
END SUB

DEFINT A-R, U-Z
DEFCUR S-T
SUB mInvoices ()
	'List invoices between 2 dates
	CLS
	LOCATE 5, 10: PRINT "Invoice listing from date:";
	LOCATE 6, 10: PRINT "                  to date:";
	LOCATE 7, 10: PRINT "                 Salesman:";
601 EntryD 5, 37, e, b, f$:   IF e THEN EXIT SUB ELSE IF b THEN 601
602 EntryD 6, 37, e, b, t$:   IF e THEN EXIT SUB ELSE IF b THEN 601
603 EntryU 7, 37, 1, e, b, m$: IF e THEN EXIT SUB ELSE IF b THEN 602
	f$ = RIGHT$(f$, 2) + LEFT$(f$, 2) + MID$(f$, 4, 2)
	t$ = RIGHT$(t$, 2) + LEFT$(t$, 2) + MID$(t$, 4, 2)
	LOCATE 8, 10: PRINT "Please wait, to cancel press ESC ";
	CLOSE : SHELL "AC.COM ..\Data\Invoices.Inv ..\Data\Invoices /L18 /+5,1 /+1,4 /+9,1 /+8,1 /+7,1 /+6,1 /Q"
	OpenFiles
	CLOSE #9: OPEN "..\Data\Invoices" FOR RANDOM AS #9 LEN = LEN(Inx)
	COLOR 1, 7
	PageNum = 0: HInvoices: Printing
	e = LOF(9) \ LEN(Inx)
	f = 1
	FOR x = 1 TO e
	IF INKEY$ = CHR$(27) THEN EXIT FOR
	Complete x, e
	COLOR 3
	GET #9, x, Inx
	IF Inx.Man = m$ THEN
	   IF Inx.Dat >= f$ AND Inx.Dat <= t$ THEN
		  COLOR 11
		  GET #4, Inx.Num, Inv
		  IF f THEN
			 f = 0
		  ELSE
			 IF p$ <> Inv.Cus THEN GOSUB 605
		  END IF
		  AccDist t
		  IF CusFind(Inv.Cus) = 0 THEN ERROR 200
		  LineNum = LineNum + 1: IF LineNum > 60 THEN HInvoices
		  LPRINT USING "& "; Inv.Man; Inv.Num; Cnx.Nam; Inv.Bdt; Inv.Ddt; Inv.Chk;
		  LPRINT USING " &  & ######.##  &  &  &"; Inv.Grs; Inv.Crd; VAL(Inv.Grs) - VAL(Inv.Crd); Inv.Tax; Inv.Frt; Inv.Net;
		  LPRINT USING " ######.##"; VAL(Inv.Pmt); t; VAL(Inv.Grs) - VAL(Inv.Crd) - t
		  S1 = S1 + VAL(Inv.Grs)
		  S2 = S2 + VAL(Inv.Crd)
		  S3 = S3 + VAL(Inv.Tax)
		  S4 = S4 + VAL(Inv.Frt)
		  S5 = S5 + VAL(Inv.Net)
		  S6 = S6 + VAL(Inv.Pmt)
		  S7 = S7 + t
	   END IF
	END IF
	NEXT
	GOSUB 605
	LPRINT : LPRINT TAB(73); : LPRINT USING "#######.##"; T1; T2; T1 - T2; T3; T4; T5; T6; T7; T1 - T2 - T7
	EXIT SUB
605 LPRINT : LPRINT TAB(73); : LPRINT USING "#######.##"; S1; S2; S1 - S2; S3; S4; S5; S6; S7; S1 - S2 - S7
	LPRINT : LineNum = LineNum + 3: IF LineNum > 60 THEN HInvoices
	T1 = T1 + S1: S1 = 0
	T2 = T2 + S2: S2 = 0
	T3 = T3 + S3: S3 = 0
	T4 = T4 + S4: S4 = 0
	T5 = T5 + S5: S5 = 0
	T6 = T6 + S6: S6 = 0
	T7 = T7 + S7: S7 = 0
	p$ = Inv.Cus
	RETURN
END SUB

DEFSNG A-Z
SUB Monitor ()
	DEF SEG = 0
	   Segment = &HB800
	DEF SEG
END SUB

SUB OpenFiles ()
	OPEN "..\Data\Customer.MAS" FOR RANDOM LOCK READ WRITE AS #1 LEN = LEN(Cus)
	OPEN "..\Data\Customer.NAM" FOR RANDOM LOCK READ WRITE AS #2 LEN = LEN(Cnx)
  
	OPEN "..\Data\Invoices.MAS" FOR RANDOM LOCK READ WRITE AS #4 LEN = LEN(Inv)
	OPEN "..\Data\Invoices.INX" FOR RANDOM LOCK READ WRITE AS #5 LEN = LEN(Inx)
	OPEN "..\Data\Invoices.CUS" FOR RANDOM LOCK READ WRITE AS #6 LEN = LEN(Inx)
	OPEN "..\Data\Invoices.INV" FOR RANDOM LOCK READ WRITE AS #7 LEN = LEN(Inx)
	OPEN "..\Data\Invoices.Chk" FOR RANDOM LOCK READ WRITE AS #8 LEN = LEN(Rct)
	OPEN "..\Data\Invoices.SAL" FOR RANDOM LOCK READ WRITE AS #9 LEN = LEN(Sal)

	OPEN "..\Data\Material.MAS" FOR RANDOM LOCK READ WRITE AS #10 LEN = LEN(Mat)
	OPEN "..\Data\Material.INX" FOR RANDOM LOCK READ WRITE AS #11 LEN = LEN(Mnx)
END SUB

SUB Printing ()
	WIDTH 80, 50
	COLOR 0, 0: CLS
	COLOR 14, 5: LOCATE 18
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
SUB Remarks (N1$, N2$, e)
	e = 0
	N1$ = SPACE$(58)
	N2$ = SPACE$(58)
	LOCATE 15, 1: PRINT "Remarks:"
	LOCATE 16, 1: PRINT "Remarks:"
510 EntryS 15, 12, 58, e, b, N1$: IF b THEN 510 ELSE IF e THEN EXIT SUB
511 EntryS 16, 12, 58, e, b, N2$: IF b THEN 510 ELSE IF e THEN EXIT SUB
	LOCATE 18, 20: PRINT " Press Y if you are sure of the remarks "
	SOUND 1000, .5: KeyHit f$
	IF f$ <> "Y" THEN LOCATE 18, 20: PRINT SPC(40); : GOTO 510
END SUB

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
	Top = LOF(2) \ LEN(Cnx)
	Bot = 1
	r = 7
	GOSUB C2
C1: GET #2, n, Cnx
	COLOR 15, 5: LOCATE r, 42: PRINT USING "&ณ&"; Cnx.Cod; Cnx.Nam
	KeyHit n$
	COLOR 0, 2: LOCATE r, 42: PRINT USING "&ณ&"; Cnx.Cod; Cnx.Nam
	SELECT CASE n$
	CASE CHR$(13)
		 GET #1, Cnx.Num, Cus
		 IF Cus.Nam < "!" GOTO C1
		 f$ = Cus.Cod
		 SCREEN , , 0, 0
		 COLOR 14, 0
		 EXIT SUB
	CASE CHR$(0) + "P", CHR$(0) + "M" 'Dn
		 IF n + 1 > Top GOTO C1 ELSE n = n + 1
		 IF r + 1 > 20 THEN CALL scroll(42, 7, 74, 20, 1) ELSE r = r + 1
	CASE CHR$(0) + "H", CHR$(0) + "K" 'Up
		 IF n - 1 < 1 GOTO C1 ELSE n = n - 1
		 IF r - 1 < 7 THEN CALL BkScroll(42, 7, 74, 20, 1) ELSE r = r - 1
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
 
C2: COLOR 0, 2: LOCATE 6, 40: PRINT " ษออออัออออออออออออออออออออออออออออป "
	FOR x = Bot TO Top: GET #2, x, Cnx
	LOCATE , 40: PRINT USING " บ&ณ&บ "; Cnx.Cod; Cnx.Nam
	IF CSRLIN > 20 THEN EXIT FOR
	NEXT
	LOCATE , 40: PRINT " ศออออฯออออออออออออออออออออออออออออผ ": COLOR 15
	LOCATE , 40: PRINT "  "; CHR$(17); "ู "; CHR$(24); " "; CHR$(25); "  PgUp  PgDn  Home  End  Esc "
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


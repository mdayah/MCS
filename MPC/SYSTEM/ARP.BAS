DEFINT A-Z: DEFLNG L: DEFDBL T

DECLARE SUB HCustomer ()
DECLARE SUB Heading ()
DECLARE SUB TimeDate ()
DECLARE SUB KeyHit (f$)
DECLARE SUB KeyPres (f$)
DECLARE SUB HitKey (f$)

DECLARE SUB WinMat (f$)
DECLARE SUB WinCus (f$)
DECLARE SUB WinMssg (f$)
DECLARE SUB WinInv (e, b)

DECLARE SUB OpenFiles ()
DECLARE SUB Monitor ()
DECLARE SUB AccDist ()
DECLARE SUB HiLite (m, p)

DECLARE SUB InvSort ()
DECLARE SUB InvDisp ()
DECLARE SUB InvPost ()
DECLARE SUB InvVoid ()
DECLARE SUB RctPost ()
DECLARE SUB InvFind (n$, f, e)
DECLARE SUB MatClose (p)
DECLARE SUB OnHand (r)

DECLARE SUB CusDisp (x)
DECLARE SUB CusShow ()
DECLARE SUB CusSort ()
DECLARE SUB CusEdit ()
DECLARE SUB CusLine (r, Lm, Ly)

DECLARE FUNCTION NewItem ()
DECLARE FUNCTION CusFind (f$)
DECLARE FUNCTION NumFind (f&)

REM $INCLUDE: '..\Include\Library.Dec'
REM $INCLUDE: '..\Include\Accounts.REC'
REM $INCLUDE: '..\Include\Customer.Rec'
REM $INCLUDE: '..\Include\SInvoice.Rec'
REM $INCLUDE: '..\Include\Receipts.Rec'
REM $INCLUDE: '..\Include\Material.REC'

	DIM SHARED Acc AS Accounts
	DIM SHARED Cus AS Customer
	DIM SHARED Cnx AS CusIndex
	DIM SHARED Inv AS Invoices
	DIM SHARED Inx AS InvIndex
	DIM SHARED Rct AS Receipts
	DIM SHARED Mat AS Material
	DIM SHARED Mnx AS MatIndex

	DIM SHARED Menu(6, 6) AS INTEGER
	DIM SHARED Days(12) AS INTEGER
	DIM SHARED LineNum AS INTEGER
	DIM SHARED PageNum AS INTEGER
	DIM SHARED Segment AS INTEGER
	DIM SHARED Sort    AS INTEGER
	DIM SHARED Top     AS INTEGER
	DIM SHARED Bot     AS INTEGER
  
	DIM SHARED oCu$
	DIM SHARED Pmt$
	DIM SHARED Grs$
	DIM SHARED Crd$
	DIM SHARED Tax$
	DIM SHARED Frt$
	DIM SHARED net$
	DIM SHARED Cst$, oCs$
	DIM SHARED Amt$, oAm$
	DIM SHARED New(300) AS STRING * 8
	DIM SHARED Lqt, Lpr, Tup
	DIM SHARED Itm(0 TO 15) AS STRING * 3, oIt(0 TO 15) AS STRING * 3
	DIM SHARED Qty(0 TO 15) AS STRING * 8, oQt(0 TO 15) AS STRING * 8
	DIM SHARED Cog(0 TO 15) AS STRING * 8, oCo(0 TO 15) AS STRING * 8
	DIM SHARED Prc(0 TO 15) AS STRING * 8, oPr(0 TO 15) AS STRING * 8
	DIM SHARED Ini(20) AS INTEGER
	DIM SHARED InN(20) AS STRING * 8
	DIM SHARED Due(20) AS STRING * 8
	DIM SHARED Rcv(20) AS STRING * 8

'--------------------------------------------------------------------------
'   test customer net amount for month and year
'   update customer if net does not match
'
'   OpenFiles
'   FOR x = 1 TO LOF(1) \ LEN(Cus)
'   GET #1, x, Cus
'   m& = (Cus.Mgr - Cus.Mcr + Cus.Mtx + Cus.Mfr)
'   y& = (Cus.Ygr - Cus.Ycr + Cus.Ytx + Cus.Yfr)
'   IF m& <> Cus.Mnt THEN
'      PRINT "M", Cus.Cod, Cus.Mnt, m&
'      Cus.Mnt = m&
'      PUT #1, x, Cus
'   END IF
'   IF y& <> Cus.Ynt THEN
'      PRINT "Y", Cus.Cod, Cus.Ynt, y&
'      Cus.Ynt = y&
'      PUT #1, x, Cus
'   END IF
'   mt& = mt& + m&
'   yt& = yt& + y&
'   mc& = mc& + Cus.Mnt
'   yc& = yc& + Cus.Ynt
'   NEXT
'   PRINT mt&, mc&
'   PRINT yt&, yc&
'   CLOSE
'   STOP
'----------------------------------------------------------------------------
	FOR y = 1 TO 6: FOR x = 1 TO 2: READ Menu(y, x): NEXT x, y
	FOR y = 1 TO 12: READ Days(y): NEXT
	KEY OFF: KEY 1, "1": KEY 2, "2": KEY 3, "3": KEY 4, "4": KEY 5, "5"
			 KEY 6, "6": KEY 7, "7": KEY 8, "8": KEY 9, "9": KEY 10, "0"

	ON ERROR GOTO Errors
	Monitor
	CLS

10  CLOSE : WIDTH 80, 25: DEF SEG = Segment: BLOAD "Menu.Scr", 0: DEF SEG
	OpenFiles
	REDIM SHARED Old(LOF(10) \ LEN(Mat), 2) AS LONG
	COLOR 15, 0: LOCATE 23, 30
	PRINT USING "###,### Invoices "; LOF(4) \ LEN(Inv);
	CLOSE
	HitKey f$: f = VAL(f$)
	IF f$ = "0" THEN f = 10
	OpenFiles
	IF f$ = CHR$(27) GOTO Terminate
	ON f GOTO InvNew, InvEdt, Receipt, 20, 30, 40, Customer, 50, MonEnd, YerEnd
	GOTO 10
20  RUN "ARD": GOTO 10
30  RUN "ARR": GOTO 10
40  RUN "ARI": GOTO 10
50  RUN "ART": GOTO 10
InvNew: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	Edit = 0
	Inv.Num = "        ":   L1 = 0: L2 = 0
	Inv.Cus = "    ":       oCu$ = ""
	Inv.Man = " "
	Inv.Ref = "         "
	Inv.Bdt = "          "
	Inv.Ddt = "          "
	Inv.Pdt = "**-**-****"
	Inv.Chk = "*Not Paid*"
	Inv.Pmt = "    0.00"
	Inv.Grs = "    0.00"
	Inv.Crd = "    0.00"
	Inv.Tax = "    0.00"
	Inv.Frt = "    0.00"
	ERASE Itm, oIt, Qty, oQt, Cog, oCo, Prc, oPr
	WIDTH 80, 50: DEF SEG = Segment: BLOAD "Invoices.SCR", 0: DEF SEG

100 EntryN 9, 3, 8, 0, e, b, Inv.Num:   IF e THEN 10 ELSE IF b THEN 100
	IF Inv.Num < "       1" THEN WinMssg "Must have an invoice No.": GOTO 100
	IF NumFind(VAL(Inv.Num)) THEN IF Inx.Num <> Edit THEN WinMssg "Duplicate invoice number": GOTO 100
	IF NewItem THEN WinMssg "Duplicate invoice number": GOTO 100
	IF Edit = 0 THEN Inx.Num = 1 + LOF(4) \ LEN(Inv) ELSE Inx.Num = Edit
102 EntryU 11, 12, 4, e, b, Inv.Cus:    IF e THEN 10 ELSE IF b THEN 100
	IF CusFind(Inv.Cus) = 0 THEN WinCus f$: IF f$ = "" THEN 102 ELSE Inv.Cus = f$
	GET #1, Cnx.Num, Cus: IF Cus.Nam < "!" THEN 102
	LOCATE , 12: PRINT Cus.Cod: Inv.Cus = Cus.Cod
	LOCATE , 2: PRINT Cus.Nam
	LOCATE , 2: PRINT Cus.Man
	LOCATE , 2: PRINT Cus.Add
	LOCATE , 2: PRINT Cus.Str
	LOCATE , 2: PRINT Cus.Cty; Cus.Sta; " "; Cus.Zip

110 EntryS 9, 46, 9, e, b, Inv.Ref:     IF e THEN 10 ELSE IF b THEN 102
	IF LEFT$(Inv.Ref, 1) = "D" THEN
	   Inv.Ref = "Dummy     "
	   LOCATE 9, 46: PRINT Inv.Ref
	END IF
111 EntryU 10, 46, 1, e, b, Inv.Man:    IF e THEN 10 ELSE IF b THEN 110
112 EntryD 11, 46, e, b, Inv.Bdt:       IF e THEN 10 ELSE IF b THEN 111
	IF Inv.Bdt < "!" THEN Inv.Bdt = DATE$: GOTO 112
	IF Inv.Ddt < "!" THEN Inv.Ddt = DayMath(Inv.Bdt, VAL(Cus.Ter))
113 EntryD 12, 46, e, b, Inv.Ddt:       IF e THEN 10 ELSE IF b THEN 112
	IF Inv.Ddt < "!" THEN Inv.Ddt = DayMath(Inv.Bdt, VAL(Cus.Ter))
	IF Cus.Pay = "C" THEN
	   Inv.Pdt = Inv.Bdt
	   Inv.Chk = "Cash      "
	   Inv.Pmt = Inv.net
	ELSE
	   Inv.Pdt = "**-**-****"
	   Inv.Chk = "*Not Paid*"
	   Inv.Pmt = "    0.00"
	   IF Inv.Ref = "Dummy    " THEN Inv.Pmt = "       0"
	END IF
	LOCATE 14, 46: PRINT Inv.Pdt
	LOCATE 15, 46: PRINT Inv.Chk
	LOCATE 16, 48: PRINT Inv.Pmt
116 EntryN 9, 72, 8, 2, e, b, Inv.Grs:  IF e THEN 10 ELSE IF b THEN 113
117 EntryN 10, 72, 8, 2, e, b, Inv.Crd: IF e THEN 10 ELSE IF b THEN 116
	COLOR 15, 3: LOCATE 12, 72: PRINT USING "#####.##"; VAL(Inv.Grs) - VAL(Inv.Crd);
118 EntryN 13, 72, 8, 2, e, b, Inv.Tax: IF e THEN 10 ELSE IF b THEN 117
119 EntryN 14, 72, 8, 2, e, b, Inv.Frt: IF e THEN 10 ELSE IF b THEN 118
	LOCATE 16, 72: PRINT USING "#####.##"; VAL(Inv.Grs) - VAL(Inv.Crd) + VAL(Inv.Tax) + VAL(Inv.Frt)
	Picture 16, 72, 8, Inv.net
	IF Cus.Pay = "C" THEN
	   Inv.Pmt = Inv.net
	   LOCATE 16, 48: PRINT Inv.Pmt
	END IF
	r = 0
120 EntryN r + 23, 2, 3, 0, e, b, Itm(r): IF e THEN 10
	IF b THEN r = r - 1: IF r < 0 THEN 119 ELSE 120
	IF Itm(r) = "   " THEN
		L1 = L1 - 100 * VAL(Cog(r))
		L2 = L2 - 100 * VAL(Prc(r))
		FOR x = r TO 14
		Itm(x) = Itm(x + 1): Qty(x) = Qty(x + 1)
		Cog(x) = Cog(x + 1): Prc(x) = Prc(x + 1)
		NEXT
		IF r < 15 THEN CALL scroll(2, r + 23, 79, 37, 1)
		COLOR 3, 0:  LOCATE 37, 2:  PRINT "   ณ                            ณ        ณ  ณ        ณ       ณ       ณ        "
		COLOR 10, 0: LOCATE 39, 47: PRINT USING "#####.##"; L1 / 100;
		COLOR 15, 3: LOCATE 39, 72: PRINT USING "#####.##"; L2 / 100;
		IF Itm(r) < "  1" GOTO SaveInv ELSE 120
	END IF
	IF Itm(r) < "  1" OR VAL(Itm(r)) > UBOUND(Old, 1) THEN WinMat Itm(r): IF Itm(r) < "  1" THEN 120
	GET #10, VAL(Itm(r)), Mat
	IF Mat.Nam < "!" THEN 120
	LOCATE r + 23
	COLOR 14, 0: LOCATE , 2:  PRINT Mat.Num; : Itm(r) = Mat.Num
	COLOR 10, 0: LOCATE , 6:  PRINT Mat.Nam;
	COLOR 10, 0: LOCATE , 44: PRINT Mat.Mes
121 EntryN r + 23, 35, 8, 2, e, b, Qty(r): IF e THEN 10 ELSE IF b THEN 120
	IF Qty(r) < "    0.01" THEN BEEP: GOTO 121
	OnHand r
	LQ = VAL(Qty(r)) * 100
	IF LQ > Lqt THEN
	   COLOR 15, 4: LOCATE 39, 3, 1
	   PRINT USING "OnHand:######.## & ?"; Lqt / 100; Mat.Mes
'      color 3,0:BEEP: KeyHit f$: LOCATE 39, 3, 0: PRINT SPC(21);
'      IF f$ <> "F" THEN 121
	END IF
	COLOR 10, 0: LOCATE r + 23, 47: PRINT USING "#####.##"; LQ * Tup / 100
	IF Tup < .01 THEN LOCATE 39, 3: PRINT "Suspended [No Cost]!"; : BEEP: KeyHit f$: GOTO InvNew
	L1 = L1 - 100 * VAL(Cog(r))
	Picture r + 23, 47, 8, Cog(r)
	L1 = L1 + 100 * VAL(Cog(r))
	LOCATE 39, 47: PRINT USING "#####.##"; L1 / 100;
	LOCATE r + 23, 56: PRINT USING "##.####"; VAL(Mat.Prc);
	IF Prc(r) < "    0.01" THEN
		LOCATE , 64: PRINT USING "##.####"; VAL(Mat.Prc);
		LOCATE , 72: PRINT USING "#####.##"; LQ * VAL(Mat.Prc) / 100;
		Picture r + 23, 72, 8, Prc(r): L2 = L2 + 100 * VAL(Prc(r))
	END IF
	L2 = L2 - 100 * VAL(Prc(r))
128 EntryN r + 23, 72, 8, 2, e, b, Prc(r): IF e THEN 10
	COLOR 3, 0: LOCATE 39, 3, 0: PRINT SPC(21);
	L2 = L2 + 100 * VAL(Prc(r))
	COLOR 15, 3: LOCATE 39, 72: PRINT USING "#####.##"; L2 / 100;
	IF b THEN 121
	IF Prc(r) < "    0.01" THEN BEEP: GOTO 128
	COLOR 10, 0: LOCATE r + 23, 64: PRINT USING "##.####"; VAL(Prc(r)) / VAL(Qty(r));
	IF Prc(r) < Cog(r) THEN COLOR 12, 0: LOCATE r + 23, 72: PRINT Prc(r); : SOUND 770, 2: SOUND 999, 1
	r = r + 1: IF r < 15 THEN 120 ELSE r = 14
SaveInv: 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	Picture 12, 72, 8, f$
	Picture 39, 47, 8, Cst$
	Picture 39, 72, 8, Amt$
	IF Amt$ = f$ THEN
	   IF Confirm = 0 GOTO 120
	   LOCATE 39, 3: PRINT USING "Invoice Number [#####]"; Inx.Num
	   Sort = Sort + 1: New(Sort) = Inv.Num
	   f$ = "": FOR x = 0 TO 14
	   IF Itm(x) > "  0" THEN
		  GET #10, VAL(Itm(x)), Mat
		  f$ = f$ + Itm(x) + Qty(x) + Cog(x) + Prc(x)
		  Mat.Msq = Mat.Msq + VAL(Qty(x)) * 100
		  Mat.Msc = Mat.Msc + VAL(Cog(x)) * 100
		  Mat.Msp = Mat.Msp + VAL(Prc(x)) * 100
		  Mat.Ysq = Mat.Ysq + VAL(Qty(x)) * 100
		  Mat.Ysc = Mat.Ysc + VAL(Cog(x)) * 100
		  Mat.Ysp = Mat.Ysp + VAL(Prc(x)) * 100
		  PUT #10, VAL(Itm(x)), Mat '---------------update inventory--------
	   ELSE
		  EXIT FOR
	   END IF
	   NEXT
	   Inv.Dst = f$
	   PUT #4, Inx.Num, Inv '-----------------------save invoice-------------

	   Inx.Cus = Inv.Cus
	   Inx.Man = Inv.Man
	   Inx.Inv = VAL(Inv.Num)
	   Inx.Dat = RIGHT$(Inv.Bdt, 2) + LEFT$(Inv.Bdt, 2) + MID$(Inv.Bdt, 4, 2)
	   Inx.Mrk = " "
	   PUT #5, Inx.Num, Inx '-----------------------save index---------------

	   Cus.Mgr = Cus.Mgr + VAL(Inv.Grs) * 100: Cus.Ygr = Cus.Ygr + VAL(Inv.Grs) * 100
	   Cus.Mcr = Cus.Mcr + VAL(Inv.Crd) * 100: Cus.Ycr = Cus.Ycr + VAL(Inv.Crd) * 100
	   Cus.Mtx = Cus.Mtx + VAL(Inv.Tax) * 100: Cus.Ytx = Cus.Ytx + VAL(Inv.Tax) * 100
	   Cus.Mfr = Cus.Mfr + VAL(Inv.Frt) * 100: Cus.Yfr = Cus.Yfr + VAL(Inv.Frt) * 100
	   Cus.Mnt = Cus.Mnt + VAL(Inv.net) * 100: Cus.Ynt = Cus.Ynt + VAL(Inv.net) * 100
	   Cus.Mpd = Cus.Mpd + VAL(Inv.Pmt) * 100: Cus.Ypd = Cus.Ypd + VAL(Inv.Pmt) * 100
	   Cus.Mco = Cus.Mco + VAL(Cst$) * 100:    Cus.Yco = Cus.Yco + VAL(Cst$) * 100
	   PUT #1, Cnx.Num, Cus '----------------------update customer-----------

	   InvPost '-----------------------------------update accounts-----------

	   IF oCu$ > "    " THEN
		  IF CusFind(oCu$) = 0 THEN ERROR 200
		  FOR x = 0 TO 14
		  IF oIt(x) > "  0" THEN
			 GET #10, VAL(oIt(x)), Mat
			 Mat.Msq = Mat.Msq - VAL(oQt(x)) * 100
			 Mat.Msc = Mat.Msc - VAL(oCo(x)) * 100
			 Mat.Msp = Mat.Msp - VAL(oPr(x)) * 100
			 Mat.Ysq = Mat.Ysq - VAL(oQt(x)) * 100
			 Mat.Ysc = Mat.Ysc - VAL(oCo(x)) * 100
			 Mat.Ysp = Mat.Ysp - VAL(oPr(x)) * 100
			 PUT #10, VAL(oIt(x)), Mat '-----------update inventory----------
		  ELSE
			 EXIT FOR
		  END IF
		  NEXT
  
		  GET #1, Cnx.Num, Cus
		  Cus.Mgr = Cus.Mgr - VAL(Grs$) * 100: Cus.Ygr = Cus.Ygr - VAL(Grs$) * 100
		  Cus.Mcr = Cus.Mcr - VAL(Crd$) * 100: Cus.Ycr = Cus.Ycr - VAL(Crd$) * 100
		  Cus.Mtx = Cus.Mtx - VAL(Tax$) * 100: Cus.Ytx = Cus.Ytx - VAL(Tax$) * 100
		  Cus.Mfr = Cus.Mfr - VAL(Frt$) * 100: Cus.Yfr = Cus.Yfr - VAL(Frt$) * 100
		  Cus.Mnt = Cus.Mnt - VAL(net$) * 100: Cus.Ynt = Cus.Ynt - VAL(net$) * 100
		  Cus.Mpd = Cus.Mpd - VAL(Pmt$) * 100: Cus.Ypd = Cus.Ypd - VAL(Pmt$) * 100
		  Cus.Mco = Cus.Mco - VAL(oCs$) * 100: Cus.Yco = Cus.Yco - VAL(oCs$) * 100
		  PUT #1, Cnx.Num, Cus '-------------------update customer-----------

		  InvVoid '--------------------------------update accounts-----------
	   END IF
	   IF Sort > 299 THEN InvSort
	   IF Edit GOTO InvEdt ELSE GOTO InvNew
	END IF
	GOTO 120
InvEdt: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	WIDTH 80, 50: DEF SEG = Segment: BLOAD "Invoices.SCR", 0: DEF SEG
200 EntryN 9, 3, 8, 0, e, b, Inv.Num: IF e THEN 10 ELSE IF b THEN 200
	IF NumFind(VAL(Inv.Num)) THEN
	   REDIM SHARED Old(LOF(10) \ LEN(Mat), 2) AS LONG
	   InvDisp
	   L1 = 100 * VAL(Cst$): L2 = 100 * VAL(Amt$)
	   IF Inv.Chk <> "*Not Paid*" AND Inv.Chk <> "Cash      " THEN WinMssg "Paid invoice [LOCKED]": GOTO 200
	   Edit = Inx.Num
	ELSE
	   WinMssg "Invoice not found!": GOTO 200
	END IF
	GOTO 100
Receipt: 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	WIDTH 80, 50: DEF SEG = Segment: BLOAD "Receipts.SCR", 0: DEF SEG
300 EntryU 6, 29, 4, e, b, Rct.Cus: IF e THEN 10 ELSE IF b THEN 300
	IF CusFind(Rct.Cus) = 0 THEN WinCus f$: IF f$ = "" THEN 300 ELSE Rct.Cus = f$
	GET #1, Cnx.Num, Cus: IF Cus.Nam < "!" THEN 300
	LOCATE 6, 29: PRINT Rct.Cus
	LOCATE 8
	LOCATE , 14:  PRINT Cus.Nam
	LOCATE , 14:  PRINT Cus.Man
	LOCATE , 14:  PRINT Cus.Add
	LOCATE , 14:  PRINT Cus.Str
	LOCATE , 14:  PRINT Cus.Cty; Cus.Sta; " "; Cus.Zip
	InvFind Cus.Cod, b, e
302 IF b = 0 THEN WinMssg "No invoices for this customer": GOTO 300
	ERASE Ini, InN, Due, Rcv: L1 = 0: L2 = 0: y = 1: z = 0
	Rct.Dat = "          "
	Rct.Chk = "          "
	Rct.Amt = "          "
	COLOR 7
	FOR x = b TO e
	z = z + 1
	GET #6, x, Inx

	IF Inx.Mrk = " " THEN
	   GET #4, Inx.Num, Inv
	   IF Inv.net <> Inv.Pmt OR Inv.Ref = "Dummy    " THEN
		  IF y > 20 THEN EXIT FOR
		  LOCATE 20 + y, 16
		  PRINT USING "&ณ&ณ&ณ  #####.##ณ  #####.##"; Inv.Num; Inv.Bdt; Inv.Ddt; VAL(Inv.net) - VAL(Inv.Pmt); VAL(Inv.net) - VAL(Inv.Pmt)
		  Ini(y) = Inx.Num
		  InN(y) = Inv.Num
		  Picture 20 + y, 49, 8, Due(y)
		  Rcv(y) = Due(y)
		  L1 = L1 + 100 * VAL(Due(y))
		  L2 = L2 + 100 * VAL(Rcv(y))
		  y = y + 1
	   END IF
	END IF
	NEXT
	b = b + z - 1
	FOR x = y TO 20
	   LOCATE 20 + x, 16
	   PRINT "        ณ          ณ          ณ          ณ          "
	NEXT
	COLOR 15
	LOCATE 6, 58:  PRINT USING "#######.##"; Cus.Ybb / 100
	LOCATE 7, 58:  PRINT USING "#######.##"; Cus.Ynt / 100
	LOCATE 9, 58:  PRINT USING "#######.##"; (Cus.Ybb + Cus.Ynt) / 100
	LOCATE 10, 58: PRINT USING "#######.##"; Cus.Ypd / 100
	LOCATE 12, 58: PRINT USING "#######.##"; (Cus.Ybb + Cus.Ynt - Cus.Ypd) / 100
	LOCATE 42, 47: PRINT "          "; : LOCATE , 58: PRINT "          "
	LOCATE 44, 31: PRINT "          "; : LOCATE , 58: PRINT "          "
	LOCATE 45, 31: PRINT "          "; : LOCATE , 58: PRINT "          "
	IF L1 = 0 THEN WinMssg "No invoices due"
	COLOR 7
	LOCATE 42, 47: PRINT USING "#######.##"; L1 / 100
	LOCATE 42, 58: PRINT USING "#######.##"; L2 / 100
	Picture 42, 47, 10, Amt$
	LOCATE 47, 16: PRINT "Press [PgDn] for next page and any key to continue"
	DO: f$ = INKEY$: LOOP WHILE f$ = ""
	LOCATE 47, 16: PRINT "                                                  "
	IF f$ = CHR$(0) + "Q" THEN 302
310 FOR x = 1 TO 20
	IF InN(x) > "       0" THEN
312    L2 = L2 - 100 * VAL(Rcv(x))
	   EntryN x + 20, 60, 8, 2, e, b, Rcv(x): IF e THEN 300
	   L2 = L2 + 100 * VAL(Rcv(x))
	   LOCATE 42, 58: PRINT USING "#######.##"; L2 / 100
	   Picture 42, 58, 10, Pmt$
	   IF b THEN x = x - 1: IF x < 1 THEN 310 ELSE 312
	ELSE
	   EXIT FOR
	END IF
	NEXT
	Rct.Dat = DATE$
314 EntryD 44, 31, e, b, Rct.Dat:        IF e THEN 300 ELSE IF b THEN 310
	IF Rct.Dat < "!" THEN WinMssg "Date please!": GOTO 314
316 EntryS 45, 31, 10, e, b, Rct.Chk:    IF e THEN 300 ELSE IF b THEN 314
	IF Rct.Chk < "!" THEN WinMssg "Check number please!": GOTO 316
318 EntryN 44, 58, 10, 2, e, b, Rct.Amt: IF e THEN 300 ELSE IF b THEN 316
	IF Rct.Amt <> Pmt$ THEN WinMssg "Applied & Check amount are unequal": GOTO 318
	IF Confirm THEN
	   f$ = ""
	   FOR x = 1 TO 20
	   IF Rcv(x) > "    0.00" THEN
		  GET #4, Ini(x), Inv
		  f$ = f$ + InN(x) + Rcv(x)
		  LOCATE 47, 36: PRINT USING "#####.##"; VAL(Inv.Pmt) + VAL(Rcv(x))
		  Inv.Pdt = Rct.Dat
		  Inv.Chk = Rct.Chk
		  Picture 47, 36, 8, Inv.Pmt
		  PUT #4, Ini(x), Inv '----------------------------<INVOICES.DAT>
	   END IF
	   NEXT
	   Rct.Cus = Cus.Cod
	   Rct.Inv = f$
	   PUT #9, 1 + LOF(9) \ LEN(Rct), Rct '----------------<INVOICES.CHK>
	   Cus.Mpd = Cus.Mpd + VAL(Rct.Amt) * 100
	   Cus.Ypd = Cus.Ypd + VAL(Rct.Amt) * 100
'       Cus.Crd = Cus.Crd + VAL(Crd$) * 100
	   PUT #1, Cnx.Num, Cus '------------------------------<CUSTOMER.DTA>
	   RctPost
	   GOTO 300
	END IF
	GOTO 310
Customer: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	IF Sort THEN InvSort
	OpenFiles
	WIDTH 80, 50: DEF SEG = Segment: BLOAD "Customer.SCR", 0: DEF SEG
				  DEF SEG = Segment: BLOAD "Invoices.SCR", 16512: DEF SEG
	Bot = 1: Top = LOF(3) / LEN(Cnx): CusDisp x
	r = 2: n = 1: m = 4: HiLite m, 1
600 IF n > 0 THEN GET #3, n, Cnx
	COLOR 11, 1: LOCATE r, 2: PRINT USING "&ณ&"; Cnx.Cod; Cnx.Nam
	KeyHit n$
	COLOR 0, 3:  LOCATE r, 2: PRINT USING "&ณ&"; Cnx.Cod; Cnx.Nam
	IF n$ = CHR$(13) THEN n$ = MID$(" Add  Modify  Erase  Display  List  Invoices ", 1 + Menu(m, 1), 1)
	SELECT CASE n$
	CASE CHR$(27) 'Esc
		GOTO 10
	CASE CHR$(0) + "M" 'right
		HiLite m, 0
		m = m + 1: IF m > 6 THEN m = 1
		HiLite m, 1
	CASE CHR$(0) + "K" 'left
		HiLite m, 0
		m = m - 1: IF m < 1 THEN m = 6
		HiLite m, 1
	CASE CHR$(0) + "P" 'Dn
		IF n + 1 > Top THEN 600 ELSE n = n + 1
		IF r + 1 > 48 THEN CALL scroll(2, 2, 34, 48, 1) ELSE r = r + 1
	CASE CHR$(0) + "H" 'Up
		IF n - 1 < 1 THEN 600 ELSE n = n - 1
		IF r - 1 < 2 THEN CALL BkScroll(2, 2, 34, 48, 1) ELSE r = r - 1
	CASE CHR$(0) + "I" 'PgUp
		Bot = n - r - 45
		IF Bot < 1 THEN Bot = 1
		CusDisp x: n = x - 48 + r
	CASE CHR$(0) + "Q" 'PgDn
		Bot = n - r + 49: IF Bot + 46 > Top THEN Bot = Top - 46: IF Bot < 1 THEN Bot = 1
		CusDisp x: n = x + r - 48
	CASE CHR$(0) + "G" 'Home
		Bot = 1: CusDisp x: n = r - 1
	CASE CHR$(0) + "O" 'End
		Bot = Top - 46
		IF Bot < 1 THEN Bot = 1 ELSE CusDisp x: n = Top + r - 48
	CASE "L"
		HiLite m, 0
		m = 5: HiLite m, 1
		IF Confirm THEN
		   PageNum = 0: Heading
		   FOR y = 1 TO LOF(3) / LEN(Cnx)
		   IF INKEY$ = CHR$(27) THEN EXIT FOR
		   GET #3, y, Cnx
		   GET #1, Cnx.Num, Cus
		   IF Cus.Nam > "!" THEN
			  LineNum = LineNum + 6: IF LineNum > 60 THEN Heading
			  LPRINT USING " & "; Cus.Cod; Cus.Nam; Cus.Man; Cus.Sco; Cus.Typ: LPRINT TAB(36);
			  LPRINT USING " & "; Cus.Tel; Cus.Ste; Cus.Dat: LPRINT TAB(36);
			  LPRINT USING " & "; Cus.Add; Cus.Sad; Cus.Ban: LPRINT TAB(36);
			  LPRINT USING " & "; Cus.Str; Cus.SSr; Cus.Acc: LPRINT TAB(36);
			  LPRINT USING " & "; Cus.Cty; Cus.Sta; Cus.Zip; Cus.SCt; Cus.SSt; Cus.SZi; Cus.Tax
			  LPRINT
		   END IF
		   NEXT
		END IF
	CASE "E"
		HiLite m, 0
		m = 3: HiLite m, 1
		CusShow
		L1 = Cus.Ybb + Cus.Ygr + Cus.Ycr + Cus.Ypd
		IF L1 <> 0 THEN
		   WinMssg "Active customer, no erase"
		ELSE
		   IF Confirm THEN
			  Cnx.Cod = "þþþþ"
			  Cnx.Nam = "þþþþþþþþþþþþþþþþþþþþþþþþþþþþ"
			  Cus.Crd = 0: Cus.Chg = 0
			  Cus.Mbb = 0: Cus.Ybb = 0
			  Cus.Mgr = 0: Cus.Ygr = 0
			  Cus.Mcr = 0: Cus.Ycr = 0
			  Cus.Mtx = 0: Cus.Ytx = 0
			  Cus.Mfr = 0: Cus.Yfr = 0
			  Cus.Mnt = 0: Cus.Ynt = 0
			  Cus.Mpd = 0: Cus.Ypd = 0
			  Cus.Mco = 0: Cus.Yco = 0
			  PUT #2, Cnx.Num, Cnx
			  PUT #1, Cnx.Num
			  CusShow
			  CusSort
			  CusDisp x
		   END IF
		END IF
	CASE "D"
		HiLite m, 0
		m = 4: HiLite m, 1
		CusShow
	CASE "A"
		HiLite m, 0
		m = 1: HiLite m, 1
		y = 0
		FOR z = 1 TO Top: GET #2, z, Cnx
		IF LEFT$(Cnx.Nam, 1) = "þ" THEN y = z: EXIT FOR
		NEXT
		IF y = 0 THEN Cnx.Num = 1 + LOF(1) \ LEN(Cus)
		IF Cnx.Num > 999 THEN WinMssg "Customer Master is full" ELSE CusEdit
	CASE "M"
		HiLite m, 0
		m = 2: HiLite m, 1
		CusEdit
	CASE "I"
		HiLite m, 0
		m = 6: HiLite m, 1
		CusShow
		InvFind Cus.Cod, b, e
		IF b THEN WinInv b, e ELSE WinMssg "No invoices for this customer"
	CASE ELSE
		WinMssg "Select from highlited keys"
	END SELECT
	GOTO 600
MonEnd:  'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	COLOR 6, 0: CLS : f$ = ""
	LOCATE 12, 20: PRINT "    Remember to BACKUP before closing    "
	LOCATE 13, 20: PRINT " Type YES if you wish to close the MONTH "
900 EntryU 13, 26, 3, e, b, f$: IF e THEN 10 ELSE IF b THEN 900
	IF f$ = "YES" THEN
	   GOSUB CusDet
	   LOCATE 15, 20: PRINT " One moment please.....CLOSING the MONTH "
	   FOR x = 1 TO LOF(1) \ LEN(Cus)
	   GET #1, x, Cus
		   Cus.Mbb = Cus.Mbb + Cus.Mgr - Cus.Mcr + Cus.Mtx + Cus.Mfr - Cus.Mpd
		   Cus.Mgr = 0
		   Cus.Mcr = 0
		   Cus.Mtx = 0
		   Cus.Mfr = 0
		   Cus.Mnt = 0
		   Cus.Mpd = 0
		   Cus.Mco = 0
	   PUT #1, x, Cus
	   NEXT
	   MatClose 1 'close Material Month
	END IF
	GOTO 10
YerEnd:   'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	COLOR 6, 0: CLS : f$ = ""
	LOCATE 12, 20: PRINT "    Remember to BACKUP before closing    "
	LOCATE 13, 20: PRINT " Type YES if you wish to close the YEAR  "
910 EntryU 13, 26, 3, e, b, f$: IF e THEN 10 ELSE IF b THEN 910
	IF f$ = "YES" THEN
	   GOSUB CusDet
	   LOCATE 15, 20: PRINT "One moment please....CLOSING MONTH & YEAR"
	   FOR x = 1 TO LOF(1) \ LEN(Cus)
	   GET #1, x, Cus
		   Cus.Mbb = Cus.Mbb + Cus.Mgr - Cus.Mcr + Cus.Mtx + Cus.Mfr - Cus.Mpd
		   Cus.Ybb = Cus.Ybb + Cus.Ygr - Cus.Ycr + Cus.Ytx + Cus.Yfr - Cus.Ypd
		   Cus.Mgr = 0: Cus.Ygr = 0
		   Cus.Mcr = 0: Cus.Ycr = 0
		   Cus.Mtx = 0: Cus.Ytx = 0
		   Cus.Mfr = 0: Cus.Yfr = 0
		   Cus.Mnt = 0: Cus.Ynt = 0
		   Cus.Mpd = 0: Cus.Ypd = 0
		   Cus.Mco = 0: Cus.Yco = 0
	   PUT #1, x, Cus
	   NEXT
	   MatClose 2 'close Material Month
	END IF
	GOTO 10
CusDet: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	LOCATE 14, 20: PRINT " Printing Detailed Customers Master File "
	PageNum = 0: HCustomer
	L0 = 0: L1 = 0: L2 = 0: L3 = 0
	L4 = 0: L5 = 0: L6 = 0: L7 = 0
	L8 = 0: L9 = 0: La = 0: Lb = 0
	Lc = 0: Ld = 0: Le = 0: Lf = 0
	e = LOF(2) \ LEN(Cnx)
	FOR x = 1 TO e
	GET #3, x, Cnx
	IF Cnx.Cod <> "þþþþ" THEN
	   GET #1, Cnx.Num, Cus
	   LineNum = LineNum + 3: IF LineNum > 61 THEN HCustomer
	   LPRINT USING "& &"; Cus.Cod; LEFT$(Cus.Nam, 26);
	   LPRINT USING " #######.##"; Cus.Mgr / 100; Cus.Mcr / 100; Cus.Mtx / 100; Cus.Mfr / 100; Cus.Mnt / 100;
	   LPRINT USING " #######.##"; Cus.Mbb / 100; (Cus.Mbb + Cus.Mnt) / 100; Cus.Mpd / 100; (Cus.Mbb + Cus.Mnt - Cus.Mpd) / 100;
	   LPRINT USING " #######.##"; (Cus.Mgr - Cus.Mcr) / 100; Cus.Mco / 100; (Cus.Mgr - Cus.Mcr - Cus.Mco) / 100

	   LPRINT TAB(32);
	   LPRINT USING " #######.##"; Cus.Ygr / 100; Cus.Ycr / 100; Cus.Ytx / 100; Cus.Yfr / 100; Cus.Ynt / 100;
	   LPRINT USING " #######.##"; Cus.Ybb / 100; (Cus.Ybb + Cus.Ynt) / 100; Cus.Ypd / 100; (Cus.Ybb + Cus.Ynt - Cus.Ypd) / 100;
	   LPRINT USING " #######.##"; (Cus.Ygr - Cus.Ycr) / 100; Cus.Yco / 100; (Cus.Ygr - Cus.Ycr - Cus.Yco) / 100
	   LPRINT
	   L0 = L0 + Cus.Mgr
	   L1 = L1 + Cus.Mcr
	   L2 = L2 + Cus.Mtx
	   L3 = L3 + Cus.Mfr
	   L4 = L4 + Cus.Mnt
	   L5 = L5 + Cus.Mbb
	   L6 = L6 + Cus.Mpd
	   L7 = L7 + Cus.Mco

	   L8 = L8 + Cus.Ygr
	   L9 = L9 + Cus.Ycr
	   La = La + Cus.Ytx
	   Lb = Lb + Cus.Yfr
	   Lc = Lc + Cus.Ynt
	   Ld = Ld + Cus.Ybb
	   Le = Le + Cus.Ypd
	   Lf = Lf + Cus.Yco
	END IF
	NEXT
	LPRINT
	LPRINT TAB(32);
	LPRINT USING " #######.##"; L0 / 100; L1 / 100; L2 / 100; L3 / 100; L4 / 100; L5 / 100; (L4 + L5) / 100; L6 / 100; (L4 + L5 - L6) / 100;
	LPRINT USING " #######.##"; (L0 - L1) / 100; L7 / 100; (L0 - L1 - L7) / 100
	LPRINT TAB(32);
	LPRINT USING " #######.##"; L8 / 100; L9 / 100; La / 100; Lb / 100; Lc / 100; Ld / 100; (Lc + Ld) / 100; Le / 100; (Lc + Ld - Le) / 100;
	LPRINT USING " #######.##"; (L8 - L9) / 100; Lf / 100; (L8 - L9 - Lf) / 100
	RETURN
Errors: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	SELECT CASE ERR
		   CASE 70:   WinMssg "Permission denied"
		   CASE 200:  WinMssg "Unable to find customer code"
		   CASE ELSE: WinMssg "Error:" + STR$(ERR) + "  Line:" + STR$(ERL)
	END SELECT
	RESUME 10
Terminate: 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	CLOSE
	END

DATA 1,5, 6,8, 14,7, 21,9, 30,6, 36,10
DATA 31%,28%,31%,30%,31%,30%,31%,31%,30%,31%,30%,31%

DEFINT L, T
SUB AccDist ()
	ERASE Itm, oIt, Qty, oQt, Cog, oCo, Prc, oPr
	FOR x = 0 TO 14
		Itm(x) = MID$(Inv.Dst, 1 + 27 * x, 3):  oIt(x) = Itm(x)
		Qty(x) = MID$(Inv.Dst, 4 + 27 * x, 8):  oQt(x) = Qty(x)
		Cog(x) = MID$(Inv.Dst, 12 + 27 * x, 8): oCo(x) = Cog(x)
		Prc(x) = MID$(Inv.Dst, 20 + 27 * x, 8): oPr(x) = Prc(x)
		IF Itm(x) < "  0" THEN EXIT FOR
	NEXT
END SUB

DEFSNG A-B, D-Z
FUNCTION Confirm ()
	PCOPY 0, 1
	COLOR 14, 4: LOCATE 45
	LOCATE , 28: PRINT "                      "
	LOCATE , 28: PRINT " Press [*] to confirm "
	LOCATE , 28: PRINT "                      "
	COLOR 12, 0: LOCATE 46
	LOCATE , 27: PRINT "Û"
	LOCATE , 27: PRINT "Û"
	LOCATE , 27: PRINT "ÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛ"
	SOUND 1000, .2: SOUND 1100, .4
	KeyHit f$: IF f$ = "*" THEN Confirm = 1
	PCOPY 1, 0
END FUNCTION

DEFINT A-B, D-Z
SUB CusDisp (x)
	IF Bot > 0 AND Top > 0 THEN
	   LOCATE 2: COLOR 0, 3
	   FOR x = Bot TO Top
	   GET #3, x, Cnx
	   LOCATE , 2: PRINT USING "&ณ&"; Cnx.Cod; Cnx.Nam
	   IF CSRLIN = 49 THEN EXIT FOR
	   NEXT
	ELSE Bot = 1
	END IF
END SUB

SUB CusEdit ()
	CusShow
	p$ = Cus.Nam
	c$ = Cus.Cod
	c = Cnx.Num
610 EntryU 2, 47, 4, e, b, Cus.Cod:     IF e THEN 650 ELSE IF b THEN 610
	IF Cus.Cod < "!" THEN WinMssg "Code please?": GOTO 610
	IF Cus.Cod <> c$ THEN              'check for duplicate code
	   IF CusFind(Cus.Cod) THEN WinMssg "Duplicate Code!": GOTO 610
	END IF
611 EntryS 2, 52, 28, e, b, Cus.Nam:    IF e THEN 650 ELSE IF b THEN 610
	IF Cus.Nam < "!" THEN WinMssg "Name must not be blank": GOTO 610
612 EntryS 6, 52, 28, e, b, Cus.Man:    IF e THEN 650 ELSE IF b THEN 611
613 EntryS 7, 52, 14, e, b, Cus.Tel:    IF e THEN 650 ELSE IF b THEN 612
614 EntryS 8, 52, 28, e, b, Cus.Add:    IF e THEN 650 ELSE IF b THEN 613
615 EntryS 9, 52, 28, e, b, Cus.Str:    IF e THEN 650 ELSE IF b THEN 614
616 EntryS 10, 52, 14, e, b, Cus.Cty:   IF e THEN 650 ELSE IF b THEN 615
617 EntryU 10, 67, 2, e, b, Cus.Sta:    IF e THEN 650 ELSE IF b THEN 616
618 EntryU 10, 70, 10, e, b, Cus.Zip:   IF e THEN 650 ELSE IF b THEN 617

619 EntryS 14, 52, 28, e, b, Cus.Sco:   IF e THEN 650 ELSE IF b THEN 618
620 EntryS 15, 52, 14, e, b, Cus.Ste:   IF e THEN 650 ELSE IF b THEN 619
621 EntryS 16, 52, 28, e, b, Cus.Sad:   IF e THEN 650 ELSE IF b THEN 620
622 EntryS 17, 52, 28, e, b, Cus.SSr:   IF e THEN 650 ELSE IF b THEN 621
623 EntryS 18, 52, 14, e, b, Cus.SCt:   IF e THEN 650 ELSE IF b THEN 622
624 EntryU 18, 67, 2, e, b, Cus.SSt:    IF e THEN 650 ELSE IF b THEN 623
625 EntryU 18, 70, 10, e, b, Cus.SZi:   IF e THEN 650 ELSE IF b THEN 624

626 EntryU 22, 52, 1, e, b, Cus.Typ:    IF e THEN 650 ELSE IF b THEN 625
627 EntryD 23, 52, e, b, Cus.Dat:       IF e THEN 650 ELSE IF b THEN 626
628 EntryS 24, 52, 28, e, b, Cus.Ban:   IF e THEN 650 ELSE IF b THEN 627
629 EntryS 25, 52, 15, e, b, Cus.Acc:   IF e THEN 650 ELSE IF b THEN 628
630 EntryS 26, 52, 15, e, b, Cus.Tax:   IF e THEN 650 ELSE IF b THEN 629
631 EntryU 27, 52, 1, e, b, Cus.Pay:    IF e THEN 650 ELSE IF b THEN 630
632 EntryN 28, 52, 2, 0, e, b, Cus.Dis: IF e THEN 650 ELSE IF b THEN 631
633 EntryN 28, 71, 2, 0, e, b, Cus.Ter: IF e THEN 650 ELSE IF b THEN 632
   
	IF Confirm THEN
	   Cnx.Nam = Cus.Nam
	   Cnx.Cod = Cus.Cod
	   Cnx.Num = c
	   PUT #2, Cnx.Num, Cnx
	   PUT #1, Cnx.Num, Cus
	   IF p$ <> Cus.Nam OR c$ <> Cus.Cod THEN CusSort
	   Top = LOF(1) \ LEN(Cus)
	   CusDisp x
	ELSE
	   GOTO 633
	END IF
650
END SUB

FUNCTION CusFind (f$)
   b = 1: t = LOF(3) \ LEN(Cnx)
   DO UNTIL (t < b)
	  m = (t + b) \ 2: GET #3, m, Cnx
	  IF f$ = Cnx.Cod THEN CusFind = 1: EXIT DO
	  IF f$ > Cnx.Cod THEN b = m + 1 ELSE t = m - 1
   LOOP
END FUNCTION

DEFLNG L
DEFSNG A-K, M-Q, S-Z
SUB CusLine (r, Lm, Ly)
	IF Lm < 0 THEN COLOR 12 ELSE COLOR 10
	LOCATE r, 52: PRINT USING "########,.##-"; Lm / 100;
	IF Lm < 0 THEN COLOR 12 ELSE COLOR 10
	LOCATE r, 67: PRINT USING "########,.##-"; Ly / 100;
END SUB

DEFSNG L, R
SUB CusShow ()
	COLOR 10, 0
	GET #1, Cnx.Num, Cus
	LOCATE 3, 47:  PRINT USING "####"; Cnx.Num;
	LOCATE 2, 47:  PRINT Cus.Cod;
	LOCATE 2, 52:  PRINT Cus.Nam;
	LOCATE 6, 52:  PRINT Cus.Man;
	LOCATE 7, 52:  PRINT Cus.Tel;
	LOCATE 8, 52:  PRINT Cus.Add;
	LOCATE 9, 52:  PRINT Cus.Str;
	LOCATE 10, 52: PRINT Cus.Cty; " "; Cus.Sta; " "; Cus.Zip;
   
	LOCATE 14, 52: PRINT Cus.Sco;
	LOCATE 15, 52: PRINT Cus.Ste;
	LOCATE 16, 52: PRINT Cus.Sad;
	LOCATE 17, 52: PRINT Cus.SSr;
	LOCATE 18, 52: PRINT Cus.SCt; " "; Cus.SSt; " "; Cus.SZi;
   
	LOCATE 22, 52: PRINT Cus.Typ;
	LOCATE 23, 52: PRINT Cus.Dat;
	LOCATE 24, 52: PRINT Cus.Ban;
	LOCATE 25, 52: PRINT Cus.Acc;
	LOCATE 26, 52: PRINT Cus.Tax;
	LOCATE 27, 52: PRINT Cus.Pay;
	LOCATE 28, 52: PRINT Cus.Dis;
	LOCATE 28, 71: PRINT Cus.Ter;
	
	CusLine 31, Cus.Mgr, Cus.Ygr
	CusLine 32, Cus.Mcr, Cus.Ycr
	CusLine 33, Cus.Mtx, Cus.Ytx
	CusLine 34, Cus.Mfr, Cus.Yfr
	
	CusLine 36, Cus.Mgr - Cus.Mcr + Cus.Mtx + Cus.Mfr, Cus.Ygr - Cus.Ycr + Cus.Ytx + Cus.Yfr
	CusLine 37, Cus.Mbb, Cus.Ybb
	CusLine 38, Cus.Chg, Cus.Chg
	CusLine 40, Cus.Mbb + Cus.Mgr - Cus.Mcr + Cus.Mtx + Cus.Mfr + Cus.Chg, Cus.Ybb + Cus.Ygr - Cus.Ycr + Cus.Ytx + Cus.Yfr + Cus.Chg
	CusLine 41, Cus.Mpd, Cus.Ypd
	CusLine 43, Cus.Mbb + Cus.Mgr - Cus.Mcr + Cus.Mtx + Cus.Mfr + Cus.Chg - Cus.Mpd, Cus.Ybb + Cus.Ygr - Cus.Ycr + Cus.Ytx + Cus.Yfr + Cus.Chg - Cus.Ypd
	LOCATE 47, 43: PRINT USING "########.##"; (Cus.Mgr - Cus.Mcr) / 100;
	LOCATE 48, 43: PRINT USING "########.##"; (Cus.Ygr - Cus.Ycr) / 100;
	LOCATE 47, 56: PRINT USING "########.##"; Cus.Mco / 100;
	LOCATE 48, 56: PRINT USING "########.##"; Cus.Yco / 100;
	
	LOCATE 47, 69: PRINT USING "########.##"; (Cus.Mgr - Cus.Mcr - Cus.Mco) / 100;
	LOCATE 48, 69: PRINT USING "########.##"; (Cus.Ygr - Cus.Ycr - Cus.Yco) / 100;
END SUB

SUB CusSort ()
	CLOSE
	SHELL "AC.COM ..\Data\Customer.Inx ..\Data\Customer.Nam /L34 /+1,32 /Q"
	OpenFiles
END SUB

DEFINT R
DEFLNG L
SUB DispCus (r, L1, L2)
	LOCATE r, 55: PRINT USING "########,.##"; L1 / 100
	LOCATE r, 68: PRINT USING "########,.##"; L2 / 100
END SUB

DEFSNG L, R
SUB HCustomer ()
	Heading
	LPRINT TAB(68); "CUSTOMER MASTER LISTING"
	LPRINT : LPRINT
	LPRINT "                                  Gross     Credits     Sales       Other      Net     Beginning    Total      Total     Balance   ================================"
	LPRINT "Code       Customer Name          Sales     Returns      Tax       Charges    Amount    Balance      Due      Receipts     Due     Net Sales     Cost      Profit  "
	LPRINT "==== ========================== ========== ========== ========== ========== ========== ========== ========== ========== ========== ========== ========== =========="
END SUB

DEFINT A-Z
SUB Heading ()
	LPRINT CHR$(12); CHR$(27); "@"; : WIDTH "LPT1:", 255: LPRINT CHR$(27); "M"
	PageNum = PageNum + 1: LineNum = 13
	LPRINT "Date: "; DATE$; TAB(61); "       McKENRY PRODUCE COMPANY        "; TAB(153); "Page: "; PageNum
	LPRINT "Time: "; TIME$; TAB(61); "717 Willow Avenue, Knoxville, TN 37915"
	LPRINT :         LPRINT TAB(61); "         ACCOUNTS RECEIVABLE          ": LPRINT
END SUB

SUB HiLite (m, p)
	IF p THEN f = 4: b = 7 ELSE f = 14: b = 1
	LOCATE 50, Menu(m, 1) + 35
	f$ = MID$(" Add  Modify  Erase  Display  List  Invoices ", Menu(m, 1), Menu(m, 2))
	COLOR f, b: PRINT MID$(f$, 1, 2);
	COLOR f - 3, b: PRINT MID$(f$, 3);
END SUB

DEFSNG A-Z
SUB HitKey (f$)
	DO: f$ = INKEY$: LOOP WHILE f$ <> ""
	DO: f$ = INKEY$: TimeDate: LOOP WHILE f$ = ""
END SUB

DEFINT A-K, M-S, U-Z
DEFLNG L
DEFDBL T
SUB InvDisp ()
	COLOR 15, 1
	IF CusFind(Inx.Cus) THEN
	   GET #1, Cnx.Num, Cus
	   LOCATE 12, 2: PRINT Cus.Nam
	   LOCATE , 2:   PRINT Cus.Man
	   LOCATE , 2:   PRINT Cus.Add
	   LOCATE , 2:   PRINT Cus.Str
	   LOCATE , 2:   PRINT Cus.Cty; Cus.Sta; " "; Cus.Zip
	ELSE
	   ERROR 200
	END IF
	COLOR 14, 0
	GET #4, Inx.Num, Inv
	LOCATE 9, 3:   PRINT Inv.Num
	LOCATE 9, 46:  PRINT Inv.Ref
	LOCATE 10, 46: PRINT Inv.Man
	LOCATE 11, 12: PRINT Inv.Cus
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
	LOCATE 16, 72: PRINT Inv.net
	oCu$ = Inv.Cus
	Pmt$ = Inv.Pmt
	Grs$ = Inv.Grs
	Crd$ = Inv.Crd
	Tax$ = Inv.Tax
	Frt$ = Inv.Frt
	net$ = Inv.net
	AccDist
	LOCATE 23
	FOR x = 0 TO 14
	IF VAL(oIt(x)) > 0 THEN
	   GET #10, VAL(oIt(x)), Mat
	   COLOR 14, 0: LOCATE , 2:  PRINT Mat.Num;
	   COLOR 10, 0: LOCATE , 6:  PRINT Mat.Nam;
	   COLOR 10, 0: LOCATE , 44: PRINT Mat.Mes;
	   COLOR 14, 0: LOCATE , 35: PRINT oQt(x);
	   COLOR 10, 0: LOCATE , 47: PRINT oCo(x);
	   COLOR 10, 0: LOCATE , 56: PRINT USING "##.####"; VAL(Mat.Prc);
	   COLOR 10, 0: LOCATE , 64: PRINT USING "##.####"; VAL(oPr(x)) / VAL(oQt(x));
	   COLOR 14, 0: LOCATE , 72: PRINT oPr(x)
	   Old(VAL(oIt(x)), 1) = VAL(oQt(x)) * 100
	   Old(VAL(oIt(x)), 2) = VAL(oCo(x)) * 100
	   T1 = T1 + VAL(oCo(x))
	   T2 = T2 + VAL(oPr(x))
	ELSE
	   COLOR 3, 0: LOCATE , 2: PRINT "   ณ                            ณ        ณ  ณ        ณ       ณ       ณ        "
	END IF
	NEXT
	COLOR 15, 1
	LOCATE 39, 47: PRINT USING "#####.##"; T1; : Picture 39, 47, 8, Cst$: oCs$ = Cst$
	LOCATE 39, 72: PRINT USING "#####.##"; T2; : Picture 39, 72, 8, Amt$: oAm$ = Amt$
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

DEFLNG L
DEFDBL T
SUB InvPost ()
	ON LOCAL ERROR GOTO ErInv

	LOCK #12, 1 : GET #12, 1, Acc
	Acc.Mdb = Acc.Mdb + VAL(Inv.Pmt) * 100
	Acc.Ydb = Acc.Ydb + VAL(Inv.Pmt) * 100
	PUT #12, 1, Acc: UNLOCK #12, 1 '------------debit cash----------------

	LOCK #12, 4 : GET #12, 4, Acc
	Acc.Mdb = Acc.Mdb + VAL(Inv.net) * 100
	Acc.Ydb = Acc.Ydb + VAL(Inv.net) * 100
	PUT #12, 4, Acc: UNLOCK #12, 4 '------------debit receivable----------

	LOCK #12, 4 : GET #12, 4, Acc
	Acc.Mcr = Acc.Mcr + VAL(Inv.Pmt) * 100
	Acc.Ycr = Acc.Ycr + VAL(Inv.Pmt) * 100
	PUT #12, 4, Acc: UNLOCK #12, 4 '------------credit receivable---------

	LOCK #12, 60 : GET #12, 60, Acc
	Acc.Mdb = Acc.Mdb + VAL(Cst$) * 100
	Acc.Ydb = Acc.Ydb + VAL(Cst$) * 100
	PUT #12, 60, Acc: UNLOCK #12, 60 '----------debit cost of goods-------

	LOCK #12, 9 : GET #12, 9, Acc
	Acc.Mcr = Acc.Mcr + VAL(Cst$) * 100
	Acc.Ycr = Acc.Ycr + VAL(Cst$) * 100
	PUT #12, 9, Acc: UNLOCK #12, 9 '------------credit inventory----------

	LOCK #12, 50 : GET #12, 50, Acc
	Acc.Mcr = Acc.Mcr + VAL(Amt$) * 100
	Acc.Ycr = Acc.Ycr + VAL(Amt$) * 100
	PUT #12, 50, Acc: UNLOCK #12, 50 '----------credit sales revenue------

	LOCK #12, 51 : GET #12, 51, Acc
	Acc.Mcr = Acc.Mcr + VAL(Inv.Frt) * 100
	Acc.Ycr = Acc.Ycr + VAL(Inv.Frt) * 100
	PUT #12, 51, Acc: UNLOCK #12, 51 '----------credit other revenue------

	LOCK #12, 34 : GET #12, 34, Acc
	Acc.Mcr = Acc.Mcr + VAL(Inv.Tax) * 100
	Acc.Ycr = Acc.Ycr + VAL(Inv.Tax) * 100
	PUT #12, 34, Acc: UNLOCK #12, 34 '-----------credit Sales Tax Payable--
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

DEFSNG A-Z
SUB InvSort ()
	CLOSE
	IF Sort THEN
	   COLOR 20, 0: LOCATE 23, 35: PRINT "Sorting...."
	   Sort = 0: ERASE New
	   SHELL "AC.COM ..\Data\Invoices.INX ..\Data\Invoices.CUS /L18 /+1,4 /+9,1 /+8,1 /+7,1 /+6,1 /Q"
	   SHELL "AC.COM ..\Data\Invoices.INX ..\Data\Invoices.INV /L18 /+9,1 /+8,1 /+7,1 /+6,1 /Q"
	END IF
END SUB

DEFINT A-K, M-S, U-Z
DEFLNG L
DEFDBL T
SUB InvVoid ()
	ON LOCAL ERROR GOTO ErVoi
	LOCK #12, 1 : GET #12, 1, Acc
	Acc.Mdb = Acc.Mdb - VAL(Pmt$) * 100
	Acc.Ydb = Acc.Ydb - VAL(Pmt$) * 100
	PUT #12, 1, Acc: UNLOCK #12, 1 '---------debit cash----------------

	LOCK #12, 4 : GET #12, 4, Acc
	Acc.Mdb = Acc.Mdb - VAL(net$) * 100
	Acc.Ydb = Acc.Ydb - VAL(net$) * 100
	PUT #12, 4, Acc: UNLOCK #12, 4 '---------debit receivable----------

	LOCK #12, 4 : GET #12, 4, Acc
	Acc.Mcr = Acc.Mcr - VAL(Pmt$) * 100
	Acc.Ycr = Acc.Ycr - VAL(Pmt$) * 100
	PUT #12, 4, Acc: UNLOCK #12, 4 '---------credit receivable---------

	LOCK #12, 60 : GET #12, 60, Acc
	Acc.Mdb = Acc.Mdb - VAL(oCs$) * 100
	Acc.Ydb = Acc.Ydb - VAL(oCs$) * 100
	PUT #12, 60, Acc: UNLOCK #12, 60 '-------debit cost of goods-------

	LOCK #12, 9 : GET #12, 9, Acc
	Acc.Mcr = Acc.Mcr - VAL(oCs$) * 100
	Acc.Ycr = Acc.Ycr - VAL(oCs$) * 100
	PUT #12, 9, Acc: UNLOCK #12, 9 '---------credit Inventory----------

	LOCK #12, 50 : GET #12, 50, Acc
	Acc.Mcr = Acc.Mcr - VAL(oAm$) * 100
	Acc.Ycr = Acc.Ycr - VAL(oAm$) * 100
	PUT #12, 50, Acc: UNLOCK #12, 50 '-------credit sales revenue------

	LOCK #12, 51 : GET #12, 51, Acc
	Acc.Mcr = Acc.Mcr - VAL(Frt$) * 100
	Acc.Ycr = Acc.Ycr - VAL(Frt$) * 100
	PUT #12, 51, Acc: UNLOCK #12, 51 '-------credit other revenue------

	LOCK #12, 34 : GET #12, 34, Acc
	Acc.Mcr = Acc.Mcr - VAL(Tax$) * 100
	Acc.Ycr = Acc.Ycr - VAL(Tax$) * 100
	PUT #12, 34, Acc: UNLOCK #12, 34 '-------credit Sales Tax Payable--
EXIT SUB

ErVoi:
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

DEFSNG A-Z
SUB KeyHit (f$)
'    DO: LOOP WHILE INKEY$ <> ""
	DO: f$ = INKEY$: LOOP WHILE f$ = ""
	f$ = UCASE$(f$)
END SUB

SUB KeyPres (f$)
	DO: LOOP WHILE INKEY$ > ""
	DO: f$ = INKEY$: LOOP WHILE f$ = ""
	f$ = UCASE$(f$)
END SUB

DEFINT A-Z
SUB MatClose (p)
	SELECT CASE p
		CASE 1 '==close month==
			FOR x = 1 TO LOF(10) \ LEN(Mat)
				GET #10, x, Mat
				Mat.Mbq = Mat.Mbq + Mat.Mpq + Mat.Mmq - Mat.Mkq - Mat.Msq - Mat.Mwq
				Mat.Mbp = Mat.Mbp + Mat.Mpp + Mat.Mmp - Mat.Mkp - Mat.Msc - Mat.Mwp
				Mat.Mpq = 0: Mat.Mpp = 0 'purchases
				Mat.Mmq = 0: Mat.Mmp = 0 'production
				Mat.Mkq = 0: Mat.Mkp = 0 'requisitions
				Mat.Msq = 0: Mat.Msc = 0 'cost of sales
				Mat.Mwq = 0: Mat.Mwp = 0 'spoilage
				Mat.Msp = 0              'sales
				PUT #10, x, Mat
			NEXT
		CASE 2 '==close year==
			FOR x = 1 TO LOF(10) \ LEN(Mat)
				GET #10, x, Mat
				Mat.Ybq = Mat.Ybq + Mat.Ypq + Mat.Ymq - Mat.Ykq - Mat.Ysq - Mat.Ywq
				Mat.Ybp = Mat.Ybp + Mat.Ypp + Mat.Ymp - Mat.Ykp - Mat.Ysc - Mat.Ywp
				Mat.Ypq = 0: Mat.Ypp = 0 'purchases
				Mat.Ymq = 0: Mat.Ymp = 0 'production
				Mat.Ykq = 0: Mat.Ykp = 0 'requisitions
				Mat.Ysq = 0: Mat.Ysc = 0 'cost of sales
				Mat.Ywq = 0: Mat.Ywp = 0 'spoilage
				Mat.Ygq = 0: Mat.Ygp = 0 'gains or loss
				Mat.Ysp = 0              'sales
				PUT #10, x, Mat
			NEXT
	END SELECT
END SUB

DEFSNG A-Z
SUB Monitor ()
	SCREEN 0, 0, 0, 0
	DEF SEG = 0
	   Segment = &HB800
	DEF SEG
END SUB

DEFINT A-Z
FUNCTION NewItem ()
	FOR x = 1 TO 300
	IF New(x) > "        " THEN
	   IF Inv.Num = New(x) THEN NewItem = 1
	ELSE EXIT FOR
	END IF
	NEXT
END FUNCTION

FUNCTION NumFind (f&)
   b = 1: t = LOF(7) \ LEN(Inx)
   DO UNTIL (t < b)
	  m = (t + b) \ 2: GET #7, m, Inx
	  IF f& = Inx.Inv THEN NumFind = 1: EXIT DO
	  IF f& > Inx.Inv THEN b = m + 1 ELSE t = m - 1
   LOOP
END FUNCTION

DEFLNG L
DEFSNG A-K, M-Q, S, U-Z
DEFDBL T
SUB OnHand (r)
	Lqt = Mat.Ybq + Mat.Ypq + Mat.Ymq + Old(VAL(Itm(r)), 1)
	Lqt = Lqt - Mat.Ykq - Mat.Ysq - Mat.Ywq - Mat.Ygq
	Lpr = Mat.Ybp + Mat.Ypp + Mat.Ymp + Old(VAL(Itm(r)), 2)
	Lpr = Lpr - Mat.Ykp - Mat.Ysc - Mat.Ywp - Mat.Ygp
	Tup = VAL(Mat.Ref)
	IF Tup = 0 THEN
	   IF Lqt = 0 OR Lpr = 0 THEN Tup = VAL(Mat.Ref) ELSE Tup = Lpr / Lqt
	END IF
END SUB

DEFSNG L, R, T
SUB OpenFiles ()
	InvSort
	OPEN "..\Data\Customer.MAS" FOR RANDOM LOCK READ WRITE AS #1 LEN = LEN(Cus)
	OPEN "..\Data\Customer.INX" FOR RANDOM LOCK READ WRITE AS #2 LEN = LEN(Cnx)
	OPEN "..\Data\Customer.NAM" FOR RANDOM LOCK READ WRITE AS #3 LEN = LEN(Cnx)
  
	OPEN "..\Data\Invoices.MAS" FOR RANDOM LOCK READ WRITE AS #4 LEN = LEN(Inv)
	OPEN "..\Data\Invoices.INX" FOR RANDOM LOCK READ WRITE AS #5 LEN = LEN(Inx)
	OPEN "..\Data\Invoices.CUS" FOR RANDOM LOCK READ WRITE AS #6 LEN = LEN(Inx)
	OPEN "..\Data\Invoices.INV" FOR RANDOM LOCK READ WRITE AS #7 LEN = LEN(Inx)
	OPEN "..\Data\Invoices.DUE" FOR RANDOM LOCK READ WRITE AS #8 LEN = LEN(Inx)
	OPEN "..\Data\Invoices.Chk" FOR RANDOM LOCK READ WRITE AS #9 LEN = LEN(Rct)
   
	OPEN "..\Data\Material.MAS" FOR RANDOM LOCK READ WRITE AS #10 LEN = LEN(Mat)
	OPEN "..\Data\Material.INX" FOR RANDOM LOCK READ WRITE AS #11 LEN = LEN(Mnx)
	OPEN "..\Data\Accounts.MAS" FOR RANDOM SHARED AS #12 LEN = LEN(Acc)
END SUB

SUB RctPost ()
	ON LOCAL ERROR GOTO ErRct

	LOCK #12, 1 : GET #12, 1, Acc
	Acc.Mdb = Acc.Mdb + VAL(Rct.Amt) * 100
	Acc.Ydb = Acc.Ydb + VAL(Rct.Amt) * 100
	PUT #12, 1, Acc: UNLOCK #12, 1 '--------------------<Debit Cash>

	LOCK #12, 4 : GET #12, 4, Acc
	Acc.Mcr = Acc.Mcr + VAL(Rct.Amt) * 100
	Acc.Ycr = Acc.Ycr + VAL(Rct.Amt) * 100
	PUT #12, 4, Acc: UNLOCK #12, 4 '--------------------<Credit A/R>
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

SUB WinInv (e, t)
	SCREEN , , 1, 0
	PCOPY 0, 1
	b = e: r = 6: n = b: GOSUB W2: SCREEN , , 1, 1
W1: GET #6, n, Inx: GET #4, Inx.Num, Inv
	COLOR 0, 7: LOCATE r, 3: PRINT USING "&ณ&ณ&ณ&"; Inv.Num; Inv.Bdt; Inv.Chk; Inv.net
	KeyHit n$
	COLOR 15, 4: LOCATE r, 3: PRINT USING "&ณ&ณ&ณ&"; Inv.Num; Inv.Bdt; Inv.Chk; Inv.net
	SELECT CASE n$
	CASE CHR$(13)
		SCREEN , , 2, 2
		InvDisp
		KeyHit f$
		SCREEN , , 1, 1
	CASE CHR$(0) + "P" 'Dn
		IF n + 1 > t GOTO W1 ELSE n = n + 1
		IF r + 1 > 20 THEN CALL scroll(3, 6, 41, 20, 1) ELSE r = r + 1
	CASE CHR$(0) + "H" 'Up
		IF n - 1 < e GOTO W1 ELSE n = n - 1
		IF r - 1 < 6 THEN CALL BkScroll(3, 6, 41, 20, 1) ELSE r = r - 1
	CASE CHR$(0) + "I" 'PgUp
		b = n - r - 9: IF b < e THEN b = e
		GOSUB W2: n = b + r - 6
	CASE CHR$(0) + "Q" 'PgDn
		b = 21 - r + n
		IF t - 14 > e THEN
		   IF b > t - 14 THEN b = t - 14: n = t - 20 + r ELSE n = n + 15
		   GOSUB W2
		END IF
	CASE CHR$(0) + "G" 'Home
		b = e: GOSUB W2: n = e + r - 6
	CASE CHR$(0) + "O" 'End
		b = t - 14: IF b < e THEN b = e
		GOSUB W2: n = b + r - 6
	CASE ELSE
		SCREEN , , 0, 0
		EXIT SUB
	END SELECT
	GOTO W1

W2: COLOR 15, 4: LOCATE 5, 1: PRINT " ษอNUMBERอัอออDATEอออัออCHECKอออัอAMOUNTอป "
	FOR x = b TO t: GET #6, x, Inx: GET #4, Inx.Num, Inv
	PRINT USING " บ&ณ&ณ&ณ&บ "; Inv.Num; Inv.Bdt; Inv.Chk; Inv.net
	IF CSRLIN > 20 THEN EXIT FOR
	NEXT
	PRINT " ศออออออออฯออออออออออฯออออออออออฯออออออออผ "
	RETURN
END SUB

SUB WinMat (f$)
	PCOPY 0, 1
	SCREEN , , 1, 1
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
	PCOPY 0, 1
	COLOR 14, 4: LOCATE 12
	LOCATE , 36: PRINT "ษอ"; STRING$(LEN(f$), "อ"); "อป"
	LOCATE , 36: PRINT "บ "; f$; " บ"
	LOCATE , 36: PRINT "ศอ"; STRING$(LEN(f$), "อ"); "อผ"
	COLOR 12, 0: LOCATE 13
	LOCATE , 35: PRINT "Û"
	LOCATE , 35: PRINT "Û"
	LOCATE , 35: PRINT "ÛÛÛÛ"; STRING$(LEN(f$), "Û")
	SOUND 660, .1: SOUND 770, .1: KeyHit c$
	PCOPY 1, 0
END SUB


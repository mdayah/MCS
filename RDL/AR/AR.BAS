DEFINT A-Z: DEFLNG L
DECLARE SUB KeyHit (f$)
DECLARE SUB Logo ()
DECLARE SUB EraseRec (f$)
DECLARE SUB NoErase (f$)
DECLARE SUB Updated (f$)
DECLARE SUB Lookup (f$)
DECLARE SUB DateDue (b$, d$)
DECLARE SUB AgingDays (f$, n)
DECLARE SUB StampName (x, f$, n$)
DECLARE SUB DispLine (r, m&, y&)
DECLARE SUB Picture (r, c, s, f$)
DECLARE SUB EntryD (r, c, e, b, b$, d$)
DECLARE SUB EntryS (r, c, s, e, b, f$)
DECLARE SUB EntryN (r, c, s, e, b, f, f$)
DECLARE SUB Heading ()
DECLARE SUB HDeposit ()
DECLARE SUB HInvoice ()
DECLARE SUB HMatMas ()
DECLARE SUB HProMas ()
DECLARE SUB HCusMas ()
DECLARE SUB HAging ()
DECLARE SUB HFlow ()
DECLARE SUB HStatement ()
TYPE Customer
   Num AS STRING * 2: Nam AS STRING * 30: Add AS STRING * 30
   Cty AS STRING * 20: Sta AS STRING * 2: Zip AS STRING * 5
   Tel AS STRING * 14: Crd AS LONG
   Mbf AS LONG: Mgr AS LONG: Mtx AS LONG: Mpd AS LONG
   Ybf AS LONG: Ygr AS LONG: Ytx AS LONG: Ypd AS LONG
   END TYPE
TYPE Procedure
   Num AS STRING * 3: Nam AS STRING * 30
   Qnt AS STRING * 2: Set AS STRING * 5
   Sng AS STRING * 5: Tax AS STRING * 1
   Mqt AS LONG: Mpr AS LONG
   Yqt AS LONG: Ypr AS LONG
   END TYPE
TYPE Invoices
   Cus AS STRING * 2: Bdt AS STRING * 8: Ddt AS STRING * 8
   Pdt AS STRING * 8: Chk AS STRING * 8: Pmt AS STRING * 6
   Grs AS STRING * 6: Tax AS STRING * 5: Net AS STRING * 6
   Dst AS STRING * 216
   END TYPE
TYPE Index
   Ddt AS STRING * 6: Cus AS STRING * 2: Mrk AS STRING * 1
   Inv AS STRING * 5
   END TYPE

   DIM Cus AS Customer
   DIM Pro AS Procedure
   DIM Inv AS Invoices
   DIM Inx AS Index

   DIM SHARED DayN(12) AS INTEGER
   DIM SHARED LineNum AS INTEGER
   DIM SHARED PageNum AS INTEGER
   DIM SHARED Segment AS INTEGER

   DIM Pt(0 TO 8) AS STRING * 13
   DIM Pn(0 TO 8) AS STRING * 3
   DIM Pq(0 TO 8) AS STRING * 2
   DIM Pp(0 TO 8) AS STRING * 5
   DIM Tx(0 TO 8) AS STRING * 4

   DIM oPt(0 TO 7) AS STRING * 13
   DIM oPn(0 TO 7) AS STRING * 3
   DIM oPq(0 TO 8) AS STRING * 2
   DIM oPp(0 TO 7) AS STRING * 5
   DIM oTx(0 TO 8) AS STRING * 4
  
   DIM PayNum(0 TO 14) AS STRING * 6
   DIM PayAmt(0 TO 14) AS STRING * 6

   FOR x = 1 TO 12: READ DayN(x): NEXT
   KEY OFF: KEY 1, "1": KEY 2, "2": KEY 3, "3": KEY 4, "4": KEY 5, "5"
			KEY 6, "6": KEY 7, "7": KEY 8, "8": KEY 9, "9": KEY 10, "0"
   OPEN "INVOICES.NUM" FOR INPUT AS #1
   INPUT #1, LastInv, EndInv, LastChk, EndChk: CLOSE
   SalesTax! = 8!
   Segment = &HB800
   p$ = "######"
   q$ = "########.##"
'-----------------------------------------------------------------------------
'purge upaid invoices.
'   CLOSE
'   OPEN "Invoices.New" FOR RANDOM AS #1 LEN = LEN(Inv)
'   OPEN "Invoices.Nnx" FOR RANDOM AS #2 LEN = LEN(Inx)
'   OPEN "Invoices.DTA" FOR RANDOM AS #3 LEN = LEN(Inv)
'   FOR x = 1 TO LOF(3) / LEN(Inv)
'   GET #3, x, Inv
'   IF Inv.Net <> Inv.Pmt THEN
'      n = n + 1
'      Inx.Ddt = RIGHT$(Inv.Ddt, 2) + LEFT$(Inv.Ddt, 2) + MID$(Inv.Ddt, 4, 2)
'      Inx.Cus = Inv.Cus
'      Inx.Inv = RIGHT$(STR$(100000 + n), 5)
'      Inx.Mrk = " "
'      PUT #1, n, Inv
'      PUT #2, n, Inx
'      PRINT n, Inv.Net, Inv.Pmt
'   END IF
'   NEXT
'   CLOSE
'   KILL "Invoices.DTA"
'   KILL "Invoices.INX"
'   KILL "Invoices.DUE"
'   KILL "Invoices.CUS"
'   NAME "Invoices.New" AS "Invoices.DTA"
'   NAME "Invoices.Nnx" AS "Invoices.INX"
'   SHELL "SORTF INVOICES.INX INVOICES.DUE /L14 /+1,8"
'   SHELL "SORTF INVOICES.DUE INVOICES.CUS /L14 /+7,8"
'   STOP
'-----------------------------------------------------------------------------
'  ON TIMER(900) GOSUB StpClk
'=========================================================================
10  COLOR 0, 15: CLOSE : TIMER OFF
	DEF SEG = Segment: BLOAD "Screen.MNU", 0: DEF SEG
	LOCATE 23, 21: PRINT USING "[Li#####]  [Ei#####] [Lc#####] [Ec#####]"; LastInv; EndInv; LastChk; EndChk;
20  LOCATE 10, 23, 0: PRINT DATE$; : LOCATE 10, 48: PRINT " "; TIME$; " ";
	k$ = INKEY$: IF k$ = "" THEN 20
	IF k$ = "0" THEN GOTO Terminate
	Choice = VAL(k$): GOSUB OpenFiles
	ON Choice GOTO Invoices, NewInv, Payment, NewCheck, Reports, Customer, Procedure, Materials, Utility
	GOTO 10
Invoices: '==================================================================
	DEF SEG = Segment: BLOAD "Screen.Inv", 0: DEF SEG : Rec$ = "     "
110 EntryN 4, 15, 5, e, b, f, Rec$: IF e THEN 10 ELSE IF b THEN 110
	ERASE Pt, Pn, Pq, Pp, Tx, oPt, oPn, oPq, oPp, oTx
	LAmt = 0: LTax = 0: Num$ = "": Grs$ = "": Tax$ = ""
	COLOR 0, 7
	IF VAL(Rec$) = 0 THEN
	   Rec$ = RIGHT$(STR$(1000001 + LOF(3) / LEN(Inv)), 5)
	   LOCATE 4, 15: PRINT Rec$;
	   LOCATE 6, 3: PRINT "Enter NEW invoice  "
	   Inv.Cus = "": Inv.Bdt = "": Inv.Pdt = "**-**-**"
	   Inv.Ddt = "": Inv.Pmt = "": Inv.Chk = "Not Paid"
	ELSE
	   IF VAL(Rec$) > LOF(3) / LEN(Inv) THEN LOCATE 6, 3: PRINT "No such invoice    ": BEEP: GOTO 110
	   LOCATE 6, 3: PRINT "Update Old invoice "
	   GOSUB UpdateInv
	   IF VAL(Inv.Pmt) > 0 THEN LOCATE 6, 3: PRINT "Locked PAID invoice": GOTO 110
	END IF
120 EntryN 4, 36, 2, e, b, f, Inv.Cus: IF e THEN 10 ELSE IF b THEN 120
	IF VAL(Inv.Cus) > 60 OR VAL(Inv.Cus) < 1 THEN Lookup "C": GOTO 120
	IF f = 0 THEN
	   GET #1, VAL(Inv.Cus), Cus
	   IF Cus.Nam > "!" THEN
		  LOCATE 5, 25: PRINT Cus.Nam: LOCATE 6, 25: PRINT Cus.Add
		  LOCATE 7, 25: PRINT Cus.Cty; " "; Cus.Sta; "  "; Cus.Zip
		  Inv.Ddt = "": Inv.Cus = Cus.Num
	   ELSE
		  Inv.Cus = "": GOTO 120
	   END IF
	END IF
121 EntryD 3, 72, e, b, Inv.Bdt, Inv.Ddt: IF e THEN 10 ELSE IF b THEN 120
	IF Inv.Bdt < "!" THEN
	   Inv.Bdt = LEFT$(DATE$, 6) + RIGHT$(DATE$, 2)
	   DateDue Inv.Bdt, Inv.Ddt
	   LOCATE 3, 72: PRINT Inv.Bdt
	END IF
122 EntryD 4, 72, e, b, Inv.Ddt, f$: IF e THEN 10 ELSE IF b THEN 121
	IF Inv.Ddt < "!" THEN DateDue Inv.Bdt, Inv.Ddt: LOCATE 4, 72: PRINT Inv.Ddt

130 x = 0
131 EntryS x + 11, 2, 13, e, b, Pt(x): IF e THEN 10
	IF b THEN x = x - 1: IF x < 0 THEN 122 ELSE 131
	IF x = 0 THEN IF Pt(x) < "!" THEN LOCATE , 2: PRINT "Patient name?": BEEP: GOTO 131
132 EntryN x + 11, 16, 3, e, b, f, Pn(x): IF e THEN 10 ELSE IF b THEN 131
	IF Pn(x) < "  1" THEN
	   IF x < 7 THEN
		  CALL scroll(2, x + 11, 79, 18, 1)
		  IF Pt(x) > "!" THEN Pt(x + 1) = Pt(x): LOCATE , 2: PRINT Pt(x)
	   END IF
	   COLOR 7, 0: LOCATE 18, 15: PRINT "³   ³"; SPC(30); "³     ³  ³      ³     ³       "
	   LAmt = LAmt - VAL(Pp(x)): LTax = LTax - VAL(Tx(x)): COLOR 10, 0: GOSUB 180
	   FOR z = x TO 7
	   Pt(z) = Pt(z + 1): Pn(z) = Pn(z + 1): Pq(z) = Pq(z + 1)
	   Pp(z) = Pp(z + 1): Tx(z) = Tx(z + 1)
	   NEXT
	   IF Pn(x) < "!" AND Pt(x) < "!" THEN 140 ELSE 132
	END IF
	IF Pn(x) > "120" OR Pn(x) < "  1" THEN Lookup "P": GOTO 132
	IF f = 0 THEN
	  GET #2, VAL(Pn(x)), Pro
	  IF Pro.Nam > "!" THEN
		 LOCATE x + 11, 16: PRINT Pro.Num: Pn(x) = Pro.Num
		 LOCATE x + 11, 20: PRINT Pro.Nam
		 LOCATE x + 11, 51: PRINT Pro.Set: GOSUB Pprc
	  ELSE
		 LOCATE x + 11, 20: PRINT "Blank Record.................."
		 Pn(x) = "  0": BEEP: GOTO 132
	  END IF
	END IF
133 EntryN x + 11, 57, 2, e, b, f, Pq(x): IF e THEN 10 ELSE IF b THEN 132
	IF Pq(x) < " 1" THEN BEEP: GOTO 133
	IF f = 0 THEN GOSUB Pprc
134 LAmt = LAmt - VAL(Pp(x))
	EntryN x + 11, 61, 5, e, b, f, Pp(x): IF e THEN 10
	IF Pn(x) = "060" OR Pn(x) = "120" THEN
	   IF ABS(VAL(Pp(x))) > 9999 THEN BEEP: Pp(x) = " >MAX": GOTO 134
	   LOCATE x + 11, 61: PRINT USING "#####"; -ABS(VAL(Pp(x)));
	   Picture x + 11, 61, 5, Pp(x)
	END IF
	LAmt = LAmt + VAL(Pp(x)): GOSUB 170
	IF b THEN 133
	IF f = 0 THEN GOSUB Ptax
135 LTax = LTax - VAL(Tx(x))
	EntryN x + 11, 68, 4, e, b, f, Tx(x): IF e THEN 10
	IF Pn(x) = "060" OR Pn(x) = "120" THEN
	   IF ABS(VAL(Tx(x))) > 999 THEN BEEP: Tx(x) = ">MAX": GOTO 135
	   LOCATE x + 11, 68: PRINT USING "####"; -ABS(VAL(Tx(x)));
	   Picture x + 11, 68, 4, Tx(x)
	END IF
	LTax = LTax + VAL(Tx(x)): GOSUB 170
	IF b THEN 134
139 x = x + 1: IF x < 8 THEN 131

140 Picture 20, 60, 6, Inv.Grs
	Picture 20, 67, 5, Inv.Tax
	Picture 20, 74, 6, Inv.Net
	'IF VAL(Inv.Net) < 0 THEN 120
	COLOR 0, 7: SOUND 1200, 2
	LOCATE 20, 3: PRINT "Hit END to save invoice  ";
	KeyHit f$: IF f$ = CHR$(27) THEN 10
	IF f$ = "1" THEN
	   Sort = 1
	   PRINT " POSTING ";
	   f$ = "": FOR x = 0 TO 7
	   IF Pn(x) < "!" THEN EXIT FOR
	   f$ = f$ + Pt(x) + Pn(x) + Pq(x) + Pp(x) + Tx(x)
	   GET #2, VAL(Pn(x)), Pro
	   Pro.Mqt = Pro.Mqt + VAL(Pq(x)): Pro.Mpr = Pro.Mpr + VAL(Pp(x))
	   Pro.Yqt = Pro.Yqt + VAL(Pq(x)): Pro.Ypr = Pro.Ypr + VAL(Pp(x))
	   PUT #2, VAL(Pn(x)), Pro '------------------------------(save procedure)
	   NEXT
	   Inv.Dst = f$
	   Inx.Mrk = " "
	   Inv.Pdt = "**-**-**"
	   Inv.Chk = "Not Paid"
	   Inv.Pmt = "      "
	   IF VAL(Inv.Net) = 0 THEN
		  Inx.Mrk = "P"
		  Inv.Pdt = LEFT$(DATE$, 6) + RIGHT$(DATE$, 2)
		  Inv.Chk = "        "
		  Inv.Pmt = "     0"
	   ELSE
		  IF Cus.Crd > 0 THEN
			 Inv.Pdt = Inv.Bdt: Inv.Chk = "Cr Appld"
			 IF Cus.Crd >= VAL(Inv.Net) THEN
				 Inv.Pmt = Inv.Net: Inx.Mrk = "P"
				 Cus.Crd = Cus.Crd - VAL(Inv.Pmt)
			 ELSE
				 RSET Inv.Pmt = RTRIM$(STR$(Cus.Crd))
				 Cus.Crd = 0
			 END IF
		  END IF
	   END IF
	   PUT #3, VAL(Rec$), Inv '-------------------------------(save invoice  )
	   Inx.Ddt = RIGHT$(Inv.Ddt, 2) + LEFT$(Inv.Ddt, 2) + MID$(Inv.Ddt, 4, 2)
	   Inx.Cus = Inv.Cus
	   Inx.Inv = Rec$
	   PUT #4, VAL(Rec$), Inx '-------------------------------(save index    )
	   Cus.Mgr = Cus.Mgr + VAL(Inv.Grs): Cus.Ygr = Cus.Ygr + VAL(Inv.Grs)
	   Cus.Mtx = Cus.Mtx + VAL(Inv.Tax): Cus.Ytx = Cus.Ytx + VAL(Inv.Tax)
	   PUT #1, VAL(Inv.Cus), Cus '----------------------------(save customer )
			  
	   IF Num$ > "" THEN
		  PRINT " REVERSING "
		  GET #1, VAL(Num$), Cus
		  Cus.Mgr = Cus.Mgr - VAL(Grs$): Cus.Ygr = Cus.Ygr - VAL(Grs$)
		  Cus.Mtx = Cus.Mtx - VAL(Tax$): Cus.Ytx = Cus.Ytx - VAL(Tax$)
		  PUT #1, VAL(Num$), Cus '----------------------------(edit customer )
		  FOR x = 0 TO 7
		  IF oPn(x) < "!" THEN EXIT FOR
		  GET #2, VAL(oPn(x)), Pro
		  Pro.Mqt = Pro.Mqt - VAL(oPq(x)): Pro.Mpr = Pro.Mpr - VAL(oPp(x))
		  Pro.Yqt = Pro.Yqt - VAL(oPq(x)): Pro.Ypr = Pro.Ypr - VAL(oPp(x))
		  PUT #2, VAL(oPn(x)), Pro '--------------------------(edit procedure)
		  NEXT
	   ELSE
		  SOUND 1200, 2
		  LOCATE 20, 3: PRINT "Hit END to print invoices"; SPC(20);
		  KeyHit f$: IF f$ = CHR$(27) THEN 10
		  IF f$ = "1" THEN
			 COLOR 7, 0: LOCATE 23, 1: PRINT SPACE$(80);
			 FromInv = LastInv + 1: ToInv = LOF(3) / LEN(Inv)
			 GOSUB PrintInv: LastInv = x - 1
		  END IF
	   END IF
	   GOTO Invoices
	END IF
	COLOR 7, 0: LOCATE 20, 3: PRINT SPC(45); : GOTO 120

Pprc: GET #2, VAL(Pn(x)), Pro: LOCATE x + 11, 61
	  L1 = VAL(Pro.Set) * (VAL(Pq(x)) \ VAL(Pro.Qnt)) + VAL(Pro.Sng) * (VAL(Pq(x)) MOD VAL(Pro.Qnt))
	  IF L1 > 99999 THEN PRINT "*****": BEEP: Pq(x) = "  ": RETURN 133 ELSE PRINT USING "#####"; L1
	  LAmt = LAmt - VAL(Pp(x)): Picture x + 11, 61, 5, Pp(x): LAmt = LAmt + VAL(Pp(x))
Ptax: GET #2, VAL(Pn(x)), Pro: LTax = LTax - VAL(Tx(x)): LOCATE x + 11, 68
	  IF Pro.Tax = "Y" THEN Tax! = SalesTax! * VAL(Pp(x)) / 100 ELSE Tax! = 0
	  PRINT USING "####"; Tax!
	  Picture x + 11, 68, 4, Tx(x): LTax = LTax + VAL(Tx(x))
170   LOCATE x + 11, 74: PRINT USING "######"; VAL(Pp(x)) + VAL(Tx(x))
180   LOCATE 20, 60: PRINT USING "######"; LAmt
	  LOCATE 20, 67: PRINT USING "#####"; LTax
	  LOCATE 20, 74: PRINT USING "######"; LAmt + LTax;
	  RETURN
UpdateInv: '=================================================================
	GET #4, VAL(Rec$), Inx: GET #3, VAL(Inx.Inv), Inv: GET #1, VAL(Inv.Cus), Cus
	COLOR 0, 7: LOCATE 4, 15: PRINT Inx.Inv; : LOCATE , 36: PRINT Inv.Cus
	COLOR 12, 0
	LOCATE 3, 72: PRINT Inv.Bdt: LOCATE 4, 72: PRINT Inv.Ddt
	LOCATE 5, 25: PRINT Cus.Nam: LOCATE 5, 72: PRINT Inv.Pdt
	LOCATE 6, 25: PRINT Cus.Add: LOCATE 6, 72: PRINT Inv.Chk
	LOCATE 7, 25: PRINT Cus.Cty; " "; Cus.Sta; "  "; Cus.Zip: LOCATE 7, 74: PRINT Inv.Pmt
	Num$ = Inv.Cus: Rec$ = Inx.Inv: z = 0
	FOR x = 0 TO 7: LOCATE x + 11
	Pt(x) = MID$(Inv.Dst, z + 1, 13): LOCATE , 2:  PRINT Pt(x); : oPt(x) = Pt(x)
	Pn(x) = MID$(Inv.Dst, z + 14, 3): LOCATE , 16: PRINT Pn(x); : oPn(x) = Pn(x)
	IF Pn(x) < "!" THEN EXIT FOR
			 GET #2, VAL(Pn(x)), Pro: LOCATE , 20: PRINT Pro.Nam;
									  LOCATE , 51: PRINT Pro.Set;
	Pq(x) = MID$(Inv.Dst, z + 17, 2): LOCATE , 57: PRINT Pq(x); : oPq(x) = Pq(x)
	Pp(x) = MID$(Inv.Dst, z + 19, 5): LOCATE , 61: PRINT Pp(x); : oPp(x) = Pp(x)
	Tx(x) = MID$(Inv.Dst, z + 24, 4): LOCATE , 68: PRINT Tx(x); : oTx(x) = Tx(x)
	LAmt = LAmt + VAL(Pp(x)): LTax = LTax + VAL(Tx(x))
	LOCATE , 74: PRINT USING "######"; VAL(Pp(x)) + VAL(Tx(x))
	z = z + 27
	NEXT
	LOCATE 20, 60: PRINT Inv.Grs: Grs$ = Inv.Grs
	LOCATE 20, 67: PRINT Inv.Tax: Tax$ = Inv.Tax
	LOCATE 20, 74: PRINT Inv.Net;
	COLOR 7, 0
	FOR z = x TO 7
	LOCATE z + 11, 2: PRINT SPC(13); "³   ³"; SPC(30); "³     ³  ³      ³     ³       "
	NEXT
	RETURN
NewInv: '====================================================================
	FromInv = LastInv + 1: ToInv = LOF(3) / LEN(Inv)
	GOSUB PrintInv: LastInv = x - 1
	GOTO 10
PrintInv: '==================================================================
	COLOR 0, 7
	LOCATE 23, 21: PRINT "Change Forms to INVOICES and hit any key"
	KeyHit f$: IF f$ = CHR$(27) THEN 10
	LOCATE 23, 21: PRINT "          Printing Invoices             "
	FOR x = FromInv TO ToInv
	IF INKEY$ = CHR$(27) THEN EXIT FOR
	Logo
	GET #4, x, Inx: GET #3, x, Inv: GET #1, VAL(Inv.Cus), Cus
	LPRINT TAB(60); "Invoice N§ "; CHR$(14); CHR$(27); "r"; "1"; Inx.Inv;
	LPRINT CHR$(27); "F"; CHR$(27); "M"; CHR$(27); "r"; "0"
	LPRINT TAB(10); "Dr. "; Cus.Nam
	LPRINT TAB(14); Cus.Add;
	LPRINT TAB(74); "Invoice Date: "; CHR$(27); "r"; "2"; Inv.Bdt; CHR$(27); "r"; "0"
	LPRINT TAB(14); RTRIM$(Cus.Cty); ",  "; Cus.Sta; "  "; Cus.Zip;
	LPRINT TAB(74); "    Due Date: "; CHR$(27); "r"; "2"; Inv.Ddt; CHR$(27); "r"; "0"
	LPRINT
	LPRINT "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍ»"
	LPRINT "º               ³                                ³    ³  Unit   ³ Extended³  Sales  ³  Total  º"
	LPRINT "º    Patient    ³           Procedure            ³ Qt ³  Price  ³   Price ³   Tax   ³  Amount º"
	LPRINT "ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄ¶"
	z = 0
	FOR y = 0 TO 7
	Pt0$ = MID$(Inv.Dst, z + 1, 13)
	Pn0$ = MID$(Inv.Dst, z + 14, 3)
	IF Pn0$ < "!" THEN
	   LPRINT "º               ³                                ³    ³         ³         ³         ³         º"
	ELSE
	   GET #2, VAL(Pn0$), Pro
	   Pq0$ = MID$(Inv.Dst, z + 17, 2)
	   Pp0$ = MID$(Inv.Dst, z + 19, 5)
	   Tx0$ = MID$(Inv.Dst, z + 24, 4)
	   LPRINT "º "; Pt0$; TAB(17); "³ "; Pro.Nam; " ³ "; Pq0$; " ";
	   LPRINT USING "³ ####.## "; VAL(Pp0$) / VAL(Pq0$) / 100; VAL(Pp0$) / 100; VAL(Tx0$) / 100; (VAL(Pp0$) + VAL(Tx0$)) / 100;
	   LPRINT "º"
	   LAmt = LAmt + VAL(Pp0$) + VAL(Tx0$)
	END IF
	z = z + 27
	NEXT
	LPRINT "ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÁÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄ¶"
	LPRINT "º                                                               ³";
	LPRINT USING " ####.## ³"; VAL(Inv.Grs) / 100; VAL(Inv.Tax) / 100;
	LPRINT CHR$(27); "r"; "1"; CHR$(27); "G";
	LPRINT USING " ####.## "; VAL(Inv.Net) / 100;
	LPRINT CHR$(27); "H"; CHR$(27); "r"; "0"; "º"
	LPRINT "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍ¼"
	LPRINT : LPRINT : LPRINT : LPRINT : LPRINT : LPRINT : LPRINT
	NEXT
	RETURN
Payment: '===================================================================
	IF Sort THEN GOSUB SortFiles
	DEF SEG = Segment: BLOAD "Screen.Pay", 0: DEF SEG
	COLOR 7, 0: FOR x = 1 TO 23: GET #1, x, Cus
	LOCATE x + 1, 2: PRINT Cus.Num; "³"; Cus.Nam; : NEXT
	r = 12: GOSUB CheckFile
300 Picture r, 2, 2, f$: x = VAL(f$)
302 GET #1, x, Cus
	COLOR 0, 7: LOCATE r, 2, 0: PRINT Cus.Num + "³" + Cus.Nam;
	KeyHit c$
	IF LEN(c$) = 2 THEN c$ = RIGHT$(c$, 1)
304 SELECT CASE c$
	CASE "H", "P"
		 COLOR 7, 0: LOCATE r, 2, 0: PRINT Cus.Num + "³" + Cus.Nam;
		 IF c$ = "P" THEN r = r + 1 ELSE r = r - 1
		 IF r < 2 THEN r = 2: IF Cus.Num = "01" THEN SOUND 800, 2 ELSE CALL bkscroll(2, 2, 34, 24, 1): x = x - 1: GOTO 302
		 IF r > 24 THEN r = 24: IF Cus.Num = "60" THEN SOUND 800, 2 ELSE CALL scroll(2, 2, 34, 24, 1): x = x + 1: GOTO 302
	CASE CHR$(27)
		 GOTO 10
	CASE CHR$(13)
		 COLOR 13, 0: LOCATE r, 2, 0: PRINT Cus.Num + "³" + Cus.Nam;
		 COLOR 0, 7: ERASE PayNum, PayAmt: LAmt = 0: GOSUB SearchInv
		 FOR i = 4 TO 18
306      GET #6, n
		 LOCATE i, 37
		 IF B6$ = Cus.Num THEN
			GET #3, VAL(D6$), Inv: n = n + 1
			IF Inv.Pmt = Inv.Net THEN 306
			PRINT USING "&³"; D6$; Inv.Bdt; Inv.Ddt;
			PRINT USING "#######º  º       "; VAL(Inv.Net) - VAL(Inv.Pmt)
			LAmt = LAmt + VAL(Inv.Net) - VAL(Inv.Pmt): PayNum(i - 4) = D6$
		 ELSE
			PRINT "     ³        ³        ³       º  º       "
		 END IF
		 NEXT
		 LOCATE 20, 61: PRINT USING "#######"; LAmt
		 LOCATE 20, 38: PRINT SPC(15); : LOCATE , 72: PRINT SPC(7);
		 LOCATE 23, 50: PRINT SPC(8); : LOCATE , 72: PRINT SPC(7);
		 LOCATE 24, 50: PRINT SPC(8); : LOCATE , 72: PRINT SPC(7);
		 IF LAmt = 0 THEN 300 ELSE Lpay = 0

307      FOR y = 0 TO 14
		 IF PayNum(y) < "!" THEN EXIT FOR
308      GET #3, VAL(PayNum(y)), Inv: Lpay = Lpay - VAL(PayAmt(y))
		 EntryN y + 4, 73, 6, e, b, f, PayAmt(y): IF e THEN 300
		 Lpay = Lpay + VAL(PayAmt(y)): LOCATE 20, 72: PRINT USING "#######"; Lpay
		 IF VAL(PayAmt(y)) > VAL(Inv.Net) - VAL(Inv.Pmt) THEN BEEP: GOTO 308
		 IF b THEN
			y = y - 1: IF y < 0 THEN y = 0
			GOTO 308
		 END IF
		 NEXT
		
		 IF Lpay = 0 THEN 307
310      EntryD 23, 50, e, b, ChkDte$, d$: IF e THEN 300
		 IF b THEN
			y = y - 1: IF y < 0 THEN y = 0
			GOTO 308
		 END IF
		 IF ChkDte$ < "0" THEN BEEP: GOTO 310
312      EntryS 24, 50, 8, e, b, ChkNum$: IF e THEN 300 ELSE IF b THEN 310
		 IF ChkNum$ < "!" THEN BEEP: GOTO 312
314      EntryN 23, 72, 7, e, b, f, ChkAmt$: IF e THEN 300 ELSE IF b THEN 312
		 IF VAL(ChkAmt$) < Lpay THEN BEEP: GOTO 314
		 IF VAL(ChkAmt$) > Lpay AND Lpay < LAmt THEN BEEP: GOTO 314
		 LOCATE 24, 72: PRINT USING "#######"; VAL(ChkAmt$) - Lpay;
		 Picture 24, 72, 7, f$: LCrd = VAL(f$)
		 COLOR 7, 0: SOUND 1200, 2: LOCATE 20, 38: PRINT " END will post "
		 KeyHit f$
		 IF f$ = "1" THEN
			Sort = 1: f$ = "": LOCATE 20, 38: PRINT "Check is POSTED"
			FOR y = 0 TO 14
			IF VAL(PayAmt(y)) > 0 THEN
			   GET #3, VAL(PayNum(y)), Inv
			   Inv.Pdt = ChkDte$
			   Inv.Chk = ChkNum$
			   RSET Inv.Pmt = LTRIM$(RTRIM$(STR$(VAL(Inv.Pmt) + VAL(PayAmt(y)))))
			   PUT #3, VAL(PayNum(y)), Inv
			   IF Inv.Pmt = Inv.Net THEN
				  GET #4, VAL(PayNum(y)), Inx
				  Inx.Mrk = "P"
				  PUT #4, VAL(PayNum(y)), Inx
			   END IF
			   END IF
			NEXT
			Cus.Crd = Cus.Crd + LCrd
			Cus.Mpd = Cus.Mpd + VAL(ChkAmt$)
			Cus.Ypd = Cus.Ypd + VAL(ChkAmt$)
			PUT #1, x, Cus
			LSET A7$ = Cus.Num
			LSET B7$ = ChkDte$
			LSET C7$ = ChkNum$
			LSET D7$ = ChkAmt$
			PUT #7, LOF(7) / 25 + 1
			GOTO 300
		 END IF
		 COLOR 0, 7: LOCATE 20, 38: PRINT "               ": GOTO 307
	CASE ELSE
	END SELECT
	GOTO 300
NewCheck: '==================================================================
	FromChk = LastChk + 1: GOSUB Deposits
	IF x > LOF(7) / 25 THEN LastChk = LOF(7) / 25
	GOTO 10
Deposits: '==================================================================
	L1 = 0: L2 = 0: PageNum = 0: HDeposit: GOSUB CheckFile
	LOCATE 23, 21: PRINT "        Printing Check Receipts         "
	FOR x = FromChk TO LOF(7) / 25
	IF INKEY$ = CHR$(27) THEN EXIT FOR
	GET #7, x: GET #1, VAL(A7$), Cus
	LPRINT USING "&  &   #####   &    & $$######,.##"; Cus.Num; Cus.Nam; x; B7$; C7$; VAL(D7$) / 100
	L1 = L1 + VAL(D7$): L2 = L2 + 1
	LineNum = LineNum + 1: IF LineNum > 58 THEN HDeposit
	NEXT
	LPRINT : LPRINT TAB(44); : LPRINT USING " ##### Checks Totaling $$######,.##"; L2; L1 / 100
	RETURN
Reports: '===================================================================
	IF Sort THEN GOSUB SortFiles
	DEF SEG = Segment: BLOAD "Screen.RPT", 0: DEF SEG
500 LOCATE 10, 23, 0: PRINT DATE$; : LOCATE 10, 48: PRINT " "; TIME$; " ";
	TIMER ON: k$ = INKEY$: IF k$ = "" THEN 500 ELSE TIMER OFF
	IF k$ = "0" THEN 10 ELSE Choice = VAL(k$): COLOR 0, 7
	ON Choice GOSUB CusMas, ProMas, MatMas, Aging, CashFlow, Register, ReprtInv, ReprtSta, AddLabel
	GOTO Reports
CusMas: '====================================================================
	L1 = 0: L2 = 0: L3 = 0: L4 = 0: L5 = 0: L6 = 0: L7 = 0: L8 = 0
	LOCATE 23, 21: PRINT "        Printing Clients Summary        "
	PageNum = 0: HCusMas
	FOR x = 1 TO 60
	IF INKEY$ = CHR$(27) THEN EXIT FOR
	GET #1, x, Cus
	IF Cus.Nam > "!" THEN
	   LPRINT Cus.Num; " "; Cus.Nam; "  ";
	   LPRINT USING "######.##"; Cus.Mbf / 100; Cus.Mgr / 100; Cus.Mtx / 100; Cus.Mpd / 100; (Cus.Mbf + Cus.Mgr + Cus.Mtx - Cus.Mpd) / 100; : LPRINT "  ";
	   LPRINT USING "#######.##"; Cus.Ybf / 100; Cus.Ygr / 100; Cus.Ytx / 100; Cus.Ypd / 100; (Cus.Ybf + Cus.Ygr + Cus.Ytx - Cus.Ypd) / 100
	   L1 = L1 + Cus.Mbf: L2 = L2 + Cus.Mgr: L3 = L3 + Cus.Mtx: L4 = L4 + Cus.Mpd
	   L5 = L5 + Cus.Ybf: L6 = L6 + Cus.Ygr: L7 = L7 + Cus.Ytx: L8 = L8 + Cus.Ypd
	   LineNum = LineNum + 1: IF LineNum > 58 THEN HCusMas
	END IF
	NEXT
	LPRINT : LPRINT TAB(36);
	LPRINT USING "######.##"; L1 / 100; L2 / 100; L3 / 100; L4 / 100; (L1 + L2 + L3 - L4) / 100; : LPRINT "  ";
	LPRINT USING "#######.##"; L5 / 100; L6 / 100; L7 / 100; L8 / 100; (L5 + L6 + L7 - L8) / 100
	RETURN
ProMas: '====================================================================
	L1 = 0: L2 = 0: L3 = 0: L4 = 0: PageNum = 0: HProMas
	LOCATE 23, 21: PRINT "      Printing Procedures Summary       "
	FOR x = 1 TO 60
	IF INKEY$ = CHR$(27) THEN EXIT FOR
	GET #2, x, Pro
	IF Pro.Nam > "!" THEN
	   LPRINT USING "&  &  [&]  ####.##"; Pro.Num; Pro.Nam; Pro.Tax; VAL(Pro.Set) / 100;
	   LPRINT USING "    ######  #######,.##"; Pro.Mqt; Pro.Mpr / 100; Pro.Yqt; Pro.Ypr / 100
	   L1 = L1 + Pro.Mqt: L2 = L2 + Pro.Mpr: L3 = L3 + Pro.Yqt: L4 = L4 + Pro.Ypr
	   LineNum = LineNum + 1: IF LineNum > 58 THEN HProMas
	END IF
	NEXT
	LPRINT : LPRINT TAB(50); : LPRINT USING "    ######  #######,.##"; L1; L2 / 100; L3; L4 / 100
	RETURN
MatMas: '====================================================================
	L1 = 0: L2 = 0: L3 = 0: L4 = 0: PageNum = 0: HMatMas
	LOCATE 23, 21: PRINT "      Printing Materials Summary        "
	FOR x = 61 TO 120
	IF INKEY$ = CHR$(27) THEN EXIT FOR
	GET #2, x, Pro
	IF Pro.Nam > "!" THEN
	   LPRINT USING "& &  &  ###.## ###.##"; Pro.Num; Pro.Nam; Pro.Qnt; VAL(Pro.Set) / 100; VAL(Pro.Sng) / 100;
	   LPRINT USING "   ###### #######,.##"; Pro.Mqt; Pro.Mpr / 100; Pro.Yqt; Pro.Ypr / 100
	   L1 = L1 + Pro.Mqt: L2 = L2 + Pro.Mpr: L3 = L3 + Pro.Yqt: L4 = L4 + Pro.Ypr
	   LineNum = LineNum + 1: IF LineNum > 58 THEN HMatMas
	END IF
	NEXT
	LPRINT : LPRINT TAB(54); : LPRINT USING "   ###### #######,.##"; L1; L2 / 100; L3; L4 / 100
	RETURN
Aging: '=====================================================================
	L1 = 0: L2 = 0: L3 = 0: L4 = 0: L5 = 0: L6 = 0: L7 = 0
	LA = 0: LB = 0: LC = 0: LD = 0: LE = 0: LF = 0: LG = 0
	LOCATE 23, 21: PRINT "     Printing Aging of Receivables      "
	Firstime = 1: Prev$ = "": PageNum = 0: HAging
	ToDt$ = RIGHT$(DATE$, 2) + LEFT$(DATE$, 2) + MID$(DATE$, 4, 2)
	AgingDays ToDt$, w: File$ = "CUS": GOSUB IndexFile
	FOR x = 1 TO LOF(6) / 14: GET #6, x
	IF INKEY$ = CHR$(27) THEN EXIT FOR
	IF C6$ = " " THEN
	   IF Prev$ <> B6$ THEN
		  IF Firstime THEN Firstime = 0 ELSE GOSUB 532
		  GET #1, VAL(B6$), Cus: LPRINT LEFT$(Cus.Nam, 24); : Prev$ = B6$
	   END IF
	   GET #3, VAL(D6$), Inv: AgingDays A6$, z
	   LPRINT TAB(26); : LPRINT USING " &"; Inv.Ddt; D6$;
	   d = (365 * (VAL(LEFT$(A6$, 2)) - VAL(LEFT$(ToDt$, 2)))) + (z - w)
	   IF d < -60 THEN y = 42: L1 = L1 + VAL(Inv.Net) - VAL(Inv.Pmt): GOTO 530
	   IF d < -30 THEN y = 50: L2 = L2 + VAL(Inv.Net) - VAL(Inv.Pmt): GOTO 530
	   IF d < 0 THEN y = 58:   L3 = L3 + VAL(Inv.Net) - VAL(Inv.Pmt): GOTO 530
	   IF d = 0 THEN y = 66:   L4 = L4 + VAL(Inv.Net) - VAL(Inv.Pmt): GOTO 530
	   IF d > 60 THEN y = 90:  L7 = L7 + VAL(Inv.Net) - VAL(Inv.Pmt): GOTO 530
	   IF d > 30 THEN y = 82:  L6 = L6 + VAL(Inv.Net) - VAL(Inv.Pmt): GOTO 530
	   IF d > 0 THEN y = 74:   L5 = L5 + VAL(Inv.Net) - VAL(Inv.Pmt): GOTO 530
530    LPRINT TAB(y); : LPRINT USING "####.##"; (VAL(Inv.Net) - VAL(Inv.Pmt)) / 100
	   LineNum = LineNum + 1: IF LineNum > 58 THEN HAging
	END IF
	NEXT
	GOSUB 532: LPRINT : LPRINT TAB(41);
	LPRINT USING " ####.##"; LA / 100; LB / 100; LC / 100; LD / 100; LE / 100; LF / 100; LG / 100
	RETURN
532 LPRINT : LPRINT TAB(41); : LPRINT USING "#####.##"; L1 / 100; L2 / 100; L3 / 100; L4 / 100; L5 / 100; L6 / 100; L7 / 100: LPRINT
	LA = LA + L1: LB = LB + L2: LC = LC + L3: LD = LD + L4: LE = LE + L5: LF = LF + L6: LG = LG + L7
	L1 = 0: L2 = 0: L3 = 0: L4 = 0: L5 = 0: L6 = 0: L7 = 0
	LineNum = LineNum + 3: IF LineNum > 58 THEN HAging
	RETURN
CashFlow: '====================================================================
	L1 = 0: L2 = 0: L3 = 0: L4 = 0: L5 = 0: L6 = 0
	LOCATE 23, 21: PRINT "     Printing Receivables Cash Flow     "
	Prev$ = "": Firstime = 1: PageNum = 0: HFlow
	File$ = "DUE": GOSUB IndexFile
	FOR x = 1 TO LOF(6) / 14: GET #6, x
	IF INKEY$ = CHR$(27) THEN EXIT FOR
	IF C6$ = " " THEN
	   GET #3, VAL(D6$), Inv: GET #1, VAL(B6$), Cus
	   IF Firstime THEN Firstime = 0: Prev$ = A6$
	   IF Prev$ <> A6$ THEN
		  L4 = L4 + L1: L5 = L5 + L2: L6 = L6 + L3
		  LPRINT : LPRINT TAB(53);
		  LPRINT USING "#####.## "; L1 / 100; L2 / 100; L3 / 100; L6 / 100;
		  LPRINT LEFT$(Prev$, 2); "-"; MID$(Prev$, 3, 2); "-"; RIGHT$(Prev$, 2): LPRINT
		  LineNum = LineNum + 3: Prev$ = A6$: L1 = 0: L2 = 0: L3 = 0
	   END IF
	   IF LineNum > 58 THEN HFlow ELSE LineNum = LineNum + 1
	   LPRINT USING "& "; LEFT$(Cus.Nam, 27); D6$; Inv.Bdt; Inv.Ddt;
	   LPRINT USING "#####.## "; VAL(Inv.Net) / 100; VAL(Inv.Pmt) / 100; (VAL(Inv.Net) - VAL(Inv.Pmt)) / 100
	   L1 = L1 + VAL(Inv.Net): L2 = L2 + VAL(Inv.Pmt): L3 = L1 - L2
	END IF
	NEXT x
	LPRINT : LPRINT TAB(53); : LPRINT USING "#####.## "; L1 / 100; L2 / 100; L3 / 100; (L6 + L3) / 100;
	LPRINT LEFT$(Prev$, 2); "-"; MID$(Prev$, 3, 2); "-"; RIGHT$(Prev$, 2)
	LPRINT : LPRINT TAB(53); : LPRINT USING "#####.## "; (L1 + L4) / 100; (L2 + L5) / 100; (L3 + L6) / 100; (L3 + L6) / 100
	RETURN
AddLabel: '==================================================================
	LOCATE 23, 21: PRINT " Change Forms to LABELS and hit any key "
	EntryS 23, 61, 1, e, b, f$: IF e THEN RETURN
	LOCATE 23, 21: PRINT "    Printing Clients' Address Labels    "
	LPRINT CHR$(27); "@";
	WIDTH LPRINT 255
	LPRINT CHR$(27); "E";
	FOR x = 1 TO 60
	IF INKEY$ = CHR$(27) THEN EXIT FOR
	GET #1, x, Cus
	IF Cus.Nam > "!" THEN
	   LPRINT : LPRINT
	   LPRINT Cus.Nam
	   LPRINT Cus.Add
	   LPRINT RTRIM$(Cus.Cty); ",  "; Cus.Sta; "  "; Cus.Zip
	   LPRINT
	END IF
	NEXT
	RETURN
Register: '==================================================================
	L1 = 0: L2 = 0: L3 = 0: L4 = 0: COLOR 0, 7
	LOCATE 23, 21: PRINT "       Printing Invoice Register        "
	PageNum = 0: HInvoice
	FOR x = EndInv + 1 TO LOF(3) / LEN(Inv)
	IF INKEY$ = CHR$(27) THEN EXIT FOR
	GET #4, x, Inx: GET #3, x, Inv: GET #1, VAL(Inv.Cus), Cus
	LPRINT USING "& "; Inx.Inv; LEFT$(Cus.Nam, 26); Inv.Bdt; : LPRINT Inv.Ddt;
	LPRINT USING "  ####.##"; VAL(Inv.Grs) / 100; VAL(Inv.Tax) / 100; VAL(Inv.Net) / 100; VAL(Inv.Pmt) / 100; (VAL(Inv.Net) - VAL(Inv.Pmt)) / 100
	L1 = L1 + VAL(Inv.Grs): L2 = L2 + VAL(Inv.Tax): L3 = L3 + VAL(Inv.Net): L4 = L4 + VAL(Inv.Pmt)
	LineNum = LineNum + 1: IF LineNum > 58 THEN HInvoice
	NEXT
	LPRINT
	LPRINT TAB(51); : LPRINT USING "######.##"; L1 / 100; L2 / 100; L3 / 100; L4 / 100; (L3 - L4) / 100
	FromChk = EndChk + 1: GOSUB Deposits
	RETURN
ReprtInv: '==================================================================
	f$ = "     "
	LOCATE 23, 21: PRINT "Reprint from invoice: [     ] to [     ]"
510 EntryN 23, 44, 5, e, b, f, f$: IF e THEN RETURN ELSE IF b THEN 510
	FromInv = VAL(f$): IF FromInv < 1 THEN BEEP: GOTO 510
	IF FromInv > LOF(3) / LEN(Inv) THEN BEEP: GOTO 510
512 EntryN 23, 55, 5, e, b, f, f$: IF e THEN RETURN ELSE IF b THEN 510
	ToInv = VAL(f$): IF ToInv > LOF(3) / LEN(Inv) THEN BEEP: GOTO 512
	IF ToInv < FromInv THEN BEEP: GOTO 512
	GOSUB PrintInv: RETURN
ReprtSta: '==================================================================
	f$ = "  ": COLOR 0, 7
	LOCATE 23, 21: PRINT "    Reprint statement for client:       "
520 EntryN 23, 56, 2, e, b, f, f$: IF e THEN RETURN ELSE IF b THEN 520
	IF VAL(f$) > 60 OR VAL(f$) < 1 THEN BEEP: GOTO 520
	GET #1, VAL(f$), Cus: IF Cus.Nam < "!" THEN BEEP: GOTO 520
	GOSUB Message
	FromCus = VAL(f$): ToCus = VAL(f$): GOSUB Statement: RETURN
Message: '===================================================================
	Message1$ = SPACE$(80): Message2$ = SPACE$(80)
	LOCATE 23, 1: PRINT "ÃÂÂÂÅÂÂÂÂ1ÂÂÂÂÅÂÂÂÂ2ÂÂÂÂÅÂÂÂÂ3ÂÂÂÂÅÂÂÂÂ4ÂÂÂÂÅÂÂÂÂ5ÂÂÂÂÅÂÂÂÂ6ÂÂÂÂÅÂÂÂÂ7ÂÂÂÂÅÂÂÂÂ´";
522 LOCATE 22, 1: PRINT "Type message:";
524 EntryS 24, 1, 80, e, b, Message1$: IF e OR b THEN 524
526 EntryS 25, 1, 80, e, b, Message2$: IF e THEN 10 ELSE IF b THEN 524
	LOCATE 22, 1: PRINT "[Y] to accept";
	IF UCASE$(INPUT$(1)) = "Y" THEN RETURN ELSE 522
Statement: '=================================================================
	COLOR 0, 7
	LOCATE 23, 21: PRINT " Change Forms to STATEMENTS and hit key "
	IF INPUT$(1) = CHR$(27) THEN RETURN
	LOCATE 23, 21: PRINT "       Printing Client Statements       "
	L1 = 0: L2 = 0: L3 = 0: L4 = 0
	FOR x = FromCus TO ToCus
	IF INKEY$ = CHR$(27) THEN EXIT FOR
	GET #1, x, Cus
	IF Cus.Nam > "!" THEN
	   PageNum = 0: GOSUB HStatement
	   FOR y = 1 TO LOF(3) / LEN(Inv)
	   GET #4, y, Inx
	   IF Inx.Cus = Cus.Num THEN
		  IF LineNum > 62 THEN
			 GOSUB StatTotal
			 LPRINT TAB(83); "Continued..."
			 GOSUB HStatement
		  END IF
		  GET #3, VAL(Inx.Inv), Inv
		  IF Inx.Mrk = "P" THEN
			 Today$ = LEFT$(DATE$, 2) + RIGHT$(DATE$, 2)
			 InvDt$ = LEFT$(Inv.Bdt, 2) + RIGHT$(Inv.Bdt, 2)
			 PayDt$ = LEFT$(Inv.Pdt$, 2) + RIGHT$(Inv.Pdt, 2)
			 IF InvDt$ = Today$ OR PayDt$ = Today$ THEN GOSUB StatLine
		  ELSE
			 GOSUB StatLine
		  END IF
	   END IF
	   NEXT
	   WHILE LineNum < 63
	   LPRINT "º       ³          ³         ³         ³         ³         ³          ³          ³           º"
	   LineNum = LineNum + 1: WEND
	   GOSUB StatTotal
	   LPRINT CHR$(27); "4"; CHR$(27); "G"; CHR$(27); "E"; CHR$(27); "r"; "1";
	   LPRINT "Balance is due by 10th of month.  A service charge of 1«% will be applied to unpaid balance."
	   L1 = 0: L2 = 0: L3 = 0: L4 = 0
	END IF
	NEXT
	RETURN
StatLine:
	LPRINT "º "; Inx.Inv; " ³ "; Inv.Bdt; " ³";
	LPRINT USING " ####.## ³"; VAL(Inv.Grs) / 100; VAL(Inv.Tax) / 100; VAL(Inv.Net) / 100; VAL(Inv.Pmt) / 100;
	IF Inv.Pdt = "**-**-**" THEN
	   LPRINT "          ³          ³";
	ELSE
	   LPRINT USING " & ³"; Inv.Pdt; Inv.Chk;
	END IF
	LPRINT USING " #####,.## º"; (VAL(Inv.Net) - VAL(Inv.Pmt)) / 100
	L1 = L1 + VAL(Inv.Grs): L2 = L2 + VAL(Inv.Tax): L3 = L3 + VAL(Inv.Net): L4 = L4 + VAL(Inv.Pmt)
	LineNum = LineNum + 1
	RETURN
StatTotal:
	LPRINT "ÇÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄ¶"
	LPRINT "º                  ³";
	LPRINT USING "#####.## ³"; L1 / 100; L2 / 100; L3 / 100; L4 / 100;
	LPRINT "          ³          ³ "; CHR$(27); "r"; "1"; CHR$(27); "G";
	LPRINT USING "#####,.## "; (L3 - L4) / 100;
	LPRINT CHR$(27); "H"; CHR$(27); "r"; "0"; "º"
	LPRINT "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍ¼"
	RETURN
HStatement: '================================================================
	LPRINT CHR$(12); : Logo: PageNum = PageNum + 1: LineNum = 30
	LPRINT TAB(30); "  Monthly  Statement   "
	LPRINT TAB(30); "   Date: "; DATE$; TAB(71);
	LPRINT USING "Page: ###"; PageNum
	LPRINT CHR$(27); "M"; CHR$(27); "F"; CHR$(27); "H"
	LPRINT TAB(13); "ÚÄ"; TAB(50); "Ä¿"
	LPRINT TAB(13); "  Dr. "; Cus.Nam
	LPRINT TAB(19); Cus.Add
	LPRINT TAB(19); RTRIM$(Cus.Cty); ",  "; Cus.Sta; "  "; Cus.Zip
	LPRINT TAB(13); "ÀÄ"; TAB(50); "ÄÙ"
	LPRINT : LPRINT
	LPRINT CHR$(27); "r"; "1"; CHR$(27); "G"; CHR$(27); "E";
	LPRINT "       "; Message1$
	LPRINT "       "; Message2$; CHR$(27); "r"; "0"; CHR$(27); "H"; CHR$(27); "F"
	LPRINT "ÉÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍ»"
	LPRINT "º  Inv  ³ Invoice  ³   Sub   ³  Sales  ³         ³ Amount  ³   Date   ³  Check   ³    Net    º"
	LPRINT "º  Num  ³   Date   ³  Total  ³   Tax   ³  Total  ³  Paid   ³   Paid   ³  Number  ³    Due    º"
	LPRINT "ÇÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄ¶"
	RETURN
PrintSta: '==================================================================
	GOSUB Message
	FromCus = 1: ToCus = 60: GOSUB Statement: RETURN
Customer: '==================================================================
	DEF SEG = Segment: BLOAD "Screen.Cus", 0: DEF SEG
	COLOR 7, 0: FOR x = 1 TO 23: GET #1, x, Cus
	LOCATE x + 1, 2: PRINT Cus.Num; "³"; Cus.Nam; : NEXT
	r = 12
600 Picture r, 2, 2, f$: x = VAL(f$)
602 GET #1, x, Cus
	COLOR 0, 7: LOCATE r, 2, 0: PRINT Cus.Num + "³" + Cus.Nam;
	KeyHit c$
	IF LEN(c$) = 2 THEN c$ = RIGHT$(c$, 1) ELSE c$ = UCASE$(c$)
604 SELECT CASE c$
	CASE "H", "P"
		 COLOR 7, 0: LOCATE r, 2, 0: PRINT Cus.Num + "³" + Cus.Nam;
		 IF c$ = "P" THEN r = r + 1 ELSE r = r - 1
		 IF r < 2 THEN r = 2: IF Cus.Num = "01" THEN SOUND 800, 2 ELSE CALL bkscroll(2, 2, 34, 24, 1): x = x - 1: GOTO 602
		 IF r > 24 THEN r = 24: IF Cus.Num = "60" THEN SOUND 800, 2 ELSE CALL scroll(2, 2, 34, 24, 1): x = x + 1: GOTO 602
	CASE CHR$(27)
		 GOTO 10
	CASE "I", "S"
		 DEF SEG = Segment: BSAVE "SCREEN.TMP", 0, &H1000: DEF SEG
		 SS$ = "             "
		 IF c$ = "S" THEN EntryS 25, 10, 13, e, b, SS$: IF e OR b THEN 600
		 IF Sort THEN GOSUB SortFiles
		 GOSUB SearchInv: A = 2: q = 1
606      DEF SEG = Segment: BLOAD "Screen.Lst", 0: DEF SEG
		 COLOR 7, 0: LOCATE 2, 5
		 DO WHILE B6$ = Cus.Num AND CSRLIN < 24
			GET #3, VAL(D6$), Inv: n = n + 1
			p = INSTR(UCASE$(Inv.Dst), UCASE$(RTRIM$(SS$)))
			IF p > 0 THEN
			   LOCATE , 5: PRINT USING "&³"; D6$; MID$(Inv.Dst, p, 8); Inv.Bdt; Inv.Ddt; Inv.Pdt; Inv.Chk; " " + Inv.Net; " " + Inv.Pmt; : PRINT USING "#######"; VAL(Inv.Net) - VAL(Inv.Pmt)
			END IF
			GET #6, n
		 LOOP
608      COLOR 12, 0: LOCATE A, 3: PRINT CHR$(16)
		 KeyHit s$
		 IF LEN(s$) = 2 THEN s$ = RIGHT$(s$, 1)
		 LOCATE A, 3: PRINT " "
		 SELECT CASE s$
		 CASE "H"
			  A = A - 1: IF A < 2 THEN A = 23
		 CASE "P"
			  A = A + 1: IF A > 23 THEN A = 2
		 CASE CHR$(13)
			  Picture A, 5, 5, Rec$
			  IF Rec$ > "     " THEN
				 DEF SEG = Segment: BSAVE "SCREEN.XXX", 0, &H1000
				 BLOAD "Screen.INV", 0: DEF SEG
				 GOSUB UpdateInv
				 TIMER ON: WHILE INKEY$ = "": WEND: TIMER OFF
				 DEF SEG = Segment: BLOAD "Screen.XXX", 0: DEF SEG
			  END IF
		 CASE "M"
			  IF q THEN 606
		 CASE CHR$(27)
			  DEF SEG = Segment: BLOAD "Screen.TMP", 0: DEF SEG : GOTO 600
		 CASE ELSE
		 END SELECT
		 GOTO 608
	CASE "D", "U", "E", "C"
		 COLOR 7, 0
		 LOCATE 2, 38: PRINT Cus.Nam: LOCATE 3, 38: PRINT Cus.Add
		 LOCATE 4, 38: PRINT Cus.Cty; " "; Cus.Sta; "  "; Cus.Zip
		 LOCATE 5, 50: PRINT Cus.Tel
		 COLOR 0, 7
		 DispLine 10, Cus.Mgr, Cus.Ygr
		 DispLine 11, Cus.Mtx, Cus.Ytx: L1 = Cus.Mgr + Cus.Mtx: L2 = Cus.Ygr + Cus.Ytx
		 DispLine 13, L1, L2
		 DispLine 14, Cus.Mbf, Cus.Ybf: L1 = L1 + Cus.Mbf: L2 = L2 + Cus.Ybf
		 DispLine 16, L1, L2
		 DispLine 17, Cus.Mpd, Cus.Ypd: L1 = L1 - Cus.Mpd: L2 = L2 - Cus.Ypd
		 DispLine 19, L1, L2
		 IF c$ = "C" THEN GOSUB Verify
		 IF c$ = "E" THEN
			IF Cus.Ygr > 0 THEN NoErase f$ ELSE EraseRec f$
			IF f$ = "1" THEN
			   Cus.Nam = SPACE$(30)
			   Cus.Add = SPACE$(30)
			   Cus.Cty = SPACE$(20)
			   Cus.Sta = SPACE$(2)
			   Cus.Zip = SPACE$(5)
			   Cus.Tel = SPACE$(14)
			   Cus.Crd = 0
			   Cus.Mbf = 0: Cus.Mgr = 0: Cus.Mtx = 0: Cus.Mpd = 0
			   Cus.Ybf = 0: Cus.Ygr = 0: Cus.Ytx = 0: Cus.Ypd = 0
			   PUT #1, x, Cus
			   StampName x, "C", Cus.Nam
			END IF
			GOTO 602
		 END IF
		 IF c$ = "U" THEN
			COLOR 13, 0: LOCATE r, 2, 0: PRINT Cus.Num + "³" + Cus.Nam;
610         EntryS 2, 38, 30, e, b, Cus.Nam: IF e THEN 600 ELSE IF b THEN 615
611         EntryS 3, 38, 30, e, b, Cus.Add: IF e THEN 600 ELSE IF b THEN 610
612         EntryS 4, 38, 20, e, b, Cus.Cty: IF e THEN 600 ELSE IF b THEN 611
613         EntryS 4, 59, 2, e, b, Cus.Sta:  IF e THEN 600 ELSE IF b THEN 612
614         EntryN 4, 63, 5, e, b, f, Cus.Zip: IF e THEN 600 ELSE IF b THEN 613
615         EntryS 5, 50, 14, e, b, Cus.Tel: IF e THEN 600 ELSE IF b THEN 614
			IF Cus.Nam > "!" THEN Updated f$: IF f$ = "1" THEN PUT #1, x, Cus: StampName x, "C", Cus.Nam: GOTO 602
			GOTO 610
		 END IF
	CASE ELSE
	END SELECT
	GOTO 600
Verify: '====================================================================
	FOR y = 1 TO LOF(3) / LEN(Inv)
	GET #3, y, Inv: GET #4, x, Inx
	IF Inv.Cus = Cus.Num THEN L1 = L1 + VAL(Inv.Net): L2 = L2 + VAL(Inv.Pmt)
	NEXT
	IF Cus.Ybf + Cus.Ygr + Cus.Ytx - Cus.Ypd <> L1 - L2 THEN BEEP: COLOR 7, 0
	LOCATE 19, 56: PRINT USING "#######,.##"; (L1 - L2) / 100
	RETURN
Procedure: '=================================================================
	DEF SEG = Segment: BLOAD "Screen.Pro", 0: DEF SEG
	COLOR 7, 0: FOR x = 1 TO 23: GET #2, x, Pro
	LOCATE x + 1, 2: PRINT Pro.Num; "³"; Pro.Nam; : NEXT
	r = 12
700 Picture r, 2, 3, f$: x = VAL(f$)
702 GET #2, x, Pro: COLOR 0, 7: LOCATE r, 2, 0: PRINT Pro.Num + "³" + Pro.Nam;
	KeyHit c$
	IF LEN(c$) = 2 THEN c$ = RIGHT$(c$, 1) ELSE c$ = UCASE$(c$)
704 SELECT CASE c$
	CASE "H", "P"
		 COLOR 7, 0: LOCATE r, 2, 0: PRINT Pro.Num + "³" + Pro.Nam;
		 IF c$ = "P" THEN r = r + 1 ELSE r = r - 1
		 IF r < 2 THEN r = 2: IF Pro.Num = "001" THEN SOUND 800, 2 ELSE CALL bkscroll(2, 2, 35, 24, 1): x = x - 1: GOTO 702
		 IF r > 24 THEN r = 24: IF Pro.Num = "060" THEN SOUND 800, 2 ELSE CALL scroll(2, 2, 35, 24, 1): x = x + 1: GOTO 702
	CASE CHR$(27)
		 GOTO 10
	CASE "D", "U", "E"
		 COLOR 12, 0
		 LOCATE 2, 38: PRINT Pro.Nam
		 LOCATE 4, 62: PRINT Pro.Set
		 LOCATE 5, 66: PRINT Pro.Tax
		 COLOR 0, 7
		 LOCATE 13, 60: PRINT USING "######,"; Pro.Mqt: LOCATE 13, 72: PRINT USING "######,"; Pro.Yqt
		 LOCATE 18, 56: PRINT USING q$; Pro.Mpr / 100: LOCATE 18, 68: PRINT USING q$; Pro.Ypr / 100
		 IF c$ = "E" THEN
			IF Pro.Ypr > 0 THEN NoErase f$ ELSE EraseRec f$
			IF f$ = "1" THEN
			   Pro.Nam = SPACE$(30)
			   Pro.Qnt = SPACE$(2)
			   Pro.Set = SPACE$(5)
			   Pro.Sng = SPACE$(5)
			   Pro.Tax = SPACE$(1)
			   Pro.Mqt = 0: Pro.Mpr = 0
			   Pro.Yqt = 0: Pro.Ypr = 0
			   PUT #2, x, Pro
			   StampName x, "P", Pro.Nam
			END IF
			GOTO 702
		 END IF
		 IF c$ = "U" THEN
			COLOR 13, 0: LOCATE r, 2, 0: PRINT Pro.Num + "³" + Pro.Nam;
710         EntryS 2, 38, 30, e, b, Pro.Nam: IF e THEN 700 ELSE IF b THEN 712
711         EntryN 4, 62, 5, e, b, f, Pro.Set: IF e THEN 700 ELSE IF b THEN 710
			IF Pro.Num = "060" THEN Pro.Set = "-1000" ELSE IF VAL(Pro.Set) < 1 THEN BEEP: GOTO 711
			Pro.Sng = Pro.Set: Pro.Qnt = " 1"
712         EntryS 5, 66, 1, e, b, Pro.Tax: IF e THEN 700 ELSE IF b THEN 711
			Pro.Tax = UCASE$(Pro.Tax): IF INSTR("YN", Pro.Tax) = 0 THEN BEEP: GOTO 712
			IF Pro.Nam > "!" THEN Updated f$: IF f$ = "1" THEN PUT #2, x, Pro: StampName x, "P", Pro.Nam: GOTO 702
			GOTO 710
		 END IF
	CASE ELSE
	END SELECT
	GOTO 700
Materials: '=================================================================
	DEF SEG = Segment: BLOAD "Screen.Mat", 0: DEF SEG
	COLOR 7, 0: FOR x = 61 TO 83: GET #2, x, Pro
	LOCATE x - 59, 2: PRINT Pro.Num; "³"; Pro.Nam; : NEXT
	r = 12
800 Picture r, 2, 3, f$: x = VAL(f$)
802 GET #2, x, Pro: COLOR 0, 7: LOCATE r, 2, 0: PRINT Pro.Num + "³" + Pro.Nam;
	KeyHit c$
	IF LEN(c$) = 2 THEN c$ = RIGHT$(c$, 1) ELSE c$ = UCASE$(c$)
804 SELECT CASE c$
	CASE "H", "P"
		 COLOR 7, 0: LOCATE r, 2, 0: PRINT Pro.Num + "³" + Pro.Nam;
		 IF c$ = "P" THEN r = r + 1 ELSE r = r - 1
		 IF r < 2 THEN r = 2: IF Pro.Num = "061" THEN SOUND 800, 2 ELSE CALL bkscroll(2, 2, 35, 24, 1): x = x - 1: GOTO 802
		 IF r > 24 THEN r = 24: IF Pro.Num = "120" THEN SOUND 800, 2 ELSE CALL scroll(2, 2, 35, 24, 1): x = x + 1: GOTO 802
	CASE CHR$(27)
		 GOTO 10
	CASE "D", "U", "E"
		 COLOR 7, 0
		 LOCATE 2, 38: PRINT Pro.Nam: LOCATE 3, 65: PRINT Pro.Qnt
		 LOCATE 4, 62: PRINT Pro.Set: LOCATE 5, 62: PRINT Pro.Sng
		 COLOR 0, 7
		 LOCATE 13, 58: PRINT USING "########,"; Pro.Mqt: LOCATE 13, 70: PRINT USING "########,"; Pro.Yqt
		 LOCATE 18, 56: PRINT USING q$; Pro.Mpr / 100: LOCATE 18, 68: PRINT USING q$; Pro.Ypr / 100
		 IF c$ = "E" THEN
			IF Pro.Ypr > 0 THEN NoErase f$ ELSE EraseRec f$
			IF f$ = "1" THEN
			   Pro.Nam = SPACE$(30)
			   Pro.Qnt = SPACE$(2)
			   Pro.Set = SPACE$(5)
			   Pro.Sng = SPACE$(5)
			   Pro.Tax = SPACE$(1)
			   Pro.Mqt = 0: Pro.Mpr = 0
			   Pro.Yqt = 0: Pro.Ypr = 0
			   PUT #2, x, Pro
			   StampName x - 60, "M", Pro.Nam
			END IF
			GOTO 802
		 END IF
		 IF c$ = "U" THEN
			COLOR 13, 0: LOCATE r, 2, 0: PRINT Pro.Num + "³" + Pro.Nam;
810         EntryS 2, 38, 30, e, b, Pro.Nam: IF e THEN 800 ELSE IF b THEN 813
811         EntryN 3, 65, 2, e, b, f, Pro.Qnt: IF e THEN 800 ELSE IF b THEN 810
			IF VAL(Pro.Qnt) < 1 THEN BEEP: GOTO 811 ELSE Pro.Tax = "N"
812         EntryN 4, 62, 5, e, b, f, Pro.Set: IF e THEN 800 ELSE IF b THEN 811
			IF Pro.Num = "120" THEN Pro.Set = "-1000" ELSE IF VAL(Pro.Set) < 1 THEN BEEP: GOTO 812
813         EntryN 5, 62, 5, e, b, f, Pro.Sng: IF e THEN 800 ELSE IF b THEN 812
			IF VAL(Pro.Sng) < 1 THEN BEEP: GOTO 813
			IF Pro.Nam > "!" THEN Updated f$: IF f$ = "1" THEN PUT #2, x, Pro: StampName x - 60, "M", Pro.Nam: GOTO 802
			GOTO 810
		 END IF
	CASE ELSE
	END SELECT
	GOTO 800
Utility: '===================================================================
	IF Sort THEN GOSUB SortFiles
	DEF SEG = Segment: BLOAD "Screen.UTI", 0: DEF SEG
	TIMER ON
900 LOCATE 10, 23, 0: PRINT DATE$; : LOCATE 10, 48: PRINT " "; TIME$; " ";
	k$ = INKEY$: IF k$ = "" THEN 900 ELSE TIMER OFF
	IF k$ = "0" THEN 10 ELSE Choice = VAL(k$): COLOR 0, 7
	ON Choice GOTO Months, Years, 900, 900, DumpInv, 900, 900, Backup, Rstore
	GOTO Utility
Backup: '====================================================================
	CLOSE : CLS : SHELL "Backup *.* A:": GOSUB OpenFiles: GOTO Utility
Rstore: '====================================================================
	CLOSE : CLS : SHELL "Restore A: *.*": GOSUB OpenFiles: GOTO Utility
Months: '=====================================================================
	GOSUB CheckFile: Post$ = ""
	IF LastInv < LOF(3) / LEN(Inv) OR LastChk < LOF(7) / 25 THEN
	   LOCATE 23, 21: PRINT "Invoices or Receipts need to be printed."
	   GOTO 900
	END IF
	LOCATE 23, 21: PRINT "Type YES if you want to close the MONTH."
910 EntryS 23, 26, 3, e, b, Post$: IF e THEN 900 ELSE IF b THEN 910
	IF UCASE$(Post$) = "YES" THEN
	   CLS : COLOR 20, 7: LOCATE 20, 29
	   PRINT "CLOSING END-OF-PERIOD"; : COLOR 0, 7
	   GOSUB CusMas
	   GOSUB ProMas
	   GOSUB MatMas
'      GOSUB Aging
'      GOSUB CashFlow
	   GOSUB Register
	   GOSUB PrintSta
	   EndInv = LOF(3) / LEN(Inv): EndChk = LOF(7) / 25
	   LOCATE 23, 21: PRINT "         Updating Master Files          "
	   FOR x = 1 TO 120
	   GET #1, x, Cus: GET #2, x, Pro
	   Cus.Mbf = Cus.Mbf + Cus.Mgr + Cus.Mtx - Cus.Mpd
	   Cus.Mgr = 0: Cus.Mtx = 0: Cus.Mpd = 0
	   Pro.Mqt = 0: Pro.Mpr = 0
	   PUT #1, x, Cus: PUT #2, x, Pro
	   NEXT
	END IF
	GOTO Utility
Years: '======================================================================
	GOSUB CheckFile: Post$ = ""
	IF LastInv < LOF(3) / LEN(Inv) OR LastChk < LOF(7) / 25 THEN
	   LOCATE 23, 21: PRINT "Invoices or Receipts need to be printed."
	   GOTO 900
	END IF
	LOCATE 23, 21: PRINT "Type YES if you want to close the YEAR. "
920 EntryS 23, 26, 3, e, b, Post$: IF e THEN 900 ELSE IF b THEN 920
	IF UCASE$(Post$) = "YES" THEN
	   GOSUB CusMas: GOSUB ProMas: GOSUB MatMas
	   FOR x = 1 TO 120
	   GET #1, x, Cus: GET #2, x, Pro
	   Cus.Ybf = Cus.Ybf + Cus.Ygr + Cus.Ytx - Cus.Ypd
	   Cus.Ygr = 0: Cus.Ytx = 0: Cus.Ypd = 0
	   Pro.Yqt = 0: Pro.Ypr = 0
	   PUT #1, x, Cus: PUT #2, x, Pro
	   NEXT
	END IF
	GOTO Utility
DumpInv: '===================================================================
	Post$ = ""
	LOCATE 23, 21: PRINT "Type YES to dump all paid invoices."
950 EntryS 23, 26, 3, e, b, Post$: IF e THEN 900 ELSE IF b THEN 920
	IF Post$ = "YES" THEN
	   File$ = "INX": GOSUB IndexFile
	   CLOSE #7: OPEN "Invoices.DTA" FOR RANDOM AS #7 LEN = LEN(Inv)
	   FIELD #7, LEN(Inv) AS A7$
	   FOR x = 1 TO LOF(3) / LEN(Inv)
	   GET #3, x, Inv
	   IF Inv.Net > Inv.Pmt THEN
		  GET #3, x, A7$
		  RSET A6$ = RIGHT$(Inv.Ddt, 2) + LEFT$(Inv.Ddt, 2) + MID$(Inv.Ddt, 4, 2)
		  RSET B6$ = Inv.Cus
		  RSET C6$ = " "
		  RSET D6$ = RIGHT$(STR$(100001 + LOC(6)), 5)
		  PUT #6: PUT #7
	   END IF
	   NEXT
	   LastInv = LOF(7) / LEN(Inv)
	   EndInv = LastInv
	   LastChk = 0
	   EndChk = 0
	   CLOSE : SHELL "COPY INVOICES.DTA B:"
	   KILL "INVOICES.DTA"
	   GOSUB SortFiles
	END IF
	GOTO Utility
SearchInv: '=================================================================
	File$ = "CUS": GOSUB IndexFile
	i! = LOF(6) / 14: n = 0
DO: i! = i! / 2: n = n + i!: IF n < 1 THEN n = 1: EXIT DO ELSE GET #6, n
	IF B6$ >= Cus.Num THEN i! = -ABS(i!) ELSE i! = ABS(i!)
LOOP UNTIL ABS(i!) < 1
	WHILE B6$ < Cus.Num AND n < LOF(6) / 14: GET #6, n + 1: n = n + 1: WEND
	RETURN
IndexFile: '=================================================================
	CLOSE #6: OPEN "Invoices." + File$ FOR RANDOM AS #6 LEN = 14
	FIELD #6, 6 AS A6$, 2 AS B6$, 1 AS C6$, 5 AS D6$
	RETURN
CheckFile: '=================================================================
	CLOSE #7: OPEN "Invoices.CHK" FOR RANDOM AS #7 LEN = 25
	FIELD #7, 2 AS A7$, 8 AS B7$, 8 AS C7$, 7 AS D7$
	RETURN
SortFiles: '=================================================================
	COLOR 13, 0: CLS : CLOSE : Sort = 0
	LOCATE 10, 1
	SHELL "SORTF INVOICES.INX INVOICES.DUE /L14 /+1,8 /Q"
	SHELL "SORTF INVOICES.DUE INVOICES.CUS /L14 /+7,8 /Q"
OpenFiles: '=================================================================
	OPEN "Customer.DTA" FOR RANDOM AS #1 LEN = LEN(Cus)
	OPEN "Procedur.DTA" FOR RANDOM AS #2 LEN = LEN(Pro)
	OPEN "Invoices.DTA" FOR RANDOM AS #3 LEN = LEN(Inv)
	OPEN "Invoices.INX" FOR RANDOM AS #4 LEN = LEN(Inx)
	RETURN
Terminate: '=================================================================
	CLOSE : OPEN "INVOICES.NUM" FOR OUTPUT AS #1
	WRITE #1, LastInv, EndInv, LastChk, EndChk: CLOSE
	IF Sort THEN GOSUB SortFiles: CLOSE
	CLS
	END
StpClk:
	SOUND 800, 3: TIMER OFF: RETURN 10
'=============================================================================
DATA 31%,28%,31%,30%,31%,30%,31%,31%,30%,31%,30%,31%

DEFINT L
SUB AgingDays (d$, n)
	n = 0
	Days = VAL(RIGHT$(d$, 2))
	Months = VAL(MID$(d$, 3, 2))
	FOR x = 1 TO Months - 1
	n = n + DayN(x)
	NEXT
	n = n + Days
END SUB

STATIC SUB DateDue (InvDate$, DueDate$)
	 Months = VAL(LEFT$(InvDate$, 2))
	 Years = VAL(RIGHT$(InvDate$, 2))
	 Months = Months + 1
	 IF Months > 12 THEN Years = Years + 1: Months = 1
	 d$ = "01"
	 m$ = RIGHT$(STR$(100 + Months), 2)
	 y$ = RIGHT$(STR$(100 + Years), 2)
	 DueDate$ = m$ + "-" + d$ + "-" + y$
END SUB

DEFLNG M, Y
DEFSNG A-L, N-Q, S-X, Z
SUB DispLine (r, m, y)
	LOCATE r, 56: PRINT USING "#######,.##"; m / 100
	LOCATE r, 68: PRINT USING "#######,.##"; y / 100
END SUB

DEFINT A-Q, S-Z
SUB EntryD (r, c, e, b, b$, d$)
	 x = 0: e = 0: b = 0: f = 1
	 COLOR 0, 7: LOCATE r, c: PRINT b$;
d10: LOCATE r, c + x, 1, 1, 12: IF x = 2 OR x = 5 THEN PRINT "-"; : x = x + 1
	 TIMER ON
	 DO: ky$ = INKEY$: LOOP WHILE ky$ = ""
	 TIMER OFF
	 IF ky$ = CHR$(27) THEN e = 1: EXIT SUB
	 IF INSTR("0123456789" + CHR$(13) + CHR$(8), ky$) = 0 THEN GOTO d10
	 IF f THEN
		IF ky$ = CHR$(8) THEN b = 1: GOTO d30
		IF ky$ = CHR$(13) GOTO d30
		f = 0: PRINT SPACE$(8): LOCATE r, c
	 END IF
	 IF ky$ = CHR$(13) GOTO d20
	 IF ky$ = CHR$(8) THEN
		IF x = 3 OR x = 6 THEN x = x - 2 ELSE x = x - 1
		IF x < 0 THEN b = 1: b$ = "        ": GOTO d30
		PRINT " "; : GOTO d10
	 END IF
	 PRINT ky$                                          '<Reject>
	 x = x + 1: IF x > 7 THEN SOUND 640, 1: x = 7
	 GOTO d10
d20: b$ = "": FOR x = 0 TO 7: b$ = b$ + CHR$(SCREEN(r, c + x)): NEXT
	 Months = VAL(LEFT$(b$, 2))
	 DayW = VAL(MID$(b$, 4, 2))
	 Years = VAL(RIGHT$(b$, 2))
	 IF (Years MOD 4) THEN DayN(2) = 28 ELSE DayN(2) = 29
	 IF Months > 12 OR Months < 1 THEN x = 0: BEEP: GOTO d10
	 IF DayW > DayN(Months) OR DayW < 1 THEN x = 3: BEEP: GOTO d10
	 IF Years > VAL(RIGHT$(DATE$, 2)) + 1 OR Years < VAL(RIGHT$(DATE$, 2)) - 1 THEN x = 6: BEEP: GOTO d10
	 DateDue b$, d$
d30: COLOR 10, 0: LOCATE r, c, 0: PRINT b$;
END SUB

STATIC SUB EntryN (r, c, s, e, b, f, f$)
	 x = 0: e = 0: b = 0: f = 1: p$ = STRING$(s, "#")
	 COLOR 0, 7: LOCATE r, c, 0: PRINT f$;
n10: LOCATE r, c + x, 1, 1, 12
	 TIMER ON
	 DO: ky$ = INKEY$: LOOP WHILE ky$ = ""
	 TIMER OFF
	 IF ky$ = CHR$(27) THEN e = 1: EXIT SUB
	 IF INSTR("-+0123456789." + CHR$(13) + CHR$(8), ky$) = 0 THEN GOTO n10
	 IF ky$ = "." THEN f = 0: f$ = STRING$(s, " "): GOTO n30
	 IF ky$ = "-" THEN Lookup "P": GOTO n10
	 IF ky$ = "+" THEN Lookup "M": GOTO n10
	 IF f THEN
		IF ky$ = CHR$(13) GOTO n30
		IF ky$ = CHR$(8) THEN b = 1: GOTO n30
		PRINT STRING$(s, " "); : LOCATE r, c: f = 0
	 END IF
	 IF ky$ = CHR$(13) THEN GOTO n20
	 IF ky$ = CHR$(8) THEN PRINT " "; : x = x - 1: IF x < 0 THEN b = 1: GOTO n20 ELSE GOTO n10
	 PRINT ky$; : x = x + 1: IF x > s - 1 THEN SOUND 640, 1: x = s - 1: GOTO n10
	 GOTO n10
n20: f$ = "": FOR x = 0 TO s - 1: f$ = f$ + CHR$(SCREEN(r, c + x)): NEXT
	 LOCATE r, c: PRINT USING p$; VAL(f$);
	 f$ = "": FOR x = 0 TO s - 1: f$ = f$ + CHR$(SCREEN(r, c + x)): NEXT
n30: COLOR 10, 0: LOCATE r, c, 0: PRINT f$;
END SUB

STATIC SUB EntryS (r, c, s, e, b, f$)
	 x = 0: e = 0: b = 0: f = 1
	 COLOR 0, 7: LOCATE r, c, 0: PRINT f$;
s10: LOCATE r, c + x, 1, 1, 12
	 TIMER ON
	 DO: ky$ = INKEY$: LOOP WHILE ky$ = ""
	 TIMER OFF
	 IF ky$ = CHR$(27) THEN e = 1: EXIT SUB
	 IF f THEN
		IF ky$ = "." THEN f$ = STRING$(s, " "): GOTO s30
		IF ky$ = CHR$(8) THEN b = 1: GOTO s30
		IF ky$ = CHR$(13) THEN GOTO s30
		f = 0
	 END IF
	 IF ky$ = CHR$(8) THEN PRINT " "; : x = x - 1: IF x < 0 THEN b = 1: GOTO s20
	 IF ky$ = CHR$(13) THEN GOTO s20
	 IF ky$ < CHR$(32) THEN GOTO s10
	 PRINT ky$; : x = x + 1: IF x > s - 1 THEN SOUND 640, 1: x = x - 1
	 GOTO s10
s20: Picture r, c, s, f$
s30: COLOR 10, 0: LOCATE r, c, 0: PRINT f$;
END SUB

DEFSNG A-Z
SUB EraseRec (f$)
	SOUND 1000, 2
	COLOR 7, 0: LOCATE 3, 71: PRINT "END will"
				LOCATE 4, 71: PRINT " ERASE  "
	KeyHit f$
	COLOR 0, 7: LOCATE 3, 71: PRINT "        "
				LOCATE 4, 71: PRINT "        "
END SUB

SUB HAging ()
	 Heading
	 LPRINT TAB(39); "AGING OF RECEIVABLES"
	 LPRINT : LPRINT
	 LPRINT "                            Due     Inv  =======PAST DUE========   Due   ======================="
	 LPRINT " Customer Name              Date    Num  90-Days 60-Days 30-Days  Today  30-Days 60-Days 90-Days"
	 LPRINT "========================= ======== ===== ======= ======= ======= ======= ======= ======= ======="
END SUB

SUB HCusMas ()
	Heading
	LPRINT TAB(42); "CLIENT MASTER"
	LPRINT CHR$(27); "P"; CHR$(15);
	LPRINT : LPRINT
	LPRINT "                                    ==================M O N T H=================   =====================Y E A R====================="
	LPRINT "No   CLIENT NAME                    B. FORWD  SUB-TOT   TAX     PAYMENT  BALANCE   B. FORWD  SUB TOTAL    TAX     PAYMENT   BALANCE "
	LPRINT "== ==============================   ======== ======== ======== ======== ========   ========= ========= ========= ========= ========="
END SUB

SUB HDeposit ()
	 Heading
	 LPRINT TAB(41); "CHECK RECEIPTS"
	 LPRINT : LPRINT
	 LPRINT "                                     CTRL                 CHECK       CHECK   "
	 LPRINT "ID    CLIENT NAME                   NUMBER  CHECK DATE    NUMBER      AMOUNT  "
	 LPRINT "==  ==============================  ======  ==========  ==========  =========="
END SUB

SUB Heading ()
	LPRINT CHR$(12);
	LPRINT CHR$(27); "@";
	WIDTH LPRINT 255
	LPRINT CHR$(27); "M";
	PageNum = PageNum + 1: LineNum = 12
	LPRINT TIME$; TAB(38); "ROGERS' DENTAL LABORATORY"
	LPRINT DATE$; TAB(36); " S.White St., Athens, TN "; TAB(88); "Page: "; PageNum
	LPRINT
END SUB

SUB HFlow ()
	 Heading
	 LPRINT TAB(44); "CASH FLOW"
	 LPRINT : LPRINT
	 LPRINT "                             Inv    Inv.     Due     Invoice  Amount   Amount   Total           "
	 LPRINT " Customer Name               Num    Date     Date    Amount    Paid     Due    To Date  Due Date"
	 LPRINT "=========================== ===== ======== ======== ======== ======== ======== ======== ========"
END SUB

SUB HInvoice ()
	Heading
	LPRINT TAB(40); "INVOICE  REGISTER": LPRINT : LPRINT
	LPRINT " INV                             INVOICE    DUE      SUB     SALES             AMOUNT   AMOUNT "
	LPRINT " NUM    CLIENT NAME                DATE     DATE    TOTAL     TAX     TOTAL     PAID      DUE  "
	LPRINT "===== ========================== ======== ======== ======== ======== ======== ======== ========"
END SUB

SUB HMatMas ()
	 Heading
	 LPRINT TAB(41); "MATERIALS MASTER"
	 LPRINT : LPRINT
	 LPRINT "                                   SET    SET  SINGLE  =====M O N T H=====  ======Y E A R======"
	 LPRINT "Num   MATERIAL NAME                SIZE  PRICE  PRICE    QUANT     PRICE      QUANT     PRICE   "
	 LPRINT "=== ============================== ==== ====== ======  ======= ===========  ======= ==========="
END SUB

SUB HProMas ()
	 Heading
	 LPRINT TAB(41); "PROCEDURE MASTER"
	 LPRINT : LPRINT
	 LPRINT "                                                     =====M O N T H=====    ======Y E A R======"
	 LPRINT "Num    PROCEDURE NAME                TAX   PRICE      QUANT     PRICE        QUANT     PRICE   "
	 LPRINT "===  ==============================  ===  =======    ======= ===========    ======= ==========="
END SUB

SUB KeyHit (f$)
	TIMER ON
	DO: f$ = INKEY$: LOOP WHILE f$ = ""
	TIMER OFF
END SUB

SUB Logo ()
	LPRINT CHR$(27); "@";
	WIDTH LPRINT 255
	LPRINT CHR$(14); CHR$(27); "G"; CHR$(27); "E"; CHR$(27); "r"; "2";
	LPRINT TAB(17); "ROGERS' DENTAL LAB"; CHR$(27); "H"
	LPRINT TAB(29); "      P.O.Box 1073      "
	LPRINT TAB(29); "  4 South White Street  "
	LPRINT TAB(27); "Athens, Tennessee 37371-1073"; TAB(66); "(615) 745-7115"
	LPRINT : LPRINT CHR$(27); "U"; "1"; CHR$(27); "r"; "0"
END SUB

SUB Lookup (v$)
	LOCATE , , 0
	DEF SEG = Segment
	BSAVE "Screen.TMP", 0, &H1000
	BLOAD "Screen.VW" + v$, 0
	KeyHit f$
	BLOAD "Screen.TMP", 0: DEF SEG
END SUB

SUB NoErase (f$)
	SOUND 1000, 2
	COLOR 7, 0: LOCATE 3, 71: PRINT " ACTIVE "
				LOCATE 4, 71: PRINT "NO ERASE"
	KeyHit f$
	COLOR 0, 7: LOCATE 3, 71: PRINT "        "
				LOCATE 4, 71: PRINT "        "
END SUB

DEFINT A-Z
SUB Picture (r, c, s, f$)
 f$ = ""
 FOR x = 0 TO s - 1
 f$ = f$ + CHR$(SCREEN(r, c + x))
 NEXT
END SUB

STATIC SUB StampName (x, f$, n$)
	DEF SEG = Segment
	BSAVE "SCREEN.TMP", 0, &H1000
	BLOAD "Screen.VW" + f$, 0
	SELECT CASE x
	CASE 1 TO 20
		 z = 0
	CASE 21 TO 40
		 z = 1
	CASE 41 TO 60
		 z = 2
	END SELECT
	LOCATE 3 + x - 20 * z, 5 + 26 * z: PRINT LEFT$(n$, 22);
	BSAVE "SCREEN.VW" + f$, 0, &H1000
	BLOAD "Screen.TMP", 0: DEF SEG
END SUB

DEFSNG A-Z
SUB Updated (f$)
	SOUND 1000, 2
	COLOR 7, 0: LOCATE 3, 71: PRINT "END will"
				LOCATE 4, 71: PRINT "  SAVE  "
	KeyHit f$
	COLOR 0, 7: LOCATE 3, 71: PRINT "        "
				LOCATE 4, 71: PRINT "        "
END SUB


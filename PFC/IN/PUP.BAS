DEFINT A-Z: DEFDBL T: DEFLNG L
DECLARE FUNCTION Confirm ()
DECLARE FUNCTION WinMat ()

DECLARE SUB Material ()
DECLARE SUB MatShow ()
DECLARE SUB MatInit ()
DECLARE SUB PurVend ()
DECLARE SUB PurInvc ()
DECLARE SUB PurMatr (L)
DECLARE SUB PurDisp (L)

DECLARE SUB HiLite (m, p)
DECLARE SUB OpenFiles ()
DECLARE SUB TimeDate ()
DECLARE SUB KeyHit (f$)
DECLARE SUB HitKey (f$)

DECLARE SUB WinMssg (f$)
DECLARE SUB HPurAna ()
DECLARE SUB HNewPur ()
DECLARE SUB Heading ()
DECLARE SUB HInventory ()

'$INCLUDE: '..\BI\Library.DEC'
'$INCLUDE: '..\BI\Material.REC'
'$INCLUDE: '..\BI\MatIndex.REC'
'$INCLUDE: '..\BI\Vendors.REC'
'$INCLUDE: '..\BI\PInvoice.REC'
'$INCLUDE: '..\BI\Purchase.REC'
  
   'record variables
	DIM SHARED Mat AS Material
	DIM SHARED Mnx AS MatIndex
	DIM SHARED Pur AS Purchase
	DIM SHARED Ven AS Vendors
	DIM SHARED Inv AS Invoices
   'common variables
	DIM SHARED Sort    AS INTEGER
	DIM SHARED Top     AS INTEGER
	DIM SHARED Bot     AS INTEGER
   'array variables
	DIM SHARED oNm(26) AS STRING * 3
	DIM SHARED oPr(26) AS STRING * 8
	DIM SHARED oQu(26) AS LONG
	DIM SHARED Qua(26) AS LONG
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
	CALL MhOpenScreenLib("..\IN\INS.SLB" + CHR$(0), e)
	CALL MhShadowOn(0, 8)
	CALL MhBlinkOff
   'load purchase & job numbers
	OPEN "INVENTOR.NUM" FOR INPUT AS #1
	INPUT #1, LastPur, EOMpur, EOYpur, LastJob, EOMjob, EOYjob
	CLOSE
   'initialize materials module
	CALL MatInit
Menu: '=======================================================================
10  CALL MhDisplayByName(0, 0, "PurMenu", e)
	HitKey f$: f = VAL(f$)
	IF f$ = "0" THEN f = 10
	IF f$ = CHR$(27) GOTO Terminate
	ON f GOTO EntPur, NewPur, MonPur, MatMon, MatYear, PurPrc, Materials, Verify, eMonth, eYear
	GOTO 10
EntPur: '=====================================================================
	OpenFiles
	CALL MhDisplayByName(0, 0, "Purchase", e)

110 EntryN 4, 18, 5, 0, e, b, Pur.Inv: IF e THEN 10 ELSE IF b THEN 110
	IF VAL(Pur.Inv) > LOF(5) \ LEN(Inv) THEN WinMssg "Invoice not found": GOTO 110
	IF VAL(Pur.Inv) < 1 THEN 110

	ERASE oNm, Qua, oQu, oPr
	ERASE Pur.Itm
	ERASE Pur.Qty
	ERASE Pur.Fra
	ERASE Pur.Prc
	L0 = 0
	L1 = 0

	GET #5, VAL(Pur.Inv), Inv
	GET #4, VAL(Inv.Ven), Ven
	PurVend
	PurInvc
	PurMatr L0
	IF Inv.Pur > "    0" THEN
	   PurDisp L1
	ELSE
	   IF L0 = 0 THEN WinMssg "No inventory on this invoice": GOTO 110
	   Inv.Pur = RIGHT$(STR$(100001 + LOF(3) \ LEN(Pur)), 5)
	END IF
	LOCATE 4, 72:  PRINT USING "#####.##"; L0 / 100
	LOCATE 48, 72: PRINT USING "#####.##"; L1 / 100
112 x = 1
114 EntryN x + 21, 2, 3, 0, e, b, Pur.Itm(x): IF e THEN 10
	IF b THEN x = x - 1: IF x < 1 THEN 110 ELSE 114
	IF Pur.Itm(x) = "   " THEN
	   L1 = L1 - VAL(Pur.Prc(x)) * 100
	   FOR z = x TO 24
		  Qua(z) = Qua(z + 1)
		  Pur.Itm(z) = Pur.Itm(z + 1)
		  Pur.Qty(z) = Pur.Qty(z + 1)
		  Pur.Fra(z) = Pur.Fra(z + 1)
		  Pur.Prc(z) = Pur.Prc(z + 1)
	   NEXT
	   Qua(z) = 0
	   Pur.Itm(z) = ""
	   Pur.Qty(z) = ""
	   Pur.Fra(z) = ""
	   Pur.Prc(z) = ""
	   IF x < 25 THEN CALL scroll(2, x + 21, 79, 46, 1)
	   COLOR 6, 0: LOCATE 46, 2: PRINT "   ≥                                ≥       ≥     ≥    ≥      ≥      ≥        "
	   COLOR 10:   LOCATE 48, 72: PRINT USING "#####.##"; L1 / 100;
	   IF Pur.Itm(x) > "  0" THEN 114 ELSE 119
	ELSE
	   IF Pur.Itm(x) < "  1" THEN IF WinMat THEN 114 ELSE Pur.Itm(x) = Mat.Num
	END IF
	GET #1, VAL(Pur.Itm(x)), Mat
	IF Mat.Nam < "!" THEN IF WinMat THEN 114
	Pur.Itm(x) = Mat.Num
	COLOR 14, 0
	LOCATE x + 21, 2:  PRINT Mat.Num;
	LOCATE x + 21, 6:  PRINT Mat.Nam;
	LOCATE x + 21, 39: PRINT Mat.Fra; LEFT$(Mat.Mes, 3);

116 EntryN x + 21, 47, 5, 0, e, b, Pur.Qty(x): IF e THEN 10 ELSE IF b THEN 114
117 EntryN x + 21, 53, 4, 0, e, b, Pur.Fra(x): IF e THEN 10 ELSE IF b THEN 116
	IF Pur.Fra(x) >= Mat.Fra THEN WinMssg "Maximum fraction " + Mat.Fra: GOTO 117
	Qua(x) = VAL(Pur.Fra(x)) + VAL(Pur.Qty(x)) * VAL(Mat.Fra)
	'IF Qua(x) < 1 THEN WinMssg "Quantity please": GOTO 117
	LOCATE x + 21, 58: PRINT Mat.Ref

	L1 = L1 - VAL(Pur.Prc(x)) * 100
118 EntryN x + 21, 72, 8, 2, e, b, Pur.Prc(x): IF e THEN 10
	L1 = L1 + VAL(Pur.Prc(x)) * 100:           IF b THEN 117
	'IF Pur.Prc(x) < "    0.01" THEN WinMssg "Price please!": GOTO 118
	LOCATE x + 21, 65: PRINT USING "##.###"; VAL(Pur.Prc(x)) / Qua(x)
	IF ABS(VAL(Pur.Prc(x)) / Qua(x) - VAL(Mat.Ref)) > .1 * VAL(Mat.Ref) THEN WinMssg "Validate your price??"
	LOCATE 48, 72: PRINT USING "#####.##"; L1 / 100;
	x = x + 1: IF x < 26 THEN 114
			 
119 IF L0 <> L1 THEN 112
	IF Confirm THEN
	   PUT #5, VAL(Pur.Inv), Inv
	   PUT #3, VAL(Inv.Pur), Pur
	   FOR x = 1 TO 25
	   IF VAL(oNm(x)) > 0 THEN      '---------------------Void old materials
		  GET #1, VAL(oNm(x)), Mat
		  Mat.Mpq = Mat.Mpq - oQu(x) * 100: Mat.Mpp = Mat.Mpp - VAL(oPr(x)) * 100
		  Mat.Ypq = Mat.Ypq - oQu(x) * 100: Mat.Ypp = Mat.Ypp - VAL(oPr(x)) * 100
		  PUT #1, VAL(oNm(x)), Mat
	   ELSE
		  EXIT FOR
	   END IF
	   NEXT
	   FOR x = 1 TO 25
	   IF VAL(Pur.Itm(x)) > 0 THEN  '---------------------Post new materials
		  GET #1, VAL(Pur.Itm(x)), Mat
		  Mat.Mpq = Mat.Mpq + Qua(x) * 100: Mat.Mpp = Mat.Mpp + VAL(Pur.Prc(x)) * 100
		  Mat.Ypq = Mat.Ypq + Qua(x) * 100: Mat.Ypp = Mat.Ypp + VAL(Pur.Prc(x)) * 100
		  PUT #1, VAL(Pur.Itm(x)), Mat
	   ELSE
		  EXIT FOR
	   END IF
	   NEXT
	   GOTO EntPur
	END IF
	GOTO 112
NewPur: '====================================================================
	OpenFiles
	COLOR 15, 4: LOCATE 24, 30: PRINT "Print New Purchase Entries."
	IF LastPur >= LOF(3) \ LEN(Pur) THEN WinMssg "No new PURCHASES to print": GOTO 10
	FromPur = LastPur + 1
	GOSUB PrtPur
	GOTO 10
MonPur: '====================================================================
	OpenFiles
	COLOR 15, 4: LOCATE 26, 30: PRINT "Print Month's Purcahses."
	IF EndPur >= LOF(3) \ LEN(Pur) THEN WinMssg "No PURCHASES this month": GOTO 10
	FromPur = EOMpur + 1
	GOSUB PrtPur
	GOTO 10
PrtPur: '====================================================================
	L1 = 0: L2 = 0
	LOCATE 43, 20: PRINT "      Printing inventory purchases       "
	PageNum = 0: HNewPur
	FOR x = FromPur TO LOF(3) \ LEN(Pur)
		GET #3, x, Pur
		GET #5, VAL(Pur.Inv), Inv
		GET #4, VAL(Inv.Ven), Ven
		PurMatr L0
		LPRINT USING "& "; Ven.Nam; Pur.Inv; Inv.Bdt; Inv.Ddt; Inv.Pdt; Inv.Chk; " " + Inv.Net;
		LPRINT USING " ######.##  &"; L0 / 100; Inv.Pur;
		FOR y = 1 TO 25
		IF VAL(Pur.Itm(y)) > 0 THEN
		   GET #1, VAL(Pur.Itm(y)), Mat
		   LPRINT TAB(107);
		   LPRINT USING " &"; Mat.Num; Mat.Nam;
		   LPRINT USING "#####:#### ######.##"; VAL(Pur.Qty(y)); VAL(Pur.Fra(y)); VAL(Pur.Prc(y))
		   LineNum = LineNum + 1: IF LineNum > 59 THEN HNewPur
		   L1 = L1 + 100 * VAL(Pur.Prc(y))
		END IF
		NEXT
		LPRINT : LPRINT TAB(155);
		LPRINT USING "######.##"; L1 / 100: LPRINT
		L2 = L2 + L1
		L1 = 0
		LineNum = LineNum + 3: IF LineNum > 59 THEN HNewPur
	NEXT
	LPRINT TAB(154); : LPRINT USING "#######.##"; L2 / 100
	LastPur = LOF(3) \ LEN(Pur)
	RETURN
MatMon: '====================================================================
	OpenFiles
	COLOR 15, 4: LOCATE 28, 30: PRINT "Print Materials Master MONTH"
	COLOR 14, 4: LOCATE 43, 20: PRINT "  Press [Escape] to terminate printing   "
	PageNum = 0: HInventory
	L0 = 0
	L1 = 0
	L2 = 0
	L3 = 0
	L4 = 0
	L5 = 0
	L6 = 0
	L7 = 0
	FOR y = 1 TO LOF(1) \ LEN(Mat)
	IF INKEY$ = CHR$(27) THEN EXIT FOR
	GET #2, y, Mnx
	GET #1, Mnx.Num, Mat
	IF Mat.Nam > "!" THEN
	   LineNum = LineNum + 1: IF LineNum > 60 THEN HInventory
	   LPRINT USING "& & &"; Mat.Typ + Mat.Num; Mat.Nam; Mat.Mes
	   LPRINT USING "#######.##"; Mat.Mbq / 100; Mat.Mbp / 100; Mat.Mpq / 100; Mat.Mpp / 100;
	   LPRINT USING "#######.##"; Mat.Mkq / 100; Mat.Mkp / 100; Mat.Mwq / 100; Mat.Mwp / 100;
	   LPRINT USING "#######.##"; (Mat.Mbq + Mat.Mpq - Mat.Mkq - Mat.Mwq) / 100; 0;
	   LPRINT USING "#######.##"; (Mat.Mbp + Mat.Mpp - Mat.Mkq - Mat.Mwp) / 100; 0
	   L0 = L0 + Mat.Mbq: L1 = L1 + Mat.Mbp
	   L2 = L2 + Mat.Mpq: L3 = L3 + Mat.Mpp
	   L4 = L4 + Mat.Mkq: L5 = L5 + Mat.Mkp
	   L6 = L6 + Mat.Mwq: L7 = L7 + Mat.Mwp
	END IF
	NEXT
	LPRINT : LPRINT TAB(45);
	LPRINT USING "#######.##"; L0 / 100; L1 / 100; L2 / 100; L3 / 100; L4 / 100; L5 / 100; L6 / 100; L7 / 100; 0; 0;
	LPRINT USING "#######.##"; (L0 + L2 - L4 - L6) / 100; (L1 + L3 - L5 - L7) / 100
	GOTO 10
MatYear: '===================================================================
	OpenFiles
	COLOR 15, 4: LOCATE 30, 30: PRINT "Print Materials Master YEAR"
	COLOR 14, 4: LOCATE 43, 20: PRINT "  Press [Escape] to terminate printing   "
	PageNum = 0: HInventory
	L0 = 0
	L1 = 0
	L2 = 0
	L3 = 0
	L4 = 0
	L5 = 0
	L6 = 0
	L7 = 0
	L8 = 0
	L9 = 0
	FOR y = 1 TO LOF(1) \ LEN(Mat)
	IF INKEY$ = CHR$(27) THEN EXIT FOR
	GET #2, y, Mnx
	GET #1, Mnx.Num, Mat
	IF Mat.Nam > "!" THEN
	   LineNum = LineNum + 1: IF LineNum > 60 THEN HInventory
	   LPRINT USING "& & &"; Mat.Typ + Mat.Num; Mat.Nam; Mat.Mes;
	   LPRINT USING "#######.##"; Mat.Ybq / 100; Mat.Ybp / 100; Mat.Ypq / 100; Mat.Ypp / 100; Mat.Ykq / 100; Mat.Ykp / 100;
	   LPRINT USING "#######.##"; Mat.Ywq / 100; Mat.Ywp / 100; Mat.Ygq / 100; Mat.Ygp / 100;
	   LPRINT USING "#######.##"; (Mat.Ybq + Mat.Ypq - Mat.Ykq - Mat.Ywq - Mat.Ygq) / 100;
	   LPRINT USING "#######.##"; (Mat.Ybp + Mat.Ypp - Mat.Ykp - Mat.Ywp - Mat.Ygp) / 100
	   L0 = L0 + Mat.Ybq: L1 = L1 + Mat.Ybp
	   L2 = L2 + Mat.Ypq: L3 = L3 + Mat.Ypp
	   L4 = L4 + Mat.Ykq: L5 = L5 + Mat.Ykp
	   L6 = L6 + Mat.Ywq: L7 = L7 + Mat.Ywp
	   L8 = L8 + Mat.Ygq: L9 = L9 + Mat.Ygp
	END IF
	NEXT
	LPRINT : LPRINT TAB(45);
	LPRINT USING "#######.##"; L0 / 100; L1 / 100; L2 / 100; L3 / 100; L4 / 100; L5 / 100; L6 / 100; L7 / 100; L8 / 100; L9 / 100;
	LPRINT USING "#######.##"; (L0 + L2 - L4 - L6 - L8) / 100; (L1 + L3 - L5 - L7 - L9) / 100
	GOTO 10
Materials: '=================================================================
	CALL Material
	GOTO 10
Verify: '====================================================================
	CLS
	LOCATE 50, 1: PRINT "Checking please wait.....";
	LOCATE 49, 1
	OpenFiles
	FOR x = 1 TO LOF(5) \ LEN(Inv)
	GET #5, x, Inv
	L1 = 0: L2 = 0
	PurMatr L0
	IF L0 THEN
	   IF VAL(Inv.Pur) > 0 THEN
		  GET #3, VAL(Inv.Pur), Pur
		  FOR y = 1 TO 25
		  IF Pur.Itm(y) > "  0" THEN
			 L1 = L1 + 100 * VAL(Pur.Prc(y))
		  ELSE
			 EXIT FOR
		  END IF
		  NEXT
		  IF L0 <> L1 THEN PRINT USING "Invoice No: & total: #####.##  purchase:#####.##"; Pur.Inv; L0 / 100; L1 / 100
	   ELSE
		  PRINT USING "Invoice No: ##### does not have a purchase."; x
	   END IF
	END IF
	NEXT
	WinMssg "Press any key to continue"
	GOTO 10
PurPrc: '====================================================================
	COLOR 15, 4: LOCATE 32, 30: PRINT "Print Purchase Price Analysis"
	COLOR 14, 4: LOCATE 43, 20: PRINT "Sorting Purchases file, one moment please"
	FromPur = EOMpur + 1
	GOSUB PurInx
	LOCATE 43, 20: PRINT " Printing purchase price analysis report "
	GOSUB Analysis
	GOTO 10
Analysis: '==================================================================
	OpenFiles
	L1 = 0: L2 = 0: L3 = 0: L4 = 0
	OPEN "Purchase.INX" FOR RANDOM AS #9 LEN = 32
	FIELD #9, 3 AS a1$, 5 AS b1$, 5 AS c1$, 5 AS d1$, 4 AS e1$, 8 AS f1$, 2 AS g$
	PageNum = 0: HPurAna: GET #9, 1: p$ = a1$
	FOR x = 1 TO LOF(9) \ 32
	GET #9, x
	IF p$ <> a1$ THEN GOSUB TotItem
	GET #1, VAL(a1$), Mat
	GET #5, VAL(b1$), Inv
	GET #4, VAL(Inv.Ven), Ven
	LineNum = LineNum + 1: IF LineNum > 59 THEN HPurAna
	L = VAL(d1$) * VAL(Mat.Fra) + VAL(e1$)
	LPRINT USING "& "; Ven.Nam; b1$; Inv.Bdt; Inv.Ddt; Inv.Pdt; Inv.Chk; a1$; Mat.Nam;
	LPRINT USING "####### & &"; L; Mat.Mes; f1$;
	LPRINT USING "###.### &  +###.##%"; VAL(f1$) / L; Mat.Ref; 100 * ((VAL(f1$) / L) - VAL(Mat.Ref)) / VAL(Mat.Ref)
	L1 = L1 + L
	L2 = L2 + 100 * VAL(f1$)
	NEXT
	IF L1 > 0 THEN
	   GOSUB TotItem
	   LPRINT TAB(113);
	   LPRINT USING " ##########     #######.##"; L3 + L1; (L4 + L2) / 100;
	   IF (L3 + L1) = 0 THEN
		  LPRINT USING "  #.###"; 0
	   ELSE
		  LPRINT USING " ##.###"; (L4 + L2) / (L3 + L1) / 100
	   END IF
	END IF
	RETURN
TotItem:
	LPRINT : LPRINT TAB(113);
	LPRINT USING " ##########     #######.##"; L1; L2 / 100;
	IF L1 = 0 THEN
	   LPRINT USING " ##.###"; 0
	ELSE
	   LPRINT USING " ##.###"; L2 / L1 / 100
	END IF
	LPRINT : LineNum = LineNum + 3
	p$ = a1$
	L3 = L3 + L1: L1 = 0
	L4 = L4 + L2: L2 = 0
	RETURN
PurInx: '=====================================================================
	OpenFiles
	CLOSE #9: OPEN "Purchase" FOR OUTPUT AS #9
	FOR x = FromPur TO LOF(3) \ LEN(Pur)
	GET #3, x, Pur
	GET #5, VAL(Pur.Inv), Inv
	FOR y = 1 TO 25
	IF VAL(Pur.Itm(y)) > 0 THEN
	   PRINT #9, USING "&&&&&&"; Pur.Itm(y); Pur.Inv; Inv.Pur; Pur.Qty(y); Pur.Fra(y); Pur.Prc(y)
	ELSE
	   EXIT FOR
	END IF
	NEXT
	NEXT
	CLOSE
	SHELL "SORTF Purchase Purchase.INX /L32 /+1,8 /Q"
	KILL "Purchase"
	RETURN
eMonth: '======================================================================
	OpenFiles
	f$ = "   "
	LOCATE 43, 20: PRINT " Type YES if you wish to close the month "
800 EntryU 43, 26, 3, e, b, f$: IF b THEN 800 ELSE IF e THEN 10
	IF f$ = "YES" THEN
	   FromPur = EOMpur + 1
	   GOSUB PrtPur
	   GOSUB PurInx
	   GOSUB Analysis
	   EOMpur = LOF(3) \ LEN(Pur)
	END IF
	GOTO 10
eYear: '=======================================================================
	OpenFiles
	f$ = "   "
	LOCATE 23, 20: PRINT " Type YES if you wish to close the year  "
900 EntryU 23, 26, 3, e, b, f$: IF b THEN 900 ELSE IF e THEN 10
	IF f$ = "YES" THEN
	   FromPur = EOYpur + 1
	   GOSUB PrtPur
	   GOSUB PurInx
	   GOSUB Analysis
	   EOYpur = LOF(3) \ LEN(Pur)
	END IF
	GOTO 10
Errors: '====================================================================
	WinMssg "Error:" + STR$(ERR) + "  Line:" + STR$(ERL)
Terminate: '=================================================================
	CLOSE : OPEN "INVENTOR.NUM" FOR OUTPUT AS #1
	WRITE #1, LastPur, EOMpur, EOYpur, LastJob, EOMjob, EOYjob: CLOSE
	CLS : SYSTEM

DATA 1,5, 7,8, 16,7, 24,9, 34,6

DEFSNG A-B, D-Z
FUNCTION Confirm ()
	PCOPY 0, 1
	COLOR 1, 3: LOCATE 13
	LOCATE , 36: PRINT "                      "
	LOCATE , 36: PRINT " Press [*] to CONFIRM "
	LOCATE , 36: PRINT "                      "
	COLOR 11, 0: LOCATE 14
	LOCATE , 35: PRINT "€"
	LOCATE , 35: PRINT "€"
	LOCATE , 35: PRINT "ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ"
	SOUND 1000, .1: SOUND 800, .3: KeyHit f$
	IF f$ = "*" THEN Confirm = 1 ELSE Confirm = 0
	PCOPY 1, 0
END FUNCTION

DEFSNG C
SUB Heading ()
	 LPRINT CHR$(12); CHR$(27); "@";
	 WIDTH "LPT1:", 255: LPRINT CHR$(27); "M"
	 PageNum = PageNum + 1: LineNum = 13
	 LPRINT "Date: "; DATE$; TAB(61); "      PROCESSED FOODS CORPORATION     "; TAB(153); "Page: "; PageNum
	 LPRINT "Time: "; TIME$; TAB(61); "707 Willow Avenue, Knoxville, TN 37915"
	 LPRINT :         LPRINT TAB(61); "         INVENTORY  PURCHASING        ": LPRINT
END SUB

SUB HInventory ()
	Heading
	LPRINT TAB(70); "  MATERIALS LISTING  "
	LPRINT
	LPRINT
	LPRINT "                                             ===BEG. BALANCE==== =====PURCHACES===== ====PLANT USAGE==== ======WASTE======== =======LOSSES====== ==ENDING INVENTORY="
	LPRINT "Code         Description               Measr Quantity      Cost  Quantity      Cost  Quantity      Cost  Quantity      Cost  Quantity      Cost  Quantity      Cost "
	LPRINT "===== ================================ ===== =================== =================== =================== =================== =================== ==================="
END SUB

SUB HitKey (f$)
	DO: f$ = INKEY$: LOOP WHILE f$ <> ""
	DO: f$ = INKEY$: TimeDate: LOOP WHILE f$ = ""
END SUB

SUB HNewPur ()
	Heading
	LPRINT TAB(71); "INVENTORY PURCHASES"
	LPRINT
	LPRINT "                              Inv   Invoice                 Date      Check     Invoice  Inventory   Purch Itm"
	LPRINT "        Vendor Name           Num     Date     Due Date     Paid      Number     Total     Total      Num  Num   Inventory Description          Quantity     Cost  "
	LPRINT "============================ ===== ========== ========== ========== ========== ========= ==========  ===== === ================================ ========= ========="
END SUB

SUB HPurAna ()
	Heading
	LPRINT TAB(68); "PURCHASE PRICE ANALYSIS"
	LPRINT
	LPRINT
	LPRINT "                              INV    INVOICE     DUE        DATE      CHECK    ITM             ITEM                                 TOTAL   UNIT   REFR   Percent "
	LPRINT "        VENDOR NAME           NUM     DATE       DATE       PAID      NUMBER   NUM          DESCRIPTION                QUANTITY     COST    COST   COST   Variance"
	LPRINT "============================ ===== ========== ========== ========== ========== === ================================  ============ ======== ====== ======  ========"
END SUB

SUB KeyHit (f$)
	DO: f$ = INKEY$: LOOP WHILE f$ > ""
	DO: f$ = INKEY$: LOOP WHILE f$ = ""
	f$ = UCASE$(f$)
END SUB

SUB OpenFiles ()
	CLOSE
	OPEN "Material.MAS" FOR RANDOM AS #1 LEN = LEN(Mat)
	OPEN "Material.INX" FOR RANDOM AS #2 LEN = LEN(Mnx)
	OPEN "Inventor.PUR" FOR RANDOM AS #3 LEN = LEN(Pur)
	OPEN "..\AP\Vendors .MAS" FOR RANDOM AS #4 LEN = LEN(Ven)
	OPEN "..\AP\Invoices.MAS" FOR RANDOM AS #5 LEN = LEN(Inv)
END SUB

DEFINT A-K, M-Z
DEFLNG L
SUB PurDisp (L)
	COLOR 14, 0
	GET #3, VAL(Inv.Pur), Pur
	FOR x = 1 TO 25
	oNm(x) = Pur.Itm(x)
	LOCATE x + 21
	IF oNm(x) > "  0" THEN
	   GET #1, VAL(oNm(x)), Mat
	   oQu(x) = VAL(Pur.Qty(x)) * VAL(Mat.Fra) + VAL(Pur.Fra(x))
	   oPr(x) = Pur.Prc(x)
	   LOCATE , 2:  PRINT Mat.Num;
	   LOCATE , 6:  PRINT Mat.Nam;
	   LOCATE , 39: PRINT Mat.Fra; LEFT$(Mat.Mes, 3);
	   LOCATE , 47: PRINT Pur.Qty(x);
	   LOCATE , 53: PRINT Pur.Fra(x);
	   LOCATE , 58: PRINT Mat.Ref;
	   LOCATE , 65: PRINT USING "##.###"; VAL(oPr(x)) / oQu(x);
	   LOCATE , 72: PRINT oPr(x)
	   L = L + VAL(oPr(x)) * 100
	ELSE
	   COLOR 6
	   LOCATE , 2
	   PRINT "   ≥                                ≥       ≥     ≥    ≥      ≥      ≥        "
	END IF
	NEXT
END SUB

DEFSNG A-Z
SUB PurInvc ()
   COLOR 11, 1
   LOCATE 9
   LOCATE , 46: PRINT Inv.Ref
   LOCATE , 46: PRINT Inv.Bdt
   LOCATE , 46: PRINT Inv.Ddt
   LOCATE , 46: PRINT Inv.Pdt
   LOCATE , 46: PRINT Inv.Chk
   LOCATE 9
   LOCATE , 72: PRINT Inv.Grs
   LOCATE , 72: PRINT Inv.Dis
   LOCATE , 72: PRINT Inv.Crd
   PRINT
   LOCATE , 72: PRINT Inv.Net
END SUB

DEFINT A-K, M-Z
DEFLNG L
SUB PurMatr (L)
	L = 0
	FOR x = 0 TO 9
	IF MID$(Inv.Dst, 1 + 2 * x, 2) = "30" THEN
	   L = L + 100 * VAL(MID$(Inv.Dst, 21 + 8 * x, 8))
	END IF
	NEXT
END SUB

DEFSNG A-Z
SUB PurVend ()
	COLOR 11, 1
	LOCATE 8
	LOCATE , 2: PRINT Ven.Nam
	LOCATE , 2: PRINT Ven.Ord
	LOCATE , 2: PRINT Ven.Cnt
	LOCATE , 2: PRINT Ven.Add
	LOCATE , 2: PRINT Ven.Str
	LOCATE , 2: PRINT Ven.Cty; " "; Ven.Sta; " "; Ven.Zip
	LOCATE , 2: PRINT Ven.Tel
END SUB

SUB TimeDate ()
 COLOR 13, 0
 LOCATE 19, 22, 0: PRINT " "; DATE$; " "
 T$ = " " + LEFT$(TIME$, 2)
 IF TIME$ > "11:59:59" THEN f$ = "pm " ELSE f$ = "am "
 IF TIME$ > "12:59:59" THEN T$ = " " + STR$(VAL(T$) - 12)
 LOCATE 19, 47: PRINT T$; MID$(TIME$, 3, 6) + f$
END SUB

DEFINT A-Z
FUNCTION WinMat ()
	PCOPY 0, 1: CALL MhDisplayByName(0, 0, "MatWindow", e)
	n = 1
	Top = LOF(2) \ LEN(Mnx)
	Bot = 1
	r = 17
	GOSUB T2
T1: GET #2, n, Mnx
	COLOR 15, 4: LOCATE r, 21: PRINT USING "&≥###≥&"; Mnx.Typ; Mnx.Num; Mnx.Nam
	KeyHit n$
	COLOR 0, 7: LOCATE r, 21: PRINT USING "&≥###≥&"; Mnx.Typ; Mnx.Num; Mnx.Nam
	SELECT CASE n$
	CASE CHR$(13)
		 GET #1, Mnx.Num, Mat
		 IF Mat.Nam < "!" GOTO T1
		 PCOPY 1, 0
		 EXIT FUNCTION
	CASE CHR$(0) + "P" 'Dn
		 IF n + 1 > Top GOTO T1 ELSE n = n + 1
		 IF r + 1 > 30 THEN CALL scroll(21, 17, 59, 30, 1) ELSE r = r + 1
	CASE CHR$(0) + "H" 'Up
		 IF n - 1 < 1 GOTO T1 ELSE n = n - 1
		 IF r - 1 < 17 THEN CALL BkScroll(21, 17, 59, 30, 1) ELSE r = r - 1
	CASE CHR$(0) + "I" 'PgUp
		 Bot = n - r - 17: IF Bot < 1 THEN Bot = 1
		 GOSUB T2: n = Bot + r - 17
	CASE CHR$(0) + "Q" 'PgDn
		 Bot = 31 - r + n
		 IF Bot > Top - 13 THEN Bot = Top - 13: n = Top - 30 + r ELSE n = n + 14
		 GOSUB T2
	CASE CHR$(0) + "G" 'Home
		 Bot = 1: GOSUB T2
		 n = r - 16
	CASE CHR$(0) + "O" 'End
		 Bot = Top - 13: GOSUB T2
		 n = Top - 30 + r
	CASE ELSE
		 PCOPY 1, 0
		 WinMat = 1
		 EXIT FUNCTION
	END SELECT
	GOTO T1

T2: COLOR 0, 7: LOCATE 17
	FOR x = Bot TO Top
	GET #2, x, Mnx
	LOCATE , 21: PRINT USING "&≥###≥&"; Mnx.Typ; Mnx.Num; Mnx.Nam
	IF CSRLIN > 30 THEN EXIT FOR
	NEXT
	RETURN
END FUNCTION

DEFSNG A-Z
SUB WinMssg (f$)
	PCOPY 0, 1
	COLOR 14, 4: LOCATE 26
	LOCATE , 36: PRINT " …Õ"; STRING$(LEN(f$), "Õ"); "Õª "
	LOCATE , 36: PRINT " ∫ "; f$; " ∫ "
	LOCATE , 36: PRINT " »Õ"; STRING$(LEN(f$), "Õ"); "Õº "
	COLOR 12, 0: LOCATE 27
	LOCATE , 35: PRINT "€"
	LOCATE , 35: PRINT "€"
	LOCATE , 35: PRINT "ﬂﬂﬂ"; STRING$(LEN(f$) + 2, "ﬂ")
	SOUND 770, .1: SOUND 970, .2: KeyHit f$
	PCOPY 1, 0
END SUB


DEFINT A-Z: DEFDBL T: DEFLNG L

DECLARE FUNCTION FindInv (n&)

DECLARE SUB IN_Files ()
DECLARE SUB MatShow ()
DECLARE SUB MatSort ()
DECLARE SUB MatEdit (r, n)
DECLARE SUB MatDisp (x)
DECLARE SUB MatLine (r, L1, L2, L3, L4)
DECLARE SUB Reindex ()
DECLARE SUB DispPur ()
DECLARE SUB WinMat (f$)
DECLARE SUB HPurAna ()
DECLARE SUB HNewPur ()
DECLARE SUB HMaterial ()
DECLARE SUB HInvMonth ()
DECLARE SUB HInvYear ()

REM $INCLUDE: 'BI\Library.REC'
REM $INCLUDE: 'BI\Material.REC'
REM $INCLUDE: 'BI\Payables.Rec'
REM $INCLUDE: 'BI\Purchase.REC'
  
   'record variables
	DIM SHARED Mat AS Material
	DIM SHARED Mnx AS MatIndex
	DIM SHARED Pur AS Purchase
	DIM SHARED Ven AS Vendors
	DIM SHARED Inv AS Invoices
   'common variables
	DIM SHARED Itm(21) AS STRING * 3
	DIM SHARED Qty(21) AS STRING * 8
	DIM SHARED Amt(21) AS STRING * 8
	

Count: '=====================================================================
	
	CLS : COLOR 15, 6
	LOCATE 11, 10: PRINT " Code       Description                  Quantity         Cost   "
	COLOR 10, 0
	FOR x = 1 TO LOF(2) \ LEN(Mnx)
	GET #2, x, Mnx
	GET #1, Mnx.Num, Mat
	LOCATE 12, 50: PRINT USING "#######.##"; Mat.Ybq / 100;
	LOCATE 12, 65: PRINT USING "#######.##"; Mat.Ybp / 100;
	Picture 12, 50, 10, q$
	Picture 12, 65, 10, p$
	LOCATE 12, 10: PRINT Mat.Typ + Mat.Num; " "; Mat.Nam; TAB(61); Mat.Mes;
700 EntryN 12, 50, 10, 2, e, b, q$: IF b THEN x = x - 1
710 EntryN 12, 65, 10, 2, e, b, p$: IF b THEN 700 ELSE
	Mat.Mbq = 100 * VAL(q$)
	Mat.Mbp = 100 * VAL(p$)
	Mat.Ybq = 100 * VAL(q$)
	Mat.Ybp = 100 * VAL(p$)
	PUT #1, Mnx.Num, Mat
	NEXT
	
Reindex: '====================================================================
	COLOR 15, 1: LOCATE 19, 30: PRINT "Rebuild Inventroy Index"
	LOCATE 23, 20: PRINT "Reindexing inventory file record Nง [   ]";
	Reindex

DEFINT T
SUB DispPur ()
	COLOR 2, 0
	GET #5, Inv.Pur, Pur
	FOR r = 0 TO 19
		IF VAL(Pur.Itm(r)) > 0 THEN
		   GET #3, VAL(Pur.Itm(r)), Mat
		   qPrint Pur.Itm(r), r + 22, 3, 0, 1:  Itm(r) = Pur.Itm(r)
		   qPrint Mat.Nam, r + 22, 9, 0, 1
		   qPrint Pur.Qty(r), r + 22, 38, 0, 1: Qty(r) = Pur.Qty(r)
		   qPrint Mat.Mes, r + 22, 46, 0, 1
		   qPrint Mat.Ref, r + 22, 51, 0, 1
		   qPrint Pur.Amt(r), r + 22, 71, 0, 1: Amt(r) = Pur.Amt(r)
		   LOCATE r + 22, 60: PRINT USING "####.###"; VAL(Amt(r)) / VAL(Qty(r));
		   L1 = L1 + VAL(Amt(r)) * 100
		END IF
	NEXT
	COLOR 15, 2: LOCATE 43, 71: PRINT USING "#####.##"; L1 / 100;
END SUB

DEFSNG A-Z
SUB HInvMonth ()
	Heading
	LPRINT TAB(66); " INVENTORY LISTING - MONTH ": LPRINT : LPRINT CHR$(27); "P"; CHR$(15);
	LPRINT "                                           ===BEGINNING BALANCE=== =======PURCHACES======= ======PRODUCTION======= ======PLANT USAGE====== =========SALES========= =========WASTE========= ====ENDING INVENTORY===            "
	LPRINT " Code         Description         Measure   Quantity      Cost      Quantity      Cost      Quantity      Cost     Quantity      Cost      Quantity      Cost      Quantity      Cost      Quantity      Cost       Unit Cost "
	LPRINT "====== ========================== =======  ======================= ======================= ======================= ======================= ======================= ======================= ======================= ==========="
END SUB

SUB HInvYear ()
	Heading
	LPRINT TAB(67); " INVENTORY LISTING - YEAR ": LPRINT : LPRINT CHR$(27); "P"; CHR$(15);
	LPRINT "                                           ===BEGINNING BALANCE=== =======PURCHACES======= ======PRODUCTION======= ======PLANT USAGE====== =========SALES========= =========WASTE========= ====ENDING INVENTORY===            "
	LPRINT " Code         Description         Measure   Quantity      Cost      Quantity      Cost      Quantity      Cost     Quantity      Cost      Quantity      Cost      Quantity      Cost      Quantity      Cost       Unit Cost "
	LPRINT "====== ========================== =======  ======================= ======================= ======================= ======================= ======================= ======================= ======================= ==========="
END SUB

SUB HMaterial ()
	Heading
	LPRINT TAB(70); " INVENTORY LISTING ": LPRINT : LPRINT
	LPRINT "  Type   Num          Description           Units  Measure  Container  Reference Cost  Reorder Point  Reorder Quantity"
	LPRINT "  ====   ===   ==========================   =====  =======  =========  ==============  =============  ================"
END SUB

SUB HNewPur ()
	Heading
	LPRINT TAB(70); "INVENTORY PURCHASES": LPRINT : LPRINT
	LPRINT "                              INV   INVOICE      DUE        DATE      CHECK     INVOICE   MATERIALS  PURCH     ITM"
	LPRINT "        VENDOR NAME           NUM     DATE       DATE       PAID      NUMBER     TOTAL      TOTAL     NUM      NUM      ITEM DISCRIPTION        QUANTITY    PRICE  "
	LPRINT "============================ ===== ========== ========== ========== ========== ========== =========  =====     === =========================== ========== ========="
END SUB

SUB HPurAna ()
	Heading
	LPRINT TAB(68); "PURCHASE PRICE ANALYSIS": LPRINT : LPRINT
	LPRINT "                              INV    INVOICE     DUE        DATE      CHECK    PURCH ITM        ITEM                               TOTAL    UNIT     REFE     %    "
	LPRINT "        VENDOR NAME           NUM     DATE       DATE       PAID      NUMBER    NUM  NUM     DISCRIPTION              QUANTITY     PRICE    COST     COST  Variance"
	LPRINT "============================ ===== ========== ========== ========== ========== ===== === ========================== ============ ======== ======== ======= ========"
END SUB

SUB IN_Files ()
	CLOSE #2:  OPEN "Vendors .MAS" FOR RANDOM SHARED AS #2 LEN = LEN(Ven)
	CLOSE #3:  OPEN "Material.MAS" FOR RANDOM SHARED AS #3 LEN = LEN(Mat)
	CLOSE #4:  OPEN "Material.INX" FOR RANDOM SHARED AS #4 LEN = LEN(Mnx)
	CLOSE #5:  OPEN "Material.PUR" FOR RANDOM SHARED AS #5 LEN = LEN(Pur)
	CLOSE #7:  OPEN "Invoices.MAS" FOR RANDOM SHARED AS #7 LEN = LEN(Inv)
	CLOSE #11: OPEN "LPT1:" FOR OUTPUT AS #11
END SUB

DEFINT A-Z
SUB MatClose (p)
	SELECT CASE p
		CASE 1 '==close month==
			FOR x = 1 TO LOF(1) \ LEN(Mat)
				GET #1, x, Mat
				Mat.Mbq = Mat.Mbq + Mat.Mpq + Mat.Mmq - Mat.Mkq - Mat.Msq - Mat.Mwq
				Mat.Mbp = Mat.Mbp + Mat.Mpp + Mat.Mmp - Mat.Mkp - Mat.Msc - Mat.Mwp
				Mat.Mpq = 0: Mat.Mpp = 0 'purchases
				Mat.Mmq = 0: Mat.Mmp = 0 'production
				Mat.Mkq = 0: Mat.Mkp = 0 'requisitions
				Mat.Msq = 0: Mat.Msc = 0 'cost of sales
				Mat.Mwq = 0: Mat.Mwp = 0 'spoilage
				Mat.Msp = 0              'sales
				PUT #1, x, Mat
			NEXT
		CASE 2 '==close year==
			FOR x = 1 TO LOF(1) \ LEN(Mat)
				Mat.Ybq = Mat.Ybq + Mat.Ypq + Mat.Ymq - Mat.Ykq - Mat.Ysq - Mat.Ywq
				Mat.Ybp = Mat.Ybp + Mat.Ypp + Mat.Ymp - Mat.Ykp - Mat.Ysc - Mat.Ywp
				Mat.Ypq = 0: Mat.Ypp = 0 'purchases
				Mat.Ymq = 0: Mat.Ymp = 0 'production
				Mat.Ykq = 0: Mat.Ykp = 0 'requisitions
				Mat.Ysq = 0: Mat.Ysc = 0 'cost of sales
				Mat.Ywq = 0: Mat.Ywp = 0 'spoilage
				Mat.Ygq = 0: Mat.Ygp = 0 'gains or loss
				Mat.Ysp = 0              'sales
				PUT #1, x, Mat
			NEXT
	END SELECT
END SUB

SUB MatEdit (r, n)
	p$ = Mnx.Typ + Mnx.Nam
611 EntryU r, 6, 3, e, b, Mat.Typ:     IF e THEN 620 ELSE IF b THEN 611
	IF Mat.Typ < "!" THEN WinMssg "Type please": GOTO 611
612 EntryS r, 10, 26, e, b, Mat.Nam:   IF e THEN 620 ELSE IF b THEN 611
	IF Mat.Nam < "!" THEN WinMssg "Name must not be blank": GOTO 612
613 EntryN r, 37, 4, 0, e, b, Mat.Fra: IF e THEN 620 ELSE IF b THEN 612
	IF Mat.Fra < "   1" THEN WinMssg "Fraction must not be zero": GOTO 613
614 EntryS r, 42, 2, e, b, Mat.Mes:    IF e THEN 620 ELSE IF b THEN 613
	IF Mat.Mes < "!" THEN WinMssg "How about a measure": GOTO 614
615 EntryS r, 45, 2, e, b, Mat.Con:    IF e THEN 620 ELSE IF b THEN 614
	IF Mat.Con < "!" THEN WinMssg "Dont you want a container": GOTO 615
616 EntryN r, 49, 7, 3, e, b, Mat.Ref: IF e THEN 620 ELSE IF b THEN 615
	IF Mat.Ref < "   .001" THEN WinMssg "I must have a price": GOTO 616
617 EntryN r, 57, 8, 4, e, b, Mat.Prc: IF e THEN 620 ELSE IF b THEN 616
	IF Mat.Prc < "   .01" THEN WinMssg "I must have a price": GOTO 617
618 EntryN r, 67, 5, 0, e, b, Mat.Rpt: IF e THEN 620 ELSE IF b THEN 617
619 EntryN r, 75, 5, 0, e, b, Mat.Rqt: IF e THEN 620 ELSE IF b THEN 618
	'WinMssg "Press [*] to save"
	IF Confirm("save Inventory") THEN
	   Mat.Num = RIGHT$(STR$(1000 + Mnx.Num), 3)
	   Mnx.Typ = Mat.Typ
	   Mnx.Nam = Mat.Nam
	   PUT #2, n, Mnx
	   PUT #1, Mnx.Num, Mat
	   IF p$ <> Mnx.Typ + Mnx.Nam THEN
		  MatSort
	   END IF
	ELSE
	   GOTO 611
	END IF
620
END SUB

DEFLNG L
DEFSNG A-K, M-Q, S-Z
SUB MatLine (r, L1, L2, L3, L4)
	LOCATE r, 21: PRINT USING "#######.##"; L1 / 100;
	LOCATE r, 32: PRINT USING "#######.##"; L2 / 100;
	LOCATE r, 43: IF L1 <> 0 THEN PRINT USING "###.##"; L2 / L1;  ELSE PRINT "      ";
	LOCATE r, 52: PRINT USING "#######.##"; L3 / 100;
	LOCATE r, 63: PRINT USING "#######.##"; L4 / 100;
	LOCATE r, 74: IF L3 <> 0 THEN PRINT USING "###.##"; L4 / L3;  ELSE PRINT "      ";
END SUB

DEFINT A-K, M-Q, S, U-Z
DEFDBL T
SUB MatMonth ()
	StatLin "Print Inventory - Month"
	PageNum = 0: HInvMonth
	FOR y = 1 TO LOF(1) \ LEN(Mat)
	IF INKEY$ = CHR$(27) THEN EXIT FOR
	GET #2, y, Mnx
	GET #1, Mnx.Num, Mat
	IF Mat.Nam > "!" THEN
	   LineNum = LineNum + 1: IF LineNum > 60 THEN HInvMonth
	   Lq = (Mat.Mbq + Mat.Mpq + Mat.Mmq - Mat.Mkq - Mat.Msq - Mat.Mwq)
	   Lp = (Mat.Mbp + Mat.Mpp + Mat.Mmp - Mat.Mkp - Mat.Msc - Mat.Mwp)
	   LPRINT USING "& "; Mat.Typ + Mat.Num; Mat.Nam; Mat.Fra; Mat.Mes;
	   LPRINT USING "  #######.##"; Mat.Mbq / 100; Mat.Mbp / 100; Mat.Mpq / 100; Mat.Mpp / 100; Mat.Mmq / 100; Mat.Mmp / 100;
	   LPRINT USING "  #######.##"; Mat.Mkq / 100; Mat.Mkp / 100; Mat.Msq / 100; Mat.Msc / 100; Mat.Mwq / 100; Mat.Mwp / 100;
	   LPRINT USING "  #######.##"; Lq / 100; Lp / 100;
	   IF Lq <> 0 THEN LPRINT USING "  #######.##"; Lp / Lq ELSE LPRINT
	   L0 = L0 + Mat.Mbq
	   L1 = L1 + Mat.Mbp
	   L2 = L2 + Mat.Mpq
	   L3 = L3 + Mat.Mpp
	   L4 = L4 + Mat.Mmq
	   L5 = L5 + Mat.Mmp
	   L6 = L6 + Mat.Mkq
	   L7 = L7 + Mat.Mkp
	   L8 = L8 + Mat.Msq
	   L9 = L9 + Mat.Msc
	   La = La + Mat.Mwq
	   Lb = Lb + Mat.Mwp
	END IF
	NEXT
	LPRINT : LPRINT TAB(43);
	LPRINT USING " ########.##"; L0 / 100; L1 / 100; L2 / 100; L3 / 100; L4 / 100; L5 / 100; L6 / 100; L7 / 100; L8 / 100; L9 / 100; La / 100; Lb / 100;
	LPRINT USING " ########.##"; (L0 + L2 + L4 - L6 - L8 - La) / 100; (L1 + L3 + L5 - L7 - L9 - Lb) / 100
END SUB

DEFSNG A-Z
SUB MatShow ()
	PCOPY 0, 1
	CALL MhDisplayByName(0, 0, "in-MatDet", e)
	COLOR 14, 6
	LOCATE 10, 14: PRINT Mat.Typ; "-"; Mat.Num
	LOCATE 12, 14: PRINT Mat.Nam
	LOCATE 14, 14: PRINT LTRIM$(Mat.Fra); " "; RTRIM$(Mat.Mes); "/"; Mat.Con
	LOCATE 10, 69: PRINT Mat.Ref; "/"; Mat.Mes
	LOCATE 11, 69: PRINT Mat.Prc; "/"; Mat.Mes
	LOCATE 13, 70: PRINT Mat.Rpt; " "; Mat.Mes
	LOCATE 14, 70: PRINT Mat.Rqt; " "; Mat.Mes
   
	COLOR , 6: MatLine 21, Mat.Msq, Mat.Msp, Mat.Ysq, Mat.Ysp
	COLOR , 6: MatLine 22, Mat.Msq, Mat.Msc, Mat.Ysq, Mat.Ysc
	COLOR , 0: MatLine 24, Mat.Msq, Mat.Msp - Mat.Msc, Mat.Ysq, Mat.Ysp - Mat.Ysc
   
	COLOR , 6: MatLine 31, Mat.Mbq, Mat.Mbp, Mat.Ybq, Mat.Ybp
	COLOR , 6: MatLine 32, Mat.Mpq, Mat.Mpp, Mat.Ypq, Mat.Ypp
	COLOR , 6: MatLine 33, Mat.Mmq, Mat.Mmp, Mat.Ymq, Mat.Ymp
	COLOR , 0: MatLine 35, Mat.Mbq + Mat.Mpq + Mat.Mmq, Mat.Mbp + Mat.Mpp + Mat.Mmp, Mat.Ybq + Mat.Ypq + Mat.Ymq, Mat.Ybp + Mat.Ypp + Mat.Ymp
	COLOR , 6: MatLine 37, Mat.Msq, Mat.Msc, Mat.Ysq, Mat.Ysc
	COLOR , 6: MatLine 38, Mat.Mkq, Mat.Mkp, Mat.Ykq, Mat.Ykp
	COLOR , 6: MatLine 39, Mat.Mwq, Mat.Mwp, Mat.Ywq, Mat.Ywp
	COLOR , 6: MatLine 40, 0, 0, Mat.Ygq, Mat.Ygp
	COLOR , 0: MatLine 42, Mat.Mkq + Mat.Msq + Mat.Mwq, Mat.Mkp + Mat.Msc + Mat.Mwp, Mat.Ykq + Mat.Ysq + Mat.Ywq + Mat.Ygq, Mat.Ykp + Mat.Ysc + Mat.Ywp + Mat.Ygp
	COLOR , 6: MatLine 44, Mat.Mbq + Mat.Mpq + Mat.Mmq - Mat.Mkq - Mat.Msq - Mat.Mwq, Mat.Mbp + Mat.Mpp + Mat.Mmp - Mat.Mkp - Mat.Msc - Mat.Mwp, Mat.Ybq + Mat.Ypq + Mat.Ymq - Mat.Ykq - Mat.Ysq - Mat.Ywq - Mat.Ygq, Mat.Ybp + Mat.Ypp + Mat.Ymp - Mat. _
Ysc - Mat.Ykp - Mat.Ywp - Mat.Ygp
	KeyPres f$
	PCOPY 1, 0
END SUB

SUB MatSort ()
	CLOSE
	SHELL "SortF Material.Inx Material /L31 /+1,29 /Q"
	KILL "Material.Inx": NAME "Material" AS "Material.INX"
	IN_Files
END SUB

DEFINT A-K, M-S, U-Z
DEFLNG L
DEFDBL T
SUB MatYear ()
	StatLin "Print Inventory - Year"
	PageNum = 0: HInvYear
	FOR y = 1 TO LOF(1) \ LEN(Mat)
	IF INKEY$ = CHR$(27) THEN EXIT FOR
	GET #2, y, Mnx
	GET #1, Mnx.Num, Mat
	IF Mat.Nam > "!" THEN
	   LineNum = LineNum + 1: IF LineNum > 60 THEN HInvYear
	   Lq = (Mat.Ybq + Mat.Ypq + Mat.Ymq - Mat.Ykq - Mat.Ysq - Mat.Ywq)
	   Lp = (Mat.Ybp + Mat.Ypp + Mat.Ymp - Mat.Ykp - Mat.Ysc - Mat.Ywp)
	   LPRINT USING "& "; Mat.Typ + Mat.Num; Mat.Nam; Mat.Fra; Mat.Mes;
	   LPRINT USING "  #######.##"; Mat.Ybq / 100; Mat.Ybp / 100; Mat.Ypq / 100; Mat.Ypp / 100; Mat.Ymq / 100; Mat.Ymp / 100;
	   LPRINT USING "  #######.##"; Mat.Ykq / 100; Mat.Ykp / 100; Mat.Ysq / 100; Mat.Ysc / 100; Mat.Ywq / 100; Mat.Ywp / 100;
	   LPRINT USING "  #######.##"; Lq / 100; Lp / 100;
	   IF Lq <> 0 THEN LPRINT USING "  #######.##"; Lp / Lq ELSE LPRINT
	   L0 = L0 + Mat.Ybq
	   L1 = L1 + Mat.Ybp
	   L2 = L2 + Mat.Ypq
	   L3 = L3 + Mat.Ypp
	   L4 = L4 + Mat.Ymq
	   L5 = L5 + Mat.Ymp
	   L6 = L6 + Mat.Ykq
	   L7 = L7 + Mat.Ykp
	   L8 = L8 + Mat.Ysq
	   L9 = L9 + Mat.Ysc
	   La = La + Mat.Ywq
	   Lb = Lb + Mat.Ywp
	END IF
	NEXT
	LPRINT : LPRINT TAB(43);
	LPRINT USING " ########.##"; L0 / 100; L1 / 100; L2 / 100; L3 / 100; L4 / 100; L5 / 100; L6 / 100; L7 / 100; L8 / 100; L9 / 100; La / 100; Lb / 100;
	LPRINT USING " ########.##"; (L0 + L2 + L4 - L6 - L8 - La) / 100; (L1 + L3 + L5 - L7 - L9 - Lb) / 100
END SUB

SUB minMatMas ()
	CALL MhDisplayByName(0, 0, "in-MatMast", e)
	StatLin "Material Inventory Master"
	SubMenu 1, "D"
	b = 1: t = LOF(4) \ LEN(Mnx): GOSUB i2
	r = 3: n = 1
i1: GET #4, n, Mnx: GET #3, Mnx.Num, Mat
	COLOR 11, 7: LOCATE r, 2, 0: PRINT USING "&ณ&ณ&ณ&ณ&ณ&ณ &ณ&ณ & ณ &"; Mat.Num; Mnx.Typ; Mnx.Nam; Mat.Fra; Mat.Mes; Mat.Con; Mat.Ref; Mat.Prc; Mat.Rpt; Mat.Rqt;
	KeyPres n$
	IF n$ = CHR$(13) THEN SubMenu 0, n$
	SELECT CASE n$
	CASE CHR$(27) 'Esc
		EXIT SUB
	CASE CHR$(0) + "M" '================Right==
		SubMenu 0, n$
	CASE CHR$(0) + "K" '================Left===
		SubMenu 0, n$
	CASE CHR$(0) + "P"  '===============Down===
		IF n + 1 > t GOTO i1 ELSE n = n + 1
		COLOR 27, 1: LOCATE r, 2, 0: PRINT USING "&ณ&ณ&ณ&ณ&ณ&ณ &ณ&ณ & ณ &"; Mat.Num; Mnx.Typ; Mnx.Nam; Mat.Fra; Mat.Mes; Mat.Con; Mat.Ref; Mat.Prc; Mat.Rpt; Mat.Rqt;
		IF r + 1 > 47 THEN CALL scroll(2, 3, 79, 47, 1) ELSE r = r + 1
	CASE CHR$(0) + "H"  '===============Up=====
		IF n - 1 < 1 GOTO i1 ELSE n = n - 1
		COLOR 27, 1: LOCATE r, 2, 0: PRINT USING "&ณ&ณ&ณ&ณ&ณ&ณ &ณ&ณ & ณ &"; Mat.Num; Mnx.Typ; Mnx.Nam; Mat.Fra; Mat.Mes; Mat.Con; Mat.Ref; Mat.Prc; Mat.Rpt; Mat.Rqt;
		IF r - 1 < 3 THEN CALL BkScroll(2, 3, 79, 47, 1) ELSE r = r - 1
	CASE CHR$(0) + "I"  '===============PgUp===
		b = n - r - 42: IF b < 1 THEN b = 1
		GOSUB i2
	CASE CHR$(0) + "Q"  '===============PgDn===
		b = n - r + 48: IF b > t - 44 THEN b = t - 44
		IF b < 1 THEN b = 1
		GOSUB i2
	CASE CHR$(0) + "G"  '===============Home===
		b = 1: GOSUB i2
	CASE CHR$(0) + "O"  '===============End====
		b = t - 44: IF b < 1 THEN b = 1
		GOSUB i2
	CASE "L"
		SubMenu 0, n$
		IF Confirm("Print Inventory List") THEN
			PageNum = 0: HMaterial
			FOR y = 1 TO LOF(1) \ LEN(Mat)
				IF INKEY$ = CHR$(27) THEN EXIT FOR
				GET #2, y, Mnx
				GET #1, Mnx.Num, Mat
				IF Mat.Nam > "!" THEN
				   LineNum = LineNum + 1: IF LineNum > 60 THEN HMaterial
				   LPRINT USING "   &"; Mat.Typ; Mat.Num; Mat.Nam; Mat.Fra; Mat.Mes; Mat.Con;
				   LPRINT USING "       &     "; Mat.Ref; Mat.Prc; Mat.Rpt; Mat.Rqt
				END IF
			NEXT
		END IF
	CASE "E"
		SubMenu 0, n$
		L1 = Mat.Ybp + Mat.Ypp + Mat.Ysp + Mat.Ykp + Mat.Ywp
		IF L1 <> 0 THEN
		   WinMssg "Active item, no erase"
		ELSE
		   IF Confirm("ERASE Item") THEN
			  Mnx.Typ = "þþþ"
			  Mnx.Nam = "þþþþþþþþþþþþþþþþþþþþþþþþþþ"
			  PUT #1, Mnx.Num
			  PUT #1, Mnx.Num, Mat.Num
			  PUT #2, n, Mnx
			  MatSort
			  GOSUB i2
		   END IF
		END IF
	CASE "D"
		SubMenu 0, n$
		MatShow
	CASE "A"
		SubMenu 0, n$
		y = 0
		FOR z = 1 TO Top: GET #2, z, Mnx                 'see if a deleted
		IF LEFT$(Mnx.Nam, 1) = "þ" THEN y = z: EXIT FOR  'record is found
		NEXT                                             'in master file
		IF y = 0 THEN Mnx.Num = 1 + LOF(1) \ LEN(Mat): y = Mnx.Num 'append if not found
		IF Mnx.Num > 999 THEN
		   WinMssg "Inventory Master is full"
		ELSE
		   GET #1, Mnx.Num, Mat
		   COLOR 2, 0: LOCATE r, 2: PRINT USING "###ณ   ณ                          ณ    ณ  ณ  ณ        ณ        ณ       ณ      "; Mnx.Num
		   MatEdit r, y
		END IF
	CASE "M"
		SubMenu 0, n$
		MatEdit r, n
	END SELECT
	GOTO i1
i2: COLOR 27, 1: LOCATE 3
	FOR x = b TO t
		GET #4, x, Mnx: GET #3, Mnx.Num, Mat
		LOCATE , 2: PRINT USING "&ณ&ณ&ณ&ณ&ณ&ณ &ณ&ณ & ณ &"; Mat.Num; Mnx.Typ; Mnx.Nam; Mat.Fra; Mat.Mes; Mat.Con; Mat.Ref; Mat.Prc; Mat.Rpt; Mat.Rqt
		IF CSRLIN > 47 THEN EXIT FOR
	NEXT
	n = b + r - 3
	RETURN
END SUB

SUB minPurNew ()
	CALL MhDisplayByName(0, 0, "in-Purchase", e)
	StatLin "Inventory Purchase"
	ERASE Itm, Qty, Amt
	f$ = "        ": L1 = 0
110 EntryN 8, 18, 8, 0, e, b, f$: IF e THEN EXIT SUB ELSE IF b THEN 110
	In = FindInv(VAL(f$))
	IF In = 0 THEN WinMssg "Invoice not found": GOTO 110
	GET #7, In, Inv
	GET #2, VAL(Inv.Ven), Ven
	L0 = 0
	FOR x = 0 TO 24
		IF Inv.Act(x) = "09" THEN L0 = L0 + 100 * VAL(Inv.Amt(x))
	NEXT

	qPrint Ven.Num, 12, 3, 0, 1
	qPrint Ven.Nam, 13, 3, 0, 1
	qPrint Ven.Add, 14, 3, 0, 1
	qPrint Ven.Cty + " " + Ven.Sta + " " + Ven.Zip, 15, 3, 0, 1

	qPrint Inv.Ref, 12, 50, 0, 1
	qPrint Inv.Bdt, 13, 50, 0, 1
	qPrint Inv.Ddt, 14, 50, 0, 1
	qPrint Inv.Pdt, 15, 50, 0, 1
	qPrint Inv.Chk, 16, 50, 0, 1

	qPrint Inv.Grs, 12, 71, 0, 1
	qPrint Inv.Dis, 13, 71, 0, 1
	qPrint Inv.Crd, 14, 71, 0, 1
	qPrint Inv.Net, 16, 71, 0, 1
	
	LOCATE 8, 71: PRINT USING "#####.##"; L0 / 100
	IF Inv.Pur > 0 THEN
	   DispPur
	   Picture 43, 71, 8, f$: L1 = VAL(f$) * 100
	ELSE
	   Inv.Pur = 1 + LOF(5) \ LEN(Pur)
	END IF
112 r = 0
114 EntryS r + 22, 3, 3, e, b, Pur.Itm(r): IF e THEN EXIT SUB
	IF b THEN r = r - 1: IF r < 0 THEN 112 ELSE 114
	IF Pur.Itm(r) = "   " THEN
	   L1 = L1 - VAL(Pur.Amt(r)) * 100
	   FOR z = r TO 19
		   Pur.Itm(z) = Pur.Itm(z + 1)
		   Pur.Qty(z) = Pur.Qty(z + 1)
		   Pur.Amt(z) = Pur.Amt(z + 1)
	   NEXT
	   IF r < 20 THEN CALL scroll(4, r + 22, 77, 41, 1)
	   COLOR 4, 0: LOCATE 41, 4: PRINT "   ณ                            ณ            ณ         ณ         ณ          ";
	   COLOR 15, 2: LOCATE 43, 71: PRINT USING "#####.##"; L1 / 100;
	   IF VAL(Pur.Itm(r)) = 0 THEN 119 ELSE 114
	ELSE
	   IF VAL(Pur.Itm(r)) < 1 THEN WinMat Pur.Itm(r): IF Pur.Itm(r) < "  1" THEN 114
	END IF
	GET #3, VAL(Pur.Itm(r)), Mat
	IF Mat.Nam < "!" THEN WinMat Pur.Itm(r):  IF Pur.Itm(r) < "  1" THEN 114
	Pur.Itm(r) = Mat.Num
	qPrint Pur.Itm(r), r + 22, 3, 0, 1
	qPrint Mat.Nam, r + 22, 9, 0, 1
	qPrint Mat.Mes, r + 22, 46, 0, 1
116 EntryN r + 22, 38, 8, 2, e, b, Pur.Qty(r): IF e THEN EXIT SUB ELSE IF b THEN 114
	IF Pur.Qty(r) < "    0.01" THEN WinMssg "Quantity please!": GOTO 116
	qPrint Mat.Ref, r + 22, 51, 0, 1
	L1 = L1 - VAL(Pur.Amt(r)) * 100
118 EntryN r + 22, 71, 8, 2, e, b, Pur.Amt(r): IF e THEN EXIT SUB
	L1 = L1 + VAL(Pur.Amt(r)) * 100: IF b THEN 116
	IF Pur.Amt(r) < "    0.01" THEN WinMssg "Price please!": GOTO 118
	IF ABS(VAL(Pur.Amt(r)) / VAL(Pur.Qty(r)) - VAL(Mat.Ref)) > .1 * VAL(Mat.Ref) THEN
	   SOUND 770, .2: SOUND 970, .1
	   COLOR 12, 0
	ELSE
	   COLOR 2, 0
	END IF
	LOCATE r + 22, 60: PRINT USING "####.###"; VAL(Pur.Amt(r)) / VAL(Pur.Qty(r));
	COLOR 15, 2: LOCATE 43, 71: PRINT USING "#####.##"; L1 / 100;
	r = r + 1: IF r < 20 THEN 114 ELSE r = 19: GOTO 114

119 Picture 8, 71, 8, f$: Picture 43, 71, 8, t$
	IF f$ <> t$ THEN 114
	IF Confirm("Purchase") = 0 THEN 114
	PUT #7, In, Inv
	PUT #5, Inv.Pur, Pur
	FOR r = 0 TO 19
	IF VAL(Pur.Itm(r)) > 0 THEN  '---------------------Post new materials
	   GET #3, VAL(Pur.Itm(r)), Mat
	   Mat.Mpq = Mat.Mpq + VAL(Pur.Qty(r)) * 100: Mat.Mpp = Mat.Mpp + VAL(Pur.Amt(r)) * 100
	   Mat.Ypq = Mat.Ypq + VAL(Pur.Qty(r)) * 100: Mat.Ypp = Mat.Ypp + VAL(Pur.Amt(r)) * 100
	   PUT #3, VAL(Pur.Itm(r)), Mat
	ELSE
	   EXIT FOR
	END IF
	NEXT
	FOR r = 0 TO 19
	IF VAL(Itm(r)) > 0 THEN  '---------------------Void old materials
	   GET #3, VAL(Itm(r)), Mat
	   Mat.Mpq = Mat.Mpq - VAL(Qty(r)) * 100: Mat.Mpp = Mat.Mpp - VAL(Amt(r)) * 100
	   Mat.Ypq = Mat.Ypq - VAL(Qty(r)) * 100: Mat.Ypp = Mat.Ypp - VAL(Amt(r)) * 100
	   PUT #3, VAL(Itm(r)), Mat
	ELSE
	   EXIT FOR
	END IF
	NEXT
	minPurNew
END SUB

SUB minPurPri ()
	StatLin "Purchase Price Analysis"
	
	LOCATE 23, 20: PRINT "Sorting Purchases file, one moment please";
	CLOSE #9: OPEN "Purchase" FOR OUTPUT AS #9
	FOR x = EndPur + 1 TO LOF(3) \ LEN(Pur)
		GET #3, x, Pur
	'    GET #5, Pur.Inv, Inv
		FOR y = 1 TO 20
			IF VAL(Pur.Itm(y)) > 0 THEN
			   PRINT #9, USING "&&#####&&"; Pur.Itm(y); Pur.Inv; Inv.Pur; Pur.Qty(y); Pur.Amt(y)
			ELSE
			   EXIT FOR
			END IF
		NEXT
	NEXT
	CLOSE #9
	SHELL "SORTF Purchase Purchase.INX /L31 /+1,8 /Q"
	KILL "Purchase"

	LOCATE 23, 20: PRINT " Printing purchase price analysis report ";
	OPEN "Purchase.INX" FOR RANDOM AS #9 LEN = 31
	FIELD #9, 3 AS A1$, 5 AS b1$, 5 AS c1$, 8 AS d1$, 8 AS E1$, 2 AS f$
	PageNum = 0: HPurAna
	GET #9, 1
	p$ = A1$
	FOR x = 1 TO LOF(9) \ 31
		GET #9, x
		IF p$ <> A1$ THEN GOSUB TotItem
		GET #5, VAL(b1$), Inv
		GET #4, VAL(Inv.Ven), Ven
		GET #1, VAL(A1$), Mat
		LineNum = LineNum + 1: IF LineNum > 59 THEN HPurAna
		LPRINT USING "& "; Ven.Nam; b1$; Inv.Bdt; Inv.Ddt; Inv.Pdt; Inv.Chk; c1$; A1$; Mat.Nam;
		LPRINT USING " & & & ###.### &  +###.##%"; d1$; Mat.Mes; E1$; VAL(E1$) / VAL(d1$); Mat.Ref; 100 * ((VAL(E1$) / VAL(d1$)) - VAL(Mat.Ref)) / VAL(Mat.Ref)
		T1 = T1 + VAL(d1$): T2 = T2 + VAL(E1$)
	NEXT
	GOSUB TotItem
	LPRINT TAB(115);
	LPRINT USING "########.##  #######.## ###.###"; T3 + T1; T4 + T2; (T4 + T2) / (T3 + T1)
	EXIT SUB
TotItem:
	LPRINT : LPRINT TAB(115);
	LPRINT USING "########.##  #######.## ###.###"; T1; T2; T2 / T1
	LPRINT : LineNum = LineNum + 3
	p$ = A1$: T3 = T3 + T1: T4 = T4 + T2: T1 = 0: T2 = 0
	RETURN
END SUB

SUB minPurPrt ()
	StatLin "Print Inventory purchases"
	PageNum = 0: HNewPur
	FOR x = FromPur TO LOF(3) \ LEN(Pur)
	T1 = 0: T2 = 0
	GET #3, x, Pur
	GET #5, VALPur.Inv, Inv
	GET #4, VAL(Inv.Ven), Ven
	FOR z = 1 TO 25
		IF Inv.Act(z) = "09" THEN T1 = T1 + VAL(Inv.Amt(z))
	NEXT
	LPRINT USING "& "; Ven.Nam; Pur.Inv; Inv.Bdt; Inv.Ddt; Inv.Pdt; Inv.Chk; " " + Inv.Net;
	LPRINT USING " ######.## &"; T1; Pur.Inv;

	FOR y = 1 TO 20
		IF VAL(Pur.Itm(y)) > 0 THEN
		   GET #1, VAL(Pur.Itm(y)), Mat
		   T2 = T2 + VAL(Pur.Amt(y))
		   LPRINT TAB(111);
		   LPRINT USING " &"; Mat.Num; Mat.Nam; Pur.Qty(y); Mat.Mes; Pur.Amt(y)
		   LineNum = LineNum + 1: IF LineNum > 59 THEN HNewPur
		END IF
	NEXT
	LPRINT : LPRINT TAB(156); : LPRINT USING "#####.##"; T2: LPRINT
	T3 = T3 + T2: T2 = 0: LineNum = LineNum + 3
	NEXT
	LPRINT TAB(154); : LPRINT USING "#######.##"; T3
END SUB

SUB minVerify ()
	StatLin "Verify Purchase Invoices"
	FOR x = 1 TO LOF(5) \ LEN(Inv)
		GET #5, x, Inv
		L1 = 0: L2 = 0: f = 0
		FOR z = 1 TO 25
			IF Inv.Act(z) = "09" THEN L1 = L1 + 100 * VAL(Inv.Amt(z)): f = 1
		NEXT
		IF f THEN
		   IF Inv.Pur > 0 THEN
			  GET #3, Inv.Pur, Pur
			  FOR y = 1 TO 20
				  IF VAL(Pur.Itm(y)) > 0 THEN L2 = L2 + 100 * VAL(Pur.Amt(y)) ELSE EXIT FOR
			  NEXT
			  IF L1 <> L2 THEN PRINT USING "Invoice No: & total: #####.##  purchase:#####.##"; Pur.Inv; L1 / 100; L2 / 100
		   ELSE
			  PRINT USING "Invoice No:##### does not have a purchase."; x: KeyPres f$
		   END IF
		END IF
	NEXT
	PRINT "Hit any key to continue": KeyPres f$
END SUB

DEFINT L, T
SUB Reindex ()
	IN_Files
	FOR x = 1 TO LOF(1) / LEN(Mat)
	GET #1, x, Mat
	IF Mat.Nam > "!" THEN
	   Mnx.Typ = Mat.Typ
	   Mnx.Nam = Mat.Nam
	ELSE
	   Mnx.Typ = "þþþ"
	   Mnx.Nam = "þþþþþþþþþþþþþþþþþþþþþþþþþþ"
	END IF
	Mnx.Num = x
	PUT #2, x, Mnx
	LOCATE 23, 57: PRINT Mat.Num;
	NEXT
	MatSort
END SUB

SUB WinMat (f$)
	PCOPY 0, 1
	n = 1
	Top = LOF(4) \ LEN(Mnx)
	Bot = 1
	r = 7
	GOSUB T2
T1: GET #4, n, Mnx
	COLOR 15, 5: LOCATE r, 22: PRINT USING "&###ณ&"; Mnx.Typ; Mnx.Num; Mnx.Nam
	KeyPres n$
	COLOR 11, 1: LOCATE r, 22: PRINT USING "&###ณ&"; Mnx.Typ; Mnx.Num; Mnx.Nam
	SELECT CASE n$
	CASE CHR$(13)
		 GET #3, Mnx.Num, Mat
		 IF Mat.Nam < "!" GOTO T1
		 Picture r, 25, 3, f$
		 PCOPY 1, 0
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
		 PCOPY 1, 0
		 EXIT SUB
	END SELECT
	GOTO T1

T2: COLOR 11, 1: LOCATE 6, 20: PRINT " ษออออออัออออออออออออออออออออออออออป "
	FOR x = Bot TO Top: GET #4, x, Mnx
	LOCATE , 20: PRINT USING " บ&###ณ&บ "; Mnx.Typ; Mnx.Num; Mnx.Nam
	IF CSRLIN > 20 THEN EXIT FOR
	NEXT
	LOCATE , 20: PRINT " ศออออออฯออออออออออออออออออออออออออผ ": COLOR 15
	LOCATE , 20: PRINT "  "; CHR$(17); "ู "; CHR$(24); " "; CHR$(25); "  PgUp  PgDn  Home  End  Esc "
	RETURN
END SUB


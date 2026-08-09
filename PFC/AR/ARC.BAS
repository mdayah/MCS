DEFINT A-Z: DEFLNG L
DECLARE FUNCTION Confirm ()
DECLARE SUB OpenFiles ()
DECLARE SUB KeyHit (f$)
DECLARE SUB WinMssg (f$)
DECLARE SUB CusOrdr (n$, f, e)
DECLARE SUB Heading ()
DECLARE SUB CusDisp (x)
DECLARE SUB CusHdng ()
DECLARE SUB CusShow ()
DECLARE SUB CusSort ()
DECLARE SUB CusEdit (n)
DECLARE SUB CusLine (r, Lm, Ly)
DECLARE SUB WinOrdr (f, T)
DECLARE SUB CusLite (m, p)
DECLARE SUB OrdShow ()

'$INCLUDE: '..\BI\Library.Dec'
'$INCLUDE: '..\BI\Products.REC'
'$INCLUDE: '..\BI\ProIndex.REC'
'$INCLUDE: '..\BI\Customer.Rec'
'$INCLUDE: '..\BI\CusIndex.Rec'
'$INCLUDE: '..\BI\Orders.Rec'
'$INCLUDE: '..\BI\OrdIndex.Rec'
'$INCLUDE: '..\BI\Material.REC'
'$INCLUDE: '..\BI\MatIndex.REC'
'$INCLUDE: '..\BI\Recipes.REC'

   'record variables
	COMMON SHARED Pro AS Products
	COMMON SHARED Pnx AS ProIndex
	COMMON SHARED Mat AS Material
	COMMON SHARED Mnx AS MatIndex
	COMMON SHARED Rcp AS Recipes
	COMMON SHARED Cus AS Customer
	COMMON SHARED Cnx AS CusIndex
	COMMON SHARED Ord AS Orders
	COMMON SHARED Onx AS OrdIndex

	DIM SHARED Menu(6, 6) AS INTEGER
	DIM SHARED Sort    AS INTEGER
	DIM SHARED Top     AS INTEGER
	DIM SHARED Bot     AS INTEGER
	DATA 1,5, 6,8, 14,7, 21,9, 30,10, 40,6

DEFINT L
SUB CusDisp (x)
	IF Bot > 0 AND Top > 0 THEN
	   LOCATE 2: COLOR 0, 3
	   FOR x = Bot TO Top
	   GET #4, x, Cnx
	   LOCATE , 2: PRINT USING "&³&"; Cnx.Cod; Cnx.Nam
	   IF CSRLIN = 49 THEN EXIT FOR
	   NEXT
	END IF
END SUB

SUB CusEdit (n)
	CusShow
	p$ = Cus.Nam
611 EntryS 2, 52, 28, e, b, Cus.Nam:    IF e THEN 650 ELSE IF b THEN 611
	IF Cus.Nam < "!" THEN WinMssg "Name must not be blank": GOTO 611
612 EntryS 6, 52, 28, e, b, Cus.Man:    IF e THEN 650 ELSE IF b THEN 611
613 EntryT 7, 52, e, b, Cus.Tel:        IF e THEN 650 ELSE IF b THEN 612
614 EntryT 8, 52, e, b, Cus.Fax:        IF e THEN 650 ELSE IF b THEN 613
615 EntryS 9, 52, 28, e, b, Cus.Add:    IF e THEN 650 ELSE IF b THEN 614
616 EntryS 10, 52, 28, e, b, Cus.Str:   IF e THEN 650 ELSE IF b THEN 615
617 EntryS 11, 52, 14, e, b, Cus.Cty:   IF e THEN 650 ELSE IF b THEN 616
618 EntryU 11, 67, 2, e, b, Cus.Sta:    IF e THEN 650 ELSE IF b THEN 617
619 EntryU 11, 70, 10, e, b, Cus.Zip:   IF e THEN 650 ELSE IF b THEN 618

620 EntryS 15, 52, 28, e, b, Cus.Sco:   IF e THEN 650 ELSE IF b THEN 619
621 EntryT 16, 52, e, b, Cus.Ste:       IF e THEN 650 ELSE IF b THEN 620
622 EntryS 17, 52, 28, e, b, Cus.Sad:   IF e THEN 650 ELSE IF b THEN 621
623 EntryS 18, 52, 28, e, b, Cus.SSr:   IF e THEN 650 ELSE IF b THEN 622
624 EntryS 19, 52, 14, e, b, Cus.SCt:   IF e THEN 650 ELSE IF b THEN 623
625 EntryU 19, 67, 2, e, b, Cus.SSt:    IF e THEN 650 ELSE IF b THEN 624
626 EntryU 19, 70, 10, e, b, Cus.SZi:   IF e THEN 650 ELSE IF b THEN 625

627 EntryU 23, 52, 1, e, b, Cus.Typ:    IF e THEN 650 ELSE IF b THEN 626
628 EntryS 24, 52, 28, e, b, Cus.Acc:   IF e THEN 650 ELSE IF b THEN 627
629 EntryT 25, 52, e, b, Cus.Aph:       IF e THEN 650 ELSE IF b THEN 628
630 EntryT 26, 52, e, b, Cus.Afx:       IF e THEN 650 ELSE IF b THEN 629
631 EntryN 28, 52, 2, 0, e, b, Cus.Day: IF e THEN 650 ELSE IF b THEN 630
632 EntryS 28, 60, 20, e, b, Cus.Ter:   IF e THEN 650 ELSE IF b THEN 631

	IF Confirm THEN
	   Cus.Num = RIGHT$(STR$(1000 + Cnx.Num), 3)
	   Cnx.Cod = Cus.Num
	   Cnx.Nam = Cus.Nam
	   PUT #4, n, Cnx
	   PUT #3, Cnx.Num, Cus
	   IF p$ <> Cus.Nam THEN CusSort
	   Top = LOF(3) \ LEN(Cus)
	   CusDisp x
	ELSE
	   GOTO 611
	END IF
650
END SUB

DEFSNG A-Z
SUB CusHdng ()
	Heading
	LPRINT TAB(69); "CUSTOMER MASTER LISTING"
	LPRINT
	LPRINT
	LPRINT "Code       Customer Name            Billing Address                        Shipping Address                       Account Information"
	LPRINT "====  ============================  ====================================== ====================================== ======================================"
END SUB

DEFINT A-Z
SUB CusInit ()
	FOR y = 1 TO 6
	FOR x = 1 TO 2
	READ Menu(y, x)
	NEXT
	NEXT
END SUB

DEFLNG L
DEFSNG A-K, M-Q, S-Z
SUB CusLine (r, Lm, Ly)
	LOCATE r, 52: PRINT USING "########,.##-"; Lm / 100;
	LOCATE r, 67: PRINT USING "########,.##-"; Ly / 100;
END SUB

DEFINT A-Q, S-Z
SUB CusLite (m, p)
	IF p THEN f = 4: b = 7 ELSE f = 14: b = 1
	LOCATE 50, Menu(m, 1) + 35
	f$ = MID$(" Add  Modify  Erase  Display  Invoices  List ", Menu(m, 1), Menu(m, 2))
	COLOR f, b: PRINT MID$(f$, 1, 2);
	COLOR f - 3, b: PRINT MID$(f$, 3);
END SUB

DEFSNG A-Z
SUB CusShow ()
	COLOR 10, 0
	GET #3, Cnx.Num, Cus
	LOCATE 2, 48:  PRINT Cus.Num
	LOCATE 2, 52:  PRINT Cus.Nam
	LOCATE 6, 52:  PRINT Cus.Man
	LOCATE 7, 52:  PRINT Cus.Tel
	LOCATE 8, 52:  PRINT Cus.Fax
	LOCATE 9, 52:  PRINT Cus.Add
	LOCATE 10, 52: PRINT Cus.Str
	LOCATE 11, 52: PRINT Cus.Cty; " "; Cus.Sta; " "; Cus.Zip
   
	LOCATE 15, 52: PRINT Cus.Sco
	LOCATE 16, 52: PRINT Cus.Ste
	LOCATE 17, 52: PRINT Cus.Sad
	LOCATE 18, 52: PRINT Cus.SSr
	LOCATE 19, 52: PRINT Cus.SCt; " "; Cus.SSt; " "; Cus.SZi
   
	LOCATE 23, 52: PRINT Cus.Typ
	LOCATE 24, 52: PRINT Cus.Acc
	LOCATE 25, 52: PRINT Cus.Aph
	LOCATE 26, 52: PRINT Cus.Afx
	
	LOCATE 28, 52: PRINT Cus.Day; " Days "; Cus.Ter
	
	CusLine 31, Cus.Mgr, Cus.Ygr
	CusLine 32, Cus.Mcr, Cus.Ycr
	CusLine 33, Cus.Mtx, Cus.Ytx
	CusLine 34, Cus.Mfr, Cus.Yfr
	CusLine 35, Cus.Moc, Cus.Yoc
	
	CusLine 37, Cus.Mgr - Cus.Mcr + Cus.Mtx + Cus.Mfr + Cus.Moc, Cus.Ygr - Cus.Ycr + Cus.Ytx + Cus.Yfr + Cus.Yoc
	CusLine 38, Cus.Mbb, Cus.Ybb
	
	CusLine 40, Cus.Mbb + Cus.Mgr - Cus.Mcr + Cus.Mtx + Cus.Mfr + Cus.Moc, Cus.Ybb + Cus.Ygr - Cus.Ycr + Cus.Ytx + Cus.Yfr + Cus.Yoc
	CusLine 41, Cus.Mpd, Cus.Ypd
	CusLine 43, Cus.Mbb + Cus.Mgr - Cus.Mcr + Cus.Mtx + Cus.Mfr + Cus.Moc - Cus.Mpd, Cus.Ybb + Cus.Ygr - Cus.Ycr + Cus.Ytx + Cus.Yfr + Cus.Yoc - Cus.Ypd
	COLOR 10
	LOCATE 47, 43: PRINT USING "########.##"; (Cus.Mgr - Cus.Mcr) / 100
	LOCATE 48, 43: PRINT USING "########.##"; (Cus.Ygr - Cus.Ycr) / 100
	LOCATE 47, 56: PRINT USING "########.##"; Cus.Mco / 100
	LOCATE 48, 56: PRINT USING "########.##"; Cus.Yco / 100
	LOCATE 47, 69: PRINT USING "########.##"; (Cus.Mgr - Cus.Mcr - Cus.Mco) / 100
	LOCATE 48, 69: PRINT USING "########.##"; (Cus.Ygr - Cus.Ycr - Cus.Yco) / 100
END SUB

SUB CusSort ()
	CLOSE
	SHELL "SortF ..\AR\Customer.Inx ..\AR\Customer /L33 /+4,28 /Q"
	KILL "..\AR\Customer.INX"
	NAME "..\AR\Customer" AS "..\AR\Customer.INX"
	OpenFiles
END SUB

DEFINT A-K, M-Z
DEFLNG L
SUB Customer ()
	CALL MhOpenScreenLib("..\AR\ARS.SLB" + CHR$(0), e)
	CALL MhShadowOn(0, 8)
	CALL MhBlinkOff
	CALL MhDisplayByName(0, 0, "Customer", e)
	
	Bot = 1: Top = LOF(4) \ LEN(Cnx): CusDisp x
	r = 2: n = 1: m = 4: CusLite m, 1
600 IF n > 0 THEN GET #4, n, Cnx
	COLOR 11, 1: LOCATE r, 2: PRINT USING "&³&"; Cnx.Cod; Cnx.Nam
	KeyHit n$
	COLOR 0, 3:  LOCATE r, 2: PRINT USING "&³&"; Cnx.Cod; Cnx.Nam
	IF n$ = CHR$(13) THEN n$ = MID$(" Add  Modify  Erase  Display  Invoices  List ", 1 + Menu(m, 1), 1)
	SELECT CASE n$
	CASE CHR$(27) 'Esc
	EXIT SUB
	CASE CHR$(0) + "M" 'right
	CusLite m, 0
	m = m + 1: IF m > 6 THEN m = 1
	CusLite m, 1
	CASE CHR$(0) + "K" 'left
	CusLite m, 0
	m = m - 1: IF m < 1 THEN m = 6
	CusLite m, 1
	CASE CHR$(0) + "P" 'Dn
	IF n + 1 > Top THEN 600 ELSE n = n + 1
	IF r + 1 > 48 THEN CALL scroll(2, 2, 33, 48, 1) ELSE r = r + 1
	CASE CHR$(0) + "H" 'Up
	IF n - 1 < 1 THEN 600 ELSE n = n - 1
	IF r - 1 < 2 THEN CALL BkScroll(2, 2, 33, 48, 1) ELSE r = r - 1
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
	CusLite m, 0
	m = 6: CusLite m, 1
	IF Confirm THEN
	   PageNum = 0: CusHdng
	   FOR y = 1 TO LOF(4) \ LEN(Cnx)
	   IF INKEY$ = CHR$(27) THEN EXIT FOR
	   GET #4, y, Cnx
	   GET #3, Cnx.Num, Cus
	   IF Cus.Nam > "!" THEN
		  LineNum = LineNum + 6: IF LineNum > 60 THEN CusHdng
		  LPRINT LEFT$(Cus.Nam, 1) + Cus.Num; "  "; Cus.Nam;
		  LPRINT TAB(37); Cus.Man; TAB(76); Cus.Sco; TAB(116); Cus.Ter
		  LPRINT TAB(37); Cus.Tel; TAB(76); Cus.Ste; TAB(116); Cus.Acc
		  LPRINT TAB(37); Cus.Add; TAB(76); Cus.Sad; TAB(116); Cus.Aph
		  LPRINT TAB(37); Cus.Str; TAB(76); Cus.SSr; TAB(116); Cus.Afx
		  LPRINT TAB(37); Cus.Cty; Cus.Sta; " "; Cus.Zip; TAB(76); Cus.SCt; " "; Cus.SSt; " "; Cus.SZi
		  LPRINT
	   END IF
	   NEXT
	END IF
	CASE "E"
	CusLite m, 0
	m = 3: CusLite m, 1
	CusShow
	L1 = Cus.Ybb + Cus.Ygr + Cus.Ycr + Cus.Ypd
	IF L1 <> 0 THEN
	   WinMssg "Active customer, no erase"
	ELSE
	   IF Confirm THEN
		  Cnx.Cod = "þþþ"
		  Cnx.Nam = "þþþþþþþþþþþþþþþþþþþþþþþþþþþþ"
		  PUT #4, n, Cnx
		  PUT #3, Cnx.Num
		  CusShow
		  CusSort
		  CusDisp x
	   END IF
	END IF
	CASE "D"
	CusLite m, 0
	m = 4: CusLite m, 1
	CusShow
	CASE "A"
	CusLite m, 0
	m = 1: CusLite m, 1
	y = 0
	FOR z = 1 TO Top
	GET #4, z, Cnx
	IF LEFT$(Cnx.Nam, 1) = "þ" THEN y = z: EXIT FOR
	NEXT
	IF y = 0 THEN y = 1 + LOF(3) \ LEN(Cus): Cnx.Num = y
	IF y > 999 THEN WinMssg "Customer Master is full" ELSE CusEdit y
	CASE "M"
	CusLite m, 0
	m = 2: CusLite m, 1
	CusEdit n
	CASE "I"
	CusLite m, 0
	m = 5: CusLite m, 1
	CusShow
	CusOrdr Cus.Num, b, e
	IF b THEN WinOrdr b, e ELSE WinMssg "No ORDERS for this customer"
	CASE ELSE
	WinMssg "Select from highlited keys"
	END SELECT
	GOTO 600
END SUB

DEFINT L
SUB OrdShow ()
	PCOPY 0, 2: CALL MhDisplayByName(0, 0, "ShowOrder", e)
	GET #5, Onx.Num, Ord
	GET #3, VAL(Ord.Cus), Cus
	COLOR 30, 8
	LOCATE 15, 16: PRINT Ord.Num;
	LOCATE 16, 16: PRINT Ord.Dat;
	LOCATE 17, 16: PRINT Ord.Dsh;
	LOCATE 18, 16: PRINT Ord.Ddl;
	
	LOCATE 15, 27: PRINT Ord.Inv;
	LOCATE 16, 27: PRINT Ord.Din;
	LOCATE 18, 27: PRINT Ord.Dlv;

	LOCATE 15, 49: PRINT Ord.Due;
	LOCATE 16, 49: PRINT Ord.Dpd;
	LOCATE 17, 49: PRINT Ord.Chk;
	LOCATE 18, 51: PRINT Ord.Pmt;
	
	LOCATE 15, 67: PRINT Ord.Grs;
	LOCATE 16, 67: PRINT Ord.Crd;
	LOCATE 18, 67: PRINT Ord.Tax;
	LOCATE 19, 67: PRINT Ord.Frt;
	LOCATE 20, 67: PRINT Ord.Chg;
	LOCATE 22, 67: PRINT Ord.Net;
	
	LOCATE 20, 2:  PRINT LEFT$(Ord.In1, 57);
	LOCATE 21, 2:  PRINT LEFT$(Ord.In2, 57);
	LOCATE 22, 2:  PRINT LEFT$(Ord.In3, 57);
	
	FOR x = 1 TO 25
	LOCATE x + 23
	IF Ord.Itm(x) > "  0" THEN
	   GET #1, VAL(Ord.Itm(x)), Pro
	   LOCATE , 2:  PRINT Pro.Nam;
	   LOCATE , 35: PRINT Ord.Qty(x);
	   LOCATE , 43: PRINT Ord.Qts(x);
	   LOCATE , 51: PRINT Ord.Wts(x);
	   LOCATE , 67: PRINT Ord.Prc(x);
	END IF
	NEXT
	KeyHit f$
	PCOPY 2, 0
END SUB

SUB WinOrdr (f, T)
	PCOPY 0, 1: CALL MhDisplayByName(0, 0, "InvWindow", e)
	b = f: r = 6: n = b: GOSUB W2
W1: GET #7, n, Onx: GET #5, Onx.Num, Ord
	COLOR 0, 7: LOCATE r, 15: PRINT USING "&³&³&³&"; Ord.Num; Ord.Dat; Ord.Inv; Ord.Din;
	KeyHit n$
	COLOR 12, 1: LOCATE r, 15: PRINT USING "&³&³&³&"; Ord.Num; Ord.Dat; Ord.Inv; Ord.Din;
	SELECT CASE n$
	CASE CHR$(13)
	OrdShow
	CASE CHR$(0) + "P" 'Dn
	IF n + 1 > T GOTO W1 ELSE n = n + 1
	IF r + 1 > 21 THEN CALL scroll(15, 6, 49, 21, 1) ELSE r = r + 1
	CASE CHR$(0) + "H" 'Up
	IF n - 1 < f GOTO W1 ELSE n = n - 1
	IF r - 1 < 6 THEN CALL BkScroll(15, 6, 49, 21, 1) ELSE r = r - 1
	CASE CHR$(0) + "I" 'PgUp
	b = n - r - 10: IF b < f THEN b = f
	GOSUB W2: n = b + r - 6
	CASE CHR$(0) + "Q" 'PgDn
	b = 22 - r + n
	IF T - 15 > f THEN
	   IF b > T - 15 THEN b = T - 15: n = T - 21 + r ELSE n = n + 15
	   GOSUB W2
	END IF
	CASE CHR$(0) + "G" 'Home
	b = f: GOSUB W2: n = f + r - 6
	CASE CHR$(0) + "O" 'End
	b = T - 15: IF b < f THEN b = f
	GOSUB W2: n = b + r - 6
	CASE ELSE
	PCOPY 1, 0
	EXIT SUB
	END SELECT
	GOTO W1

W2: COLOR 12, 1: LOCATE 6
	FOR x = b TO T
	GET #7, x, Onx
	GET #5, Onx.Num, Ord
	LOCATE , 15: PRINT USING "&³&³&³&"; Ord.Num; Ord.Dat; Ord.Inv; Ord.Din
	IF CSRLIN > 21 THEN EXIT FOR
	NEXT
	RETURN
END SUB


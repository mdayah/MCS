DEFINT A-Z: DEFLNG L
DECLARE FUNCTION Confirm ()
DECLARE FUNCTION WinProd ()
DECLARE FUNCTION WinCust ()
DECLARE FUNCTION OrdFind (f$)
DECLARE FUNCTION InvCalc (x)
DECLARE FUNCTION WtTotal (x, s)
DECLARE FUNCTION CoTotal (x, s)
DECLARE FUNCTION PrTotal (x, s)
DECLARE FUNCTION GetDate (f$, T$)

DECLARE SUB CusOrdr (n$, f, e)
DECLARE SUB OpenFiles ()
DECLARE SUB HitKey (f$)
DECLARE SUB KeyHit (f$)
DECLARE SUB TimeDate ()
DECLARE SUB WinMssg (f$)

DECLARE SUB Heading ()
DECLARE SUB HAging ()
DECLARE SUB HRctReg ()
DECLARE SUB HOpnInv ()
DECLARE SUB HCusYer ()
DECLARE SUB HCusMon ()
DECLARE SUB HProMon ()
DECLARE SUB HProYer ()
DECLARE SUB HSales (f$, T$)
DECLARE SUB HInvReg (f$, T$)

DECLARE SUB ProPrnt (L1, L2)
DECLARE SUB ProLine (r, L1, L2, L3, L4)
DECLARE SUB OnHand (T#)
DECLARE SUB InvDisp (w)
DECLARE SUB InvRegs (T)
DECLARE SUB RctRegs (T)
DECLARE SUB SalRegs (T)
DECLARE SUB InvPurg ()
DECLARE SUB InvCust ()
DECLARE SUB InvForm ()
DECLARE SUB InvOpen ()
DECLARE SUB CusYear ()
DECLARE SUB CusMont ()
DECLARE SUB ChkCust ()
DECLARE SUB ProMont ()
DECLARE SUB ProYear ()
DECLARE SUB OrdTotl ()
DECLARE SUB Aging ()
DECLARE SUB UnitPrc (x)

DECLARE SUB CusInit ()
DECLARE SUB ProInit ()
DECLARE SUB Customer ()
DECLARE SUB Products ()

'$INCLUDE: '..\BI\Library.DEC'
'$INCLUDE: '..\BI\Accounts.REC'
'$INCLUDE: '..\BI\Receipts.REC'
'$INCLUDE: '..\BI\Products.REC'
'$INCLUDE: '..\BI\ProIndex.REC'
'$INCLUDE: '..\BI\Material.REC'
'$INCLUDE: '..\BI\MatIndex.REC'
'$INCLUDE: '..\BI\Recipes.REC'
'$INCLUDE: '..\BI\Customer.REC'
'$INCLUDE: '..\BI\CusIndex.REC'
'$INCLUDE: '..\BI\Orders.REC'
'$INCLUDE: '..\BI\OrdIndex.REC'
'$INCLUDE: '..\BI\SalIndex.REC'

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
	COMMON SHARED Acc AS Accounts
	COMMON SHARED Rct AS Receipts
	COMMON SHARED Sal AS SalIndex
   'common variables
	DIM SHARED Lastrct AS INTEGER
	DIM SHARED EOMrct  AS INTEGER
	DIM SHARED EOYrct  AS INTEGER
	DIM SHARED Sort    AS INTEGER
	DIM SHARED Top     AS INTEGER
	DIM SHARED Bot     AS INTEGER

   'array variables
	DIM SHARED Grs$                  'old gross
	DIM SHARED Crd$                  'old credit
	DIM SHARED Tax$                  'old tax
	DIM SHARED Frt$                  'old frieght
	DIM SHARED Chg$                  'old charge
	DIM SHARED Net$                  'old net
	
	DIM SHARED Cst$                  'new cost
	DIM SHARED Amt$                  'new sale
	DIM SHARED oCs$                  'old cost
	DIM SHARED oAm$                  'old sale

	DIM Ini(20) AS INTEGER           'order index num
	DIM Due(20) AS STRING * 8
	
	DIM SHARED Old(25) AS STRING * 7 'previous qty
	DIM SHARED oQu(26) AS LONG       'old quantity
	DIM SHARED oWt(26) AS LONG       'old weight
	DIM SHARED oCo(26) AS LONG       'old cost
	DIM SHARED oPr(26) AS LONG       'old price
	
	DIM SHARED Lw, Lc, Lp AS LONG

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

	ON ERROR GOTO Errors

   'screen initialize
	WIDTH 80, 50: SCREEN 0, 0, 1, 1
	PCOPY 0, 1:   SCREEN 0, 0, 0, 0
	CALL MhShadowOn(0, 8)
	CALL MhBlinkOff

   'load orders numbers
	OPEN "..\AR\Receipts.NUM" FOR INPUT AS #1
	INPUT #1, Lastrct, EOMrct, EOYrct
	CLOSE

   'initialize products & customer modules
	CALL ProInit
	CALL CusInit

Menu: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
1   CALL MhOpenScreenLib("..\AR\ARS.SLB" + CHR$(0), e)
2   CALL MhDisplayByName(0, 0, "AR-Menu", e)
	CLOSE
	HitKey f$
	IF f$ = CHR$(27) GOTO Terminate
	IF f$ = "0" THEN f$ = "10"
	OpenFiles
	ON VAL(f$) GOTO 10, 20, 30, 40, 2, 2, Products, Customer, sMonth, sYear
	GOTO 1

10  CALL MhDisplayByName(0, 0, "Inv-Menu", e)
	HitKey f$
	IF f$ = CHR$(27) GOTO 2
	IF f$ = "0" THEN f$ = "10"
	ON VAL(f$) GOTO InvEnt, InvNew, InvAll, InvOne, InvPurge, InvChk
	GOTO 10

20  CALL MhDisplayByName(0, 0, "Rct-Menu", e)
	HitKey f$
	IF f$ = CHR$(27) GOTO 2
	IF f$ = "0" THEN f$ = "10"
	ON VAL(f$) GOTO RctEnt, RctCan, RctNew
	GOTO 20

30  CALL MhDisplayByName(0, 0, "Sal-Menu", e)
	HitKey f$
	IF f$ = CHR$(27) GOTO 2
	IF f$ = "0" THEN f$ = "10"
	ON VAL(f$) GOTO SalDet, SalSum
	GOTO 30

40  CALL MhDisplayByName(0, 0, "Rpt-Menu", e)
	HitKey f$
	IF f$ = CHR$(27) GOTO 2
	IF f$ = "0" THEN f$ = "10"
	ON VAL(f$) GOTO Aging, InvOpn, InvReg, RctReg, ProMont, CusMont, ProYear, CusYear
	GOTO 40
InvEnt: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	CALL MhDisplayByName(0, 0, "Invoice", e)
100 EntryU 4, 6, 6, e, b, Ord.Num: IF e THEN 10 ELSE IF b THEN 100
	IF OrdFind(Ord.Num) = 0 THEN WinMssg "Sales Order not found": GOTO 100
	GET #5, Onx.Num, Ord
	IF Ord.Inv > "      " THEN Edit = 0 ELSE Edit = 1
	InvDisp Edit
104 EntryD 4, 28, e, b, Ord.Din:        IF e THEN 10 ELSE IF b THEN 100
106 EntryD 4, 39, e, b, Ord.Dlv:        IF e THEN 10 ELSE IF b THEN 104
108 EntryS 4, 50, 30, e, b, Ord.Car:    IF e THEN 10 ELSE IF b THEN 106
110 EntryD 13, 46, e, b, Ord.Due:       IF e THEN 10 ELSE IF b THEN 108
	IF Ord.Due < "0" THEN Ord.Due = DayMath$(Ord.Din, VAL(Cus.Day)): GOTO 110
112 EntryN 7, 72, 8, 2, e, b, Ord.Grs:  IF e THEN 10 ELSE IF b THEN 110
	OrdTotl
114 EntryN 8, 72, 8, 2, e, b, Ord.Crd:  IF e THEN 10 ELSE IF b THEN 112
	OrdTotl
116 EntryN 12, 72, 8, 2, e, b, Ord.Tax: IF e THEN 10 ELSE IF b THEN 114
	OrdTotl
118 EntryN 13, 72, 8, 2, e, b, Ord.Frt: IF e THEN 10 ELSE IF b THEN 116
	OrdTotl
120 EntryN 14, 72, 8, 2, e, b, Ord.Chg: IF e THEN 10 ELSE IF b THEN 118
	OrdTotl
	x = 1
122 IF Ord.Itm(x) > "  0" THEN
	   GET #1, VAL(Ord.Itm(x)), Pro
	   GOTO 128
124    EntryN x + 21, 38, 7, 2, e, b, Ord.Qts(x): IF e THEN 10
	   IF b THEN x = x - 1: IF x < 1 THEN 120 ELSE 122
	   IF Ord.Qts(x) <> Old(x) THEN
	  IF InvCalc(x) THEN 124
	  Old(x) = Ord.Qts(x)
	   END IF
126    IF WtTotal(x, 0) THEN ERROR 200
	   EntryN x + 21, 46, 8, 2, e, b, Ord.Wts(x): IF e THEN 10
	   IF WtTotal(x, 1) THEN 126
	   IF b THEN 124
128    IF PrTotal(x, 0) THEN ERROR 200
	   EntryN x + 21, 72, 8, 2, e, b, Ord.Prc(x): IF e THEN 10
	   UnitPrc x
	   IF PrTotal(x, 1) THEN 128
	   IF b THEN 126
	   x = x + 1: IF x < 26 THEN 122
	END IF
	Li = 100 * (VAL(Ord.Grs) - VAL(Ord.Crd))
	IF Lp = Li THEN IF Confirm THEN 130
	x = x - 1: IF x < 1 THEN x = 1
	GOTO 128

130 Picture 16, 72, 8, Ord.Net
	Picture 10, 72, 8, Amt$
	Picture 48, 55, 8, Cst$

	IF Edit THEN Ord.Inv = " Print"
	PUT #5, Onx.Num, Ord '--------------------------save order---------------
	FOR x = 1 TO 25
	IF VAL(Ord.Itm(x)) > 0 THEN
	   GET #1, VAL(Ord.Itm(x)), Pro
	   Pro.Msq = Pro.Msq - oQu(x) + (VAL(Ord.Qts(x)) * VAL(Pro.Fra) * 100)
	   Pro.Ysq = Pro.Ysq - oQu(x) + (VAL(Ord.Qts(x)) * VAL(Pro.Fra) * 100)
	   Pro.Msw = Pro.Msw - oWt(x) + VAL(Ord.Wts(x)) * 100
	   Pro.Ysw = Pro.Ysw - oWt(x) + VAL(Ord.Wts(x)) * 100
	   Pro.Msc = Pro.Msc - oCo(x) + VAL(Ord.Cst(x)) * 100
	   Pro.Ysc = Pro.Ysc - oCo(x) + VAL(Ord.Cst(x)) * 100
	   Pro.Msp = Pro.Msp - oPr(x) + VAL(Ord.Prc(x)) * 100
	   Pro.Ysp = Pro.Ysp - oPr(x) + VAL(Ord.Prc(x)) * 100
	   PUT #1, VAL(Ord.Itm(x)), Pro '---------------update products-----------
	ELSE EXIT FOR
	END IF
	NEXT

	   Cus.Mgr = Cus.Mgr + VAL(Ord.Grs) * 100: Cus.Ygr = Cus.Ygr + VAL(Ord.Grs) * 100
	   Cus.Mcr = Cus.Mcr + VAL(Ord.Crd) * 100: Cus.Ycr = Cus.Ycr + VAL(Ord.Crd) * 100
	   Cus.Mtx = Cus.Mtx + VAL(Ord.Tax) * 100: Cus.Ytx = Cus.Ytx + VAL(Ord.Tax) * 100
	   Cus.Mfr = Cus.Mfr + VAL(Ord.Frt) * 100: Cus.Yfr = Cus.Yfr + VAL(Ord.Frt) * 100
	   Cus.Moc = Cus.Moc + VAL(Ord.Chg) * 100: Cus.Yoc = Cus.Yoc + VAL(Ord.Chg) * 100
	   Cus.Mnt = Cus.Mnt + VAL(Ord.Net) * 100: Cus.Ynt = Cus.Ynt + VAL(Ord.Net) * 100
	   Cus.Mco = Cus.Mco + VAL(Cst$) * 100:    Cus.Yco = Cus.Yco + VAL(Cst$) * 100
	   PUT #3, VAL(Ord.Cus), Cus '-----------------update customer-----------
	   GET #8, 10, Acc
	   Acc.Mdb = Acc.Mdb + VAL(Ord.Net) * 100
	   Acc.Ydb = Acc.Ydb + VAL(Ord.Net) * 100
	   PUT #8, 10, Acc '---------------------------debit receivable----------
	   GET #8, 31, Acc
	   Acc.Mdb = Acc.Mdb + VAL(Cst$) * 100
	   Acc.Ydb = Acc.Ydb + VAL(Cst$) * 100
	   PUT #8, 31, Acc '---------------------------debit cost of goods-------
	   GET #8, 33, Acc
	   Acc.Mcr = Acc.Mcr + VAL(Cst$) * 100
	   Acc.Ycr = Acc.Ycr + VAL(Cst$) * 100
	   PUT #8, 33, Acc '---------------------------credit inventory----------
	   GET #8, 60, Acc
	   Acc.Mcr = Acc.Mcr + VAL(Amt$) * 100
	   Acc.Ycr = Acc.Ycr + VAL(Amt$) * 100
	   PUT #8, 60, Acc '---------------------------credit sales revenue------
	   GET #8, 53, Acc
	   Acc.Mcr = Acc.Mcr + VAL(Ord.Frt) * 100
	   Acc.Ycr = Acc.Ycr + VAL(Ord.Frt) * 100
	   PUT #8, 53, Acc '---------------------------credit frieght-out--------
	   GET #8, 61, Acc
	   Acc.Mcr = Acc.Mcr + VAL(Ord.Chg) * 100
	   Acc.Ycr = Acc.Ycr + VAL(Ord.Chg) * 100
	   PUT #8, 61, Acc '---------------------------credit other revenues-----
	   GET #8, 24, Acc
	   Acc.Mcr = Acc.Mcr + VAL(Ord.Tax) * 100
	   Acc.Ycr = Acc.Ycr + VAL(Ord.Tax) * 100
	   PUT #8, 24, Acc '---------------------------credit Sales Tax Payable--

	IF Edit = 0 THEN
	   GET #3, VAL(Ord.Cus), Cus
	   Cus.Mgr = Cus.Mgr - VAL(Grs$) * 100: Cus.Ygr = Cus.Ygr - VAL(Grs$) * 100
	   Cus.Mcr = Cus.Mcr - VAL(Crd$) * 100: Cus.Ycr = Cus.Ycr - VAL(Crd$) * 100
	   Cus.Mtx = Cus.Mtx - VAL(Tax$) * 100: Cus.Ytx = Cus.Ytx - VAL(Tax$) * 100
	   Cus.Mfr = Cus.Mfr - VAL(Frt$) * 100: Cus.Yfr = Cus.Yfr - VAL(Frt$) * 100
	   Cus.Moc = Cus.Moc - VAL(Chg$) * 100: Cus.Yoc = Cus.Yoc - VAL(Chg$) * 100
	   Cus.Mnt = Cus.Mnt - VAL(Net$) * 100: Cus.Ynt = Cus.Ynt - VAL(Net$) * 100
	   Cus.Mco = Cus.Mco - VAL(oCs$) * 100: Cus.Yco = Cus.Yco - VAL(oCs$) * 100
	   PUT #3, VAL(Ord.Cus), Cus '--------------update customer-----------
	   GET #8, 10, Acc
	   Acc.Mdb = Acc.Mdb - VAL(Net$) * 100
	   Acc.Ydb = Acc.Ydb - VAL(Net$) * 100
	   PUT #8, 10, Acc'-------------------------debit receivable----------
	   GET #8, 31, Acc
	   Acc.Mdb = Acc.Mdb - VAL(oCs$) * 100
	   Acc.Ydb = Acc.Ydb - VAL(oCs$) * 100
	   PUT #8, 31, Acc '------------------------debit cost of goods-------
	   GET #8, 33, Acc
	   Acc.Mcr = Acc.Mcr - VAL(oCs$) * 100
	   Acc.Ycr = Acc.Ycr - VAL(oCs$) * 100
	   PUT #8, 33, Acc'-------------------------credit Inventory----------
	   GET #8, 60, Acc
	   Acc.Mcr = Acc.Mcr - VAL(oAm$) * 100
	   Acc.Ycr = Acc.Ycr - VAL(oAm$) * 100
	   PUT #8, 60, Acc '------------------------credit sales revenue------
	   GET #8, 53, Acc
	   Acc.Mcr = Acc.Mcr - VAL(Frt$) * 100
	   Acc.Ycr = Acc.Ycr - VAL(Frt$) * 100
	   PUT #8, 53, Acc '------------------------credit freight - out------
	   GET #8, 61, Acc
	   Acc.Mcr = Acc.Mcr - VAL(Chg$) * 100
	   Acc.Ycr = Acc.Ycr - VAL(Chg$) * 100
	   PUT #8, 61, Acc '------------------------credit other revenues-----
	   GET #8, 24, Acc
	   Acc.Mcr = Acc.Mcr - VAL(Tax$) * 100
	   Acc.Ycr = Acc.Ycr - VAL(Tax$) * 100
	   PUT #8, 24, Acc '------------------------credit Sales Tax Payable--
	END IF
	GOTO InvEnt
RctEnt: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	CALL MhDisplayByName(0, 0, "Receipts", e)
300 EntryU 5, 30, 3, e, b, Rct.Cus: IF e THEN 20 ELSE IF b THEN 300
	IF Rct.Cus < "  1" THEN IF WinCust THEN 300 ELSE Rct.Cus = Cnx.Cod
	GET #3, VAL(Rct.Cus), Cus
	COLOR 14, 0
	LOCATE 5, 30:  PRINT Rct.Cus;
	LOCATE 7, 14:  PRINT Cus.Nam;
	LOCATE 8, 14:  PRINT Cus.Man;
	LOCATE 9, 14:  PRINT Cus.Add;
	LOCATE 10, 14: PRINT Cus.Str;
	LOCATE 11, 14: PRINT Cus.Cty; Cus.Sta; " "; Cus.Zip;
	COLOR 15
	LOCATE 5, 58:  PRINT USING "#######.##"; Cus.Ybb / 100;
	LOCATE 6, 58:  PRINT USING "#######.##"; Cus.Ynt / 100;
	LOCATE 8, 58:  PRINT USING "#######.##"; (Cus.Ybb + Cus.Ynt) / 100;
	LOCATE 9, 58:  PRINT USING "#######.##"; Cus.Ypd / 100;
	LOCATE 11, 58: PRINT USING "#######.##"; (Cus.Ybb + Cus.Ynt - Cus.Ypd) / 100;
	CusOrdr Cus.Num, b, e
	IF b = 0 THEN WinMssg "No invoices for this customer": GOTO 300
	ERASE Ini, Due, Rct.Inv, Rct.Amt: L1 = 0: L2 = 0: y = 1
	Rct.Dat = "          "
	Rct.Chk = "          "
	Rct.Tot = "          "
	COLOR 7
	FOR x = b TO e
	GET #7, x, Onx
	GET #5, Onx.Num, Ord
	IF Ord.Net <> Ord.Pmt AND Ord.Inv > "     0" THEN
	   IF y > 20 THEN
	  LOCATE 48, 20: PRINT "Next page?"; : BEEP
	  KeyHit f$
	  LOCATE 48, 20: PRINT "          ";
	  IF f$ = "Y" THEN
		 ERASE Ini, Due, Rct.Inv, Rct.Amt: L1 = 0: L2 = 0: y = 1
	  ELSE
		 EXIT FOR
	  END IF
	   END IF
	   LOCATE 21 + y, 18
	   PRINT USING "&ณ&ณ&ณ  #####.##ณ          "; Ord.Inv; Ord.Din; Ord.Due; VAL(Ord.Net) - VAL(Ord.Pmt)
	   Ini(y) = Onx.Num
	   Picture 21 + y, 49, 8, Due(y)
	   Rct.Inv(y) = Ord.Num
	   Rct.Amt(y) = "        "
	   L1 = L1 + 100 * VAL(Due(y))
	   L2 = L2 + 100 * VAL(Rct.Amt(y))
	   y = y + 1
	END IF
	NEXT

	FOR x = y TO 20
	LOCATE 21 + x, 16
	PRINT "        ณ          ณ          ณ          ณ          ";
	NEXT

	LOCATE 43, 47: PRINT "          "; : LOCATE , 58: PRINT "          ";
	LOCATE 45, 31: PRINT "          "; : LOCATE , 58: PRINT "          ";
	LOCATE 46, 31: PRINT "          "; : LOCATE , 58: PRINT "          ";
	IF L1 = 0 THEN WinMssg "No invoices due": GOTO 300
	COLOR 7
	LOCATE 43, 47: PRINT USING "#######.##"; L1 / 100;
	LOCATE 43, 58: PRINT USING "#######.##"; L2 / 100;
	Picture 43, 47, 10, Amt$
310 FOR x = 1 TO 20
	IF Rct.Inv(x) > "     1" THEN
312    L2 = L2 - 100 * VAL(Rct.Amt(x))
	   EntryN x + 21, 60, 8, 2, e, b, Rct.Amt(x): IF e THEN 300
	   L2 = L2 + 100 * VAL(Rct.Amt(x))
	   LOCATE 43, 58: PRINT USING "#######.##"; L2 / 100
	   Picture 43, 58, 10, Pmt$
	   IF b THEN x = x - 1: IF x < 1 THEN 310 ELSE 312
	ELSE
	   EXIT FOR
	END IF
	NEXT
	Rct.Dat = DATE$
314 EntryD 45, 31, e, b, Rct.Dat:        IF e THEN 300 ELSE IF b THEN 310
	IF Rct.Dat < "!" THEN WinMssg "Date please!": GOTO 314
316 EntryS 46, 31, 10, e, b, Rct.Chk:    IF e THEN 300 ELSE IF b THEN 314
	IF Rct.Chk < "!" THEN WinMssg "Check number please!": GOTO 316
318 EntryN 45, 58, 10, 2, e, b, Rct.Tot: IF e THEN 300 ELSE IF b THEN 316
	IF Rct.Tot <> Pmt$ THEN WinMssg "Applied & Check amount are unequal": GOTO 318
	IF Confirm THEN
	   f$ = ""
	   FOR x = 1 TO 20
	   IF Rct.Amt(x) > "    0.00" THEN
	  GET #5, Ini(x), Ord
	  LOCATE 48, 36: PRINT USING "#####.##"; VAL(Ord.Pmt) + VAL(Rct.Amt(x));
	  Ord.Dpd = Rct.Dat
	  Ord.Chk = Rct.Chk
	  Picture 48, 36, 8, Ord.Pmt
	  PUT #5, Ini(x), Ord '----------------------------<INVOICES.DAT>
	   ELSE
	  Rct.Inv(x) = "      "
	  Rct.Amt(x) = "        "
	   END IF
	   NEXT
	   Rct.Cus = Cus.Num
	   PUT #9, 1 + LOF(9) \ LEN(Rct), Rct '----------------<INVOICES.CHK>
	   Cus.Mpd = Cus.Mpd + VAL(Rct.Tot) * 100
	   Cus.Ypd = Cus.Ypd + VAL(Rct.Tot) * 100
	   PUT #3, VAL(Rct.Cus), Cus '-------------------------<CUSTOMER.DTA>
	   GET #8, 1, Acc
	   Acc.Mdb = Acc.Mdb + VAL(Rct.Tot) * 100
	   Acc.Ydb = Acc.Ydb + VAL(Rct.Tot) * 100
	   PUT #8, 1, Acc '------------------------------------<Debit Cash>
	   GET #8, 10, Acc
	   Acc.Mcr = Acc.Mcr + VAL(Rct.Tot) * 100
	   Acc.Ycr = Acc.Ycr + VAL(Rct.Tot) * 100
	   PUT #8, 10, Acc '-----------------------------------<Credit A/R>
	   GOTO 300
	END IF
	GOTO 318
RctCan: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	CALL MhDisplayByName(0, 0, "Cancel", e)
	c$ = "          "
400 EntryS 11, 31, 10, e, b, c$: IF e THEN 20 ELSE IF b THEN 400
	FOR x = 1 TO LOF(9) \ LEN(Rct)
	GET #9, x, Rct
	IF c$ = Rct.Chk THEN
	   GET #3, VAL(Rct.Cus), Cus
	   LOCATE 13, 31: PRINT Rct.Dat;
	   LOCATE 14, 31: PRINT Rct.Tot;
	   LOCATE 19, 30: PRINT Rct.Cus;
	   LOCATE 21, 15: PRINT Cus.Nam;
	   LOCATE 22, 15: PRINT Cus.Man;
	   LOCATE 23, 15: PRINT Cus.Add;
	   LOCATE 24, 15: PRINT Cus.Str;
	   LOCATE 25, 15: PRINT Cus.Cty; " "; Cus.Sta; " "; Cus.Zip;
	   LOCATE 14: COLOR 15, 4
	   FOR y = 1 TO 20                 'check if invoice was purged
	   IF Rct.Inv(y) > "     0" THEN   'terminate if it was
	  IF OrdFind(Rct.Inv(y)) = 0 THEN WinMssg "Sorry invoice was purged": GOTO 30
	  GET #5, Onx.Num, Ord
	  LOCATE , 48: PRINT Ord.Inv;
	  LOCATE , 58: PRINT Rct.Amt(y)
	   END IF
	   NEXT
	   FOR s = y TO 20
	  LOCATE s + 13, 48: PRINT "      ";
	  LOCATE s + 13, 58: PRINT "        "
	   NEXT
	   IF Rct.Tot = "Cancelled:" THEN
	  WinMssg "Check already cancelled": GOTO 400
	   ELSE
	  IF Confirm THEN
		 FOR y = 1 TO 20
		 IF OrdFind(Rct.Inv(y)) THEN
		GET #5, Onx.Num, Ord
		IF Rct.Amt(y) = Ord.Pmt THEN
		   Ord.Dpd = "**-**-****"
		   Ord.Chk = "*Not Paid*"
		   Ord.Pmt = "    0.00"
		ELSE
		   COLOR 0, 0: LOCATE 1, 1
		   PRINT USING "#####.##"; VAL(Ord.Pmt) - VAL(Rct.Amt(y));
		   Picture 1, 1, 8, Ord.Pmt
		END IF
		PUT #5, Onx.Num, Ord '------------------------<Orders  .Mas>
		 END IF
		 NEXT
		 Cus.Mpd = Cus.Mpd - VAL(Rct.Tot) * 100
		 Cus.Ypd = Cus.Ypd - VAL(Rct.Tot) * 100
		 PUT #3, VAL(Rct.Cus), Cus '----------------------<Customer.Mas>
		 GET #8, 1, Acc
		 Acc.Mdb = Acc.Mdb - VAL(Rct.Tot) * 100
		 Acc.Ydb = Acc.Ydb - VAL(Rct.Tot) * 100
		 PUT #8, 1, Acc '---------------------------------<Debit Cash  >
		 GET #8, 10, Acc
		 Acc.Mcr = Acc.Mcr - VAL(Rct.Tot) * 100
		 Acc.Ycr = Acc.Ycr - VAL(Rct.Tot) * 100
		 PUT #8, 10, Acc '--------------------------------<Credit A/R  >
		 Rct.Tot = "Cancelled:"
		 PUT #9, LOC(9), Rct '----------------------------<Receipts.Mas>
		 EXIT FOR
	  END IF
	   END IF
	END IF
	NEXT
	GOTO 400
InvAll:   'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	COLOR 16, 6: LOCATE 29, 36: PRINT " Print All New Sales Invoices "
	COLOR 28, 0: LOCATE 36, 32: PRINT "  Starting invoice number:         "
200 EntryN 36, 59, 6, 0, e, b, Num$: IF e THEN 10 ELSE IF b THEN 200
	Ln = VAL(Num$): IF Ln < 1 THEN 200
	FOR x = 1 TO LOF(5) \ LEN(Ord)
	IF INKEY$ = CHR$(27) THEN EXIT FOR
	GET #5, x, Ord
	IF Ord.Inv = " Print" THEN
	   Ord.Inv = RIGHT$(STR$(1000000 + Ln), 6)
	   PUT #5, x, Ord
	   COLOR 28, 0: LOCATE 36, 32: PRINT USING "  Printing invoice number: &  "; Ord.Inv;
	   InvForm
	   Ln = Ln + 1
	END IF
	NEXT
	GOTO 10
InvOne:   'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	CALL MhDisplayByName(0, 0, "Invoice", e)
210 EntryU 4, 6, 6, e, b, Ord.Num: IF e THEN 10 ELSE IF b THEN 210
	IF OrdFind(Ord.Num) = 0 THEN WinMssg "Sales Order not found": GOTO 210
	GET #5, Onx.Num, Ord
	InvDisp 0
	IF Ord.Inv > "      " THEN
212    EntryU 4, 19, 6, e, b, Ord.Inv: IF e THEN 10 ELSE IF b THEN 210
	   IF Ord.Inv < "     0" THEN WinMssg "New invoice number": GOTO 212
	   IF Confirm THEN PUT #5, Onx.Num, Ord: InvForm
	ELSE
	   WinMssg "Open order, no invoice": GOTO 210
	END IF
	GOTO 210
InvChk:   'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	ChkCust
	GOTO 10
InvNew:   'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	COLOR 16, 6: LOCATE 27, 36: PRINT " Print New Invoices Register "
	CALL InvRegs(0): GOTO 10
InvPurge: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	COLOR 16, 6: LOCATE 33, 36: PRINT " Purge Paid Invoices by Date "
	CALL InvPurg: GOTO 10
RctNew:   'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	COLOR 16, 3: LOCATE 31, 18: PRINT " Print New Receipts     "
	CALL RctRegs(0): Lastrct = LOF(9) \ LEN(Rct): GOTO 20
SalDet:   'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	COLOR 16, 6: LOCATE 29, 40: PRINT " Detail Sales Report  "
	CALL SalRegs(1): GOTO 30
SalSum:   'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	COLOR 16, 6: LOCATE 31, 40: PRINT " Summary Sales Report "
	CALL SalRegs(0): GOTO 30
Aging:    'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	COLOR 16, 6: LOCATE 31, 33: PRINT " Aging Report            "
	CALL Aging: GOTO 40
InvOpn:   'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	COLOR 16, 6: LOCATE 32, 33: PRINT " Open Invoices           "
	CALL InvOpen: GOTO 40
InvReg:   'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	COLOR 16, 6: LOCATE 33, 33: PRINT " Invoices Register       "
	CALL InvRegs(1): GOTO 40
RctReg:   'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	COLOR 16, 6: LOCATE 34, 33: PRINT " Receipts Register       "
	CALL RctRegs(1): GOTO 40
ProMont:  'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	COLOR 16, 6: LOCATE 38, 33: PRINT " Products Master - Month "
	CALL ProMont: GOTO 40
CusMont:  'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	COLOR 16, 6: LOCATE 39, 33: PRINT " Customer Master - Month "
	CALL CusMont: GOTO 40
ProYear:  'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	COLOR 16, 6: LOCATE 40, 33: PRINT " Products Master - Year  "
	CALL ProYear: GOTO 40
CusYear:  'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	COLOR 16, 6: LOCATE 41, 33: PRINT " Customer Master - Year  "
	CALL CusYear: GOTO 40
Products: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	CALL Products: GOTO 1
Customer: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	CALL Customer: GOTO 2
sMonth:  'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	COLOR 7, 4: f$ = ""
	LOCATE 43, 20: PRINT " Type yes if you wish to close the MONTH "
900 EntryU 43, 26, 3, e, b, f$: IF e THEN 2 ELSE IF b THEN 900
	IF f$ = "YES" THEN
	   LOCATE 44, 20: PRINT "  Printing Customers & Products Masters  "
	   CusMont
	   CusYear
	   ProMont
	   ProYear
	   LOCATE 44, 20: PRINT " One moment please.....CLOSING the MONTH "
	   FOR x = 1 TO LOF(3) \ LEN(Cus)
	   GET #3, x, Cus
	   Cus.Mbb = Cus.Mbb + Cus.Mgr - Cus.Mcr + Cus.Mtx + Cus.Mfr + Cus.Moc - Cus.Mpd
	   Cus.Mgr = 0
	   Cus.Mcr = 0
	   Cus.Mtx = 0
	   Cus.Mfr = 0
	   Cus.Moc = 0
	   Cus.Mnt = 0
	   Cus.Mpd = 0
	   Cus.Mco = 0
	   Cus.Mor = 0
	   PUT #3, x, Cus
	   NEXT
	   EOMrct = LOF(9) \ LEN(Rct)
	END IF
	GOTO 2
sYear:   'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออÛ
	COLOR 6, 0:  f$ = ""
	LOCATE 43, 20: PRINT " Type yes if you wish to close the YEAR  "
910 EntryU 43, 26, 3, e, b, f$: IF e THEN 2 ELSE IF b THEN 910
	IF f$ = "YES" THEN
	   LOCATE 44, 20: PRINT "  Printing Customers & Products Masters  "
	   CusMont
	   CusYear
	   ProMont
	   ProYear
	   LOCATE 44, 20: PRINT "One moment please....CLOSING MONTH & YEAR"
	   FOR x = 1 TO LOF(3) \ LEN(Cus)
	   GET #3, x, Cus
	   Cus.Mbb = Cus.Mbb + Cus.Mgr - Cus.Mcr + Cus.Mtx + Cus.Mfr + Cus.Moc - Cus.Mpd
	   Cus.Ybb = Cus.Ybb + Cus.Ygr - Cus.Ycr + Cus.Ytx + Cus.Yfr + Cus.Yoc - Cus.Ypd
	   Cus.Mgr = 0: Cus.Ygr = 0
	   Cus.Mcr = 0: Cus.Ycr = 0
	   Cus.Mtx = 0: Cus.Ytx = 0
	   Cus.Mfr = 0: Cus.Yfr = 0
	   Cus.Moc = 0: Cus.Yoc = 0
	   Cus.Mnt = 0: Cus.Ynt = 0
	   Cus.Mpd = 0: Cus.Ypd = 0
	   Cus.Mco = 0: Cus.Yco = 0
	   Cus.Mor = 0: Cus.Yor = 0
	   PUT #3, x, Cus
	   NEXT
	   EOMrct = LOF(9) \ LEN(Rct)
	   EOYrct = LOF(9) \ LEN(Rct)
	END IF
	GOTO 2
Errors: 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	WinMssg "Error:" + STR$(ERR) + "  Line:" + STR$(ERL)
Terminate: 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	CLOSE : OPEN "..\AR\Receipts.NUM" FOR OUTPUT AS #1
	WRITE #1, Lastrct, EOMrct, EOYrct
	END

DEFINT L
DEFDBL T
SUB Aging ()
	PageNum = 0: HAging
	FOR x = 1 TO LOF(7) \ LEN(Onx)
	IF INKEY$ = CHR$(27) THEN EXIT FOR
	GET #7, x, Onx
	GET #5, Onx.Num, Ord
	IF Ord.Pmt <> Ord.Net THEN
	   IF p$ <> Ord.Cus THEN
		  GOSUB 532
		  GET #3, VAL(Ord.Cus), Cus
		  LPRINT USING "& &"; Cus.Num; Cus.Nam; : p$ = Ord.Cus
	   END IF
	   LPRINT TAB(35); Ord.Due; " "; Ord.Inv;
	   d = DateDif(DATE$, Ord.Due)
	   Tt = VAL(Ord.Net) - VAL(Ord.Pmt)
	   IF d < -28 THEN y = 56: T1 = T1 + Tt:  GOTO 530
	   IF d < -21 THEN y = 66: T2 = T2 + Tt:  GOTO 530
	   IF d < -14 THEN y = 76: T3 = T3 + Tt:  GOTO 530
	   IF d < -7 THEN y = 86:  T4 = T4 + Tt:  GOTO 530
	   IF d < 0 THEN y = 96:   T5 = T5 + Tt:  GOTO 530
	   IF d = 0 THEN y = 106:  T6 = T6 + Tt:  GOTO 530
	   IF d > 28 THEN y = 156: T10 = T10 + Tt: GOTO 530
	   IF d > 21 THEN y = 146: T0 = T0 + Tt:  GOTO 530
	   IF d > 14 THEN y = 136: T9 = T9 + Tt:  GOTO 530
	   IF d > 7 THEN y = 126:  T8 = T8 + Tt:  GOTO 530
	   IF d > 0 THEN y = 116:  T7 = T7 + Tt
530        LPRINT TAB(y); : LPRINT USING "#####.##"; Tt
	   LineNum = LineNum + 1: IF LineNum > 60 THEN HAging
	END IF
	NEXT
	GOSUB 532: LPRINT : LPRINT TAB(55);
	LPRINT USING "######.## "; Ta; Tb; Tc; Td; Te; Tf; Tg; Th; Ti; Tj; Tk
	EXIT SUB
532 LPRINT CHR$(27); "G"
	LPRINT TAB(55);
	LPRINT USING "######.## "; T1; T2; T3; T4; T5; T6; T7; T8; T9; T0; T10;
	LPRINT CHR$(27); "H"
	Ta = Ta + T1: T1 = 0
	Tb = Tb + T2: T2 = 0
	Tc = Tc + T3: T3 = 0
	Td = Td + T4: T4 = 0
	Te = Te + T5: T5 = 0
	Tf = Tf + T6: T6 = 0
	Tg = Tg + T7: T7 = 0
	Th = Th + T8: T8 = 0
	Ti = Ti + T9: T9 = 0
	Tj = Tj + T0: T0 = 0
	Tk = Tk + T10: T10 = 0
	LineNum = LineNum + 3: IF LineNum > 60 THEN HAging
	RETURN
END SUB

DEFINT T
DEFLNG L
SUB ChkCust ()
	CLS
	LOCATE 50, 1: PRINT "One moment please, clearing variables.                                         ";
	FOR x = 1 TO LOF(3) \ LEN(Cus)
	IF INKEY$ = CHR$(27) THEN EXIT SUB
	GET #3, x, Cus
	Cus.Vnt = 0
	Cus.Vpd = 0
	PUT #3, x, Cus
	NEXT
	LOCATE 50, 1: PRINT "One moment please, accumulating totals.                                        ";
	FOR x = 1 TO LOF(5) \ LEN(Ord)
	GET #5, x, Ord
	IF Ord.Inv > "0     " THEN
	   IF INKEY$ = CHR$(27) THEN EXIT SUB
	   GET #3, VAL(Ord.Cus), Cus
	   Cus.Vnt = Cus.Vnt + 100 * VAL(Ord.Net)
	   Cus.Vpd = Cus.Vpd + 100 * VAL(Ord.Pmt)
	   PUT #3, VAL(Ord.Cus), Cus
	END IF
	NEXT
	COLOR 14, 4
	LOCATE 50, 1: PRINT "Num Customer                         Due(Master) Due(Invoices)                  ";
	COLOR 3, 0
	FOR x = 1 TO LOF(3) \ LEN(Cus)
	IF INKEY$ = CHR$(27) THEN EXIT SUB
	GET #3, x, Cus
	Ldue = Cus.Ybb + Cus.Ygr - Cus.Ycr + Cus.Ytx + Cus.Yfr + Cus.Yoc - Cus.Ypd
	PRINT USING "& "; Cus.Num; Cus.Nam;
	PRINT USING "##########,.##"; Ldue / 100; (Cus.Vnt - Cus.Vpd) / 100
	IF Cus.Vnt - Cus.Vpd <> Ldue THEN BEEP: KeyHit f$
	Ld = Ld + Ldue
	Li = Li + Cus.Vnt - Cus.Vpd
	NEXT
	PRINT
	PRINT "    Accounts receivable balance: ";
	PRINT USING "##########,.##"; Ld / 100; Li / 100
	KeyHit f$
END SUB

DEFSNG A-B, D-Z
FUNCTION Confirm ()
	PCOPY 0, 1
	COLOR 1, 3: LOCATE 33
	LOCATE , 36: PRINT "                      "
	LOCATE , 36: PRINT " Press [*] to CONFIRM "
	LOCATE , 36: PRINT "                      "
	COLOR 11, 0: LOCATE 34
	LOCATE , 35: PRINT "Û"
	LOCATE , 35: PRINT "Û"
	LOCATE , 35: PRINT "฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿"
	SOUND 1000, .1: SOUND 800, .3: KeyHit f$
	IF f$ = "*" THEN Confirm = 1 ELSE Confirm = 0
	PCOPY 1, 0
END FUNCTION

DEFINT A-B, D-K, M-Z
DEFLNG L
FUNCTION CoTotal (x, s)
	IF s THEN
	   s = 0
	   Lc = Lc + VAL(Ord.Cst(x)) * 100
	   IF Lc > 9999999 THEN
	  WinMssg "Total cost exceeds max."
	  CoTotal = 1: EXIT FUNCTION
	   END IF
	   LOCATE 48, 55: PRINT USING "#####.##"; Lc / 100;
	ELSE
	   s = 1
	   Lc = Lc - VAL(Ord.Cst(x)) * 100
	END IF
END FUNCTION

SUB CusMont ()
	PageNum = 0: HCusMon
	FOR x = 1 TO LOF(4) \ LEN(Cnx)
	GET #4, x, Cnx
	GET #3, Cnx.Num, Cus
	LineNum = LineNum + 1: IF LineNum > 60 THEN HCusMon
	LPRINT USING "& &"; Cus.Num; LEFT$(Cus.Nam, 27);
	LPRINT USING " #######.##"; Cus.Mgr / 100; Cus.Mcr / 100; Cus.Mtx / 100; Cus.Mfr / 100; Cus.Moc / 100; Cus.Mnt / 100;
	LPRINT USING " #######.##"; Cus.Mbb / 100; Cus.Mpd / 100; (Cus.Mbb + Cus.Mnt - Cus.Mpd) / 100;
	LPRINT USING " #######.##"; (Cus.Mgr - Cus.Mcr) / 100; Cus.Mco / 100; (Cus.Mgr - Cus.Mcr - Cus.Mco) / 100
	L0 = L0 + Cus.Mgr
	L1 = L1 + Cus.Mcr
	L2 = L2 + Cus.Mtx
	L3 = L3 + Cus.Mfr
	L4 = L4 + Cus.Moc
	L5 = L5 + Cus.Mnt
	L6 = L6 + Cus.Mbb
	L7 = L7 + Cus.Mpd
	L8 = L8 + Cus.Mco
	NEXT
	LPRINT : LPRINT TAB(32);
	LPRINT USING " #######.##"; L0 / 100; L1 / 100; L2 / 100; L3 / 100; L4 / 100; L5 / 100; L6 / 100; L7 / 100; (L5 + L6 - L7) / 100;
	LPRINT USING " #######.##"; (L0 - L1) / 100; L8 / 100; (L0 - L1 - L8) / 100
END SUB

DEFINT L
SUB CusOrdr (n$, f, e)
   f = 0: e = 0
   b = 1: T = LOF(7) \ LEN(Onx)
   DO UNTIL (T < b)
	  m = (T + b) \ 2: GET #7, m, Onx
	  IF n$ > Onx.Cus THEN b = m + 1 ELSE T = m - 1
   LOOP
   DO WHILE n$ > Onx.Cus AND NOT EOF(7)
	  GET #7, , Onx
   LOOP
   IF n$ = Onx.Cus THEN
	  f = LOC(7): e = f
	  DO WHILE NOT EOF(7)
	 GET #7, , Onx
	 IF n$ = Onx.Cus THEN e = LOC(7) ELSE EXIT DO
	  LOOP
   END IF
END SUB

DEFLNG L
SUB CusYear ()
	PageNum = 0: HCusYer
	FOR x = 1 TO LOF(4) \ LEN(Cnx)
	GET #4, x, Cnx
	GET #3, Cnx.Num, Cus
	LineNum = LineNum + 1: IF LineNum > 60 THEN HCusYer
	LPRINT USING "& &"; Cus.Num; LEFT$(Cus.Nam, 27);
	LPRINT USING " #######.##"; Cus.Ygr / 100; Cus.Ycr / 100; Cus.Ytx / 100; Cus.Yfr / 100; Cus.Yoc / 100; Cus.Ynt / 100;
	LPRINT USING " #######.##"; Cus.Ybb / 100; Cus.Ypd / 100; (Cus.Ybb + Cus.Ynt - Cus.Ypd) / 100;
	LPRINT USING " #######.##"; (Cus.Ygr - Cus.Ycr) / 100; Cus.Yco / 100; (Cus.Ygr - Cus.Ycr - Cus.Yco) / 100
	L0 = L0 + Cus.Ygr
	L1 = L1 + Cus.Ycr
	L2 = L2 + Cus.Ytx
	L3 = L3 + Cus.Yfr
	L4 = L4 + Cus.Yoc
	L5 = L5 + Cus.Ynt
	L6 = L6 + Cus.Ybb
	L7 = L7 + Cus.Ypd
	L8 = L8 + Cus.Yco
	NEXT
	LPRINT : LPRINT TAB(32);
	LPRINT USING " #######.##"; L0 / 100; L1 / 100; L2 / 100; L3 / 100; L4 / 100; L5 / 100; L6 / 100; L7 / 100; (L5 + L6 - L7) / 100;
	LPRINT USING " #######.##"; (L0 - L1) / 100; L8 / 100; (L0 - L1 - L8) / 100
END SUB

DEFINT L
FUNCTION GetDate (f$, T$)
	   f$ = DATE$: T$ = DATE$
	   COLOR 14, 1: LOCATE 32, 61: PRINT "                  ";
			LOCATE 33, 61: PRINT " From: __-__-____ ";
			LOCATE 34, 61: PRINT "                  ";
			LOCATE 35, 61: PRINT "   to: __-__-____ ";
			LOCATE 36, 61: PRINT "                  ";
600    EntryD 33, 68, e, b, f$: IF e THEN EXIT FUNCTION ELSE IF b THEN 600
	   IF f$ < "0" THEN 600
602    EntryD 35, 68, e, b, T$: IF e THEN EXIT FUNCTION ELSE IF b THEN 600
	   IF T$ < "0" THEN 602
	   f$ = RIGHT$(f$, 2) + LEFT$(f$, 2) + MID$(f$, 4, 2)
	   T$ = RIGHT$(T$, 2) + LEFT$(T$, 2) + MID$(T$, 4, 2)
	   GetDate = 1
END FUNCTION

DEFSNG A-Z
SUB HAging ()
	Heading
END SUB

SUB HCusMon ()
	Heading
	LPRINT TAB(70); "CUSTOMER MASTER - MTD"
	LPRINT
	LPRINT
	LPRINT "Cus                               Gross                 Sales     Frieght     Other       Net      Balance     Amount     Amount     Gross       Cost      Gross   "
	LPRINT "Num          Customer             Sales      Credit      Tax      Charges    Charges     Amount    Forward      Paid        Due      Sales     of Goods    Profit  "
	LPRINT "=== =========================== ========== ========== ========== ========== ========== ========== ========== ========== ========== ========== ========== =========="
END SUB

SUB HCusYer ()
	Heading
	LPRINT TAB(70); "CUSTOMER MASTER - YTD"
	LPRINT
	LPRINT
	LPRINT "Cus                               Gross                 Sales     Frieght     Other       Net      Balance     Amount     Amount     Gross       Cost      Gross   "
	LPRINT "Num          Customer             Sales      Credit      Tax      Charges    Charges     Amount    Forward      Paid        Due      Sales     of Goods    Profit  "
	LPRINT "=== =========================== ========== ========== ========== ========== ========== ========== ========== ========== ========== ========== ========== =========="
END SUB

DEFINT A-Z
SUB Heading ()
	 LPRINT CHR$(12); CHR$(27); "@";
	 WIDTH "LPT1:", 255: LPRINT CHR$(27); "M";
	 PageNum = PageNum + 1: LineNum = 13
	 LPRINT "Date: "; DATE$; TAB(61); "      PROCESSED FOODS CORPORATION     "; TAB(153); "Page: "; PageNum
	 LPRINT "Time: "; TIME$; TAB(61); "707 Willow Avenue, Knoxville, TN 37915"
	 LPRINT :         LPRINT TAB(61); "          ACCOUNTS RECEIVABLE         ": LPRINT
END SUB

DEFSNG A-Z
SUB HInvReg (f$, T$)
	Heading
	LPRINT TAB(70); "SALES INVOICE REGISTER"
	LPRINT TAB(71); MID$(f$, 3, 2) + "-" + MID$(f$, 5, 2) + "-" + MID$(f$, 1, 2); " - "; MID$(T$, 3, 2) + "-" + MID$(T$, 5, 2) + "-" + MID$(T$, 1, 2)
	LPRINT
	LPRINT " Inv                                 Invoice               Delivery    Date       Check       Check                         Sales    Frieght    Other            "
	LPRINT "Number Customer                        Date     Date Due     Date      Paid       Number      Amount    Gross    Credit      Tax     Charges   Charges   Amount  "
	LPRINT "====== ============================ ========== ========== ========== ========== ==========  ========= ========= ========= ========= ========= ========= ========="
END SUB

SUB HitKey (f$)
	DO: f$ = INKEY$: TimeDate: LOOP WHILE f$ = ""
END SUB

SUB HOpnInv ()
	Heading
	LPRINT TAB(71); "OPEN SALES INVOICES"
	LPRINT
	LPRINT "                                         Inv      Invoice                                 Check                     Check                                  "
	LPRINT "Code   Customer Name                    Number      Date       Due Date      Amount       Payment    Date  Paid     Number      Amount Due     Balance Due "
	LPRINT "====   ==============================   ======   ==========   ==========   ==========   ==========   ==========   ==========   ============   ============="
END SUB

SUB HProMon ()
	Heading
	LPRINT TAB(68); "MTD - PRODUCTION ANALYSIS"
	LPRINT
	LPRINT " Itm                                   ====BEGINING==== ===PRODUCTION=== ====SPOILAGE==== ===AVAILABLE==== ==TOTAL SALES=== =COST OF SALES== ==GROSS PROFIT==      "
	LPRINT " Num      Product                       unit     total   unit     total   unit     total   unit     total   unit     total   unit     total   unit     total  RATIO"
	LPRINT "===== ================================ ================ ================ ================ ================ ================ ================ ================ ====="
END SUB

SUB HProYer ()
	Heading
	LPRINT TAB(68); "YTD - PRODUCTION ANALYSIS"
	LPRINT
	LPRINT " Itm                                   ====BEGINING==== ===PRODUCTION=== ====SPOILAGE==== ===AVAILABLE==== ==TOTAL SALES=== =COST OF SALES== ==GROSS PROFIT==      "
	LPRINT " Num      Product                       unit     total   unit     total   unit     total   unit     total   unit     total   unit     total   unit     total  RATIO"
	LPRINT "===== ================================ ================ ================ ================ ================ ================ ================ ================ ====="
END SUB

SUB HRctReg ()
	Heading
	LPRINT TAB(72); "RECEIPTS REGISTER": LPRINT
	LPRINT "  Check                   Check                                          Inv                                    "
	LPRINT "  Number    Check Date    Amount    Code  Customer                      Number   Due Date   Date Paid    Amount "
	LPRINT "==========  ==========  ==========  ====  ============================  ======  ==========  ==========  ========"
END SUB

SUB HSales (f$, T$)
	Heading
	LPRINT TAB(75); "SALES REPORT"
	LPRINT TAB(71); MID$(f$, 3, 2) + "-" + MID$(f$, 5, 2) + "-" + MID$(f$, 1, 2); " - "; MID$(T$, 3, 2) + "-" + MID$(T$, 5, 2) + "-" + MID$(T$, 1, 2)
	LPRINT
	LPRINT "                                            Qty       Qty                  Cost      Sales       Inv      Invoice"
	LPRINT "Code    Product                           Ordered   Shipped    Weight    of Goods    Amount     Number      Date      Customer"
	LPRINT "=====   ==============================   ========= ========= ========== ========== ==========   ======   ==========   ======================================"
END SUB

DEFINT A-K, M-S, U-Z
DEFLNG L
DEFDBL T
FUNCTION InvCalc (x)
	IF VAL(Ord.Qts(x)) * VAL(Pro.Wat) > 99999.99 THEN
	   WinMssg "Weight maximum exceeded": GOTO ic
	END IF
	IF VAL(Ord.Qts(x)) * VAL(Pro.Ref) > 99999.99 THEN
	   WinMssg "Cost maximum exceeded": GOTO ic
	END IF
	IF VAL(Ord.Qts(x)) * VAL(Pro.Prc) > 99999.99 THEN
	   WinMssg "Price maximum exceeded": GOTO ic
	END IF
	IF WtTotal(x, 0) GOTO ic
	IF CoTotal(x, 0) GOTO ic
	IF PrTotal(x, 0) GOTO ic
	OnHand Tup
	LOCATE x + 21, 46: PRINT USING "#####.##"; VAL(Ord.Qts(x)) * VAL(Pro.Wat);
	LOCATE x + 21, 55: PRINT USING "#####.##"; VAL(Ord.Qts(x)) * VAL(Pro.Fra) * Tup;
	LOCATE x + 21, 72: PRINT USING "#####.##"; VAL(Ord.Qts(x)) * VAL(Pro.Prc);
	Picture x + 21, 46, 8, Ord.Wts(x)
	Picture x + 21, 55, 8, Ord.Cst(x)
	Picture x + 21, 72, 8, Ord.Prc(x)
	UnitPrc x
	IF WtTotal(x, 1) GOTO ic
	IF CoTotal(x, 1) GOTO ic
	IF PrTotal(x, 1) GOTO ic
	EXIT FUNCTION
ic: InvCalc = 1
END FUNCTION

DEFSNG A-Z
SUB InvCust ()
	COLOR 11, 1
	LOCATE 8
	LOCATE , 2: PRINT Cus.Nam
	LOCATE , 2: PRINT Cus.Add
	LOCATE , 2: PRINT Cus.Str
	LOCATE , 2: PRINT Cus.Cty; " "; Cus.Sta; " "; Cus.Zip
	LOCATE 13
	LOCATE , 2: PRINT Cus.Sco
	LOCATE , 2: PRINT Cus.Sad
	LOCATE , 2: PRINT Cus.SSr
	LOCATE , 2: PRINT Cus.SCt; " "; Cus.SSt; " "; Cus.SZi
END SUB

DEFINT A-K, M-Z
DEFLNG L
SUB InvDisp (w)
	ERASE Old, oQu, oWt, oCo, oPr
	Lw = 0: Lc = 0: Lp = 0
	GET #3, VAL(Ord.Cus), Cus
	InvCust

	COLOR 14, 0
	FOR x = 1 TO 25
	IF Ord.Itm(x) > "  0" THEN
	   GET #1, VAL(Ord.Itm(x)), Pro
	   Old(x) = Ord.Qts(x)
	   oQu(x) = VAL(Ord.Qts(x)) * VAL(Pro.Fra) * 100
	   oWt(x) = VAL(Ord.Wts(x)) * 100
	   oCo(x) = VAL(Ord.Cst(x)) * 100
	   oPr(x) = VAL(Ord.Prc(x)) * 100
	   IF w = 0 THEN
	  LOCATE x + 21, 46: PRINT Ord.Wts(x);
	  LOCATE x + 21, 55: PRINT Ord.Cst(x);
	  LOCATE x + 21, 72: PRINT Ord.Prc(x);
	  UnitPrc x
	  Lw = Lw + VAL(Ord.Wts(x)) * 100
	  Lc = Lc + VAL(Ord.Cst(x)) * 100
	  Lp = Lp + VAL(Ord.Prc(x)) * 100
	   ELSE
	  Ord.Qts(x) = Ord.Qty(x)
	  Ord.Din = DATE$
	  Ord.Due = DayMath$(Ord.Din, VAL(Cus.Day))
	  Ord.Dlv = Ord.Dsh
	  IF InvCalc(x) THEN EXIT SUB
	   END IF
	   LOCATE x + 21, 2: PRINT Pro.Num;
	   LOCATE x + 21, 6: PRINT LEFT$(Pro.Nam, 23);
	   LOCATE x + 21, 30: PRINT Ord.Qty(x);
	   LOCATE x + 21, 38: PRINT Ord.Qts(x);
	ELSE
	   COLOR 13, 0: LOCATE x + 21, 2
	   PRINT "   ณ                       ณ       ณ       ณ        ณ        ณ       ณ        ";
	END IF
	NEXT
	IF w THEN
	   Picture 48, 72, 8, Ord.Grs
	   Ord.Dpd = "**-**-****"
	   Ord.Chk = "*Not Paid*"
	   Ord.Pmt = "    0.00"
	   Ord.Crd = "    0.00"
	   Ord.Tax = "    0.00"
	   Ord.Frt = "    0.00"
	   Ord.Chg = "    0.00"
	END IF
	LOCATE 4, 28:  PRINT Ord.Din;
	LOCATE 4, 39:  PRINT Ord.Dlv;
	LOCATE 4, 50:  PRINT Ord.Car;
	LOCATE 13, 46: PRINT Ord.Due;
	LOCATE 14, 46: PRINT Ord.Dpd;
	LOCATE 15, 46: PRINT Ord.Chk;
	LOCATE 16, 48: PRINT Ord.Pmt;
	
	LOCATE 7, 72:  PRINT Ord.Grs; : Grs$ = Ord.Grs
	LOCATE 8, 72:  PRINT Ord.Crd; : Crd$ = Ord.Crd
	LOCATE 12, 72: PRINT Ord.Tax; : Tax$ = Ord.Tax
	LOCATE 13, 72: PRINT Ord.Frt; : Frt$ = Ord.Frt
	LOCATE 14, 72: PRINT Ord.Chg; : Chg$ = Ord.Chg
	OrdTotl
	LOCATE 4, 19:  PRINT Ord.Inv;
	LOCATE 7, 46:  PRINT Ord.Po1;
	LOCATE 8, 46:  PRINT Ord.Po2;
	LOCATE 9, 46:  PRINT Ord.Dat;
	LOCATE 10, 46: PRINT Ord.Dsh;
	LOCATE 11, 46: PRINT Ord.Ddl;
	LOCATE 48, 46: PRINT USING "#####.##"; Lw / 100;
	LOCATE 48, 55: PRINT USING "#####.##"; Lc / 100;
	LOCATE 48, 72: PRINT USING "#####.##"; Lp / 100;

	Picture 10, 72, 8, oAm$
	Picture 16, 72, 8, Net$
	Picture 48, 55, 8, oCs$
END SUB

SUB InvForm ()
	ERASE Old, oQu, oWt, oCo, oPr
	Lw = 0: Lc = 0: Lp = 0

	LPRINT CHR$(27); "@";
	LPRINT CHR$(27); "M";
	LPRINT CHR$(27); "x"; "1";

	WIDTH "LPT1:", 255
	
	GET #3, VAL(Ord.Cus), Cus

	LPRINT
	LPRINT
	LPRINT
	LPRINT
	LPRINT TAB(103); Ord.Din; " "; Ord.Dlv
	LPRINT
	LPRINT
	LPRINT
	LPRINT TAB(10); Cus.Nam; TAB(60); Cus.Sco
	LPRINT TAB(10); Cus.Add; TAB(60); Cus.Sad
	LPRINT TAB(10); Cus.Str; TAB(60); Cus.SSr
	LPRINT TAB(10); RTRIM$(Cus.Cty); ", "; Cus.Sta; "  "; Cus.Zip; TAB(60); RTRIM$(Cus.SCt); ", "; Cus.SSt; "  "; Cus.SZi
	LPRINT
	LPRINT
	LPRINT TAB(5); LEFT$(Cus.Nam, 1); Cus.Typ; Cus.Num; TAB(24); Ord.Car; TAB(62); Ord.Po1; TAB(84); Ord.Num; ; TAB(97); Cus.Ter
	LPRINT TAB(62); Ord.Po2; TAB(97); "Due Date: "; Ord.Due
	LPRINT
	LPRINT
	FOR x = 1 TO 25
	IF Ord.Itm(x) > "  0" THEN
	   GET #1, VAL(Ord.Itm(x)), Pro
	   LPRINT USING "    &       &"; Pro.Typ + Ord.Itm(x); Pro.Nam;
	   LPRINT TAB(84); Ord.Qts(x); TAB(96); Ord.Wts(x);
	   IF VAL(Ord.Qts(x)) = 0 THEN LPRINT "      0.00 ";  ELSE LPRINT USING "  #####.## "; VAL(Ord.Prc(x)) / VAL(Ord.Qts(x));
	   LPRINT USING "     &"; Ord.Prc(x)
	   Lw = Lw + 100 * VAL(Ord.Wts(x))
	   Lp = Lp + 100 * VAL(Ord.Prc(x))
	ELSE
	   LPRINT
	END IF
	NEXT
	LPRINT TAB(95); :  LPRINT USING "######.##"; Lw / 100;
	LPRINT TAB(119); : LPRINT USING "######.##"; VAL(Ord.Chg)
	LPRINT TAB(119); : LPRINT USING "######.##"; VAL(Ord.Grs) - VAL(Ord.Crd)
	LPRINT TAB(119); : LPRINT USING "######.##"; VAL(Ord.Tax)
	LPRINT TAB(119); : LPRINT USING "######.##"; VAL(Ord.Frt)
	LPRINT TAB(119); : LPRINT USING "######.##"; VAL(Ord.Net)
	LPRINT
	LPRINT
	LPRINT
END SUB

DEFINT L
DEFDBL T
SUB InvOpen ()
	PageNum = 0: HOpnInv
	GET #7, 1, Onx
   'p$ = Onx.Cus
	GET #3, VAL(Onx.Cus), Cus
	FOR x = 1 TO LOF(7) \ LEN(Onx)
	IF INKEY$ = CHR$(27) THEN EXIT FOR
	GET #7, x, Onx
	GET #5, Onx.Num, Ord
	IF Ord.Inv > "     0" AND Ord.Net <> Ord.Pmt THEN
	   IF p$ <> Ord.Cus THEN
	  LPRINT : LPRINT TAB(76);
	  LPRINT USING "#######.##   "; T0; T1
	  LPRINT : LineNum = LineNum + 3
	  p$ = Ord.Cus
	  GET #3, VAL(Ord.Cus), Cus
	  IF LineNum > 60 THEN HOpnInv
	  LPRINT USING "&   "; Cus.Typ + Cus.Num; Cus.Nam;
	  T2 = T2 + T0: T0 = 0
	  T3 = T3 + T1: T1 = 0
	   END IF
	   T0 = T0 + VAL(Ord.Net)
	   T1 = T1 + VAL(Ord.Pmt)
	   LPRINT TAB(41);
	   LPRINT USING "&   "; Ord.Inv; Ord.Din; Ord.Due;
	   LPRINT USING "  &   "; Ord.Net; Ord.Pmt;
	   LPRINT USING "&   "; Ord.Dpd; Ord.Chk;
	   LPRINT USING "########,.##    ########,.##"; VAL(Ord.Net) - VAL(Ord.Pmt); T0 - T1
	   LineNum = LineNum + 1: IF LineNum > 60 THEN HOpnInv
	END IF
	NEXT
	IF T0 THEN
	   LPRINT : LPRINT TAB(76);
	   LPRINT USING "#######.##   "; T0; T1
	   LPRINT
	END IF
	LPRINT TAB(76); : LPRINT USING "#######.##   "; T2; T3
END SUB

DEFLNG L
SUB InvPurg ()
	d$ = ""
	COLOR 29, 0: LOCATE 38, 32
	PRINT " Purge invoices to date:           "
550 EntryD 38, 56, e, b, d$: IF e THEN EXIT SUB ELSE IF b THEN 550
	IF d$ < "0" THEN 550 ELSE IF Confirm = 0 THEN 550
	d$ = RIGHT$(d$, 2) + LEFT$(d$, 2) + MID$(d$, 4, 2)
	OPEN "..\OE\Orders.TIM" FOR RANDOM AS #13 LEN = LEN(Ord)
	OPEN "..\OE\Orders.TIN" FOR RANDOM AS #14 LEN = LEN(Onx)
	PageNum = 0: HInvReg f$, T$
	FOR x = 1 TO LOF(6) \ LEN(Onx)
	IF INKEY$ = CHR$(27) THEN
	   CLOSE 13, 14
	   KILL "..\OE\Orders.TIM"
	   KILL "..\OE\Orders.TIN"
	   EXIT SUB
	END IF
	GET #6, x, Onx
	GET #5, Onx.Num, Ord
	IF Ord.Inv > "      " THEN
	   i$ = RIGHT$(Ord.Din, 2) + LEFT$(Ord.Din, 2) + MID$(Ord.Din, 4, 2)
	   IF Ord.Pmt = Ord.Net AND i$ < d$ THEN
	  GET #3, VAL(Ord.Cus), Cus
	  LineNum = LineNum + 1: IF LineNum > 60 THEN HInvReg f$, T$
	  Ts = VAL(Ord.Grs) - VAL(Ord.Crd)
	  LPRINT USING "& "; Ord.Inv; Cus.Nam; Ord.Din; Ord.Due; Ord.Dpd; Ord.Chk;
	  LPRINT USING " &  & ######.##  &  &  &"; Ord.Grs; Ord.Crd; Ts; Ord.Tax; Ord.Frt; Ord.Net;
	  LPRINT USING " ######.##"; VAL(Ord.Pmt); VAL(Ord.Net) - VAL(Ord.Pmt)
	  T1 = T1 + VAL(Ord.Grs)
	  T2 = T2 + VAL(Ord.Crd)
	  T3 = T3 + VAL(Ord.Tax)
	  T4 = T4 + VAL(Ord.Frt)
	  T5 = T5 + VAL(Ord.Net)
	  T6 = T6 + VAL(Ord.Pmt)
	   ELSE
	  y = y + 1
	  Onx.Num = y
	  PUT #13, y, Ord
	  PUT #14, y, Onx
	   END IF
	ELSE
	   y = y + 1
	   Onx.Num = y
	   PUT #13, y, Ord
	   PUT #14, y, Onx
	END IF
	NEXT
	LPRINT : LPRINT TAB(82);
	LPRINT USING "#######.##"; T1; T2; T1 - T2; T3; T4; T5; T6; T5 - T6
	IF y > 0 THEN
	   CLOSE
	   KILL "..\OE\Orders.MAS"
	   KILL "..\OE\Orders.INX"
	   KILL "..\OE\Orders.CUS"
	   NAME "..\OE\Orders.TIM" AS "..\OE\Orders.MAS"
	   NAME "..\OE\Orders.TIN" AS "..\OE\Orders.INX"
	   SHELL "SortF ..\OE\Orders.INX ..\OE\Orders.CUS /L17 /+7,9 /Q"
	   OpenFiles
	ELSE
	   CLOSE 13, 14
	   KILL "..\OE\Orders.TIM"
	   KILL "..\OE\Orders.TIN"
	END IF
END SUB

DEFINT L
SUB InvRegs (w)
	IF w THEN IF GetDate(f$, T$) = 0 THEN EXIT SUB
	PageNum = 0: HInvReg f$, T$
	FOR c = 1 TO LOF(4) \ LEN(Cnx)
	GET #4, c, Cnx
	GET #3, Cnx.Num, Cus
	CusOrdr Cus.Num, b, e
	IF b > 0 THEN
	   FOR x = b TO e
	   IF INKEY$ = CHR$(27) THEN EXIT FOR
	   GET #7, x, Onx
	   GET #5, Onx.Num, Ord
	   IF w = 0 THEN
	  IF Ord.Inv <> " Print" THEN 604
	   ELSE
	  r$ = RIGHT$(Ord.Din, 2) + LEFT$(Ord.Din, 2) + MID$(Ord.Din, 4, 2)
	  IF r$ < f$ OR r$ > T$ THEN 604
	   END IF
	   LineNum = LineNum + 1: IF LineNum > 60 THEN HInvReg f$, T$
	   LPRINT USING "& "; Ord.Inv; Cus.Nam; Ord.Din; Ord.Due; Ord.Dlv; Ord.Dpd; Ord.Chk;
	   LPRINT USING "  &"; Ord.Pmt; Ord.Grs; Ord.Crd; Ord.Tax; Ord.Frt; Ord.Chg; Ord.Net
	   T0 = T0 + VAL(Ord.Pmt)
	   T1 = T1 + VAL(Ord.Grs)
	   T2 = T2 + VAL(Ord.Crd)
	   T3 = T3 + VAL(Ord.Tax)
	   T4 = T4 + VAL(Ord.Frt)
	   T5 = T5 + VAL(Ord.Chg)
	   T6 = T6 + VAL(Ord.Net)
604    NEXT
	END IF
	NEXT
	LPRINT : LPRINT TAB(92);
	LPRINT USING "#######.##"; T0; T1; T2; T3; T4; T5; T6
END SUB

DEFSNG A-Z
SUB KeyHit (f$)
	DO: f$ = INKEY$: LOOP WHILE f$ = ""
	f$ = UCASE$(f$)
END SUB

SUB Lables ()
	LPRINT CHR$(27); "@"; CHR$(27); "E";
	FOR x = 1 TO LOF(4) \ LEN(Cnx)
	IF INKEY$ = CHR$(27) THEN EXIT FOR
	GET #4, x, Cnx
	GET #3, Cnx.Num, Cus
	LPRINT Cus.Nam; "  "; Cus.Num
	LPRINT Cus.Add
	LPRINT Cus.Str
	LPRINT RTRIM$(Cus.Cty); ", "; Cus.Sta; " "; Cus.Zip
	LPRINT
	LPRINT
	NEXT
END SUB

DEFINT A-K, M-S, U-Z
DEFLNG L
DEFDBL T
SUB OnHand (Tup)
	Lqt = Pro.Ybq + Pro.Ypq - Pro.Ysq + oQu(r) - Pro.Ywq - Pro.Ygq
	Lpr = Pro.Ybp + Pro.Ypp - Pro.Ysc + oCo(r) - Pro.Ywp - Pro.Ygp
	IF Lqt = 0 OR Lpr = 0 THEN Tup = VAL(Pro.Ref) ELSE Tup = Lpr / Lqt
END SUB

DEFSNG A-Z
SUB OpenFiles ()
	OPEN "..\IN\Products.MAS" FOR RANDOM AS #1 LEN = LEN(Pro)
	OPEN "..\IN\Products.INX" FOR RANDOM AS #2 LEN = LEN(Pnx)
	OPEN "..\AR\Customer.MAS" FOR RANDOM AS #3 LEN = LEN(Cus)
	OPEN "..\AR\Customer.INX" FOR RANDOM AS #4 LEN = LEN(Cnx)
	OPEN "..\OE\Orders  .MAS" FOR RANDOM AS #5 LEN = LEN(Ord)
	OPEN "..\OE\Orders  .INX" FOR RANDOM AS #6 LEN = LEN(Onx)
	OPEN "..\OE\Orders  .CUS" FOR RANDOM AS #7 LEN = LEN(Onx)
	OPEN "..\AP\Accounts.DTA" FOR RANDOM AS #8 LEN = LEN(Acc)
	OPEN "..\AR\Receipts.MAS" FOR RANDOM AS #9 LEN = LEN(Rct)
	OPEN "..\AR\Invoices.SAL" FOR RANDOM AS #10 LEN = LEN(Sal)
END SUB

DEFINT A-Z
FUNCTION OrdFind (f$)
   b = 1: T = LOF(6) \ LEN(Onx)
   DO UNTIL (T < b)
	  m = (T + b) \ 2: GET #6, m, Onx
	  IF f$ = Onx.Seq THEN OrdFind = 1: EXIT DO
	  IF f$ > Onx.Seq THEN b = m + 1 ELSE T = m - 1
   LOOP
END FUNCTION

DEFSNG A-Z
SUB OrdTotl ()
	LOCATE 10, 72: PRINT USING "#####.##"; VAL(Ord.Grs) - VAL(Ord.Crd)
	LOCATE 16, 72: PRINT USING "#####.##"; VAL(Ord.Grs) - VAL(Ord.Crd) + VAL(Ord.Tax) + VAL(Ord.Frt) + VAL(Ord.Chg)
END SUB

DEFINT A-K, M-Z
DEFLNG L
SUB ProMont ()
	PageNum = 0: HProMon
	FOR x = 1 TO LOF(1) \ LEN(Pro)
	GET #2, x, Pnx
	GET #1, Pnx.Num, Pro
	IF Pro.Nam > "!" THEN
	LineNum = LineNum + 1: IF LineNum > 60 THEN HProMon
	LPRINT Pro.Typ + Pro.Num; " "; Pro.Nam;
	ProPrnt Pro.Mbq, Pro.Mbp
	ProPrnt Pro.Mpq, Pro.Mpp
	ProPrnt Pro.Mwq, Pro.Mwp
	ProPrnt Pro.Mbq + Pro.Mpq - Pro.Mwq, Pro.Mbp + Pro.Mpp - Pro.Mwp
	ProPrnt Pro.Msq, Pro.Msp
	ProPrnt Pro.Msq, Pro.Msc
	ProPrnt Pro.Msq, Pro.Msp - Pro.Msc
	IF Pro.Msp <> 0 THEN LPRINT USING "###.#%"; 100 * (Pro.Msp - Pro.Msc) / Pro.Msp ELSE LPRINT "  0.0%"
	L1 = L1 + Pro.Mbp
	L2 = L2 + Pro.Mpp
	L3 = L3 + Pro.Mwp
	L4 = L4 + Pro.Msp
	L5 = L5 + Pro.Msc
	END IF
	NEXT
	LPRINT : LPRINT TAB(39);
	LPRINT USING "       #######.##"; L1 / 100; L2 / 100; L3 / 100; (L1 + L2 - L3) / 100; L4 / 100; L5 / 100; (L4 - L5) / 100;
	IF L5 <> 0 THEN LPRINT USING "###.#%"; 100 * (L4 - L5) / L5
END SUB

DEFSNG A-K, M-Z
SUB ProPrnt (L1, L2)
	IF L1 <> 0 AND L2 <> 0 THEN
	   LPRINT USING "###.## #######.##"; L2 / L1; L2 / 100;
	ELSE
	   LPRINT "  0.00       0.00";
	END IF
END SUB

DEFINT A-K, M-Z
SUB ProYear ()
	PageNum = 0: HProYer
	FOR x = 1 TO LOF(1) \ LEN(Pro)
	GET #2, x, Pnx
	GET #1, Pnx.Num, Pro
	IF Pro.Nam > "!" THEN
	LineNum = LineNum + 1: IF LineNum > 60 THEN HProYer
	LPRINT Pro.Typ + Pro.Num; " "; Pro.Nam;
	ProPrnt Pro.Ybq, Pro.Ybp
	ProPrnt Pro.Ypq, Pro.Ypp
	ProPrnt Pro.Ywq, Pro.Ywp
	ProPrnt Pro.Ybq + Pro.Ypq - Pro.Ywq, Pro.Ybp + Pro.Ypp - Pro.Ywp
	ProPrnt Pro.Ysq, Pro.Ysp
	ProPrnt Pro.Ysq, Pro.Ysc
	ProPrnt Pro.Ysq, Pro.Ysp - Pro.Ysc
	IF Pro.Ysp <> 0 THEN LPRINT USING "###.#%"; 100 * (Pro.Ysp - Pro.Ysc) / Pro.Ysp ELSE LPRINT "  0.0%"
	L1 = L1 + Pro.Ybp
	L2 = L2 + Pro.Ypp
	L3 = L3 + Pro.Ywp
	L4 = L4 + Pro.Ysp
	L5 = L5 + Pro.Ysc
	END IF
	NEXT
	LPRINT : LPRINT TAB(39);
	LPRINT USING "       #######.##"; L1 / 100; L2 / 100; L3 / 100; (L1 + L2 - L3) / 100; L4 / 100; L5 / 100; (L4 - L5) / 100;
	IF L4 <> 0 THEN LPRINT USING "###.#%"; 100 * ((L4 - L5) / L4)
END SUB

FUNCTION PrTotal (x, s)
	IF s THEN
	   s = 0
	   Lp = Lp + VAL(Ord.Prc(x)) * 100
	   IF Lp > 9999999 THEN
	  WinMssg "Total exceeds max."
	  PrTotal = 1: EXIT FUNCTION
	   END IF
	   LOCATE 48, 72: PRINT USING "#####.##"; Lp / 100;
	ELSE
	   s = 1
	   Lp = Lp - VAL(Ord.Prc(x)) * 100
	END IF
END FUNCTION

DEFINT L
DEFDBL T
SUB RctRegs (w)
	IF w THEN
	   IF GetDate(f$, T$) = 0 THEN EXIT SUB
	   f = 1
	ELSE
	   f = Lastrct + 1
	END IF
	PageNum = 0: HRctReg
	FOR x = f TO LOF(9) \ LEN(Rct)
	GET #9, x, Rct
	IF w THEN
	   r$ = RIGHT$(Rct.Dat, 2) + LEFT$(Rct.Dat, 2) + MID$(Rct.Dat, 4, 2)
	   IF r$ < f$ OR r$ > T$ THEN 660
	END IF
	GET #3, VAL(Rct.Cus), Cus
	IF LineNum > 60 THEN HRctReg
	LPRINT USING "&  "; Rct.Chk; Rct.Dat; Rct.Tot; Cus.Typ + Cus.Num; Cus.Nam;
	FOR y = 1 TO 20
	IF Rct.Inv(y) > "     0" THEN
	   IF OrdFind(Rct.Inv(y)) THEN GET #5, Onx.Num, Ord
	   LPRINT TAB(73);
	   LPRINT USING "&  "; Ord.Inv; Ord.Due; Ord.Dpd; Rct.Amt(y)
	   LineNum = LineNum + 1: IF LineNum > 60 THEN HRctReg
	   T = T + VAL(Rct.Amt(y))
	END IF
	NEXT
660 NEXT
	LPRINT : LPRINT TAB(25);
	LPRINT USING "#######.##"; T
END SUB

DEFINT T
SUB Repost ()
	'use this routine for reposting customers invoices and products
	'for the month and the year.  change the date to reflect the
	'beginning of the year and the month.
	CLS
	LOCATE 50, 1: PRINT "Repost customers invoices and products                                         ";
	FOR x = 1 TO LOF(3) \ LEN(Cus)
	GET #3, x, Cus
	Cus.Mgr = 0: Cus.Ygr = 0
	Cus.Mcr = 0: Cus.Ycr = 0
	Cus.Mtx = 0: Cus.Ytx = 0
	Cus.Mfr = 0: Cus.Yfr = 0
	Cus.Moc = 0: Cus.Yoc = 0
	Cus.Mnt = 0: Cus.Ynt = 0
	Cus.Mco = 0: Cus.Yco = 0
	PUT #3, x, Cus
	NEXT
	FOR x = 1 TO LOF(1) \ LEN(Pro)
	GET #1, x, Pro
	Pro.Msq = 0: Pro.Ysq = 0
	Pro.Msc = 0: Pro.Ysc = 0
	Pro.Msp = 0: Pro.Ysp = 0
	Pro.Msw = 0: Pro.Ysw = 0
	PUT #1, x, Pro
	NEXT
	FOR y = 1 TO LOF(5) \ LEN(Ord)
	GET #5, y, Ord
	d$ = RIGHT$(Ord.Din, 2) + LEFT$(Ord.Din, 2) + MID$(Ord.Din, 4, 2)
	IF d$ >= "920101" THEN
	   GET #3, VAL(Ord.Cus), Cus
	   FOR x = 1 TO 25
		IF VAL(Ord.Itm(x)) > 0 THEN
		   GET #1, VAL(Ord.Itm(x)), Pro
		   IF d$ >= "920501" THEN
			  Pro.Msq = Pro.Msq + (VAL(Ord.Qts(x)) * VAL(Pro.Fra) * 100)
			  Pro.Msw = Pro.Msw + VAL(Ord.Wts(x)) * 100
			  Pro.Msc = Pro.Msc + VAL(Ord.Cst(x)) * 100
			  Pro.Msp = Pro.Msp + VAL(Ord.Prc(x)) * 100
			  Cus.Mco = Cus.Mco + VAL(Ord.Cst(x)) * 100
		   END IF
		   Pro.Ysq = Pro.Ysq + (VAL(Ord.Qts(x)) * VAL(Pro.Fra) * 100)
		   Pro.Ysw = Pro.Ysw + VAL(Ord.Wts(x)) * 100
		   Pro.Ysc = Pro.Ysc + VAL(Ord.Cst(x)) * 100
		   Pro.Ysp = Pro.Ysp + VAL(Ord.Prc(x)) * 100
		   Cus.Yco = Cus.Yco + VAL(Ord.Cst(x)) * 100
		   PUT #1, VAL(Ord.Itm(x)), Pro
		ELSE EXIT FOR
		END IF
		NEXT
		IF d$ >= "920501" THEN
		   Cus.Mgr = Cus.Mgr + VAL(Ord.Grs) * 100
		   Cus.Mcr = Cus.Mcr + VAL(Ord.Crd) * 100
		   Cus.Mtx = Cus.Mtx + VAL(Ord.Tax) * 100
		   Cus.Mfr = Cus.Mfr + VAL(Ord.Frt) * 100
		   Cus.Moc = Cus.Moc + VAL(Ord.Chg) * 100
		   Cus.Mnt = Cus.Mnt + VAL(Ord.Net) * 100
		END IF
		Cus.Ygr = Cus.Ygr + VAL(Ord.Grs) * 100
		Cus.Ycr = Cus.Ycr + VAL(Ord.Crd) * 100
		Cus.Ytx = Cus.Ytx + VAL(Ord.Tax) * 100
		Cus.Yfr = Cus.Yfr + VAL(Ord.Frt) * 100
		Cus.Yoc = Cus.Yoc + VAL(Ord.Chg) * 100
		Cus.Ynt = Cus.Ynt + VAL(Ord.Net) * 100
		PUT #3, VAL(Ord.Cus), Cus
	END IF
	NEXT
END SUB

DEFDBL D
SUB SalRegs (w)
	IF GetDate(f$, T$) = 0 THEN EXIT SUB
	CLOSE : OPEN "Invoices.SAL" FOR OUTPUT AS #1: CLOSE : OpenFiles
	FOR x = 1 TO LOF(5) \ LEN(Ord)
	GET #5, x, Ord
	IF Ord.Inv > "0     " THEN
	   r$ = RIGHT$(Ord.Din, 2) + LEFT$(Ord.Din, 2) + MID$(Ord.Din, 4, 2)
	   IF r$ >= f$ AND r$ <= T$ THEN
	  IF INKEY$ = CHR$(27) THEN EXIT SUB
	  FOR y = 1 TO 25
	  IF Ord.Itm(y) > "  0" THEN
		 GET #1, VAL(Ord.Itm(y)), Pro
		 Sal.Typ = Pro.Typ
		 Sal.Itm = Ord.Itm(y)
		 Sal.Cus = Ord.Cus
		 Sal.Dat = Ord.Din
		 Sal.Qty = Ord.Qty(y)
		 Sal.Qts = Ord.Qts(y)
		 Sal.Wts = Ord.Wts(y)
		 Sal.Cst = Ord.Cst(y)
		 Sal.Prc = Ord.Prc(y)
		 Sal.Num = x
		 PUT #10, , Sal
	  ELSE EXIT FOR
	  END IF
	  NEXT
	   END IF
	END IF
	NEXT
	IF LOC(10) = 0 THEN WinMssg "No sales for this date": EXIT SUB
	CLOSE
	SHELL "SortF Invoices.SAL Invoices /L58 /+1,14 /Q"
	KILL "Invoices.SAL"
	NAME "Invoices" AS "Invoices.SAL"
	OpenFiles

	PageNum = 0: HSales f$, T$
	GET #10, 1, Sal: p$ = Sal.Itm
	GET #1, VAL(Sal.Itm), Pro

	FOR x = 1 TO LOF(10) \ LEN(Sal)
	IF INKEY$ = CHR$(27) THEN EXIT FOR
	GET #10, x, Sal
	IF p$ <> Sal.Itm THEN GOSUB 410
	GET #1, VAL(Sal.Itm), Pro
	IF w THEN
	   GET #5, Sal.Num, Ord
	   GET #3, VAL(Ord.Cus), Cus
	   LineNum = LineNum + 1: IF LineNum > 60 THEN HSales f$, T$
	   LPRINT USING "&   "; Pro.Typ + Pro.Num; Pro.Nam; Sal.Qty; Sal.Qts; Sal.Wts; Sal.Cst; Sal.Prc; Ord.Inv; Ord.Din;
	   LPRINT USING " &"; Ord.Cus; Cus.Nam
	END IF
	D1 = D1 + VAL(Sal.Qty)
	D2 = D2 + VAL(Sal.Qts)
	D3 = D3 + VAL(Sal.Wts)
	D4 = D4 + VAL(Sal.Cst)
	D5 = D5 + VAL(Sal.Prc)
	NEXT

	GOSUB 410
	IF w = 0 THEN LPRINT
	LPRINT TAB(42);
	LPRINT USING "######.## "; D6; D7;
	LPRINT USING "#######.## "; D8; D9; D0
	EXIT SUB

410 IF w THEN
	   LPRINT : LineNum = LineNum + 3
	ELSE
	   LPRINT Pro.Typ + Pro.Num; "   "; Pro.Nam;
	   LineNum = LineNum + 1
	END IF
	LPRINT TAB(42);
	LPRINT USING "######.## "; D1; D2;
	LPRINT USING "#######.## "; D3; D4; D5
	IF w THEN LPRINT
	D6 = D6 + D1: D1 = 0
	D7 = D7 + D2: D2 = 0
	D8 = D8 + D3: D3 = 0
	D9 = D9 + D4: D4 = 0
	D0 = D0 + D5: D5 = 0
	p$ = Sal.Itm
	RETURN
END SUB

DEFSNG A-Z
SUB TimeDate ()
 COLOR 13, 0
 LOCATE 19, 22, 0: PRINT " "; DATE$; " "
 T$ = " " + LEFT$(TIME$, 2)
 IF TIME$ > "11:59:59" THEN f$ = "pm " ELSE f$ = "am "
 IF TIME$ > "12:59:59" THEN T$ = " " + STR$(VAL(T$) - 12)
 LOCATE 19, 47: PRINT T$; MID$(TIME$, 3, 6) + f$
END SUB

DEFINT X
SUB UnitPrc (x)
	LOCATE x + 21, 64
	IF VAL(Ord.Qts(x)) <> 0 THEN
	   IF VAL(Ord.Prc(x)) / VAL(Ord.Qts(x)) > 999.999 THEN PRINT "***.***": EXIT SUB
	   PRINT USING "###.###"; VAL(Ord.Prc(x)) / VAL(Ord.Qts(x));
	ELSE
	   PRINT "  0.000";
	END IF
END SUB

DEFINT A-W, Y-Z
FUNCTION WinCust ()
	PCOPY 0, 1: CALL MhDisplayByName(0, 0, "CusWindow", e)
	n = 1: r = 7: Bot = 1: Top = LOF(4) \ LEN(Cnx)
	GOSUB C2
C1: GET #4, n, Cnx
	COLOR 7, 0: LOCATE r, 16: PRINT USING "&ณ&"; Cnx.Cod; Cnx.Nam
	KeyHit n$
	COLOR 4, 7: LOCATE r, 16: PRINT USING "&ณ&"; Cnx.Cod; Cnx.Nam
	SELECT CASE n$
	CASE CHR$(13)
	 Ord.Cus = Cnx.Cod
	 PCOPY 1, 0
	 EXIT FUNCTION
	CASE CHR$(0) + "P", CHR$(0) + "M" 'Dn
	 IF n + 1 > Top GOTO C1 ELSE n = n + 1
	 IF r + 1 > 20 THEN CALL scroll(16, 7, 48, 20, 1) ELSE r = r + 1
	CASE CHR$(0) + "H", CHR$(0) + "K" 'Up
	 IF n - 1 < 1 GOTO C1 ELSE n = n - 1
	 IF r - 1 < 7 THEN CALL BkScroll(16, 7, 48, 20, 1) ELSE r = r - 1
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
	 PCOPY 1, 0
	 WinCust = 1
	 EXIT FUNCTION
	END SELECT
	GOTO C1
 
C2: COLOR 4, 7: LOCATE 7
	FOR x = Bot TO Top: GET #4, x, Cnx
	LOCATE , 16: PRINT USING "&ณ&"; Cnx.Cod; Cnx.Nam
	IF CSRLIN > 20 THEN EXIT FOR
	NEXT
	RETURN
END FUNCTION

SUB WinMssg (f$)
	PCOPY 0, 2
	COLOR 17, 14: LOCATE 20
	f = LEN(f$)
	LOCATE , 26: PRINT " "; STRING$(f, " "); " "
	LOCATE , 26: PRINT " "; f$; " "
	LOCATE , 26: PRINT " "; STRING$(f, " "); " "
	
	COLOR 23, 0: LOCATE 21
	Picture 21, 25, 1, c$: LOCATE , 25: PRINT c$
	Picture 22, 25, 1, c$: LOCATE , 25: PRINT c$
	Picture 23, 25, f + 2, c$: LOCATE , 25: PRINT c$
	SOUND 770, .1: SOUND 970, .2: KeyHit f$
	PCOPY 2, 0
END SUB

FUNCTION WinProd ()
	PCOPY 0, 1: CALL MhDisplayByName(0, 0, "ProWindow", e)
	Bot = 1: Top = LOF(2) \ LEN(Pnx)
	n = 1: r = 17
	GOSUB T2
T1: GET #2, n, Pnx
	COLOR 15, 4: LOCATE r, 21: PRINT USING "&ณ###ณ&"; Pnx.Typ; Pnx.Num; Pnx.Nam
	KeyHit n$
	COLOR 0, 7: LOCATE r, 21: PRINT USING "&ณ###ณ&"; Pnx.Typ; Pnx.Num; Pnx.Nam
	SELECT CASE n$
	CASE CHR$(13)
	 GET #1, Pnx.Num, Pro
	 IF Pro.Nam < "!" GOTO T1
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
	 WinProd = 1
	 EXIT FUNCTION
	END SELECT
	GOTO T1

T2: COLOR 0, 7: LOCATE 17
	FOR x = Bot TO Top: GET #2, x, Pnx
	LOCATE , 21: PRINT USING "&ณ###ณ&"; Pnx.Typ; Pnx.Num; Pnx.Nam
	IF CSRLIN > 30 THEN EXIT FOR
	NEXT
	RETURN
END FUNCTION

DEFLNG L
FUNCTION WtTotal (x, s)
	IF s THEN
	   Lw = Lw + VAL(Ord.Wts(x)) * 100
	   IF Lw > 9999999 THEN
	  WinMssg "Total exceeds max."
	  WtTotal = 1: EXIT FUNCTION
	   END IF
	   LOCATE 48, 46: PRINT USING "#####.##"; Lw / 100;
	   s = 0
	ELSE
	   s = 1
	   Lw = Lw - VAL(Ord.Wts(x)) * 100
	END IF
END FUNCTION


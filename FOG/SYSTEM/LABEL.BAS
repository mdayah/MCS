DEFINT A-Z

   '$INCLUDE: '..\Include\Customer.Rec'

DIM Cus      AS Customer
DIM Cnx      AS CusIndex
DIM StoNum   AS STRING * 2
DIM AgrNum   AS LONG
DIM RctNum   AS LONG
DIM RptDay   AS STRING * 3
DIM EndBak   AS STRING * 10
DIM EndDay   AS STRING * 10
DIM EndWek   AS STRING * 10
DIM EndMon   AS STRING * 10
DIM EndYer   AS STRING * 10


   '$INCLUDE: '..\Include\LoadCfg.DEC'

OPEN "..\DataFile\Customer.Mas" FOR RANDOM AS #1 LEN = LEN(Cus)
OPEN "..\DataFile\Customer.Nam" FOR RANDOM AS #2 LEN = LEN(Cnx)
OPEN "..\DataFile\Label.txt" FOR OUTPUT AS #3

    CLS
    LOCATE 1, 1: PRINT "   Total Customers: "; LOF(2) \ LEN(Cnx)
    FOR x = 1 TO LOF(2) \ LEN(Cnx)
    GET #2, x, Cnx
    GET #1, Cnx.Num, Cus

    IF Cus.Mmc = 0 THEN 'never extracted label
       n = INSTR(Cus.Nam, ",")
       IF n THEN
           f$ = RTRIM$(LTRIM$(MID$(Cus.Nam, n + 1)))
           l$ = RTRIM$(LTRIM$(MID$(Cus.Nam, 1, n - 1)))
       ELSE
           f$ = RTRIM$(LTRIM$(Cus.Nam))
           l$ = ""
       END IF

       WRITE #3, StoNum, f$ + " " + l$, Cus.Add, Cus.Str, Cus.Cty, Cus.Sta, Cus.Zip
       y = y + 1
       LOCATE 2, 1: PRINT "      New Customer: "; x
       LOCATE 3, 1: PRINT "Total New Customer: "; y

       Cus.Mmc = DATEVALUE(DATE$) 'date label extracted
       PUT #1, Cnx.Num, Cus
    END IF

    NEXT
    CLOSE


DEFINT A-Z

   '$INCLUDE: '..\Include\Codes.Rec'
   '$INCLUDE: '..\Include\Combos.Rec'
   '$INCLUDE: '..\Include\Rates.Rec'

DIM Cod      AS Codes
DIM Cmb      AS Combos
DIM Rat      AS Rates

'Codes========================================================================
    OPEN "C:\Rentals\Master\Codes.Mas" FOR RANDOM AS #1 LEN = LEN(Cod)
    OPEN "C:\Rentals\DataFile\Codes.CSV" FOR OUTPUT AS #2

    CLS
    LOCATE 1, 1: PRINT "        Total Codes ", LOF(1) \ LEN(Cod)

   'Headers
    WRITE #2, "Code", "Type", "Description", "Rate Code", "Group", "Combo Code", "Fair Market Value", "Drop in Terms", "Drop in FMV", "Depreciation Life"

    FOR x = 1 TO LOF(1) \ LEN(Cod)
        GET #1, x, Cod
        WRITE #2, Cod.Cod, Cod.Typ, Cod.Des, Cod.Rat, Cod.Grp, Cod.Cmb, Cod.Fmv, Cod.Dtr, Cod.Dfm, Cod.Dep
        LOCATE 2, 1: PRINT "     Exporting Code ", x;
    NEXT
    CLOSE

'Combos=======================================================================
    OPEN "C:\Rentals\Master\Combos.Mas" FOR RANDOM AS #1 LEN = LEN(Cmb)
    OPEN "C:\Rentals\DataFile\Combos.CSV" FOR OUTPUT AS #2

    CLS
    LOCATE 1, 1: PRINT "       Total Combos ", LOF(1) \ LEN(Cmb)

   'Headers
    WRITE #2, "Combo Code", "Description", "Rate Code", "Code 1", "Code 2", "Code 3", "Code 4", "Group", "Drop in Terms", "Drop in FMV"

    FOR x = 1 TO LOF(1) \ LEN(Cmb)
        GET #1, x, Cmb
        WRITE #2, Cmb.Cod, Cmb.Des, Cmb.Rat, Cmb.Cde(1), Cmb.Cde(2), Cmb.Cde(3), Cmb.Cde(4), Cmb.Grp, Cmb.Dtr, Cmb.Dfm
        LOCATE 2, 1: PRINT "    Exporting Combo ", x;
    NEXT
    CLOSE

'Rates========================================================================
    OPEN "C:\Rentals\Master\Rates.Mas" FOR RANDOM AS #1 LEN = LEN(Rat)
    OPEN "C:\Rentals\DataFile\Rates.CSV" FOR OUTPUT AS #2

    CLS
    LOCATE 1, 1: PRINT "        Total Rates ", LOF(1) \ LEN(Rat)

   'Headers
    WRITE #2, "Rate Code", "Monthly Term", "Monthly Rate", "Monthly Term", "Monthly Rate", "Monthly Term", "Monthly Rate", "Weekly Term", "Weekly Rate", "Weekly Term", "Weekly Rate", "Weekly Term", "Weekly Rate"

    FOR x = 1 TO LOF(1) \ LEN(Rat)
        GET #1, x, Rat
        WRITE #2, Rat.Num, Rat.Mtr(1), Rat.Mrt(1), Rat.Mtr(2), Rat.Mrt(2), Mtr(3), Rat.Mrt(3), Rat.Wtr(1), Rat.Wrt(1), Rat.Wtr(2), Rat.Wrt(2), Rat.Wtr(3), Rat.Wrt(3)
        LOCATE 2, 1: PRINT "     Exporting Rate ", x;
    NEXT
    CLOSE



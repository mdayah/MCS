DEFINT A-Z

   '$INCLUDE: '..\Include\Customer.Rec'
   '$INCLUDE: '..\Include\Agreemnt.Rec'
   '$INCLUDE: '..\Include\Receipts.Rec'
   '$INCLUDE: '..\Include\Inventor.Rec'

DIM Cus      AS Customer
DIM Agr      AS Agreement
DIM Rct      AS Receipts
DIM Inv      AS Inventory

'Customers====================================================================
    OPEN "C:\Rentals\DataFile\Customer.Mas" FOR RANDOM AS #1 LEN = LEN(Cus)
    OPEN "C:\Rentals\DataFile\Customer.CSV" FOR OUTPUT AS #2

    CLS
    LOCATE 1, 1: PRINT "    Total Customers: ", LOF(1) \ LEN(Cus)

   'Headers
    WRITE #2, "Customer ID", "Salutation", "Name", "Address", "Street", "City", "State", "Zip", "Telephone", "Notes", "Social Security", "Drivers License", "Birth Date", "Marital Status", "Race", "Sex", "Employer", "Work Phone", "Dont Know",  _
"Income", "Other Income", "Distance", "Referral code", "Type", "Delete", "2nd Name", "2nd Soc. Sec", "2nd Drivers License", "2nd Birth Date", "2nd Marital Status", "2nd Race", "2nd Sex", "2nd Employer", "2nd Work phone", "2nd Dont Know",  _
"2nd Income", "2nd Other income", "Filler", "Label Date"

    FOR x = 1 TO LOF(1) \ LEN(Cus)
        GET #1, x, Cus
        WRITE #2, Cus.Num, Cus.Sal, Cus.Nam, Cus.Add, Cus.Str, Cus.Cty, Cus.Sta, Cus.Zip, Cus.Tel, Cus.Rmk, Cus.SSn, Cus.Dln, Cus.Dob, Cus.Mar, Cus.Rac, Cus.Sex, Cus.Emp, Cus.Wor, Cus.Lon, Cus.Inc, Cus.Oth, Cus.Dis, Cus.Ref, Cus.Typ, Cus.Del, Cus. _
Nm1, Cus.SS1, Cus.Dl1, Cus.Bd1, Cus.St1, Cus.Rc1, Cus.Sx1, Cus.Em1, Cus.Wt1, Cus.Ln1, Cus.In1, Cus.Ot1, Cus.Fil, Cus.Mmc
        LOCATE 2, 1: PRINT " Exporting Customer: ", x;
    NEXT
    CLOSE
'Agreements===================================================================
    OPEN "C:\Rentals\DataFile\Agreemnt.Mas" FOR RANDOM AS #1 LEN = LEN(Agr)
    OPEN "C:\Rentals\DataFile\Agreemnt.CSV" FOR OUTPUT AS #2

    CLS
    LOCATE 1, 1: PRINT "   Total Agreements: ", LOF(1) \ LEN(Agr)

   'Headers
    WRITE #2, "Number", "Date", "User", "Route", "Pager", "Customer", "Combo Code", "Single Code", "First Due Date", "Next Due Date", "Payout date", "Last Activity", "Status", "Frequency", "Late Fees", "Trip Charges", "Processing Fee",  _
"Refere to Agr", "Initial Payment", "Activity Code", "Wavier", "Bad Receipts", "Last EZA Date", "Last EZA User", "Last EZA Days", "Last FREE Date", "Last FREE Init", "Last FREE Days", "Accessories", "Filler", "Monthly Term", "Monthly Rate",  _
"Weekly Term", "Weekly Rate", "Modified Mon Term", "Modified Mon Rate", "Modified Wkly Term", "Modified Wkly Rate", "Unapplied", "Days Paid", "Weeks Paid", "Months Paid", "Number of Receipts", "Days Late", "EZA Days", "FREE Days", "Mon Income",  _
"Yr Income", "Tot Income", "Mon Discounts", "Yr Discounts", "Tot Discounts", "Mon Tax", "Yr Tax", "Tot Tax", "Mon Late Fees", "Yr Late Fees", "Tot Late Fees", "Mon Trip Charges", "Yr Trip Charges", "Tot Trip Charges", "Mon Other Charges",  _
"Yr Other Charges", "Tot Other Charges", "Mon Waiver", "Yr Waiver", "Tot Waiver", "Mon Referral", "Yr Referral", "Tot Referral", "Item 1", "Item 1", "Item 1", "Item 1", "Item 2", "Item 2", "Item 2", "Item 2", "Item 3", "Item 3", "Item 3", "Item 3",  _
"Item 4", "Item 4", "Item 4", "Item 4"
     
    FOR x = 1 TO LOF(1) \ LEN(Agr)
        GET #1, x, Agr
        WRITE #2, Agr.Num, Agr.Dat, Agr.Mgr, Agr.Rte, Agr.Pag, Agr.Cus, Agr.Cmb, Agr.Cod, Agr.Due, Agr.Nxt, Agr.Pod, Agr.Dta, Agr.Sta, Agr.Frq, Agr.Lat, Agr.Trp, Agr.Pro, Agr.Ref, Agr.Pay, Agr.Act, Agr.Wav, Agr.Bad, Agr.EZd, Agr.EZi, Agr.EZn, Agr. _
FRd, Agr.FRi, Agr.FRn, Agr.Acc, Agr.Fil, Agr.Mst, Agr.Msr, Agr.Wst, Agr.Wsr, Agr.Mtr, Agr.Mrt, Agr.Wtr, Agr.Wrt, Agr.Una, Agr.Npd, Agr.Npw, Agr.Npm, Agr.Rct, Agr.Ndl, Agr.EZA, Agr.Fre, Agr.Mri, Agr.Yri, Agr.Tri, Agr.Mds, Agr.Yds, Agr.Tds, Agr.Mtx,  _
Agr.Ytx, Agr.Ttx, Agr.Mlc, Agr.Ylc, Agr.Tlc, Agr.Mtc, Agr.Ytc, Agr.Ttc, Agr.Mot, Agr.Yot, Agr.Tot, Agr.Min, Agr.Yin, Agr.Tin, Agr.Mrf, Agr.Yrf, Agr.Trf, Agr.It1, Agr.Lo1, Agr.Ds1, Agr.Fm1, Agr.It2, Agr.Lo2, Agr.Ds2, Agr.Fm2, Agr.It3, Agr.Lo3, Agr. _
Ds3, Agr.Fm3, Agr.It4, Agr.Lo4, Agr.Ds4, Agr.Fm4
        LOCATE 2, 1: PRINT "Exporting Agreement: ", x;
    NEXT
    CLOSE
'Receipts=====================================================================
    OPEN "C:\Rentals\DataFile\Receipts.Mas" FOR RANDOM AS #1 LEN = LEN(Rct)
    OPEN "C:\Rentals\DataFile\Receipts.CSV" FOR OUTPUT AS #2

    CLS
    LOCATE 1, 1: PRINT "     Total Receipts: ", LOF(1) \ LEN(Rct)

   'Headers
    WRITE #2, "Customer Number", "Agreement Number", "Receipt Number", "User", "Bad Check", "Payment Code", "Status", "Check No", "Receipt Date", "Due Date", "Next Due Date", "Rent", "Discount", "Sales Tax", "Late Chg", "Trip Chg", "Other Chg",  _
"Waiver", "Referral", "Debit/Credit", "Amount Due", "Amount Paid", "Months/Weeks", "Days"
     
    FOR x = 1 TO LOF(1) \ LEN(Rct)
        GET #1, x, Rct
        WRITE #2, Rct.Cus, Rct.Agr, Rct.Num, Rct.Mgr, Rct.Mrk, Rct.Cod, Rct.Sta, Rct.Chk, Rct.Dat, Rct.Ddt, Rct.Nxt, Rct.Rnt, Rct.Dis, Rct.Tax, Rct.Lat, Rct.Trp, Rct.Oth, Rct.Wav, Rct.Ref, Rct.Crd, Rct.Due, Rct.Amt, Rct.Typ, Rct.Day
        LOCATE 2, 1: PRINT "  Exporting Receipt: ", x;
    NEXT
    CLOSE
'Inventory====================================================================
    OPEN "C:\Rentals\DataFile\Inventor.Mas" FOR RANDOM AS #1 LEN = LEN(Inv)
    OPEN "C:\Rentals\DataFile\Inventor.CSV" FOR OUTPUT AS #2

    CLS
    LOCATE 1, 1: PRINT "    Total Inventory ", LOF(1) \ LEN(Inv)

   'Headers
    WRITE #2, "Type", "Group", "Filler", "Code", "Brand", "Status", "Times Rented", "Service Center", "Model", "Serial", "Distributor", "Purchase Date", "Receive Date", "Activity Date", "Price", "Agreement", "Transer", "Transer from", "Transfer to", _
 "Order No", "Activity Code", "Warranty Parts", "Warranty Labor", "Rate Code", "Override", "Filler", "Notes", "Month Days Ide", "Year", "Total", "Month Days on Rent", "Year", "Total", "Month Days Awaiting", "Year", "Total", "Month Days in Service",  _
"Year", "Total", "Month Days Awaiting Return", "Year", "Total", "Month Income", "Year", "Total", "Month Depreciation", "Year", "Total", "Month Repairs", "Year", "Total"
     
    FOR x = 1 TO LOF(1) \ LEN(Inv)
        GET #1, x, Inv
        WRITE #2, Inv.Typ, Inv.Grp, Inv.Fil, Inv.Cod, Inv.Bra, Inv.Sta, Inv.Tim, Inv.Ctr, Inv.Mdl, Inv.Ser, Inv.Dis, Inv.Dtp, Inv.Dtr, Inv.Dta, Inv.Prc, Inv.Agr, Inv.Del, Inv.Tfr, Inv.Tto, Inv.Pon, Inv.Act, Inv.Par, Inv.Lab, Inv.Rat, Inv.Ovr, Inv. _
Fac, Inv.Rmk, Inv.Mdi, Inv.Ydi, Inv.Tdi, Inv.Mdr, Inv.Ydr, Inv.Tdr, Inv.Mas, Inv.Yas, Inv.Tas, Inv.Mds, Inv.Yds, Inv.Tds, Inv.Mar, Inv.Yar, Inv.Tar, Inv.Min, Inv.Yin, Inv.Tin, Inv.Mde, Inv.Yde, Inv.Tde, Inv.Mre, Inv.Yre, Inv.Tre
        LOCATE 2, 1: PRINT "          Inventory ", x;
    NEXT
    CLOSE


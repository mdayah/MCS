DEFLNG A-Z

TYPE OffInv
     Sto AS STRING * 2   'Store
     Typ AS STRING * 1   'Type
     Grp AS STRING * 2   'Group
     Ini AS STRING * 3   'User Initials
     Cod AS STRING * 4   'Code
     Bra AS STRING * 3   'Brand
     Sta AS INTEGER      'Status
     Tim AS INTEGER      'Times rented
     Ctr AS STRING * 2   'Service center
     Mdl AS STRING * 10  'Model number
     Ser AS STRING * 10  'Serial number
     Dis AS STRING * 10  'Distributor
     Dtp AS STRING * 10  'Date purchased
     Dtr AS STRING * 10  'Date received
     Dta AS STRING * 10  'Date of last activity
     Prc AS STRING * 7   'Purchase price
     Agr AS STRING * 6   'Agreement number
     Del AS STRING * 1   'Trasferred (T)
     Tfr AS STRING * 2   'Trasfered form store
     Tto AS STRING * 2   'Transfered to store
     Pon AS STRING * 10  'Purchase order number
     Act AS STRING * 2   'Activity Code
     Par AS STRING * 3   'Warranty parts
     Lab AS STRING * 3   'Warranty labor
     Rat AS STRING * 3   'Rate code
     Ovr AS STRING * 1   'Override rate (O)
     Fac AS STRING * 9   '********not used******

     Rmk AS STRING * 69  'Remarks

     Mdi AS LONG: Ydi AS LONG: Tdi AS LONG 'D-Idle
     Mdr AS LONG: Ydr AS LONG: Tdr AS LONG 'D-Rented
     Mas AS LONG: Yas AS LONG: Tas AS LONG 'D-Awaiting Service
     Mds AS LONG: Yds AS LONG: Tds AS LONG 'D-In Service
     Mar AS LONG: Yar AS LONG: Tar AS LONG 'D-Awaiting Return
     Min AS LONG: Yin AS LONG: Tin AS LONG '$-Revenues
     Mde AS LONG: Yde AS LONG: Tde AS LONG '$-Depreciation
     Mre AS LONG: Yre AS LONG: Tre AS LONG '$-Repairs
END TYPE

REM $INCLUDE: '..\Include\Inventor.REC'

DIM Oin AS OffInv
DIM Inv AS Inventory
CLS
WIDTH 80, 50

OPEN "..\OfData\Inventor.MAS" FOR ISAM OffInv "Inventory" AS #1
OPEN "..\DataFile\Inventor.MAS" FOR RANDOM AS #2 LEN = LEN(Inv)

    
    SETINDEX 1, "Ser"

    FOR x = 1 TO LOF(2) / LEN(Inv)
        GET #2, x, Inv
        SEEKEQ #1, Inv.Ser
        IF NOT EOF(1) THEN
           RETRIEVE #1, Oin
           
           PRINT USING "&³"; Oin.Sto; Oin.Ser; Inv.Dtp; Oin.Dtp; Inv.Dtr; Oin.Dtr; Inv.Dta; Oin.Dta
           IF Oin.Dtr > "0" THEN Inv.Dtp = Oin.Dtr
           IF RIGHT$(Inv.Dtp, 4) > "2000" THEN Inv.Dtp = LEFT$(Inv.Dtp, 6) + "2000"
           PUT #2, x, Inv
        ELSE
           PRINT USING "&³"; Inv.Ser; "not found"
        END IF
    NEXT


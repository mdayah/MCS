'a file for creating zip code index file for the customers master file
'extract all zip codes to a file, then read file and remove duplicates

DEFINT A-Z

REM $INCLUDE: '..\Include\Customer.Rec'
REM $INCLUDE: '..\Include\ZipCodes.Rec'
    DIM Cus AS Customer
    DIM Zip AS ZipCodes
    OPEN "..\DataFile\Customer.Mas" FOR RANDOM AS #1 LEN = LEN(Cus)
    OPEN "..\DataFile\zipo" FOR RANDOM AS #2 LEN = LEN(Zip)
    KILL "..\DataFile\zipcodes.mas"
    
    FOR x = 1 TO LOF(1) \ LEN(Cus)
        GET #1, x, Cus
        Zip.Cod = Cus.Zip
        Zip.Mrk = " "
        Zip.Num = 0
        PUT #2, x, Zip
        PRINT Cus.Zip
    NEXT
    CLOSE
    SHELL "sortf ..\DataFile\zipo ..\DataFile\zipn /L8 /+1,5 /Q"
    KILL "..\DataFile\zipo"
    NAME "..\DataFile\zipn" AS "..\DataFile\zipo"

    OPEN "..\DataFile\zipo" FOR RANDOM AS #1 LEN = LEN(Zip)
    OPEN "..\DataFile\zipn" FOR RANDOM AS #2 LEN = LEN(Zip)
    n = 0
    FOR x = 1 TO LOF(1) \ LEN(Zip)
        GET #1, x, Zip
        IF Zip.Cod = z$ THEN
           s = s + 1
           Zip.Num = s
           PUT #2, n, Zip
        ELSE
           n = n + 1
           s = 1
           z$ = Zip.Cod
           Zip.Mrk = " "
           Zip.Num = s
           PUT #2, n, Zip
           PRINT Zip.Cod
        END IF
    NEXT
    CLOSE
    KILL "..\DataFile\zipo"
    NAME "..\DataFile\zipn" AS "..\DataFile\zipcodes.mas"


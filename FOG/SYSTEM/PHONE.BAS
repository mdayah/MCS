
TYPE Activity
     Rec AS STRING * 1424
END TYPE

TYPE Actnew
     Pet(5) AS LONG         'Petty cash
     Pay(5) AS LONG         'Payout
'     Tel(5) AS LONG         'Phone
     Rec AS STRING * 1424
END TYPE

DIM Act AS Activity
DIM New AS Actnew

OPEN "..\DataFile\Activity.Mas" FOR RANDOM AS #1 LEN = 1424
OPEN "..\DataFile\Activity.Tmp" FOR RANDOM AS #2 LEN = 1472
     
GET #1, 1, Act
    FOR x = 0 TO 5
        New.Pet(x) = 0
        New.Pay(x) = 0
'        New.Tel(x) = 0
    NEXT
    New.Rec = Act.Rec
PUT #2, 1, New
CLOSE

KILL "..\DataFile\Activity.Mas"
NAME "..\DataFile\Activity.Tmp" AS "..\DataFile\Activity.Mas"


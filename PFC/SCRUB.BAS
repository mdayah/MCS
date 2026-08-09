' SCRUB.BAS - Data Anonymization Utility for PFC
' =============================================
' Processed Foods Corporation ERP System
' Replaces real PII with synthetic data.
'
' Run in DOSBox: CD PFC && VBASIC\BC SCRUB.BAS /O SCRUB.EXE
'                Then: SCRUB.EXE

DEFINT A-Z

' Include record definitions
'$INCLUDE: 'BI\EMPLOYEE.REC'
'$INCLUDE: 'BI\CUSTOMER.REC'
'$INCLUDE: 'BI\VENDORS.REC'

DIM SHARED emp AS Employee
DIM SHARED cus AS Customer
DIM SHARED ven AS Vendors

' Fake data arrays
DIM SHARED FirstName$(50), LastName$(50), Company$(50)
DIM SHARED Street$(30), City$(30)

' Initialize fake data
SUB InitFakeData
    ' First names
    FirstName$(1) = "James": FirstName$(2) = "Mary"
    FirstName$(3) = "John": FirstName$(4) = "Patricia"
    FirstName$(5) = "Robert": FirstName$(6) = "Jennifer"
    FirstName$(7) = "Michael": FirstName$(8) = "Linda"
    FirstName$(9) = "William": FirstName$(10) = "Elizabeth"
    FirstName$(11) = "David": FirstName$(12) = "Barbara"
    FirstName$(13) = "Richard": FirstName$(14) = "Susan"
    FirstName$(15) = "Joseph": FirstName$(16) = "Jessica"
    FirstName$(17) = "Thomas": FirstName$(18) = "Sarah"
    FirstName$(19) = "Charles": FirstName$(20) = "Karen"
    FirstName$(21) = "Daniel": FirstName$(22) = "Nancy"
    FirstName$(23) = "Matthew": FirstName$(24) = "Lisa"
    FirstName$(25) = "Anthony": FirstName$(26) = "Betty"
    FirstName$(27) = "Mark": FirstName$(28) = "Margaret"
    FirstName$(29) = "Donald": FirstName$(30) = "Sandra"
    FirstName$(31) = "Steven": FirstName$(32) = "Ashley"
    FirstName$(33) = "Paul": FirstName$(34) = "Kimberly"
    FirstName$(35) = "Andrew": FirstName$(36) = "Emily"
    FirstName$(37) = "Joshua": FirstName$(38) = "Donna"
    FirstName$(39) = "Kenneth": FirstName$(40) = "Michelle"
    FirstName$(41) = "Kevin": FirstName$(42) = "Dorothy"
    FirstName$(43) = "Brian": FirstName$(44) = "Carol"
    FirstName$(45) = "George": FirstName$(46) = "Amanda"
    FirstName$(47) = "Edward": FirstName$(48) = "Melissa"
    FirstName$(49) = "Ronald": FirstName$(50) = "Deborah"

    ' Last names
    LastName$(1) = "Smith": LastName$(2) = "Johnson"
    LastName$(3) = "Williams": LastName$(4) = "Brown"
    LastName$(5) = "Jones": LastName$(6) = "Garcia"
    LastName$(7) = "Miller": LastName$(8) = "Davis"
    LastName$(9) = "Rodriguez": LastName$(10) = "Martinez"
    LastName$(11) = "Hernandez": LastName$(12) = "Lopez"
    LastName$(13) = "Gonzalez": LastName$(14) = "Wilson"
    LastName$(15) = "Anderson": LastName$(16) = "Thomas"
    LastName$(17) = "Taylor": LastName$(18) = "Moore"
    LastName$(19) = "Jackson": LastName$(20) = "Martin"
    LastName$(21) = "Lee": LastName$(22) = "Perez"
    LastName$(23) = "Thompson": LastName$(24) = "White"
    LastName$(25) = "Harris": LastName$(26) = "Sanchez"
    LastName$(27) = "Clark": LastName$(28) = "Ramirez"
    LastName$(29) = "Lewis": LastName$(30) = "Robinson"
    LastName$(31) = "Walker": LastName$(32) = "Young"
    LastName$(33) = "Allen": LastName$(34) = "King"
    LastName$(35) = "Wright": LastName$(36) = "Scott"
    LastName$(37) = "Torres": LastName$(38) = "Nguyen"
    LastName$(39) = "Hill": LastName$(40) = "Flores"
    LastName$(41) = "Green": LastName$(42) = "Adams"
    LastName$(43) = "Nelson": LastName$(44) = "Baker"
    LastName$(45) = "Hall": LastName$(46) = "Rivera"
    LastName$(47) = "Campbell": LastName$(48) = "Mitchell"
    LastName$(49) = "Carter": LastName$(50) = "Roberts"

    ' Company/Vendor names
    Company$(1) = "Acme Foods Inc"
    Company$(2) = "Blue Ridge Farms"
    Company$(3) = "Central Produce"
    Company$(4) = "Delta Distributors"
    Company$(5) = "Eastern Markets"
    Company$(6) = "Fresh Valley Co"
    Company$(7) = "Green Acres Farm"
    Company$(8) = "Highland Supply"
    Company$(9) = "Interstate Foods"
    Company$(10) = "Jasper Trading"
    Company$(11) = "Kingston Produce"
    Company$(12) = "Lakeview Farms"
    Company$(13) = "Mountain Fresh"
    Company$(14) = "Northern Foods"
    Company$(15) = "Orchard Supply"
    Company$(16) = "Pacific Produce"
    Company$(17) = "Quality Foods"
    Company$(18) = "Riverside Farms"
    Company$(19) = "Sunrise Markets"
    Company$(20) = "Timber Creek Co"
    Company$(21) = "United Produce"
    Company$(22) = "Valley Foods"
    Company$(23) = "Western Supply"
    Company$(24) = "Apex Distributors"
    Company$(25) = "Beacon Foods"
    Company$(26) = "Crown Markets"
    Company$(27) = "Diamond Farms"
    Company$(28) = "Elite Produce"
    Company$(29) = "Frontier Foods"
    Company$(30) = "Golden Valley"
    Company$(31) = "Harbor Foods"
    Company$(32) = "Imperial Farms"
    Company$(33) = "Junction Supply"
    Company$(34) = "Keystone Foods"
    Company$(35) = "Liberty Markets"
    Company$(36) = "Maple Grove"
    Company$(37) = "National Produce"
    Company$(38) = "Oak Hill Farms"
    Company$(39) = "Pioneer Foods"
    Company$(40) = "Royal Distributors"
    Company$(41) = "Summit Supply"
    Company$(42) = "Triangle Foods"
    Company$(43) = "Universal Markets"
    Company$(44) = "Vista Produce"
    Company$(45) = "Westgate Foods"
    Company$(46) = "York Farms"
    Company$(47) = "Zenith Supply"
    Company$(48) = "Atlas Foods"
    Company$(49) = "Brookside Farms"
    Company$(50) = "Capitol Produce"

    ' Streets
    Street$(1) = "Main Street"
    Street$(2) = "Oak Avenue"
    Street$(3) = "Maple Drive"
    Street$(4) = "Cedar Lane"
    Street$(5) = "Pine Road"
    Street$(6) = "Elm Street"
    Street$(7) = "Washington Ave"
    Street$(8) = "Park Boulevard"
    Street$(9) = "Lake Drive"
    Street$(10) = "Hill Road"
    Street$(11) = "River Street"
    Street$(12) = "Church Lane"
    Street$(13) = "School Road"
    Street$(14) = "Mill Street"
    Street$(15) = "Spring Avenue"
    Street$(16) = "Valley Road"
    Street$(17) = "Forest Drive"
    Street$(18) = "Sunset Blvd"
    Street$(19) = "Highland Ave"
    Street$(20) = "Meadow Lane"
    Street$(21) = "Center Street"
    Street$(22) = "Grove Avenue"
    Street$(23) = "Union Road"
    Street$(24) = "Market Street"
    Street$(25) = "Bridge Lane"
    Street$(26) = "Commerce Dr"
    Street$(27) = "Industrial Bl"
    Street$(28) = "Depot Street"
    Street$(29) = "Railroad Ave"
    Street$(30) = "Factory Road"

    ' Cities
    City$(1) = "Springfield"
    City$(2) = "Franklin"
    City$(3) = "Greenville"
    City$(4) = "Bristol"
    City$(5) = "Clinton"
    City$(6) = "Fairview"
    City$(7) = "Madison"
    City$(8) = "Georgetown"
    City$(9) = "Salem"
    City$(10) = "Oxford"
    City$(11) = "Arlington"
    City$(12) = "Burlington"
    City$(13) = "Manchester"
    City$(14) = "Milton"
    City$(15) = "Newport"
    City$(16) = "Oakland"
    City$(17) = "Richmond"
    City$(18) = "Clayton"
    City$(19) = "Jackson"
    City$(20) = "Lebanon"
    City$(21) = "Riverside"
    City$(22) = "Centerville"
    City$(23) = "Kingston"
    City$(24) = "Ashland"
    City$(25) = "Dover"
    City$(26) = "Hudson"
    City$(27) = "Marion"
    City$(28) = "Chester"
    City$(29) = "Plymouth"
    City$(30) = "Windsor"
END SUB

' Generate fake SSN
FUNCTION FakeSSN$ (n AS INTEGER)
    a$ = RIGHT$("000" + LTRIM$(STR$((n MOD 900) + 100)), 3)
    b$ = RIGHT$("00" + LTRIM$(STR$((n MOD 90) + 10)), 2)
    c$ = RIGHT$("0000" + LTRIM$(STR$((n MOD 9000) + 1000)), 4)
    FakeSSN$ = a$ + "-" + b$ + "-" + c$
END FUNCTION

' Generate fake phone
FUNCTION FakePhone$ (n AS INTEGER)
    a$ = RIGHT$("000" + LTRIM$(STR$((n MOD 800) + 200)), 3)
    b$ = RIGHT$("0000" + LTRIM$(STR$((n MOD 9000) + 1000)), 4)
    FakePhone$ = "555-" + a$ + "-" + b$
END FUNCTION

' Generate fake zip
FUNCTION FakeZip$ (n AS INTEGER)
    FakeZip$ = RIGHT$("00000" + LTRIM$(STR$((n MOD 90000) + 10000)), 5)
END FUNCTION

' Pad string to length
FUNCTION PadStr$ (s$, length AS INTEGER)
    IF LEN(s$) >= length THEN
        PadStr$ = LEFT$(s$, length)
    ELSE
        PadStr$ = s$ + SPACE$(length - LEN(s$))
    END IF
END FUNCTION

' ============================================
' MAIN PROGRAM
' ============================================

PRINT "PFC Data Anonymization Utility"
PRINT "=============================="
PRINT
PRINT "Processed Foods Corporation ERP"
PRINT

InitFakeData

' -------------------------------------------
' Process PR\EMPLOYEE.MAS
' -------------------------------------------
PRINT "Processing PR\EMPLOYEE.MAS..."

IF DIR$("PR\EMPLOYEE.MAS") = "" THEN
    PRINT "  File not found, skipping."
ELSE
    OPEN "PR\EMPLOYEE.MAS" FOR RANDOM AS #1 LEN = LEN(emp)
    NumRec& = LOF(1) \ LEN(emp)

    IF NumRec& = 0 THEN
        PRINT "  Empty file, skipping."
    ELSE
        count = 0
        FOR i& = 1 TO NumRec&
            GET #1, i&, emp
            IF LTRIM$(emp.Num) <> "" THEN
                count = count + 1
                n = count

                fn = (n MOD 50) + 1
                ln = ((n * 7) MOD 50) + 1
                emp.Nam = PadStr$(FirstName$(fn) + " " + LastName$(ln), 24)

                sn = (n MOD 30) + 1
                emp.Add = PadStr$(LTRIM$(STR$(100 + n)) + " " + Street$(sn), 24)

                ct = (n MOD 30) + 1
                emp.Cty = PadStr$(City$(ct), 15)
                emp.Zip = FakeZip$(n)

                emp.SSn = FakeSSN$(n)
                emp.Tel = PadStr$(FakePhone$(n), 11)

                PUT #1, i&, emp
            END IF
        NEXT i&
        PRINT "  Processed"; count; "employee records."
    END IF
    CLOSE #1
END IF

' -------------------------------------------
' Process AR\CUSTOMER.MAS
' -------------------------------------------
PRINT "Processing AR\CUSTOMER.MAS..."

IF DIR$("AR\CUSTOMER.MAS") = "" THEN
    PRINT "  File not found, skipping."
ELSE
    OPEN "AR\CUSTOMER.MAS" FOR RANDOM AS #1 LEN = LEN(cus)
    NumRec& = LOF(1) \ LEN(cus)

    IF NumRec& = 0 THEN
        PRINT "  Empty file, skipping."
    ELSE
        count = 0
        FOR i& = 1 TO NumRec&
            GET #1, i&, cus
            IF LTRIM$(cus.Num) <> "" THEN
                count = count + 1
                n = count

                cn = (n MOD 50) + 1
                cus.Nam = PadStr$(Company$(cn), 28)

                fn = (n MOD 50) + 1
                ln = ((n * 7) MOD 50) + 1
                cus.Man = PadStr$(FirstName$(fn) + " " + LastName$(ln), 28)

                cus.Tel = PadStr$(FakePhone$(n), 12)
                cus.Fax = PadStr$(FakePhone$(n + 100), 12)

                sn = (n MOD 30) + 1
                cus.Add = PadStr$(LTRIM$(STR$(100 + n)) + " " + Street$(sn), 28)
                cus.Str = PadStr$("Suite " + LTRIM$(STR$(n MOD 500)), 28)

                ct = (n MOD 30) + 1
                cus.Cty = PadStr$(City$(ct), 14)
                cus.Zip = PadStr$(FakeZip$(n), 10)

                ' Shipping same as billing
                cus.Sco = cus.Man
                cus.Ste = cus.Tel
                cus.Sad = cus.Add
                cus.SSr = cus.Str
                cus.SCt = cus.Cty
                cus.SZi = cus.Zip

                cus.Acc = PadStr$("Account Contact", 28)
                cus.Aph = cus.Tel
                cus.Afx = cus.Fax

                PUT #1, i&, cus
            END IF
        NEXT i&
        PRINT "  Processed"; count; "customer records."
    END IF
    CLOSE #1
END IF

' -------------------------------------------
' Process AP\VENDORS.MAS
' -------------------------------------------
PRINT "Processing AP\VENDORS.MAS..."

IF DIR$("AP\VENDORS.MAS") = "" THEN
    PRINT "  File not found, skipping."
ELSE
    OPEN "AP\VENDORS.MAS" FOR RANDOM AS #1 LEN = LEN(ven)
    NumRec& = LOF(1) \ LEN(ven)

    IF NumRec& = 0 THEN
        PRINT "  Empty file, skipping."
    ELSE
        count = 0
        FOR i& = 1 TO NumRec&
            GET #1, i&, ven
            IF LTRIM$(ven.Num) <> "" THEN
                count = count + 1
                n = count

                cn = ((n + 25) MOD 50) + 1
                ven.Nam = PadStr$(Company$(cn), 28)

                fn = (n MOD 50) + 1
                ln = ((n * 7) MOD 50) + 1
                ven.Ord = PadStr$(FirstName$(fn) + " " + LastName$(ln), 28)
                ven.Cnt = ven.Ord

                sn = (n MOD 30) + 1
                ven.Add = PadStr$(LTRIM$(STR$(100 + n)) + " " + Street$(sn), 28)
                ven.Str = PadStr$("P.O. Box " + LTRIM$(STR$(n * 10)), 28)

                ct = (n MOD 30) + 1
                ven.Cty = PadStr$(City$(ct), 14)
                ven.Zip = PadStr$(FakeZip$(n), 10)

                ven.Tel = PadStr$(FakePhone$(n), 12)
                ven.Fax = PadStr$(FakePhone$(n + 100), 12)

                PUT #1, i&, ven
            END IF
        NEXT i&
        PRINT "  Processed"; count; "vendor records."
    END IF
    CLOSE #1
END IF

PRINT
PRINT "Anonymization complete!"

END

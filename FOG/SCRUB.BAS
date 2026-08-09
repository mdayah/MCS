' SCRUB.BAS - Data Anonymization Utility for FOG
' =============================================
' Replaces real PII with synthetic data while preserving
' record structure and non-PII business data.
'
' FOG contains highly sensitive data:
' - Social Security Numbers (for 2 people per customer)
' - Driver's License Numbers
' - Dates of Birth
' - Employer/Income information
' - User passwords
'
' Run in DOSBox: CD FOG && ..\VBASIC\BC SCRUB.BAS /O SCRUB.EXE
'                Then: SCRUB.EXE

DEFINT A-Z

' Include record definitions
'$INCLUDE: 'INCLUDE\CUSTOMER.REC'
'$INCLUDE: 'INCLUDE\PASSWORD.REC'

DIM SHARED cus AS Customer
DIM SHARED cidx AS CusIndex
DIM SHARED pwd AS Password

' Fake data arrays
DIM SHARED FirstName$(50), LastName$(50), Employer$(30)
DIM SHARED Street$(30), City$(30), State$(50)

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

    ' Employer names
    Employer$(1) = "Acme Corp"
    Employer$(2) = "Tech Systems"
    Employer$(3) = "City Hospital"
    Employer$(4) = "County Schools"
    Employer$(5) = "State Univ"
    Employer$(6) = "Metro Transit"
    Employer$(7) = "First Bank"
    Employer$(8) = "Auto Parts Co"
    Employer$(9) = "Food Mart"
    Employer$(10) = "Construction"
    Employer$(11) = "Electric Co"
    Employer$(12) = "Phone Company"
    Employer$(13) = "Insurance Inc"
    Employer$(14) = "Law Office"
    Employer$(15) = "Medical Grp"
    Employer$(16) = "Restaurant"
    Employer$(17) = "Retail Store"
    Employer$(18) = "Factory Inc"
    Employer$(19) = "Warehouse Co"
    Employer$(20) = "Shipping Inc"
    Employer$(21) = "Self Employed"
    Employer$(22) = "Retired"
    Employer$(23) = "Government"
    Employer$(24) = "Military"
    Employer$(25) = "Non-Profit"
    Employer$(26) = "Consulting"
    Employer$(27) = "Real Estate"
    Employer$(28) = "Accounting"
    Employer$(29) = "Engineering"
    Employer$(30) = "Sales Corp"

    ' Street names
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

    ' State codes
    State$(1) = "AL": State$(2) = "AK": State$(3) = "AZ"
    State$(4) = "AR": State$(5) = "CA": State$(6) = "CO"
    State$(7) = "CT": State$(8) = "DE": State$(9) = "FL"
    State$(10) = "GA": State$(11) = "HI": State$(12) = "ID"
    State$(13) = "IL": State$(14) = "IN": State$(15) = "IA"
    State$(16) = "KS": State$(17) = "KY": State$(18) = "LA"
    State$(19) = "ME": State$(20) = "MD": State$(21) = "MA"
    State$(22) = "MI": State$(23) = "MN": State$(24) = "MS"
    State$(25) = "MO": State$(26) = "MT": State$(27) = "NE"
    State$(28) = "NV": State$(29) = "NH": State$(30) = "NJ"
    State$(31) = "NM": State$(32) = "NY": State$(33) = "NC"
    State$(34) = "ND": State$(35) = "OH": State$(36) = "OK"
    State$(37) = "OR": State$(38) = "PA": State$(39) = "RI"
    State$(40) = "SC": State$(41) = "SD": State$(42) = "TN"
    State$(43) = "TX": State$(44) = "UT": State$(45) = "VT"
    State$(46) = "VA": State$(47) = "WA": State$(48) = "WV"
    State$(49) = "WI": State$(50) = "WY"
END SUB

' Generate fake SSN: ###-##-####
FUNCTION FakeSSN$ (n AS INTEGER)
    a$ = RIGHT$("000" + LTRIM$(STR$((n MOD 900) + 100)), 3)
    b$ = RIGHT$("00" + LTRIM$(STR$((n MOD 90) + 10)), 2)
    c$ = RIGHT$("0000" + LTRIM$(STR$((n MOD 9000) + 1000)), 4)
    FakeSSN$ = a$ + "-" + b$ + "-" + c$
END FUNCTION

' Generate fake Driver's License
FUNCTION FakeDL$ (n AS INTEGER)
    ' Format: XX#######
    st = (n MOD 50) + 1
    num$ = RIGHT$("0000000" + LTRIM$(STR$(n * 12345 MOD 10000000)), 7)
    FakeDL$ = State$(st) + num$
END FUNCTION

' Generate fake DOB: MM/DD/YYYY
FUNCTION FakeDOB$ (n AS INTEGER)
    ' Generate ages 25-75
    yr = 1950 + (n MOD 50)
    mo = (n MOD 12) + 1
    dy = (n MOD 28) + 1
    FakeDOB$ = RIGHT$("00" + LTRIM$(STR$(mo)), 2) + "/" + RIGHT$("00" + LTRIM$(STR$(dy)), 2) + "/" + LTRIM$(STR$(yr))
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

' Generate fake income
FUNCTION FakeIncome$ (n AS INTEGER)
    amt& = 20000 + (n MOD 80) * 1000
    FakeIncome$ = "$" + LTRIM$(STR$(amt&))
END FUNCTION

' Pad string to length
FUNCTION PadStr$ (s$, length AS INTEGER)
    IF LEN(s$) >= length THEN
        PadStr$ = LEFT$(s$, length)
    ELSE
        PadStr$ = s$ + SPACE$(length - LEN(s$))
    END IF
END FUNCTION

' Process a customer file
SUB ProcessCustomerFile (filename$)
    PRINT "Processing "; filename$; "..."

    IF DIR$(filename$) = "" THEN
        PRINT "  File not found, skipping."
        EXIT SUB
    END IF

    OPEN filename$ FOR RANDOM AS #1 LEN = LEN(cus)
    NumRec& = LOF(1) \ LEN(cus)
    count = 0

    FOR i& = 1 TO NumRec&
        GET #1, i&, cus
        IF LTRIM$(cus.Num) <> "" THEN
            count = count + 1
            n = count

            ' === PRIMARY PERSON ===

            ' Name
            fn = (n MOD 50) + 1
            ln = ((n * 7) MOD 50) + 1
            cus.Nam = PadStr$(FirstName$(fn) + " " + LastName$(ln), 26)

            ' Address
            sn = (n MOD 30) + 1
            cus.Add = PadStr$(LTRIM$(STR$(100 + n)) + " " + Street$(sn), 26)
            cus.Str = PadStr$("Apt " + LTRIM$(STR$(n MOD 200)), 26)

            ' City, Zip
            ct = (n MOD 30) + 1
            cus.Cty = PadStr$(City$(ct), 17)
            cus.Zip = FakeZip$(n)

            ' Phone
            cus.Tel = PadStr$(FakePhone$(n), 12)

            ' CRITICAL PII
            cus.SSn = FakeSSN$(n)
            cus.Dln = FakeDL$(n)
            cus.Dob = FakeDOB$(n)

            ' Employment
            emp = (n MOD 30) + 1
            cus.Emp = PadStr$(Employer$(emp), 15)
            cus.Wor = PadStr$(FakePhone$(n + 500), 12)
            cus.Inc = PadStr$(FakeIncome$(n), 15)
            cus.Oth = PadStr$("$0", 15)

            ' === SECOND PERSON (co-applicant) ===

            ' Name (different from primary)
            fn2 = ((n + 25) MOD 50) + 1
            ln2 = ((n * 11) MOD 50) + 1
            cus.Nm1 = PadStr$(FirstName$(fn2) + " " + LastName$(ln2), 26)

            ' CRITICAL PII for second person
            cus.SS1 = FakeSSN$(n + 5000)
            cus.Dl1 = FakeDL$(n + 5000)
            cus.Bd1 = FakeDOB$(n + 20)

            ' Employment for second person
            emp2 = ((n + 15) MOD 30) + 1
            cus.Em1 = PadStr$(Employer$(emp2), 15)
            cus.Wt1 = PadStr$(FakePhone$(n + 1000), 12)
            cus.In1 = PadStr$(FakeIncome$(n + 10), 15)
            cus.Ot1 = PadStr$("$0", 15)

            PUT #1, i&, cus
        END IF
    NEXT i&

    CLOSE #1
    PRINT "  Processed"; count; "customer records."
END SUB

' ============================================
' MAIN PROGRAM
' ============================================

PRINT "FOG Data Anonymization Utility"
PRINT "=============================="
PRINT
PRINT "WARNING: This will replace SSN, DL, DOB and other"
PRINT "         sensitive data with synthetic values."
PRINT

InitFakeData

' Process all customer files
ProcessCustomerFile "DATAFILE\CUSTOMER.MAS"
ProcessCustomerFile "OFDATA\CUSTOMER.MAS"
ProcessCustomerFile "SYSTEM\CUSTOMER.MAS"

' -------------------------------------------
' Process PASSWORD.MAS
' -------------------------------------------
PRINT "Processing MASTER\PASSWORD.MAS..."

IF DIR$("MASTER\PASSWORD.MAS") = "" THEN
    PRINT "  File not found, skipping."
ELSE
    OPEN "MASTER\PASSWORD.MAS" FOR RANDOM AS #1 LEN = LEN(pwd)
    NumRec& = LOF(1) \ LEN(pwd)
    count = 0

    FOR i& = 1 TO NumRec&
        GET #1, i&, pwd
        IF LTRIM$(pwd.Pas) <> "" THEN
            count = count + 1
            n = count

            ' Replace password with generic
            pwd.Pas = PadStr$("P" + RIGHT$("000" + LTRIM$(STR$(n)), 3), 4)

            ' Replace name
            fn = (n MOD 50) + 1
            ln = ((n * 7) MOD 50) + 1
            pwd.Nam = PadStr$(FirstName$(fn) + " " + LEFT$(LastName$(ln), 1) + ".", 19)

            ' Replace initials
            pwd.Ini = LEFT$(FirstName$(fn), 1) + LEFT$(LastName$(ln), 1) + LEFT$(LastName$(ln), 1)

            PUT #1, i&, pwd
        END IF
    NEXT i&

    CLOSE #1
    PRINT "  Processed"; count; "password records."
END IF

PRINT
PRINT "Anonymization complete!"
PRINT "Original data has been replaced with synthetic data."
PRINT
PRINT "Files processed:"
PRINT "  - DATAFILE\CUSTOMER.MAS (SSN, DL, DOB, addresses)"
PRINT "  - OFDATA\CUSTOMER.MAS (SSN, DL, DOB, addresses)"
PRINT "  - SYSTEM\CUSTOMER.MAS (SSN, DL, DOB, addresses)"
PRINT "  - MASTER\PASSWORD.MAS (passwords, user names)"

END

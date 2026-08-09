' SCRUB.BAS - Data Anonymization Utility for RDL
' =============================================
' Replaces real PII with synthetic data for the
' dental laboratory billing system.
'
' Run in DOSBox: CD RDL && VBASIC\BC SCRUB.BAS /O SCRUB.EXE
'                Then: SCRUB.EXE

DEFINT A-Z

' Customer record (from AR.BAS)
TYPE Customer
   Num AS STRING * 2
   Nam AS STRING * 30
   Add AS STRING * 30
   Cty AS STRING * 20
   Sta AS STRING * 2
   Zip AS STRING * 5
   Tel AS STRING * 14
   Crd AS LONG
   Mbf AS LONG: Mgr AS LONG: Mtx AS LONG: Mpd AS LONG
   Ybf AS LONG: Ygr AS LONG: Ytx AS LONG: Ypd AS LONG
END TYPE

DIM SHARED cus AS Customer

' Fake data arrays
DIM SHARED DentalLab$(40), City$(30), Street$(30)

' Initialize fake dental lab names
SUB InitFakeData
    ' Dental laboratory names
    DentalLab$(1) = "Precision Dental Lab"
    DentalLab$(2) = "Crown Masters Inc"
    DentalLab$(3) = "Artisan Dental Works"
    DentalLab$(4) = "Quality Ceramics Lab"
    DentalLab$(5) = "Elite Dental Studio"
    DentalLab$(6) = "Perfect Smile Lab"
    DentalLab$(7) = "ProDent Ceramics"
    DentalLab$(8) = "DentalCraft Inc"
    DentalLab$(9) = "Apex Dental Lab"
    DentalLab$(10) = "Summit Prosthetics"
    DentalLab$(11) = "Valley Dental Works"
    DentalLab$(12) = "Metro Dental Lab"
    DentalLab$(13) = "Coastal Ceramics"
    DentalLab$(14) = "Highland Dental"
    DentalLab$(15) = "Premier Dental Lab"
    DentalLab$(16) = "Advanced Dental Tech"
    DentalLab$(17) = "Classic Dental Lab"
    DentalLab$(18) = "Modern Dental Works"
    DentalLab$(19) = "Superior Ceramics"
    DentalLab$(20) = "Dental Arts Studio"
    DentalLab$(21) = "Pacific Dental Lab"
    DentalLab$(22) = "Atlantic Prosthetics"
    DentalLab$(23) = "Midtown Dental"
    DentalLab$(24) = "Sunrise Dental Lab"
    DentalLab$(25) = "Golden State Dental"
    DentalLab$(26) = "Silver Crown Lab"
    DentalLab$(27) = "Diamond Dental Works"
    DentalLab$(28) = "Crystal Ceramics"
    DentalLab$(29) = "Nationwide Dental"
    DentalLab$(30) = "Regional Dental Lab"
    DentalLab$(31) = "Community Dental"
    DentalLab$(32) = "Family Dental Lab"
    DentalLab$(33) = "Tri-State Dental"
    DentalLab$(34) = "Central Ceramics"
    DentalLab$(35) = "Northern Dental Lab"
    DentalLab$(36) = "Southern Prosthetics"
    DentalLab$(37) = "Eastern Dental Works"
    DentalLab$(38) = "Western Dental Lab"
    DentalLab$(39) = "United Dental Tech"
    DentalLab$(40) = "Alliance Dental Lab"

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
    Street$(11) = "Medical Plaza"
    Street$(12) = "Professional Dr"
    Street$(13) = "Commerce Blvd"
    Street$(14) = "Industrial Way"
    Street$(15) = "Tech Center Dr"
    Street$(16) = "Business Park"
    Street$(17) = "Office Court"
    Street$(18) = "Corporate Way"
    Street$(19) = "Executive Dr"
    Street$(20) = "Enterprise Ave"
    Street$(21) = "Center Street"
    Street$(22) = "Grove Avenue"
    Street$(23) = "Union Road"
    Street$(24) = "Market Street"
    Street$(25) = "Bridge Lane"
    Street$(26) = "Commerce Drive"
    Street$(27) = "Industrial Blvd"
    Street$(28) = "Depot Street"
    Street$(29) = "Railroad Avenue"
    Street$(30) = "Factory Road"
END SUB

' Generate fake phone
FUNCTION FakePhone$ (n AS INTEGER)
    a$ = RIGHT$("000" + LTRIM$(STR$((n MOD 800) + 200)), 3)
    b$ = RIGHT$("0000" + LTRIM$(STR$((n MOD 9000) + 1000)), 4)
    FakePhone$ = "(555) " + a$ + "-" + b$
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

PRINT "RDL Data Anonymization Utility"
PRINT "=============================="
PRINT
PRINT "Dental Laboratory Billing System"
PRINT

InitFakeData

' -------------------------------------------
' Process CUSTOMER.DTA
' -------------------------------------------
PRINT "Processing AR\CUSTOMER.DTA..."

IF DIR$("AR\CUSTOMER.DTA") = "" THEN
    PRINT "  File not found, skipping."
ELSE
    OPEN "AR\CUSTOMER.DTA" FOR RANDOM AS #1 LEN = LEN(cus)
    NumRec& = LOF(1) \ LEN(cus)
    count = 0

    FOR i& = 1 TO NumRec&
        GET #1, i&, cus
        IF LTRIM$(cus.Num) <> "" THEN
            count = count + 1
            n = count

            ' Dental lab name
            dn = (n MOD 40) + 1
            cus.Nam = PadStr$(DentalLab$(dn), 30)

            ' Address
            sn = (n MOD 30) + 1
            cus.Add = PadStr$(LTRIM$(STR$(100 + n)) + " " + Street$(sn), 30)

            ' City
            ct = (n MOD 30) + 1
            cus.Cty = PadStr$(City$(ct), 20)

            ' Zip
            cus.Zip = FakeZip$(n)

            ' Phone
            cus.Tel = PadStr$(FakePhone$(n), 14)

            PUT #1, i&, cus
        END IF
    NEXT i&

    CLOSE #1
    PRINT "  Processed"; count; "customer records."
END IF

PRINT
PRINT "Anonymization complete!"
PRINT "Customer names, addresses, and phone numbers replaced."

END

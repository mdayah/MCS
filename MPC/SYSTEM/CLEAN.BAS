TYPE Accounts
    Num AS STRING * 2
    Nam AS STRING * 27
    Typ AS STRING * 1: Crd AS STRING * 2
    Mbb AS LONG: Mdb AS LONG: Mcr AS LONG
    Ybb AS LONG: Ydb AS LONG: Ycr AS LONG
    Fil AS STRING * 1
END TYPE

TYPE Vendors
    Num AS STRING * 4
    Nam AS STRING * 28
    '------------------Payment Address
    Con AS STRING * 28
    Add AS STRING * 28
    Str AS STRING * 28
    Cty AS STRING * 14
    Sta AS STRING * 2
    Zip AS STRING * 10
    Tel AS STRING * 12
    Fax AS STRING * 12
    '------------------Company Address
    Co1 AS STRING * 28
    Ad1 AS STRING * 28
    Sr1 AS STRING * 28
    Ct1 AS STRING * 14
    St1 AS STRING * 2
    Zi1 AS STRING * 10
    Te1 AS STRING * 12
    Fa1 AS STRING * 12
    '------------------Information
    Pay AS STRING * 1
    Dis AS STRING * 2
    Trm AS STRING * 2
    Act AS STRING * 12
    Mbb AS LONG: Mgr AS LONG: Mds AS LONG: Mcr AS LONG: Mpd AS LONG
    Ybb AS LONG: Ygr AS LONG: Yds AS LONG: Ycr AS LONG: Ypd AS LONG
END TYPE

TYPE Customer
    Cod AS STRING * 4
    Nam AS STRING * 28

    Man AS STRING * 28       'billing address
    Tel AS STRING * 14
    Add AS STRING * 28
    Str AS STRING * 28
    Cty AS STRING * 14
    Sta AS STRING * 2
    Zip AS STRING * 10

    Sco AS STRING * 28       'shipping address
    Ste AS STRING * 14
    Sad AS STRING * 28
    SSr AS STRING * 28
    SCt AS STRING * 14
    SSt AS STRING * 2
    SZi AS STRING * 10

    Typ AS STRING * 1        'credit information
    Dat AS STRING * 10
    Ban AS STRING * 28
    Acc AS STRING * 15
    Tax AS STRING * 15
    Dis AS STRING * 2
    Ter AS STRING * 2
    Pay AS STRING * 1        'cash=C credit=R
    Fil AS STRING * 29

    Crd AS LONG: Chg AS LONG 'credit & service charge
    Mbb AS LONG: Ybb AS LONG 'begining balance
    Mgr AS LONG: Ygr AS LONG 'gross sales
    Mcr AS LONG: Ycr AS LONG 'credits
    Mtx AS LONG: Ytx AS LONG 'sales tax
    Mfr AS LONG: Yfr AS LONG 'freight charge
    Mnt AS LONG: Ynt AS LONG 'net amount
    Mpd AS LONG: Ypd AS LONG 'receipts
    Mco AS LONG: Yco AS LONG 'cost of goods
END TYPE

TYPE Material
    Num AS STRING * 3   'item number
    Typ AS STRING * 3   'department
    Nam AS STRING * 26  'description
    Fra AS STRING * 4   'fraction
    Mes AS STRING * 2   'measure
    Con AS STRING * 2   'container
    Rpt AS STRING * 5   'reorder point
    Rqt AS STRING * 5   'reorder quantity
    Ref AS STRING * 7   'reference price
    Cnt AS STRING * 8   'physical count
    Prc AS STRING * 8   'refernce price
    Fil AS STRING * 14  'filler
    Mbq AS LONG: Mbp AS LONG: Ybq AS LONG: Ybp AS LONG 'Beg Baclance
    Mpq AS LONG: Mpp AS LONG: Ypq AS LONG: Ypp AS LONG 'Purchases
    Mmq AS LONG: Mmp AS LONG: Ymq AS LONG: Ymp AS LONG 'Production
    Mkq AS LONG: Mkp AS LONG: Ykq AS LONG: Ykp AS LONG 'Requisitions
    Msq AS LONG: Msc AS LONG: Ysq AS LONG: Ysc AS LONG 'Cost of Sales
    Mwq AS LONG: Mwp AS LONG: Ywq AS LONG: Ywp AS LONG 'Spoilage
    Ygq AS LONG: Ygp AS LONG                           'Gains or Loss
    Msp AS LONG: Ysp AS LONG                           'Sales
END TYPE

TYPE Employee
    Dpt AS STRING * 1          'Department
    Num AS STRING * 3          'Employee number
    Nam AS STRING * 24         'Employee name
    Add AS STRING * 24         'Address
    Cty AS STRING * 15         'City
    Sta AS STRING * 2          'State
    Zip AS STRING * 5          'Zip Code
    SSn AS STRING * 11         'Social Security
    Tel AS STRING * 12         'Telephone
    Hir AS STRING * 10         'Date Hired
    Rel AS STRING * 10         'Date released
    Typ AS STRING * 1          'Salary or wages
    Mar AS STRING * 1          'Marital Status
    Dep AS STRING * 1          'Dependants
    Tax AS STRING * 1          'Tax method
    Pay AS STRING * 8          'Wages
    Ins AS STRING * 8          'Insurance
    Ded AS STRING * 8          'Other deductions
    Bon AS STRING * 8          'Bonus pay
    Cal AS STRING * 1          'Calculated
    Mrh AS LONG: Qrh AS LONG: Yrh AS LONG  'Regular hours
    Moh AS LONG: Qoh AS LONG: Yoh AS LONG  'Overtime hours
    Mrp AS LONG: Qrp AS LONG: Yrp AS LONG  'Regular pay
    Mop AS LONG: Qop AS LONG: Yop AS LONG  'Overtime pay
    Mbo AS LONG: Qbo AS LONG: Ybo AS LONG  'Bonus pay
    Mgr AS LONG: Qgr AS LONG: Ygr AS LONG  'Gross pay
    Mft AS LONG: Qft AS LONG: Yft AS LONG  'FIT
    Mfi AS LONG: Qfi AS LONG: Yfi AS LONG  'FICA
    Mmd AS LONG: Qmd AS LONG: Ymd AS LONG  'Medicare
    Min AS LONG: Qin AS LONG: Yin AS LONG  'Insurance
    Mod AS LONG: Qod AS LONG: Yod AS LONG  'Other Deductions
    Mnt AS LONG: Qnt AS LONG: Ynt AS LONG  'Net pay

    Clk(1, 6, 3) AS STRING * 4 'hours in and out
    Hrs(1, 9)    AS STRING * 6 'total hours
    Tot(2)       AS STRING * 6 'tot, reg, ovt
END TYPE

    DIM SHARED Act AS Accounts
    DIM SHARED Ven AS Vendors
    DIM SHARED Cus AS Customer
    DIM SHARED Mat AS Material
    DIM SHARED Emp AS Employee

OPEN "..\Data\Accounts.MAS" FOR RANDOM AS #1 LEN = LEN(Act)
OPEN "..\Data\Vendors .MAS" FOR RANDOM AS #2 LEN = LEN(Ven)
OPEN "..\Data\Customer.MAS" FOR RANDOM AS #3 LEN = LEN(Cus)
OPEN "..\Data\Material.MAS" FOR RANDOM AS #4 LEN = LEN(Mat)
OPEN "..\Data\Employee.MAS" FOR RANDOM AS #5 LEN = LEN(Emp)

Accounts:
    FOR x = 1 TO LOF(1) / LEN(Act)
    GET #1, x, Act
        Act.Mbb = 0: Act.Ybb = 0
        Act.Mdb = 0: Act.Ydb = 0
        Act.Mcr = 0: Act.Ycr = 0
    PUT #1, x, Act
    NEXT
Vendors:
    FOR x = 1 TO LOF(2) / LEN(Ven)
    GET #2, x, Ven
        Ven.Mbb = 0: Ven.Ybb = 0
        Ven.Mgr = 0: Ven.Ygr = 0
        Ven.Mds = 0: Ven.Yds = 0
        Ven.Mcr = 0: Ven.Ycr = 0
        Ven.Mpd = 0: Ven.Ypd = 0
    PUT #2, x, Ven
    NEXT
Customers:
    FOR x = 1 TO LOF(3) / LEN(Cus)
    GET #3, x, Cus
        Cus.Crd = 0: Cus.Chg = 0
        Cus.Mbb = 0: Cus.Ybb = 0
        Cus.Mgr = 0: Cus.Ygr = 0
        Cus.Mcr = 0: Cus.Ycr = 0
        Cus.Mtx = 0: Cus.Ytx = 0
        Cus.Mfr = 0: Cus.Yfr = 0
        Cus.Mnt = 0: Cus.Ynt = 0
        Cus.Mpd = 0: Cus.Ypd = 0
        Cus.Mco = 0: Cus.Yco = 0
    PUT #3, x, Cus
    NEXT
Materials:
    FOR x = 1 TO LOF(4) / LEN(Mat)
    GET #4, x, Mat
        Mat.Mbq = 0: Mat.Mbp = 0: Mat.Ybq = 0: Mat.Ybp = 0
        Mat.Mpq = 0: Mat.Mpp = 0: Mat.Ypq = 0: Mat.Ypp = 0
        Mat.Mmq = 0: Mat.Mmp = 0: Mat.Ymq = 0: Mat.Ymp = 0
        Mat.Mkq = 0: Mat.Mkp = 0: Mat.Ykq = 0: Mat.Ykp = 0
        Mat.Msq = 0: Mat.Msc = 0: Mat.Ysq = 0: Mat.Ysc = 0
        Mat.Mwq = 0: Mat.Mwp = 0: Mat.Ywq = 0: Mat.Ywp = 0
        Mat.Ygq = 0: Mat.Ygp = 0
        Mat.Msp = 0: Mat.Ysp = 0
    PUT #4, x, Mat
    NEXT
Employee:
    FOR x = 1 TO LOF(5) / LEN(Emp)
    GET #5, x, Emp
        Emp.Mrh = 0: Emp.Qrh = 0: Emp.Yrh = 0
        Emp.Moh = 0: Emp.Qoh = 0: Emp.Yoh = 0
        Emp.Mrp = 0: Emp.Qrp = 0: Emp.Yrp = 0
        Emp.Mop = 0: Emp.Qop = 0: Emp.Yop = 0
        Emp.Mbo = 0: Emp.Qbo = 0: Emp.Ybo = 0
        Emp.Mgr = 0: Emp.Qgr = 0: Emp.Ygr = 0
        Emp.Mft = 0: Emp.Qft = 0: Emp.Yft = 0
        Emp.Mfi = 0: Emp.Qfi = 0: Emp.Yfi = 0
        Emp.Mmd = 0: Emp.Qmd = 0: Emp.Ymd = 0
        Emp.Min = 0: Emp.Qin = 0: Emp.Yin = 0
        Emp.Mod = 0: Emp.Qod = 0: Emp.Yod = 0
        Emp.Mnt = 0: Emp.Qnt = 0: Emp.Ynt = 0
    PUT #5, x, Emp
    NEXT


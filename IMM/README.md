# IMM - Payroll System (bureau client install)

## Overview

IMM is one of several per-client installs of the same payroll product (compare BAJ, HBS, MCS): the ~2,000-line payroll engine configured for a single bookkeeping/payroll client. **The client behind the initials "IMM" is not identified in this public archive** — the earlier "Integrated Mechanical Manager" expansion was an unverified guess, not the developer's own label.

## System Classification

| Attribute | Value |
|-----------|-------|
| Type | Payroll/Accounting Subset |
| Language | Visual Basic for MS-DOS |
| Era | 1991-1995 |
| Status | Payroll-focused system |
| Modules | 3 modules |

## Module Overview

| Module | File | Size | Purpose | Lines |
|--------|------|------|---------|-------|
| **AC** | AC.BAS | 12K | Main Menu | ~375 |
| **PR** | PR.BAS | 71K | Payroll | ~2,100 |
| **OP** | OP.BAS | 5K | Operations | ~160 |

## Directory Structure

```
IMM/
├── AC/                 # Accounting Application (288K)
│   ├── AC.BAS         # Main menu
│   ├── PR.BAS         # Payroll module (most developed)
│   ├── OP.BAS         # Operations
│   ├── AC.EXE         # Compiled executable (151K)
│   ├── AC.SLB         # Screen library (17K)
│   ├── AC.MAS         # Master configuration (5.6K)
│   └── edit_ac.bat    # Editor batch file
│
├── BI/                 # Data Definitions (2.9K)
│   ├── LIBRARY.REC    # Shared declarations
│   ├── ACCOUNTS.REC   # Account structure
│   └── EMPLOYEE.REC   # Employee structure
│
├── LIB/                # Utility libraries (896K, submodule)
├── VBASIC/             # Development environment (9.8M, submodule)
└── dosbox.sh           # DOSBox launcher
```

## Key Features

### Main Menu (AC)
- Menu-driven interface
- Multi-client support
- Menu dispatch logic
- Error handling and recovery

### Payroll (PR) - Primary Module
Comprehensive payroll with employee and check management:
- **Employee Functions**
  - EmpSort, EmpShow, EmpEdit, EmpWind
- **Check Processing**
  - ChkShow, ChkWind, ChkPrnt, ChkPost, ChkVoid
- **Account Display**
  - AccShow - Account reconciliation
- **Tax Calculations**
  - Federal Income Tax
  - Social Security (FICA)
  - Medicare
- **Reporting**
  - Employee lists and master lists
  - Check registers
  - Monthly, quarterly, annual reports
  - Time card reports
  - W-2 generation
- **Number to Words**
  - Check amount conversion

### Operations (OP)
- Company/client profile management
  - optCreate - Create new client
  - optModify - Modify client
  - optSelect - Select client
  - optDate - Date operations
- Configuration file management (Config.MAS)

## Data Structures

### Employee Record (Complex)
```basic
TYPE Employee
  ' Personal Info
  Department, Number (3-digit), Name (24 chars)
  Address: Street, City, State, Zip
  SSN, Phone
  Hire/Release dates

  ' Employment
  Type (Salary/Wages)
  Marital status, Dependents, Tax method
  Base pay, Insurance, Deductions, Bonus

  ' Hours Tracking (36 numeric fields)
  Regular, Overtime 1.5x, Double-time
  Monthly, Quarterly, Yearly totals

  ' Pay Tracking
  Regular pay, Overtime pay, Bonus, Gross
  Taxes: FIT, FICA, Medicare
  Insurance, Other deductions, Net pay

  ' Time Clock
  In/Out times for multiple periods
END TYPE
```

### Index Structures
```basic
TYPE EmpIndex
  Department, Name, Number
END TYPE

TYPE EmpCheck
  Employee number, Date, Check number
  Hours and pay details
END TYPE
```

## Comparison with Similar Systems

| Feature | IMM | BAJ | HBS | MCS |
|---------|-----|-----|-----|-----|
| Main Menu | ✓ | ✓ | ✓ | ✓ |
| Payroll | ✓ | ✓ | ✓ | ✓ |
| Operations | ✓ | ✓ | ✓ | ✓ |
| General Ledger | - | - | - | ✓ |
| Code Lines | ~2,970 | 2,561 | ~2,700 | 2,779 |

**IMM has slightly more payroll code** than BAJ/HBS, suggesting it may be a more mature version.

## Source Code Statistics

- **Total Lines:** ~2,970 lines
- **Primary Module:** PR.BAS (71K - largest of the payroll systems)
- **36+ numeric fields** in employee record

## Version History

| Date | Description |
|------|-------------|
| Sep 1991 | Initial commit (AC.COM) |
| Nov 1993 | Development (v0.x) |
| Jan 1994 | Updates |
| Aug 1994 | Development (v1.x) |
| Jan 1995 | Final release (v1.x) |
| Apr 2024 | Editor batch file added |

## Running the Application

```bash
# Start DOSBox-X
./dosbox.sh

# Run application
CD AC
AC.EXE
```

## Related Systems

- **BAJ, HBS** - Nearly identical payroll systems (slightly smaller)
- **MCS** - Adds General Ledger module
- **MPC, PFC** - Full accounting with AP, AR, Inventory

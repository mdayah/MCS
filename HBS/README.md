# HBS - Payroll System (bureau client install)

## Overview

HBS is one of several per-client installs of the same payroll product (compare BAJ, IMM, MCS): the ~2,000-line payroll engine with a main menu and operations, configured for a single bookkeeping/payroll client. **The client behind the initials "HBS" is not identified in this public archive** — the earlier "Home Business System" expansion was an unverified guess, not the developer's own label.

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
| **PR** | PR.BAS | 68K | Payroll | ~2,000 |
| **OP** | OP.BAS | 5K | Operations | ~160 |

## Directory Structure

```
HBS/
├── AC/                 # Accounting Application (276K)
│   ├── AC.BAS         # Main menu
│   ├── PR.BAS         # Payroll module
│   ├── OP.BAS         # Operations
│   ├── AC.EXE         # Compiled executable (148K)
│   ├── AC.SLB         # Screen library (16K)
│   ├── AC.MAS         # Master configuration (5.6K)
│   └── EDIT_AC.BAT    # Editor batch file
│
├── BI/                 # Data Definitions (20K)
│   ├── LIBRARY.REC    # Shared declarations
│   ├── ACCOUNTS.REC   # Account structure
│   ├── EMPLOYEE.REC   # Employee structure
│   └── JOURNAL.REC    # Journal structure
│
├── LIB/                # Utility libraries (896K, submodule)
├── VBASIC/             # Development environment (9.8M, submodule)
└── dosbox.sh           # DOSBox launcher
```

## Key Features

### Main Menu (AC)
- 7 menu categories: GL, PR, AP, AR, IN, FS, OP
- Multi-client support
- Database access

### Payroll (PR) - Primary Module
Comprehensive payroll functionality with 40+ subroutines:
- **Employee Management**
  - EmpEdit, EmpSort, EmpShow, EmpWind
- **Check Processing**
  - ChkShow, ChkWind, ChkPrnt, ChkPost, ChkVoid
- **Account Display**
  - AccShow - Account reconciliation
- **Tax Calculations**
  - IncomeTax - Federal income tax
  - SocSecTax - Social Security
  - Medicare
- **Reporting**
  - mprCards - Time cards
  - mprSheet - Payroll sheet
  - mprRegister - Register
  - mprChecks - Check printing
  - mprEmpMas - Employee master list
  - mprW2s - W-2 generation
- **Period Closing**
  - mprMonth - Monthly close
  - mprQuarter - Quarterly close
  - mprYear - Year-end close
- **Utilities**
  - NumWords - Number to text (for checks)

### Operations (OP)
- Company/client profile management
- Client creation (optCreate)
- Client modification (optModify)
- Client selection (optSelect)
- Date operations (optDate)

## Data Structures

### Employee Record
```basic
TYPE Employee
  Personal: Department, Number, Name (24 chars)
  Address: Street, City, State, Zip
  SSN, Phone
  Hire/Release dates
  Type (Salary/Wages), Marital status, Dependents
  Pay: Base pay, Insurance, Deductions, Bonus
  Hours: Regular, Overtime 1.5x, Double-time
  Taxes: FIT, FICA, Medicare
  Time clock: In/out times
  Totals: Monthly, Quarterly, Yearly
END TYPE
```

## Comparison with Similar Systems

| Feature | HBS | BAJ | IMM | MCS |
|---------|-----|-----|-----|-----|
| Main Menu | ✓ | ✓ | ✓ | ✓ |
| Payroll | ✓ | ✓ | ✓ | ✓ |
| Operations | ✓ | ✓ | ✓ | ✓ |
| General Ledger | - | - | - | ✓ |
| Journal Records | ✓ | - | - | ✓ |

**HBS, BAJ, and IMM share nearly identical structure** - company-specific deployments of the same base system.

## Source Code Statistics

- **Total Lines:** ~2,700 lines
- **Primary Module:** PR.BAS (~2,000 lines)
- **40+ subroutines** in payroll module

## Version History

| Date | Description |
|------|-------------|
| Sep 1991 | Initial DOS loader |
| Nov 1993 | Database definitions |
| Aug 1994 | Screen library |
| Jan 1995 | BASIC source code |
| 2024 | Git repository (10 commits) |

## Running the Application

```bash
# Start DOSBox-X
./dosbox.sh

# Run application
CD AC
AC.EXE
```

## Related Systems

- **BAJ, IMM** - Nearly identical payroll systems
- **MCS** - Adds General Ledger module
- **MPC, PFC** - Full accounting with AP, AR, Inventory

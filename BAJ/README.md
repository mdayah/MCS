# BAJ - Accounting/Payroll System

## Overview

BAJ is an **accounting and payroll management system** providing employee management, check processing, and financial tracking capabilities.

## System Classification

| Attribute | Value |
|-----------|-------|
| Type | Payroll/Accounting Subset |
| Language | Visual Basic for MS-DOS |
| Era | 1991-1999 |
| Status | Payroll-focused system |
| Modules | 3 modules |

## Module Overview

| Module | File | Size | Purpose | Lines |
|--------|------|------|---------|-------|
| **AC** | AC.BAS | 12K | Main Menu | 375 |
| **PR** | PR.BAS | 68K | Payroll | 2,025 |
| **OP** | OP.BAS | 5K | Operations | 161 |

## Directory Structure

```
BAJ/
├── AC/                 # Accounting Application (372K)
│   ├── AC.BAS         # Main menu (375 lines)
│   ├── PR.BAS         # Payroll module (2,025 lines)
│   ├── OP.BAS         # Operations (161 lines)
│   ├── AC.EXE         # Compiled executable (150K)
│   ├── AC.SLB         # Screen library
│   ├── AC.MAS         # Master configuration
│   └── 386.QLB/LIB    # Processor libraries
│
├── BI/                 # Data Definitions (12K)
│   ├── LIBRARY.REC    # Shared declarations
│   ├── ACCOUNTS.REC   # Account structure
│   └── EMPLOYEE.REC   # Employee structure
│
├── NOVA/               # Screen editor (submodule)
├── VBASIC/             # Development environment (submodule)
└── dosbox.sh           # DOSBox launcher
```

## Key Features

### Main Menu (AC)
- Menu-driven interface
- Database access
- Module navigation

### Payroll (PR) - Primary Module
The payroll module provides comprehensive functionality:
- **Employee Management**
  - EmpEdit, EmpSort, EmpShow, EmpWind
- **Check Processing**
  - ChkFind, ChkPost, ChkPrnt, ChkVoid, ChkShow
- **Tax Calculations**
  - Federal Income Tax (FIT)
  - Social Security (FICA)
  - Medicare
- **Reporting**
  - Payroll register
  - W-2 generation
  - Monthly/quarterly/yearly reports
  - Employee master list
  - Check register

### Operations (OP)
- Company configuration
- Employee master maintenance

## Data Structures

### Employee Record
```basic
TYPE Employee
  Department, Number, Name
  Address: Street, City, State, Zip
  SSN, Phone
  Hire/Release dates
  Type (Salary/Wages), Marital status, Dependents
  Pay: Wages, Insurance, Deductions, Bonus
  Hours: Regular, Overtime (1.5x, 2x)
  Taxes: FIT, FICA, Medicare
  Totals: Monthly, Quarterly, Yearly
END TYPE
```

## Comparison with Similar Systems

| Feature | BAJ | HBS | IMM | MCS |
|---------|-----|-----|-----|-----|
| Main Menu | ✓ | ✓ | ✓ | ✓ |
| Payroll | ✓ | ✓ | ✓ | ✓ |
| Operations | ✓ | ✓ | ✓ | ✓ |
| General Ledger | - | - | - | ✓ |
| Code Lines | 2,561 | ~2,700 | ~2,970 | 2,779 |

**BAJ, HBS, IMM, and MCS share nearly identical code structure** - they are company-specific deployments of the same payroll system.

## Source Code Statistics

- **Total Lines:** 2,561 lines
- **Primary Module:** PR.BAS (2,025 lines - 79% of code)
- **Executables:** 1 main program (AC.EXE)

## Running the Application

```bash
# Start DOSBox-X
./dosbox.sh

# Run application
CD AC
AC.EXE
```

## Version History

| Date | Description |
|------|-------------|
| Sep 1991 | Initial commit |
| Nov 1993 | Updates |
| Mar 1994 | Updates |
| Jan 1999 | Final updates |
| 2024 | Git repository created |

## Related Systems

- **HBS, IMM** - Nearly identical payroll systems
- **MCS** - Adds General Ledger module
- **MPC, PFC** - Full accounting systems with AP, AR, Inventory

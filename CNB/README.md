# CNB - Payroll File Format Converter

## Overview

CNB is a **utility program** that converts payroll data between different file formats for banking operations. Unlike the accounting systems in this repository, CNB is a **single-purpose conversion tool**.

**This is a distinct utility program** - not an accounting system.

## System Classification

| Attribute | Value |
|-----------|-------|
| Type | File Format Converter |
| Language | Visual Basic for MS-DOS |
| Era | 1999 |
| Purpose | Banking data interchange |
| Size | 24 MB (with toolchain) |
| Core Code | ~608 lines |

## Directory Structure

```
CNB/
├── CNB.BAS                 # Main source (7.5K)
├── CNB.EXE                 # Compiled executable (62K)
├── CNB.SLB                 # Screen library (2.1K)
├── CNB.MAK                 # Makefile (9 bytes)
├── EDIT_CNB.BAT            # Editor batch file (loads CNB.BAS in VB)
│
├── floppy/                 # Sample test data (40K)
│   ├── PAYROLL.TXT / .OUT  # Payroll format
│   ├── 1STNAT.TXT / .OUT   # First National Bank format
│   ├── LEAGUE.TXT / .OUT   # League Credit Union format
│   └── WARNING files       # Anonymization notices
│
├── LIB/                    # Utility libraries (896K, submodule)
│                           # LIB/386.QLB contains Mh* functions
├── NOVA/                   # Screen editor (428K, submodule)
├── VBASIC/                 # Development environment (9.8MB, submodule)
└── dosbox.sh               # DOSBox launcher
```

## Functionality

### Input Formats
CNB reads three distinct file formats:

1. **Payroll Format** (PAYROLL.TXT)
   ```
   SSN (11) + Name (32) + Date (8) + Amount (11) + CR/LF
   ```

2. **First National Bank Format** (1STNAT.TXT)
   ```
   SSN (11) + Name (32) + Date (8) + Account (11) + Amount (11) + CR/LF
   ```

3. **League Credit Union Format** (LEAGUE.TXT)
   ```
   Similar structure to First National
   ```

### Output Format
Normalizes all records to standardized output (.OUT files):
```
Record Type + Blanks + ID1-ID3 + User Codes + Amount + Date + File Field + CR/LF
```

### Processing
- Reads from three input files simultaneously
- Transforms records to common output format
- Splits SSN into component ID fields
- Standardizes amount formatting
- Displays progress with running totals

## Data Structures

```basic
TYPE PayIn
  SSN AS STRING * 11
  Name AS STRING * 32
  Date AS STRING * 8
  Amount AS STRING * 11
  CR AS STRING * 2
END TYPE

TYPE NatIn
  SSN AS STRING * 11
  Name AS STRING * 32
  Date AS STRING * 8
  AcctNum AS STRING * 11
  Amount AS STRING * 11
  CR AS STRING * 2
END TYPE

TYPE PayOut
  RecType AS STRING * 1
  Blanks AS STRING * 2
  ID1-ID3 AS STRING * 3 each
  UserCodes AS STRING * various
  Amount AS STRING * formatted
  Date AS STRING * 8
  FileField AS STRING * 1
  CR AS STRING * 2
END TYPE
```

## Sample Data

The `floppy/` directory contains anonymized test data:

| File | Size | Purpose |
|------|------|---------|
| PAYROLL.TXT | 188 bytes | Sample payroll input |
| PAYROLL.OUT | 204 bytes | Converted output |
| 1STNAT.TXT | 209 bytes | First National input |
| 1STNAT.OUT | 204 bytes | Converted output |
| LEAGUE.TXT | 209 bytes | League CU input |
| LEAGUE.OUT | 204 bytes | Converted output |

## Development

- **Edit:** Use `EDIT_CNB.BAT` to load CNB.BAS in Visual Basic
- **Screens:** *.SLB files editable with `NOVA\NSEDIT.EXE`
- **Libraries:** LIB/386.QLB contains Mh* functions

## Technical Details

- **UI Library:** MicroHelp screen library
- **Display:** Logo/splash screens, scrolling record display
- **Progress:** Running totals and record counts
- **Error Handling:** Exception reporting

## Comparison with Other Systems

| Aspect | CNB | BAJ/HBS/IMM | PFC/MPC |
|--------|-----|-------------|---------|
| **Type** | Converter | Payroll System | Full ERP |
| Purpose | Format conversion | Pay processing | Business mgmt |
| Modules | 1 | 3 | 8-12 |
| Code Lines | ~608 | ~2,700 | ~6,800 |
| Data Files | Text I/O | Binary DB | Binary DB |

**CNB is a single-purpose utility** - it converts file formats rather than managing business data.

## Running the Application

```bash
# Start DOSBox-X
./dosbox.sh

# Run converter
CNB.EXE

# Input files should be in current directory:
# - PAYROLL.TXT
# - 1STNAT.TXT
# - LEAGUE.TXT
```

## Version History

| Date | Description |
|------|-------------|
| Sep 1999 | Initial commit |
| Mar 2024 | DOSBox config, sample data |
| Apr 2024 | Git submodules |

## Use Case

This tool was designed for:
- Payroll service bureaus
- Businesses submitting to multiple banks
- Data interchange between financial systems
- Standardizing payroll file formats

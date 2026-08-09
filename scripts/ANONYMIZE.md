# Data Anonymization Guide

This guide explains how to anonymize the real PII data in the legacy VB DOS files
before publishing to a public repository.

## Overview

Each project with PII data has a `SCRUB.BAS` program that:
1. Opens the binary data files using native VB DOS file I/O
2. Reads each record using the original TYPE definitions
3. Replaces PII fields with synthetic (fake) data
4. Writes the records back to the same file

This approach ensures the file format remains valid and the applications
continue to work with the synthetic data.

## Projects with Scrubbers

| Project | File | Data Replaced |
|---------|------|---------------|
| **MPC** | `MPC/SCRUB.BAS` | Employees (SSN, names), Customers, Vendors |
| **FOG** | `FOG/SCRUB.BAS` | Customers (SSN, DL, DOB for 2 people), Passwords |
| **PFC** | `PFC/SCRUB.BAS` | Employees (SSN, names), Customers, Vendors |
| **RDL** | `RDL/SCRUB.BAS` | Dental lab customers |

## Step-by-Step Process

### 1. Backup Original Data (IMPORTANT!)

Before running the scrubbers, copy the original files to the private repository:

```bash
# Create the private repo structure
cd ~/dad-data-staging

# Copy MPC data
cp ~/dad/MPC/DATA/EMPLOYEE.MAS MPC/DATA/EMPLOYEE.MAS.real
cp ~/dad/MPC/DATA/CUSTOMER.MAS MPC/DATA/CUSTOMER.MAS.real
cp ~/dad/MPC/DATA/VENDORS.MAS MPC/DATA/VENDORS.MAS.real

# Copy FOG data
cp ~/dad/FOG/DATAFILE/CUSTOMER.MAS FOG/DATAFILE/CUSTOMER.MAS.real
cp ~/dad/FOG/OFDATA/CUSTOMER.MAS FOG/OFDATA/CUSTOMER.MAS.real
cp ~/dad/FOG/MASTER/PASSWORD.MAS FOG/MASTER/PASSWORD.MAS.real

# Copy PFC data
cp ~/dad/PFC/AR/CUSTOMER.MAS PFC/AR/CUSTOMER.MAS.real
cp ~/dad/PFC/AP/VENDORS.MAS PFC/AP/VENDORS.MAS.real
cp ~/dad/PFC/PR/EMPLOYEE.MAS PFC/PR/EMPLOYEE.MAS.real

# Copy RDL data
cp ~/dad/RDL/AR/CUSTOMER.DTA RDL/AR/CUSTOMER.DTA.real
```

### 2. Run Scrubbers in DOSBox

Start DOSBox and mount the dad directory:

```
# In DOSBox
mount c ~/dad
c:
```

Then run each scrubber:

#### MPC (McKenry Produce)
```
cd MPC
..\VBASIC\VBDOS SCRUB.BAS
cd ..
```

#### FOG (Rental Management)
```
cd FOG
..\VBASIC\VBDOS SCRUB.BAS
cd ..
```

#### PFC (Processed Foods)
```
cd PFC
VBASIC\VBDOS SCRUB.BAS
cd ..
```

#### RDL (Dental Billing)
```
cd RDL
VBASIC\VBDOS SCRUB.BAS
cd ..
```

### 3. Verify the Results

After running scrubbers, verify the apps still work:

```
# Test MPC
cd MPC\SYSTEM
MPC.EXE
# Navigate to Customer/Employee screens, verify fake data appears

# Test FOG
cd FOG\SYSTEM
RTO.EXE
# Check customer screens

# Test RDL
cd RDL\AR
AR.EXE
# Check customer list
```

### 4. Compile Scrubbers (Optional)

If you want standalone .EXE files instead of running interpreted:

```
cd MPC
..\VBASIC\BC SCRUB.BAS /O;
..\VBASIC\LINK SCRUB.OBJ,SCRUB.EXE,,VBDOS.LIB;
```

## What Gets Replaced

### MPC Employee Records
- Name → "James Smith", "Mary Johnson", etc.
- Address → "123 Main Street", etc.
- City → "Springfield", "Franklin", etc.
- SSN → "100-10-1001", "101-11-1002", etc.
- Phone → "555-200-1001", etc.

### FOG Customer Records (Most Sensitive)
- Primary Person:
  - Name, Address, City, Zip, Phone
  - **SSN** → Fake format "100-10-1001"
  - **Driver's License** → Fake format "AL1234567"
  - **Date of Birth** → Fake dates 1950-2000
  - Employer, Work Phone, Income
- Second Person (co-applicant):
  - All the same fields duplicated

### FOG Password Records
- Password → "P001", "P002", etc.
- User Name → "James S.", "Mary J.", etc.
- Initials → "JSS", "MJJ", etc.

### PFC/RDL
- PFC uses the same customer/vendor/employee patterns
- RDL replaces customer identities plus each of the eight packed patient-name positions in every invoice
- RDL procedure descriptions remain intact as business reference data

## Synthetic Data Patterns

The Python generator derives a stable seed from project, file, record number, field position, and field name. Re-running the same schema produces byte-identical synthetic fields even when a different subset of projects is selected. Cross-file relationships remain consistent through preserved record IDs, not by repeating the same generated name in unrelated files.

## Business Data Preserved

Financial amounts, record IDs, procedure codes, inventory/product data, configuration, and journal entries remain unchanged unless a schema explicitly identifies an embedded identity. RDL invoices are the important exception: their packed patient-name segments are replaced while procedure, quantity, price, and tax bytes are preserved. FOG inventory remarks are replaced because they are free text, while the rest of each inventory record is retained.

## After Anonymization

1. Run `python3 scripts/sync_companions.py` to rebuild name indexes
2. Run `python3 scripts/set_demo_logins.py` to restore FOG's sorted login file
3. Rebuild the js-dos bundles and run `python3 web/audit_bundles.py`
4. Launch the applications and verify customer, invoice, procedure, payroll, and report screens

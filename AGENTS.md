# Data Preservation & Anonymization System

## Overview

To enable public release of production business software (1988-2006), personally identifiable information (PII) must be anonymized while preserving functional data structure and business logic. This document describes the dual-approach scrubbing strategy that makes open-source distribution possible:

**Why this matters:** The archived systems contained real customer names, employee records, addresses, and financial data from actual companies (McKenry Produce, a poultry business; Processed Foods Corporation; a rent-to-own store; Rogers' Dental Laboratory). Before publishing the complete source code and data files, all sensitive PII is replaced with realistic-but-fake data using deterministic functions, making the systems runnable and demonstrable without exposing real information.

1. **VB DOS Native Scrubbers** (SCRUB.BAS) - Run directly in DOS environment
2. **Modern Python Generator** (generate_synthetic.py) - Schema-driven batch processor

---

## VB DOS Scrubbers (SCRUB.BAS)

### Location
- `MPC/SCRUB.BAS` (427 lines, 15KB) - McKenry Produce Co
- `FOG/SCRUB.BAS` (397 lines, 14KB) - Rent-to-Own System
- `PFC/SCRUB.BAS` (391 lines, 13KB) - Processed Foods Corporation
- `RDL/SCRUB.BAS` (220 lines, 6.4KB) - Dental Lab Billing

### How They Work

#### Step 1: Initialize Fake Data Arrays
Each scrubber loads pre-defined arrays of fake data:
- **FirstName$(1-50)**: "James", "Mary", "John", etc.
- **LastName$(1-50)**: "Smith", "Johnson", "Williams", etc.
- **Company$(1-50)**: "Acme Foods Inc", "Blue Ridge Farms", etc.
- **Street$(1-30)**: "Main Street", "Oak Avenue", etc.
- **City$(1-30)**: "Springfield", "Franklin", "Greenville", etc.
- **State$(1-50)**: All US state codes (AL-WY)

#### Step 2: Open Binary Data Files
```vb
OPEN "DATA\EMPLOYEE.MAS" FOR RANDOM AS #1 LEN = LEN(emp)
NumRec& = LOF(1) \ LEN(emp)
```
- Uses RANDOM file access mode (record-by-record)
- File size divided by record size = number of records
- Files are VB DOS ISAM format (binary)

#### Step 3: Generate Fake Values with Deterministic Functions
Three helper functions generate fake data using modulo arithmetic on record number:

**FakeSSN$(n)** - Generates "100-10-1001" pattern
```vb
a$ = RIGHT$("000" + LTRIM$(STR$((n MOD 900) + 100)), 3)      ' 100-999
b$ = RIGHT$("00" + LTRIM$(STR$((n MOD 90) + 10)), 2)         ' 10-99
c$ = RIGHT$("0000" + LTRIM$(STR$((n MOD 9000) + 1000)), 4)  ' 1000-9999
FakeSSN$ = a$ + "-" + b$ + "-" + c$
```

**FakePhone$(n)** - Always uses "555" exchange (directory reserved)
```vb
a$ = RIGHT$("000" + LTRIM$(STR$((n MOD 800) + 200)), 3)
FakePhone$ = "555-" + a$ + "-" + RIGHT$("0000"..., 4)
```

**FakeZip$(n)** - Generates 5-digit pattern
```vb
FakeZip$ = RIGHT$("00000" + LTRIM$(STR$((n MOD 90000) + 10000)), 5)
```

**Key insight**: Uses record number `n` as seed, so results are deterministic and reproducible.

#### Step 4: Read-Modify-Write Records
```vb
FOR i& = 1 TO NumRec&
    GET #1, i&, emp                          ' Read record
    IF LTRIM$(emp.Num) <> "" THEN            ' Skip empty records
        count = count + 1
        n = count

        emp.Nam = PadStr$(FirstName$(fn) + " " + LastName$(ln), 24)  ' Replace name
        emp.Tel = FakePhone$(n)              ' Replace phone
        emp.SSn = FakeSSN$(n)                ' Replace SSN

        PUT #1, i&, emp                      ' Write back
    END IF
NEXT i&
```

- Reads each record with `GET`
- Only processes non-empty records (skips deleted entries)
- Replaces PII fields using deterministic fake data
- Writes back with `PUT` (in-place modification)
- Preserves file structure and non-PII data

#### Step 5: Close and Complete
```vb
CLOSE #1
PRINT "Processed"; count; "employee records."
```

### Field Replacement Patterns

#### MPC (McKenry Produce)
**Employee Records:**
- Name: Random combination of FirstName$(fn) + LastName$(ln)
- Address: "100-N + Street$(sn)" (e.g., "101 Main Street")
- City: City$(n % 30)
- Zip: FakeZip$(n)
- SSN: FakeSSN$(n) (format: "###-##-####")
- Phone: FakePhone$(n) (format: "555-###-####")

**Customer Records:**
- Company Name: Company$(n % 50)
- Contact Person: FirstName + LastName
- Addresses (billing & shipping): Same pattern as employees
- Bank Account: "XXXX" + padded record number (e.g., "XXXX00123")
- Bank Name: "First National Bank" (fixed)

**Vendor Records:**
- Similar to customers, with PO Box as secondary address

---

## Modern Python Scrubber

### Location
- `scripts/generate_synthetic.py` (321 lines, executable)
- `scripts/schemas.json` (22KB, field definitions)
- `scripts/ANONYMIZE.md` (documentation)

### Architecture

#### 1. Schema-Driven Field Detection
`schemas.json` defines record structures:
```json
{
  "MPC": {
    "Employee": {
      "file": "MPC/DATA/EMPLOYEE.MAS",
      "record_size": 256,
      "fields": [
        {"name": "num", "type": "STRING", "size": 8, "pii": false},
        {"name": "nam", "type": "STRING", "size": 24, "pii": true},
        {"name": "ssn", "type": "STRING", "size": 11, "pii": true}
      ]
    }
  }
}
```

**Key attributes:**
- `pii: true/false` - Flags fields to replace
- `type` - Data type (STRING, INTEGER, LONG, SINGLE, BYTES)
- `size` - Byte length for STRING fields
- `name` - Field identifier

#### 2. Smart PII Field Detection
Function `generate_fake_value(field)` uses heuristics on field name/description:

```python
if 'ssn' in name or 'ss1' in name or 'social' in desc:
    value = fake.ssn()  # "###-##-####"
elif 'dln' in name or 'driver' in desc:
    value = fake.bothify(text='??#######')  # State-format license
elif 'dob' in name or 'birth' in desc:
    value = fake.date_of_birth(minimum_age=18, maximum_age=80)
elif 'nam' in name or 'name' in desc:
    if 'company' in desc or 'vendor' in desc:
        value = fake.company()
    else:
        value = fake.name()
elif 'add' in name or 'address' in desc:
    value = fake.street_address()
```

Supports:
- Names (personal and company)
- SSN, Driver's License, DOB
- Addresses, Cities, Zip codes
- Phone, Fax, Email
- Bank accounts, Passwords
- Income/financial amounts
- Custom patterns via regex

#### 3. Binary Record Processing

```python
def process_record(data: bytes, fields: List[Dict]) -> bytes:
    """Process a single record, replacing PII fields."""
    result = bytearray()
    offset = 0

    for field in fields:
        value, new_offset = read_field(data, offset, field)

        if field.get('pii', False):
            fake_value = generate_fake_value(field)  # Replace
            result.extend(fake_value)
        else:
            result.extend(value)  # Keep original

        offset = new_offset

    return bytes(result)
```

**Key steps:**
1. Read field from binary data at current offset
2. Check `pii` flag
3. If PII: generate fake value using Faker library
4. If not PII: preserve original bytes
5. Advance offset by field size

#### 4. Header Detection
VB DOS ISAM files may have headers. Script auto-detects:
```python
for header_size in [0, 2, 4, 8, 128, 256]:
    if (len(data) - header_size) % record_size == 0:
        header = data[:header_size]
        data = data[header_size:]
        break
```

Tries common header sizes, preserves header in output.

#### 5. Batch Processing with Backup
```bash
python generate_synthetic.py --backup       # Backup originals first
python generate_synthetic.py --dry-run      # Show what would happen
python generate_synthetic.py MPC FOG        # Process specific projects
```

**Backup path structure:**
```
~/dad-data-staging/
├── MPC/DATA/EMPLOYEE.MAS.real
├── FOG/DATAFILE/CUSTOMER.MAS.real
└── ...
```

#### 6. Reproducibility
Uses seeded Faker for consistent output:
```python
Faker.seed(42)
random.seed(42)
```
- Re-running produces identical results
- Related records maintain consistency
- Data looks realistic but obviously fake

---

## Key Differences: VB DOS vs Python

| Aspect | VB DOS | Python |
|--------|--------|--------|
| **Runtime** | DOS/DOSBox interpreter | Python 3.x |
| **Fake Data** | Hard-coded arrays (50 entries each) | Faker library (unlimited variety) |
| **Records** | Single file per run | All files in schema |
| **Determinism** | Modulo arithmetic on record count | Seeded Faker (seed=42) |
| **Schema** | Hard-coded in SCRUB.BAS | External schemas.json |
| **Backup** | Manual (documented in ANONYMIZE.md) | Automatic (--backup flag) |
| **Dry-run** | Not supported | --dry-run flag |
| **Field Detection** | Hard-coded field names | Regex/heuristic pattern matching |

---

## Execution Flow

### VB DOS Method
1. Boot DOSBox and mount ~/dad as C:
2. `cd MPC`
3. `..\VBASIC\VBDOS SCRUB.BAS` (interprets and runs)
4. Scrubber reads DATA files, replaces PII, writes back
5. Close and verify: Run `MPC.EXE`, check customer/employee screens

### Python Method
1. Install dependencies: `pip install faker`
2. Backup originals: `python scripts/generate_synthetic.py --backup`
3. Test (dry-run): `python scripts/generate_synthetic.py --dry-run`
4. Execute: `python scripts/generate_synthetic.py`
5. Verify: Check that data appears realistic but anonymous

---

## Security & Privacy Considerations

### What Gets Scrubbed
✓ Names (employee, customer, vendor, contact persons)
✓ Social Security Numbers (SSN)
✓ Driver's License numbers
✓ Dates of Birth
✓ Phone numbers / Fax numbers
✓ Addresses (street, city, zip)
✓ Bank account information
✓ Passwords
✓ User initials / logins
✓ Income / salary information
✓ Company names (vendor/employer)

### What's Preserved
✓ Transaction amounts (no names)
✓ Invoice/PO numbers (reference IDs)
✓ Product codes and inventory
✓ Business logic (GL entries, tax codes)
✓ Dates of transactions (historical data)
✓ File structure and record counts

### Deterministic but Irreversible
- Same record number always gets same fake name
- Related records stay consistent (customer and invoices)
- **Cannot be reversed** - original data lost (unless backed up)
- Obviously fake (555-#### phone, Faker-generated addresses)

---

## File Locations Summary

```
/home/lucent/dad/
├── scripts/
│   ├── ANONYMIZE.md              # Step-by-step guide
│   ├── generate_synthetic.py     # Python scrubber (modern)
│   └── schemas.json              # Field definitions for Python scrubber
│
├── MPC/
│   ├── SCRUB.BAS                 # VB DOS scrubber (native)
│   ├── DATA/
│   │   ├── EMPLOYEE.MAS          # Employee records
│   │   ├── CUSTOMER.MAS          # Customer records
│   │   └── VENDORS.MAS           # Vendor records
│   └── INCLUDE/
│       ├── EMPLOYEE.REC          # Record type definitions
│       ├── CUSTOMER.REC
│       └── PAYABLES.REC
│
├── FOG/
│   ├── SCRUB.BAS                 # VB DOS scrubber
│   ├── DATAFILE/CUSTOMER.MAS     # Main customer data
│   ├── OFDATA/CUSTOMER.MAS       # Office data
│   └── MASTER/PASSWORD.MAS       # Login passwords
│
├── PFC/
│   └── SCRUB.BAS                 # VB DOS scrubber
│
└── RDL/
    └── SCRUB.BAS                 # VB DOS scrubber
```

---

## Lessons Learned

1. **VB DOS Approach**: Simple, self-contained, works in original environment. Good for one-off scrubbing but requires manual intervention for each file.

2. **Python Approach**: More flexible, schema-driven, batch processing. Can easily extend to more projects without modifying code. Requires understanding binary record structures.

3. **Deterministic Seeding**: Both methods use record count as a seed. This ensures re-running produces identical results - crucial for validation and reproducibility.

4. **ISAM File Awareness**: VB DOS ISAM files have specific structure (random access, fixed-size records). Python scrubber includes header detection to handle variations.

5. **Backup Strategy**: Original files must be backed up before scrubbing. Scrubbing is **irreversible** without backups.

6. **Testing Method**: After scrubbing, the original applications (MPC.EXE, RTO.EXE) should still run and display fake data correctly. This validates that file structure and record boundaries were preserved.

#!/usr/bin/env python3
"""
Synthetic Data Generator for Legacy VB DOS Business Software

This script generates synthetic (fake) data to replace real PII in the
legacy .MAS and .DTA files while preserving file structure and record counts.

Usage:
    python generate_synthetic.py [--backup] [--dry-run] [project...]

Examples:
    python generate_synthetic.py                    # Process all projects
    python generate_synthetic.py MPC FOG            # Process specific projects
    python generate_synthetic.py --backup           # Backup originals first
    python generate_synthetic.py --dry-run          # Show what would be done

Requirements:
    pip install faker
"""

import argparse
import hashlib
import json
import os
import shutil
import struct
import sys
from pathlib import Path
from typing import Dict, List, Any, Optional
import random
from faker import Faker

# Initialize Faker with seed for reproducibility
fake = Faker('en_US')

# Base directory (parent of scripts/)
BASE_DIR = Path(__file__).parent.parent
SCHEMAS_FILE = Path(__file__).parent / "schemas.json"


def load_schemas(path: Path = SCHEMAS_FILE) -> Dict:
    """Load record schemas from JSON file."""
    with open(path, 'r') as f:
        return json.load(f)


def calculate_record_size(fields: List[Dict]) -> int:
    """Calculate total record size from field definitions."""
    size = 0
    for field in fields:
        if field['type'] == 'STRING':
            size += field['size']
        elif field['type'] in ('LONG', 'SINGLE'):
            size += 4
        elif field['type'] == 'INTEGER':
            size += 2
        elif field['type'] == 'BYTES':
            size += field.get('size', 0)
    return size


def generate_fake_value(field: Dict, seed_key: str) -> bytes:
    """Generate a fake value for a PII field."""
    seed = int.from_bytes(
        hashlib.sha256(seed_key.encode('utf-8')).digest()[:8], 'little')
    fake.seed_instance(seed)
    rng = random.Random(seed)
    field_type = field['type']
    size = field.get('size', 4)
    name = field.get('name', '').lower()
    desc = field.get('desc', '').lower()
    fmt = field.get('format', '')

    if field_type == 'STRING':
        # Generate appropriate fake data based on field name/description
        if field.get('strategy') == 'redact':
            value = 'SYNTHETIC DEMO NOTE'
        elif 'ssn' in name or 'ss1' in name or 'social' in desc:
            value = fake.ssn()  # Returns ###-##-####
        elif 'dln' in name or 'dl1' in name or 'driver' in desc or 'license' in desc:
            value = fake.bothify(text='??#######')  # State format varies
        elif 'dob' in name or 'bd1' in name or 'birth' in desc:
            # match the MM-DD-YYYY style used throughout the data files
            value = fake.date_of_birth(minimum_age=18, maximum_age=80).strftime('%m-%d-%Y')
        elif ('tel' in name or 'phone' in desc or 'wor' in name or 'wt1' in name
              or 'ste' in name or 'aph' in name or 'fax' in name or 'afx' in name):
            value = fake.numerify('615-###-####')[:size]
        elif 'nam' in name or 'nm1' in name or 'name' in desc:
            if 'company' in desc or 'vendor' in desc:
                value = fake.company()[:size]
            else:
                value = fake.name()[:size]
        elif ('add' in name or 'sad' in name or 'ssr' in name or 'str' in name
              or 'address' in desc or 'street' in desc):
            value = fake.street_address()[:size]
        elif 'cty' in name or 'ct1' in name or 'sct' in name or 'city' in desc:
            value = fake.city()[:size]
        elif 'zip' in name or 'zi1' in name or 'szi' in name:
            value = fake.zipcode()[:size]
        elif 'ban' in name or 'bank' in desc:
            value = (fake.last_name() + " National Bank")[:size]
        elif 'acc' in name or 'account' in desc:
            # Acc is a bank account number in MPC (15B) but the accounting
            # contact person in PFC (28B) — width tells them apart
            if size >= 20:
                value = fake.name()[:size]
            else:
                value = fake.numerify('#########')[:size]
        elif 'tax' in name:
            value = fake.numerify('62-#######')[:size]  # EIN-style
        elif 'pas' in name or 'password' in desc:
            value = fake.password(length=min(size, 8), special_chars=False)
        elif 'ini' in name or 'initial' in desc:
            value = fake.lexify(text='???').upper()
        elif 'inc' in name or 'in1' in name or 'income' in desc:
            value = f"${rng.randint(20, 100) * 1000:,}"[:size]
        elif 'oth' in name or 'ot1' in name:
            value = f"${rng.randint(0, 20) * 1000:,}"[:size]
        elif 'emp' in name or 'em1' in name or 'employer' in desc:
            value = fake.company()[:size]
        elif 'sal' in name or 'salutation' in desc:
            value = fake.prefix()[:size]
        elif ('man' in name or 'contact' in desc or 'sco' in name
              or 'ord' in name or 'cnt' in name):
            value = fake.name()[:size]
        else:
            # Unknown PII field - generate random alphanumeric
            value = fake.lexify(text='?' * size)

        # Pad or truncate to exact size
        value = value[:size].ljust(size)
        return value.encode('ascii', errors='replace')

    elif field_type == 'LONG':
        return struct.pack('<l', 0)  # Keep numeric fields as-is or zero

    elif field_type == 'INTEGER':
        return struct.pack('<h', 0)

    elif field_type == 'SINGLE':
        return struct.pack('<f', 0.0)

    else:
        return b'\x00' * size


def read_field(data: bytes, offset: int, field: Dict) -> tuple:
    """Read a field value from binary data. Returns (value, new_offset)."""
    field_type = field['type']
    size = field.get('size', 4)

    if field_type == 'STRING':
        value = data[offset:offset + size]
        return value, offset + size
    elif field_type == 'LONG':
        value = data[offset:offset + 4]
        return value, offset + 4
    elif field_type == 'INTEGER':
        value = data[offset:offset + 2]
        return value, offset + 2
    elif field_type == 'SINGLE':
        value = data[offset:offset + 4]
        return value, offset + 4
    elif field_type == 'BYTES':
        value = data[offset:offset + size]
        return value, offset + size
    else:
        return data[offset:offset + size], offset + size


def process_record(data: bytes, fields: List[Dict], record_key: str) -> bytes:
    """Process a single record, replacing PII fields with fake data."""
    result = bytearray()
    offset = 0

    for field_index, field in enumerate(fields):
        value, new_offset = read_field(data, offset, field)

        if field.get('pii', False):
            # Replace with fake data
            fake_value = generate_fake_value(
                field, f"{record_key}|{field_index}|{field['name']}")
            result.extend(fake_value)
        else:
            # Keep original value
            result.extend(value)

        offset = new_offset

    return bytes(result)


def process_file(project: str, record_type: str, schema: Dict, dry_run: bool = False) -> Optional[str]:
    """Process a single data file, replacing PII with synthetic data."""
    file_path = BASE_DIR / project / schema['file']

    if not file_path.exists():
        return f"  SKIP: {file_path} (not found)"

    fields = schema.get('fields', [])
    if not fields:
        return f"  SKIP: {file_path} (no field definitions)"

    # Calculate record size
    record_size = schema.get('record_size', calculate_record_size(fields))

    # Read file
    with open(file_path, 'rb') as f:
        data = f.read()

    if len(data) == 0:
        return f"  SKIP: {file_path} (empty file)"

    original_mtime = file_path.stat().st_mtime

    # Non-record bytes: schemas.json (generated from the .REC TYPEs) states
    # how many; a text-mode artifact (LF/CR/EOF/NUL) at the end is a trailer,
    # anything else is assumed to be a leading header.
    header = trailer = b''
    slack = schema.get('header_or_slack_bytes')
    if slack is None:
        slack = len(data) % record_size
    if slack:
        if (len(data) - slack) % record_size != 0:
            return (f"  WARN: {file_path} (size {len(data)} does not fit "
                    f"record size {record_size} + slack {slack})")
        if all(b in (0x0A, 0x0D, 0x1A, 0x00) for b in data[-slack:]):
            trailer = data[-slack:]
            data = data[:-slack]
        else:
            header = data[:slack]
            data = data[slack:]
    num_records = len(data) // record_size

    if num_records == 0:
        return f"  SKIP: {file_path} (no records)"

    if dry_run:
        return f"  Would process: {file_path} ({num_records} records, {record_size} bytes each)"

    # Process each record
    new_data = bytearray(header)
    for i in range(num_records):
        record_start = i * record_size
        record_end = record_start + record_size
        record = data[record_start:record_end]

        new_record = process_record(
            record, fields, f"{project}|{schema['file']}|{i + 1}")
        new_data.extend(new_record)

    new_data.extend(trailer)

    # Write back, restoring the original mtime — commit_by_date.sh derives
    # the historical commit dates from file modification times.
    with open(file_path, 'wb') as f:
        f.write(new_data)
    os.utime(file_path, (original_mtime, original_mtime))

    return f"  OK: {file_path} ({num_records} records processed)"


def backup_file(project: str, schema: Dict) -> Optional[str]:
    """Keep the real data as a sibling <file>.original (mtime preserved).

    Never overwrites an existing .original — on a re-run the file itself
    already holds synthetic data, and clobbering the backup with it would
    destroy the last copy of the real data. .original files are gitignored
    and excluded from web bundles."""
    file_path = BASE_DIR / project / schema['file']

    if not file_path.exists():
        return None

    backup_path = file_path.with_name(file_path.name + '.original')
    if backup_path.exists():
        return None
    shutil.copy2(file_path, backup_path)
    return f"  Original kept: {backup_path}"


def main():
    parser = argparse.ArgumentParser(description='Generate synthetic data for VB DOS files')
    parser.add_argument('projects', nargs='*', help='Specific projects to process (default: all)')
    parser.add_argument('--no-backup', dest='backup', action='store_false',
                        help='Skip keeping <file>.original copies (default: keep)')
    parser.add_argument('--dry-run', action='store_true', help='Show what would be done')
    parser.add_argument('--schemas', type=Path, default=SCHEMAS_FILE, help='Path to schemas.json')
    args = parser.parse_args()

    # Load schemas
    schemas = load_schemas(args.schemas)

    # Filter to requested projects
    projects = args.projects if args.projects else [k for k in schemas.keys() if not k.startswith('_')]

    print("Synthetic Data Generator")
    print("=" * 60)
    print(f"Base directory: {BASE_DIR}")
    print(f"Projects: {', '.join(projects)}")
    if args.backup:
        print("Backups: real data kept beside each file as <name>.original")
    if args.dry_run:
        print("DRY RUN - no files will be modified")
    print("=" * 60)
    print()

    for project in projects:
        if project not in schemas:
            print(f"WARNING: No schema for project '{project}'")
            continue

        project_schemas = schemas[project]
        if isinstance(project_schemas, str):
            # Skip comments
            continue
        # generate_schemas.py output nests tables under "tables", with
        # "_unresolved"/"_companions" alongside for the audit step.
        if 'tables' in project_schemas:
            project_schemas = project_schemas['tables']

        print(f"Processing {project}:")

        for record_type, schema in project_schemas.items():
            if record_type.startswith('_') or not isinstance(schema, dict):
                continue

            if 'file' not in schema:
                continue

            # Backup if requested
            if args.backup and not args.dry_run:
                result = backup_file(project, schema)
                if result:
                    print(result)

            # Process file
            result = process_file(project, record_type, schema, args.dry_run)
            if result:
                print(result)

        print()

    print("=" * 60)
    if args.dry_run:
        print("DRY RUN complete. No files were modified.")
    else:
        print("Processing complete.")
        if args.backup:
            print("Real data kept beside each file as <name>.original")


if __name__ == '__main__':
    main()

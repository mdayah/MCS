#!/usr/bin/env python3
"""
PII audit gate for the web bundles. Run after create_bundles.py; exits
non-zero if any bundle fails.

Checks every .jsdos bundle for:
  1. forbidden paths (.original, .CPY, OFDATA/, _REAL_BACKUP/)
  2. marker strings harvested from the real data — every SSN/EIN-shaped
     string found in the *.original files and _REAL_BACKUP tree must NOT
     appear anywhere in any bundle member.

Deploy pipelines must treat a non-zero exit as a hard stop.
"""
import json
import re
import sys
import zipfile
from pathlib import Path

BASE = Path(__file__).parent.parent.resolve()
BUNDLES = Path(__file__).parent / "bundles"

# digit-boundary lookarounds: a date inside a longer digit run
# ("...10-25-19981254514...") is not an SSN
SSN_RE = re.compile(rb"(?<![\d-])\d{3}-\d{2}-\d{4}(?![\d-])")
EIN_RE = re.compile(rb"(?<![\d-])\d{2}-\d{7}(?![\d-])")

# Real company tax IDs are kept in a gitignored local file (scripts/real_markers.txt)
# so this publishable script hardcodes none of them.
_mk = BASE / "scripts" / "real_markers.txt"
KNOWN_REAL = [ln.strip().encode() for ln in _mk.read_text().splitlines() if ln.strip()] if _mk.exists() else []

# Sources that were ALREADY synthetic when their backup/.original was made
# (first-generation scrub, before this pipeline) — their contents are fake,
# so harvesting them as "real markers" only produces false positives against
# the current same-seed synthetic data. The true originals of these no
# longer exist anywhere.
HARVEST_EXCLUDE = (
    "FOG/DATAFILE", "FOG/MASTER", "FOG/SYSTEM",
    "PFC/AR/CUSTOMER.MAS", "PFC/AP/VENDORS.MAS",
)

# These backups were made after an earlier native scrub, so equality in their
# already-synthetic structured fields is not evidence of a leak. The native
# scrub did not touch free-text remarks, however, so those remain mandatory.
FIRST_GENERATION_SYNTHETIC = {
    "FOG/DATAFILE/CUSTOMER.MAS": {"Rmk"},
    "FOG/DATAFILE/INVENTOR.MAS": {"Rmk"},
    "FOG/MASTER/PASSWORD.MAS": set(),
    "FOG/SYSTEM/CUSTOMER.MAS": {"Rmk"},
    "PFC/AR/CUSTOMER.MAS": set(),
    "PFC/AP/VENDORS.MAS": set(),
}

FORBIDDEN_PARTS = ("_REAL_BACKUP", "OFDATA")
FORBIDDEN_SUFFIXES = (".original", ".CPY", ".cpy")
SCHEMAS = json.loads((BASE / "scripts/schemas.json").read_text())


def unchanged_original_fields(member: str, data: bytes) -> list[str]:
    """Return schema-marked PII fields still identical to the backup.

    Pattern scans cannot prove that names, addresses, free-text notes, or
    packed patient names were replaced. Compare those fields record by record
    at their known offsets instead. Blank fields are ignored.
    """
    parts = member.split("/")
    if len(parts) < 2 or parts[0] not in SCHEMAS:
        return []
    project, rel = parts[0], "/".join(parts[1:])
    member_key = f"{project}/{rel}"
    schema = SCHEMAS[project].get("tables", {}).get(rel)
    if not schema:
        return []
    original = BASE / project / rel
    original = original.with_name(original.name + ".original")
    if not original.exists():
        return []
    before = original.read_bytes()
    record_size = schema["record_size"]
    records = min(len(data), len(before)) // record_size
    problems = []
    offset = 0
    for field in schema["fields"]:
        size = field.get("size", 4)
        if field.get("pii"):
            if (member_key in FIRST_GENERATION_SYNTHETIC
                    and field["name"] not in FIRST_GENERATION_SYNTHETIC[member_key]):
                offset += size
                continue
            unchanged = 0
            for i in range(records):
                start = i * record_size + offset
                old = before[start:start + size]
                if old.strip(b"\x00 ") and data[start:start + size] == old:
                    unchanged += 1
            if unchanged:
                problems.append(f"{field['name']} ({unchanged} records)")
        offset += size
    return problems


def harvest_markers() -> set:
    """All SSN/EIN-shaped strings present in the real data."""
    markers = set()
    sources = list(BASE.rglob("*.original"))
    real_tree = BASE / "_REAL_BACKUP"
    if real_tree.is_dir():
        sources += [p for p in real_tree.rglob("*") if p.is_file()]
    for p in sources:
        rel = p.as_posix()
        if any(x in rel for x in HARVEST_EXCLUDE):
            continue
        try:
            data = p.read_bytes()
        except OSError:
            continue
        markers.update(SSN_RE.findall(data))
        markers.update(EIN_RE.findall(data))
    markers.update(KNOWN_REAL)
    # canonical placeholder SSNs (John Doe test data in CNB) are not PII
    markers -= {b"123-45-6789", b"987-65-4321", b"456-78-9012"}
    # area number 000 is never issued — such values are synthetic by
    # construction (CNB's generated league test data)
    markers = {m for m in markers if not m.startswith(b"000-")}
    return markers


def main():
    markers = harvest_markers()
    print(f"Harvested {len(markers)} SSN/EIN-shaped markers from real data")
    failures = 0
    bundles = sorted(BUNDLES.glob("*.jsdos"))
    if not bundles:
        print("FAIL: no .jsdos bundles found; run web/create_bundles.py first")
        sys.exit(1)
    for bundle in bundles:
        problems = []
        with zipfile.ZipFile(bundle) as zf:
            for info in zf.infolist():
                name = info.filename
                if any(part in name.split("/") for part in FORBIDDEN_PARTS):
                    problems.append(f"forbidden path: {name}")
                    continue
                if name.endswith(FORBIDDEN_SUFFIXES):
                    problems.append(f"forbidden file: {name}")
                    continue
                if info.is_dir():
                    continue
                data = zf.read(name)
                for field in unchanged_original_fields(name, data):
                    problems.append(
                        f"unchanged original PII field {field} in {name}")
                # only marker strings that also exist in real data count —
                # synthetic SSNs are supposed to look like SSNs
                found = set(SSN_RE.findall(data)) | set(EIN_RE.findall(data))
                found |= {k for k in KNOWN_REAL if k in data}
                for m in sorted(found & markers):
                    problems.append(f"REAL marker {m.decode()} in {name}")
        if problems:
            failures += 1
            print(f"FAIL {bundle.name}:")
            for p in problems[:10]:
                print(f"    {p}")
            if len(problems) > 10:
                print(f"    ... and {len(problems) - 10} more")
        else:
            print(f"PASS {bundle.name}")
    if failures:
        print(f"\n{failures} bundle(s) FAILED the PII audit — do not publish.")
        sys.exit(1)
    print("\nAll bundles passed the PII audit.")


if __name__ == "__main__":
    main()

#!/usr/bin/env python3
"""
Whole-tree PII gate for public publication.

Harvests SSN/EIN-shaped markers (and known real company tax IDs) from the real
data still on disk (the *.original siblings and the _REAL_BACKUP tree), then
scans every file that WOULD be committed to the public repo and reports any that
still contain a real marker. Files flagged here must be gitignored from the
public repo and kept only in the private data overlay.

Exit non-zero if anything real is found in a would-be-public file.

Usage: python3 scripts/audit_tree.py
"""
import re
import sys
from pathlib import Path

BASE = Path(__file__).parent.parent.resolve()

SSN_RE = re.compile(rb"(?<![\d-])\d{3}-\d{2}-\d{4}(?![\d-])")
EIN_RE = re.compile(rb"(?<![\d-])\d{2}-\d{7}(?![\d-])")
# Real company tax IDs are kept in a gitignored local file so no publishable
# script hardcodes them. Absent (e.g. in CI) the harvest still covers the rest.
_mk = Path(__file__).parent / "real_markers.txt"
KNOWN_REAL = [ln.strip().encode() for ln in _mk.read_text().splitlines() if ln.strip()] if _mk.exists() else []
# canonical placeholders / non-issued area numbers that are synthetic by construction
PLACEHOLDER = {b"123-45-6789", b"987-65-4321", b"456-78-9012"}

# never scanned and never harvested-from-as-public (real data lives here on purpose)
SKIP_PARTS = {".git", ".venv", "node_modules", "__pycache__", "_REAL_BACKUP"}
SKIP_SUFFIX = (".original",)
# CI builds these; not committed
SKIP_REL_PREFIX = ("web/bundles/",)
# real-PII data files that live only in the PRIVATE overlay (gitignored public)
PRIVATE_PATHS = {"FOG/OFDATA", "scripts/real_markers.txt"}
# files whose only matches are documented FAKE-pattern examples, safe to publish
ALLOWLIST = {"AGENTS.md", "scripts/ANONYMIZE.md"}


def harvest():
    markers = set()
    srcs = list(BASE.rglob("*.original"))
    rb = BASE / "_REAL_BACKUP"
    if rb.is_dir():
        srcs += [p for p in rb.rglob("*") if p.is_file()]
    for p in srcs:
        try:
            d = p.read_bytes()
        except OSError:
            continue
        markers |= set(SSN_RE.findall(d)) | set(EIN_RE.findall(d))
    markers |= set(KNOWN_REAL)
    markers -= PLACEHOLDER
    markers = {m for m in markers if not m.startswith(b"000-")}
    return markers


def public_files():
    for p in BASE.rglob("*"):
        if not p.is_file():
            continue
        rel = p.relative_to(BASE)
        if any(part in SKIP_PARTS for part in rel.parts):
            continue
        if p.name.endswith(SKIP_SUFFIX):
            continue
        relp = rel.as_posix()
        if any(relp.startswith(pre) for pre in SKIP_REL_PREFIX):
            continue
        if any(relp == pp or relp.startswith(pp + "/") for pp in PRIVATE_PATHS):
            continue
        if relp in ALLOWLIST:
            continue
        yield p, rel


def main():
    markers = harvest()
    print(f"Harvested {len(markers)} real SSN/EIN markers from .original + _REAL_BACKUP")
    hits = {}
    for p, rel in public_files():
        try:
            d = p.read_bytes()
        except OSError:
            continue
        found = (set(SSN_RE.findall(d)) | set(EIN_RE.findall(d))
                 | {k for k in KNOWN_REAL if k in d}) & markers
        if found:
            hits[rel.as_posix()] = sorted(m.decode("latin1") for m in found)
    if hits:
        print(f"\n!! {len(hits)} would-be-public file(s) contain REAL PII — keep these PRIVATE:\n")
        for rel in sorted(hits):
            ex = ", ".join(hits[rel][:3])
            print(f"    {rel}   ({len(hits[rel])} markers, e.g. {ex})")
        print("\nGate FAILED. Gitignore these from the public repo (move to the private overlay).")
        sys.exit(1)
    print("\nGate PASSED — no real PII in any would-be-public file.")


if __name__ == "__main__":
    main()

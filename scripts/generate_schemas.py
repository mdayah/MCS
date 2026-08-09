#!/usr/bin/env python3
"""
Generate scripts/schemas.json from the VB DOS TYPE declarations in each
project's source (.REC/.OFF/.BI record includes, plus .BAS), instead of
hand-written guesses.

Binding: for every data file (.MAS/.DTA) in a project, candidate TYPEs are
those whose computed byte size divides the file size exactly (allowing a
small header/EOF slack); ties break on filename<->TYPE-name similarity,
then record-include sources over .BAS, then larger record size. Fields are
flagged pii from their names and declaration comments; whole TYPEs that are
business reference data (chart of accounts, product catalogs, ...) are
exempt. Files nothing binds to are listed in "_unresolved", and derived
index/companion files (.NAM/.CKX/...) in "_companions" — the scrub/audit
step must account for every entry in both.

Usage:
    python3 scripts/generate_schemas.py            # report + write schemas.json
    python3 scripts/generate_schemas.py --dry-run  # report only
"""
import json
import re
import sys
from pathlib import Path

BASE = Path(__file__).parent.parent.resolve()
OUT = Path(__file__).parent / "schemas.json"

PROJECTS = ["MPC", "FOG", "PFC", "RDL", "MCS", "BAJ", "HBS", "IMM", "CNB"]

# Shared-library subdirs nested inside project dirs (git submodules) and
# backup dirs — never scan these for TYPEs or data.
SKIP_DIRS = {"VBASIC", "LIB", "NOVA", "_REAL_BACKUP", ".git", "__pycache__"}

DATA_EXTS = {".MAS", ".DTA"}
COMPANION_EXTS = {".NAM", ".CKX", ".CHK", ".NUM", ".NDX", ".IDX", ".CPY"}

# Allowed non-record bytes in a file (leading header or trailing DOS EOF
# byte). 1 covers the 0x1A CP/M-style EOF marker seen on AC.MAS files.
SLACKS = (0, 1, 2, 4, 8, 128, 256, 512)

# A TYPE smaller than this only binds a file whose name resembles it —
# 8..36-byte index TYPEs divide almost anything by coincidence.
MIN_BLIND_SIZE = 40

SCALAR_SIZES = {
    "INTEGER": 2,
    "LONG": 4,
    "SINGLE": 4,
    "DOUBLE": 8,
    "CURRENCY": 8,
}

# Keywords (matched against field name + comment, lowercased) that mark a
# field as PII. Mirrors what the verified SCRUB.BAS scrubbers replaced.
PII_PATTERNS = [
    r"\bssn?\d?\b", r"social",
    r"name", r"\bnam\b", r"\bnm\d\b",
    r"address", r"\badd\b", r"\bad\d\b", r"street", r"\bstr\b",
    r"\bcity\b", r"\bcty\b",
    r"\bzip\b",
    r"telephone", r"\btel\b", r"phone", r"\bfax\b",
    r"contact", r"\bcnt\b", r"\bord\b", r"\bman\b",
    # ship-to group: Sco 'shipping address' + bare Ste/Sad/SSr/SCt/SZi
    r"\bsco\b", r"\bste\b", r"\bsad\b", r"\bssr\b", r"\bsct\b", r"\bszi\b",
    # contact group: Aph/Afx after a 'contact information' header. Bare
    # Acc/Act are deliberately not patterns: elsewhere they mean accessories
    # and activity code. Ambiguous fields are handled by exact overrides.
    r"\baph\b", r"\bafx\b",
    r"\bbank\b", r"bank account", r"\bban\b",
    r"\btax\b",  # customer Tax(15) holds federal tax IDs
    r"password", r"\bpwd\b", r"\bpas\b", r"\bini\b",
    r"birth", r"\bdob\b", r"\bbd\d\b",
    r"driver", r"license", r"\bdln?\d?\b",
    r"email", r"\beml\b",
    r"employer",
    r"income",
    r"salutation",
    r"remarks?", r"\brmk\b", r"patient",
]
# Field-level vetoes: clearly business/GL constructs, not people.
PII_VETO = [
    r"g/?l account", r"ledger", r"chart of account", r"account type",
    r"terms", r"not used", r"filler",
]
# TYPE-level exemptions: reference/catalog/transaction record types whose
# "name"-ish fields are business data (GL account names, product names,
# dental procedure names, the company's own store locations, zip tables).
TYPE_VETO_RE = re.compile(
    r"account|journal|zipcode|^codes?$|price|rate|combo|material|product"
    r"|recipe|procedure|invoice|order|receipt|timecard|store|center|config|library"
    r"|index|line$|^old$|^actnew$",
    re.IGNORECASE,
)

# Exact exceptions for overloaded legacy abbreviations. These are applied
# after the general name/comment heuristics above.
FIELD_OVERRIDES = {
    ("MPC", "DATA/CUSTOMER.MAS", "Acc"): True,       # bank account
    ("MPC", "DATA/VENDORS.MAS", "Act"): True,       # vendor tax ID
    ("FOG", "ACCOUNTG/AC/VENDORS.MAS", "Act"): True,
    ("PFC", "AP/VENDORS.MAS", "Act"): True,
    ("PFC", "AR/CUSTOMER.MAS", "Acc"): True,        # A/R contact
}


def apply_field_overrides(project, rel, fields):
    """Apply table-aware rules and expand packed fields.

    RDL stores eight 27-byte invoice lines in Dst. Each line begins with a
    13-byte patient name followed by 14 bytes of procedure/amount data. Split
    that field so patient names can be replaced without corrupting invoices.
    """
    out = []
    for field in fields:
        if (project, rel, field["name"]) == ("RDL", "AR/INVOICES.DTA", "Dst"):
            for i in range(8):
                out.append({
                    "name": f"Patient_{i + 1}", "type": "STRING", "size": 13,
                    "pii": True, "desc": "patient name",
                })
                out.append({
                    "name": f"InvoiceLine_{i + 1}", "type": "BYTES", "size": 14,
                    "pii": False, "desc": "procedure, quantity, price, tax",
                })
            continue
        override = FIELD_OVERRIDES.get((project, rel, field["name"]))
        if override is not None:
            field["pii"] = override
        if field.get("pii") and (field["name"].lower() == "rmk"
                                 or "remark" in field.get("desc", "").lower()):
            field["strategy"] = "redact"
        out.append(field)
    return out

TYPE_RE = re.compile(r"^\s*TYPE\s+(\w+)\s*$", re.IGNORECASE)
END_RE = re.compile(r"^\s*END\s+TYPE", re.IGNORECASE)
DECL_RE = re.compile(
    r"^\s*(\w+)\s*(?:\(([^)]*)\))?\s+AS\s+"
    r"(?:(STRING)\s*\*\s*(\d+)|(\w+))\s*$",
    re.IGNORECASE,
)

REC_SOURCES = {".REC", ".OFF", ".BI"}


def array_elements(dims: str) -> int:
    """VB DOS static array inside TYPE, default lower bound 0.
    '1, 6, 3' -> 2*7*4;  '5 TO 9' -> 5."""
    n = 1
    for d in dims.split(","):
        d = d.strip()
        m = re.match(r"(-?\d+)\s+TO\s+(-?\d+)", d, re.IGNORECASE)
        if m:
            n *= int(m.group(2)) - int(m.group(1)) + 1
        elif re.fullmatch(r"-?\d+", d):
            n *= int(d) + 1
        else:
            raise ValueError(f"non-numeric array bound: {dims!r}")
    return n


def parse_types(path: Path, defs: list, problems: list):
    """Append every TYPE block in a source file to defs as
    {"name", "fields", "source"}."""
    try:
        text = path.read_text(encoding="cp437", errors="replace")
    except OSError:
        return
    cur = None
    for raw in text.splitlines():
        code, _, comment = raw.partition("'")
        comment = comment.strip()
        if cur is None:
            m = TYPE_RE.match(code)
            if m:
                cur = {"name": m.group(1), "fields": [], "source": path}
            continue
        if END_RE.match(code):
            defs.append(cur)
            cur = None
            continue
        for part in code.split(":"):
            part = part.strip()
            if not part:
                continue
            m = DECL_RE.match(part)
            if not m:
                problems.append(f"{path.name}: unparsed line in TYPE "
                                f"{cur['name']}: {part!r}")
                continue
            fname, dims, s_kw, s_len, other = m.groups()
            field = {"name": fname, "comment": comment}
            if s_kw:
                field["type"] = "STRING"
                field["size"] = int(s_len)
            else:
                field["type"] = other.upper()
            if dims:
                field["dims"] = dims.strip()
            cur["fields"].append(field)


def resolve_size(tdef, by_name, seen=None):
    """Byte size of a TYPE def; nested TYPE fields resolve via the first
    same-project definition of that name."""
    seen = seen or set()
    if tdef["name"] in seen:
        raise ValueError(f"recursive TYPE {tdef['name']}")
    total = 0
    for f in tdef["fields"]:
        ft = f["type"]
        if ft == "STRING":
            unit = f["size"]
        elif ft in SCALAR_SIZES:
            unit = SCALAR_SIZES[ft]
        elif ft in by_name:
            unit = resolve_size(by_name[ft][0], by_name,
                                seen | {tdef["name"]})
        else:
            raise ValueError(f"TYPE {tdef['name']}: unknown type {ft!r} "
                             f"for field {f['name']}")
        n = array_elements(f["dims"]) if "dims" in f else 1
        f["bytes"] = unit * n
        total += unit * n
    return total


def is_pii(field, type_name) -> bool:
    if TYPE_VETO_RE.search(type_name):
        return False
    if field.get("size", 0) < 2:
        return False  # a 1-byte flag can't hold PII (Typ/Mar/Sex/...)
    hay = (field["name"] + " " + field.get("comment", "")).lower()
    if any(re.search(v, hay) for v in PII_VETO):
        return False
    return any(re.search(p, hay) for p in PII_PATTERNS)


def project_files(proj_dir: Path, exts):
    for p in sorted(proj_dir.rglob("*")):
        if any(part in SKIP_DIRS for part in p.relative_to(proj_dir).parts):
            continue
        if p.is_file() and p.suffix.upper() in exts:
            yield p


def flatten_fields(tdef, by_name, prefix=""):
    """Emit scrubber-format field list; nested TYPEs and arrays expand to
    repeated entries so offsets stay exact."""
    out = []
    for f in tdef["fields"]:
        n = array_elements(f["dims"]) if "dims" in f else 1
        for i in range(n):
            label = prefix + f["name"] + (f"_{i}" if n > 1 else "")
            if f["type"] == "STRING":
                out.append({"name": label, "type": "STRING",
                            "size": f["size"],
                            "pii": is_pii(f, tdef["name"]),
                            "desc": f.get("comment", "")})
            elif f["type"] in SCALAR_SIZES:
                out.append({"name": label, "type": f["type"],
                            "size": SCALAR_SIZES[f["type"]],
                            "pii": False,
                            "desc": f.get("comment", "")})
            else:
                out.extend(flatten_fields(by_name[f["type"]][0], by_name,
                                          label + "."))
    return out


def fit(fsize: int, rsize: int):
    """Return (records, slack) if the file is N records + tolerated slack."""
    if rsize <= 0:
        return None
    for slack in SLACKS:
        if fsize >= slack and (fsize - slack) % rsize == 0:
            return (fsize - slack) // rsize, slack
    return None


def main():
    dry = "--dry-run" in sys.argv
    schemas = {}
    report = []
    all_sized = {}   # proj -> (sized defs, by_name) for cross-project borrowing

    for proj in PROJECTS:
        proj_dir = BASE / proj
        if not proj_dir.is_dir():
            continue
        defs, problems = [], []
        for src in project_files(proj_dir, REC_SOURCES | {".BAS"}):
            parse_types(src, defs, problems)

        by_name = {}
        for d in defs:
            by_name.setdefault(d["name"], []).append(d)
        # size every definition; drop unresolvable ones
        sized = []
        seen_sig = set()
        for d in defs:
            try:
                d["size"] = resolve_size(d, by_name)
            except ValueError as e:
                problems.append(str(e))
                continue
            sig = (d["name"], d["size"],
                   tuple(f["name"] for f in d["fields"]))
            if sig in seen_sig:
                continue  # identical re-declaration in another file
            seen_sig.add(sig)
            sized.append(d)

        all_sized[proj] = (sized, by_name)
        report.append(f"\n### {proj}: {len(sized)} distinct TYPEs "
                      f"from {len(defs)} declarations")
        for p in sorted(set(problems)):
            report.append(f"    parse: {p}")

        proj_schema = {}
        unresolved = []
        for df in project_files(proj_dir, DATA_EXTS):
            rel = df.relative_to(proj_dir).as_posix()
            fsize = df.stat().st_size
            stem = df.stem.upper()

            def sim(d):
                t = d["name"].upper()
                if t.startswith(stem[:6]) or stem.startswith(t[:6]):
                    return 2
                if len(stem) >= 4 and stem[:4] in t:
                    return 1
                return 1 if t[:5] and t[:5] in stem else 0

            cands = []
            for d in sized:
                if fsize == 0:
                    if sim(d) == 2:
                        cands.append((d, 0, 0))
                    continue
                if d["size"] < MIN_BLIND_SIZE and sim(d) == 0:
                    continue
                f = fit(fsize, d["size"])
                if f:
                    cands.append((d, *f))
            cands.sort(key=lambda c: (
                sim(c[0]),
                c[0]["source"].suffix.upper() in REC_SOURCES,
                -c[2],            # less slack is better
                c[0]["size"],     # bigger record beats trivial small types
            ), reverse=True)

            if not cands:
                unresolved.append({"file": rel, "bytes": fsize})
                report.append(f"  {rel}: {fsize}B — NO TYPE FITS (unresolved)")
                continue
            best, recs, slack = cands[0]
            confident = sim(best) > 0
            if not confident:
                # a fit by divisibility alone is a guess, not a schema —
                # never let the scrubber act on it
                unresolved.append({
                    "file": rel, "bytes": fsize,
                    "best_guess": f"{best['name']}({best['size']}B) "
                                  f"x{recs} slack={slack}",
                })
                report.append(f"  {rel}: {fsize}B — name-mismatched fit "
                              f"{best['name']}({best['size']}B) demoted to "
                              f"unresolved")
                continue
            fields = apply_field_overrides(
                proj, rel, flatten_fields(best, by_name))
            pii_names = [f["name"] for f in fields if f["pii"]]
            src_rel = str(best["source"].relative_to(BASE))
            alts = [f"{c[0]['name']}({c[0]['size']})" for c in cands[1:4]]
            flag = "" if confident else "  [name mismatch — REVIEW]"
            report.append(
                f"  {rel}: {fsize}B = {recs} x {best['name']}"
                f"({best['size']}B) slack={slack}{flag}"
                + (f"  alts={alts}" if alts else ""))
            report.append(f"      src={src_rel} pii({len(pii_names)}): "
                          + ", ".join(pii_names)[:140])
            proj_schema[rel] = {
                "file": rel,
                "record_size": best["size"],
                "record_type": best["name"],
                "type_source": src_rel,
                "records": recs,
                "header_or_slack_bytes": slack,
                "confident": confident,
                "fields": fields,
            }

        companions = [df.relative_to(proj_dir).as_posix()
                      for df in project_files(proj_dir, COMPANION_EXTS)]
        entry = {"tables": proj_schema}
        if unresolved:
            entry["_unresolved"] = unresolved
        if companions:
            entry["_companions"] = companions
        if proj_schema or unresolved or companions:
            schemas[proj] = entry
        if companions:
            report.append(f"  companions (regenerate or scrub with master): "
                          + ", ".join(companions))

    # Second pass: a file nothing local fits may use a sibling project's
    # TYPE — the payroll apps share file formats (e.g. MCS's AC.MAS is the
    # 57-byte Accounts record its own newer ACCOUNTS.REC no longer declares).
    report.append("\n### cross-project borrowing")
    for proj, entry in schemas.items():
        for u in list(entry.get("_unresolved", [])):
            stem = Path(u["file"]).stem.upper()
            fsize = u["bytes"]
            best = None
            for other, (sized, by_name) in all_sized.items():
                if other == proj:
                    continue
                for d in sized:
                    t = d["name"].upper()
                    named = t.startswith(stem[:6]) or stem.startswith(t[:6])
                    if not named or fsize == 0:
                        continue
                    f = fit(fsize, d["size"])
                    # borrowing is a last resort: exact fit (or lone EOF
                    # byte) only, otherwise sibling formats bind spuriously
                    if f and f[1] <= 1 and (best is None or f[1] < best[3]):
                        best = (other, d, f[0], f[1], by_name)
            if best is None:
                report.append(f"  {proj}/{u['file']}: still unresolved")
                continue
            other, d, recs, slack, by_name = best
            entry["tables"][u["file"]] = {
                "file": u["file"],
                "record_size": d["size"],
                "record_type": d["name"],
                "type_source": str(d["source"].relative_to(BASE)),
                "borrowed_from": other,
                "records": recs,
                "header_or_slack_bytes": slack,
                "confident": True,
                "fields": apply_field_overrides(
                    proj, u["file"], flatten_fields(d, by_name)),
            }
            entry["_unresolved"].remove(u)
            report.append(f"  {proj}/{u['file']}: {fsize}B = {recs} x "
                          f"{d['name']}({d['size']}B) slack={slack} "
                          f"[borrowed from {other}]")
        if not entry.get("_unresolved") and "_unresolved" in entry:
            del entry["_unresolved"]

    print("\n".join(report))
    if not dry:
        bak = OUT.with_suffix(".json.bak")
        if OUT.exists() and not bak.exists():
            OUT.rename(bak)  # keep the first (hand-written) version only
        OUT.write_text(json.dumps(schemas, indent=2))
        n = sum(len(v.get("tables", {})) for v in schemas.values())
        print(f"\nWrote {OUT} ({n} tables); old file saved as schemas.json.bak")


if __name__ == "__main__":
    main()

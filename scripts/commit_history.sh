#!/bin/bash
# Reconstruct commit history from file modification times, so the public repo's
# timeline reflects when the work was actually done (1988-2006), then the 2026
# preservation work. Submodules (VBASIC/LIB/NOVA) and gitignored files are
# excluded; the submodule pointers are added in a final commit.
set -e
cd "$(dirname "$0")/.."
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

find . -type f \
  -not -path './.git/*' -not -path './_REAL_BACKUP/*' -not -path './.venv/*' \
  -not -path './node_modules/*' -not -path './web/bundles/*' \
  -not -path './VBASIC/*' -not -path './LIB/*' -not -path './NOVA/*' \
  | sed 's|^\./||' | grep -vx '.gitmodules' > "$TMP/all"

git check-ignore --stdin < "$TMP/all" 2>/dev/null | sort -u > "$TMP/ign"
sort -u "$TMP/all" | comm -23 - "$TMP/ign" > "$TMP/keep"
echo "Committing $(wc -l < "$TMP/keep") files across dated commits..."

while IFS= read -r f; do
  printf '%s\t%s\n' "$(stat -c %y "$f" | cut -d' ' -f1)" "$f"
done < "$TMP/keep" > "$TMP/dated"

# Phase 1: his own work, one commit per date it was last touched (through 2006).
for d in $(awk -F'\t' '$1 <= "2006-12-31"{print $1}' "$TMP/dated" | sort -u); do
  awk -F'\t' -v D="$d" '$1==D{print $2}' "$TMP/dated" > "$TMP/list"
  git add --pathspec-from-file="$TMP/list"
  GIT_AUTHOR_DATE="$d 12:00:00" GIT_COMMITTER_DATE="$d 12:00:00" \
    git commit -q -m "Files as of $d" >/dev/null 2>&1 || true
done

# Phase 2: everything modern (preservation work) in a single commit.
awk -F'\t' '$1 > "2006-12-31"{print $2}' "$TMP/dated" > "$TMP/modern"
git add --pathspec-from-file="$TMP/modern"
git add .gitmodules VBASIC LIB NOVA
GIT_AUTHOR_DATE="2026-08-09 12:00:00" GIT_COMMITTER_DATE="2026-08-09 12:00:00" \
  git commit -q -m "Preserve for the web: anonymized demo data, in-browser DOS emulator, docs, and CI-built GitHub Pages" >/dev/null 2>&1 || true

echo "Done: $(git rev-list --count HEAD) commits, $(git log -1 --format=%cd --date=short) latest"

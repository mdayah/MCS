#!/bin/bash

SCRIPT_NAME=$(basename "$0") # Get the name of the script itself
declare -A fileDates

# Populate the associative array, excluding .git directory and the script itself
while IFS= read -r -d '' file; do
    if [[ "$file" != *".git"* && "$file" != "./$SCRIPT_NAME" ]]; then
        # Ensure we're getting the modification date correctly
        DATE=$(stat -c %y "$file" | cut -d ' ' -f 1)
        if [[ -n "$DATE" ]]; then
            fileDates["$DATE"]+="$file "
        fi
    fi
done < <(find . -type f -print0)

# Process dates in chronological order, excluding malformed dates
for date in $(echo "${!fileDates[@]}" | tr ' ' '\n' | grep -v '^00:00:00$' | sort); do
    echo "Processing files for date: $date"
    for file in ${fileDates[$date]}; do
        echo "git add \"$file\""
    done
    # Simulating the commit command with the correct date
    echo "GIT_COMMITTER_DATE=\"$date 12:00:00\" git commit --date=\"$date 12:00:00\" -m \"Added files for $date\""
done

echo "Dry run complete. If the commands are correct, remove the echo commands to perform the git operations."


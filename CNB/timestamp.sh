#!/bin/bash

# Loop through each file in the current directory
for file in CNB.*; do
  if [ -f "$file" ]; then  # Make sure it's a file
    # Get the last modification time of the file
    mod_time=$(stat -c %y "$file" | cut -d' ' -f1,2 | tr ' ' 'T')
    
    # Set the author and committer date to the file's modification time
    GIT_AUTHOR_DATE="$mod_time" GIT_COMMITTER_DATE="$mod_time" git add "$file"
  fi
done
GIT_AUTHOR_DATE="$mod_time" GIT_COMMITTER_DATE="$mod_time" git commit


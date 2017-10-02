#!/bin/sh

# Add gitlab remote
git remote add gitlab https://$USER@gitlab.cis.strath.ac.uk/cs316-2017/assessment-$USER.git && echo "Gitlab remote added successfully.\n"

# Populate gitlab
git push gitlab || exit 1

# Instructions
echo "\n\nAll set! To get the latest changes from github, just execute\n\n    git pull\n\non the command line. To push your changes to gitlab (e.g. for assessment), do \n\n    git push gitlab\n\non the command line. Enjoy!"

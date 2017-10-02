#!/bin/bash

strathclydeformat='[a-z]{3}[0-9]{5}'

# Use current username if it looks correct
if [[ $USER =~ $strathclydeformat ]]; then
  name=$USER
else
  read -p "Gitlab username: " name
fi

# Check if remote already exists
if git remote | grep -q "^gitlab\$" ; then
  echo "Warning: overwriting existing remote 'gitlab' (was $(git config --get remote.gitlab.url))"
  git remote remove gitlab
fi

# Add gitlab remote
git remote add gitlab https://$name@gitlab.cis.strath.ac.uk/cs316-2017/assessment-$name.git && echo -e "Gitlab remote added successfully.\n"

# Populate gitlab
git push gitlab || exit 1

# Instructions
echo -e "\n\nAll set! To get the latest changes from github, just execute\n\n    git pull\n\non the command line. To push your changes to gitlab (e.g. for assessment),\nfirst commit them\n\n    git commit -a -m \"<commit message>\"\n\nthen do \n\n    git push gitlab\n\non the command line. Enjoy!"

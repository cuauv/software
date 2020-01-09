#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'
shopt -s dotglob nullglob

# start config
repoPath="$HOME/opensource/subcode"
githubURL="git@github.com:cuauv/software.git"
dateFormat="%D"
# end config

# use both exclusions, one from the repo, and a backup that is harder to change
exclusions="$repoPath/open-source/exclusions"
exclusionsLocal="$PWD/exclusions"
tmpDir=".gitconfig-tmp"
cd "$repoPath"

# return repo to original state
restoreConfig() {
	[ ! -d "$tmpDir" ] && return

	for file in $tmpDir/*; do
		rm -rf $(basename "$file")
		mv "$file" .
	done

	rm -rf "$tmpDir"
}

# in case we aborted last run
restoreConfig

git pull

# create tmpdir
if [ -d "$tmpDir" ]; then
	rm -rf "$tmpDir"
fi
mkdir "$tmpDir"

# store current git config
mv .git "$tmpDir/"
cp .gitignore "$tmpDir/"

# don't publish some things
cat "$exclusions" "$exclusionsLocal" >> .gitignore

# push to github
git init
git add .
git reset "$tmpDir"
git commit -m "Update $(date +$dateFormat)"
git remote add origin "$githubURL"
git push -u --force origin master

restoreConfig


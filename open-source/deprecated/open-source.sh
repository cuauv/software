#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'
shopt -s dotglob nullglob

# Start config
publicRemote="git@github.com:alexozer/cuauv-test.git"
privateRemote="git@github.com:alexozer/cuauv-test-private.git"

repoPath="$HOME/code/auv-anon"
tmpDir="/tmp/github-push-tmp"

anonName="CUAUV Dev"
anonEmail="leader@cuauv.org"
# End config

# Clean if necessary
if [ "${1:-}" == "clean" ]; then
	echo -n "Cleaning..."
	rm -rf "$repoPath"
	echo "done."
	exit
fi

firstPush=false
anonTarget=""
if [ -d "$repoPath" ]; then
	cd "$repoPath"

	# Get anonymizing commit range
	cd public
	anonTarget="$(git rev-parse HEAD)..HEAD"
	cd ..

	# Commit changes in private repo to public repo
	cd private
	patchCommit="$(git rev-parse HEAD)"
	git pull
	[ "$patchCommit" == "$(git rev-parse HEAD)" ] && exit
	patchTarget="$patchCommit^..HEAD"
	git format-patch --stdout "$patchTarget" | git --work-tree="$repoPath/public" --git-dir="$repoPath/public/.git" am -3
	cd ..

else
	firstPush=true
	anonTarget="master"

	# Set up private repo copy
	mkdir -p "$repoPath"/private
	cd "$repoPath"
	if [ "${1:-}" == "debug" ]; then
		git clone "$privateRemote" --single-branch --depth 5 private
	else
		git clone "$privateRemote" --single-branch private
	fi

	# Set up public repo
	cp -r private public
	cd public
	git remote remove origin
	git remote add origin "$publicRemote"
	cd ..
fi

# Rewrite all commits to hide the author's name and email
cd public
for branch in `ls .git/refs/heads`; do
    # We may be doing multiple rewrites, so we must force subsequent ones.
    # We're throwing away the backups anyway.
	if [ "$branch" == "master" ]; then
		git filter-branch -d "$tmpDir" -f --env-filter "
			export GIT_AUTHOR_NAME='$anonName'
			export GIT_AUTHOR_EMAIL='$anonEmail'
			export GIT_COMMITTER_NAME='$anonName'
			export GIT_COMMITTER_EMAIL='$anonEmail'" "$anonTarget"
	else
		git branch -d "$branch"
	fi
done

if [ "$firstPush" == true ]; then
	git push -u origin master --force
else
	git push
fi

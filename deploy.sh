if [[ $TRAVIS_BRANCH != 'master' ]]; then
	exit
fi

openssl aes-256-cbc -K $encrypted_4ed650af1ebb_key -iv $encrypted_4ed650af1ebb_iv -in deploy_key.enc -out deploy_key -d
chmod 600 deploy_key
eval `ssh-agent -s`
ssh-add deploy_key
cd ..
ssh -T git@github.com
git clone git@github.com:/vshaxe/haxe-hxparser.git
cd haxe-hxparser
cp ../hxparser/hxparserjs.js .
git config user.name 'Travis CI'
git config user.email '<>'
git add -A
git commit -m "Update for vshaxe/hxparser@$TRAVIS_COMMIT"
git push origin
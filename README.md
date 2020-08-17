# Usage
## Install
```
git clone git@github.com:keer2345/keer2345.github.io.git hexo
git checkout source
npm install
cd hexo
git clone git@github.com:keer2345/hexo-theme-next.git themes/next
```

Option:
```
hexo install --registry=https://registry.npm.taobao.org
```
If see like this: `ERROR Script load failed: themes/next/scripts/tags/exturl.js`, run
```
npm install hexo-util --registry=https://registry.npm.taobao.org
```

## Npm Stable Version
If your `public/index.html` and other `*.html` file is empty, maybe your node is unstable version, please change to stable version var `n`:
```
sudo npm i -g n
sudo n stable
```
```
node -v      # v12.18.3
```

# Theme
## Next
```
git clone git@github.com:keer2345/hexo-theme-next.git theme/next
```
Fork theme of next:
```
git remote -v 
git remote add theme-next git@github.com:theme-next/hexo-theme-next.git
git remote -v 

git fetch theme-next
git merge theme-next/master
git push 
```

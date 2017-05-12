```
git clone git@github.com:keer2345/keer2345.github.io.git hexo
git checkout source
cd hexo
git clone git@github.com:keer2345/hexo-theme-next.git theme/next
```

```
hexo install --registry=https://registry.npm.taobao.org
```
If see like this: `ERROR Script load failed: themes/next/scripts/tags/exturl.js`, run
```
npm install hexo-util --registry=https://registry.npm.taobao.org
```

---
title: js vue intro of components props slots
date: 2018-10-17 20:47:16
categories: javascript
tags: vue
---

Article Series:
1. Rendering, Directives, Events
1. **Components, Props, and Slots**
1. Vue-cli
1. Vuex
1. Animations


<center>
![](https://raw.githubusercontent.com/keer2345/storehouse/master/hexo/images/2018/10/02.png)
</center>

<!-- more -->

# Intro to Vue.js: Components, Props, and Slots
## 组件以及数据传递
大大小小的网站通常由不同的部分构成，将它们抽象成许多小的部分从而易于构建，重用以及使得代码更加清晰。我们使用组件来实现，类似：
```html
<header></header>
<aside>
  <sidebar-item v-for="item in items"></sidebar-item>
</aside>
<main>
  <blogpost v-for="post in posts"></blogpost>
</main>
<footer></footer>
```

这是一个很简单的例子，但是可以看到如何构建网站。如果您是代码的维护者，则很容易理解应用的结构并能快速的找出每一部分。

我们来看看 Vue 是如何实现的：



```html
<div id="app">
    <component :text="message"></component>
</div>

<script src="https://cdn.jsdelivr.net/npm/vue@2.5.17/dist/vue.min.js"></script>

<script>
    Vue.component('component', {
        props: ['text'],
        template: `<div>{{text}}</div>`
    });

    var vue = new Vue({
        el: '#app',
        data() {
            return {
                message: "hello vue..."
            }
        }
    })
</script>
```


我们可以重复使用组件，我们也可以对 `props` 添加校验，类似 *React* 的 `PropTypes`。

```html
<div id="app">
    <h3>
        <button @click="increment">+</button>
        Adjust the data
        <button @click="decrement">-</button>
    </h3>
    <h2>
        This is the app data: <span class="num">{{ count }}</span>
    </h2>
    <hr>
    <h4>
        <child count="1"></child>
    </h4>
    <p>This is a child counter that is using a static integer as props</p>
    <hr>
    <h4>
        <child :count="count"></child>
    </h4>
    <p>This is the same child counter and it is using the vue instance data as props</p>
</div>

<script src="https://cdn.jsdelivr.net/npm/vue@2.5.17/dist/vue.min.js"></script>

<script>
    Vue.component('child', {
        props: {
            count: {
                type: Number,
                required: true
            }
        },
        template: `<div class="num">{{count}}</div>`
    });

    var vue = new Vue({
        el: '#app',
        data() {
            return {
                count: 0
            }
        },
        methods: {
            increment() {
                this.count++;
            },
            decrement() {
                this.count--;
            }
        }
    })
</script>
```


```html
<div id="app">
    <img src="https://s3-us-west-2.amazonaws.com/s.cdpn.io/28963/vue-post-photo.jpg" class="main-photo">
    <img src="https://s3-us-west-2.amazonaws.com/s.cdpn.io/28963/vue-main-profile.jpg" class="main-profile">
    <div class="main-info">
        <span class="name">Julianne Delfina</span>
        <h3>"It's lovely after it rains"</h3>
    </div>
    <hr>
    <ul>
        <li is='individual-comment' v-for="comment in  comments" :commentpost="comment"></li>
    </ul>
    <input v-model="newComment" @keyup.enter="addComment" placeholder="Add a comment" />
</div>

<script src="https://cdn.jsdelivr.net/npm/vue@2.5.17/dist/vue.min.js"></script>

<script>
    Vue.component('individual-comment', {
        props: ['commentpost'],
        template: `<li>{{commentpost}}</li>`
    })
    new Vue({
        el: '#app',
        data() {
            return {
                newComment: '',
                comments: [
                    'Looks great Julianne!',
                    'I love the sea',
                    'Where are you at?'
                ]
            }
        },
        methods: {
            addComment() {
                this.comments.push(this.newComment);
                this.newComment = '';
            }
        }
    })
</script>
```

```html
<div id="app">
    <img src="https://s3-us-west-2.amazonaws.com/s.cdpn.io/28963/vue-post-photo.jpg" class="main-photo">
    <img src="https://s3-us-west-2.amazonaws.com/s.cdpn.io/28963/vue-main-profile.jpg" class="main-profile">
    <div class="main-info">
        <span class="name">Julianne Delfina</span>
        <h3>"It's lovely after it rains"</h3>
    </div>
    <hr>
    <ul>
        <li is='individual-comment' v-for="comment in  comments" :commentpost="comment"></li>
    </ul>
    <input v-model="newComment" @keyup.enter="addComment" placeholder="Add a comment" />
</div>

<script src="https://cdn.jsdelivr.net/npm/vue@2.5.17/dist/vue.min.js"></script>

<script type="text/x-template" id="comment-template">
    <li>
        <img class="post-img" :src="commentpost.authorImg"/>
        <small>{{commentpost.author}}</small>
            <p class="comment-post">{{commentpost.text}}</p>
    </li>
/script>

<script>
    Vue.component('individual-comment', {
        props: ['commentpost'],
        template: '#comment-template'
    })
    new Vue({
        el: '#app',
        data() {
            return {
                newComment: '',
                comments: [
                    {
                        text: 'Looks great Julianne!',
                        author: 'Robin Rendle',
                        authorImg: 'https://s3-us-west-2.amazonaws.com/s.cdpn.io/28963/v-coffee.jpg'
                    },
                    {
                        text: 'I love the Sea',
                        author: 'Miriam Suzanne',
                        authorImg: 'https://s3-us-west-2.amazonaws.com/s.cdpn.io/28963/v-miriam.jpg'
                    },
                    {
                        text: 'Where are you?',
                        author: 'Geoff Graham',
                        authorImg: 'https://s3-us-west-2.amazonaws.com/s.cdpn.io/28963/v-geoff.jpg'
                    }
                ]
            }
        },
        methods: {
            addComment: function () {
                const newCommentObj = {
                    text: this.newComment,
                    author: 'Magoo',
                    authorImg: 'https://s3-us-west-2.amazonaws.com/s.cdpn.io/28963/v-skull.jpg'
                };
                this.comments.push(newCommentObj);
                this.newComment = '';
            }
        }
    })
</script>
```

## 插槽Slots
当两个组件有细微差异，内容和样式也有所差异时，可以通过重用或移植组件，我们通过插槽（slots）来处理。
```html
<div id="app">
    <h2>We can use slots to populate content</h2>
    <app-child>
        <h3>This is slot number one</h3>
    </app-child>
    <app-child>
        <h3>This is slot number two</h3>
        <small>I can put more info in, too!</small>
    </app-child>
    <app-child>
    </app-child>
</div>

<script src="https://cdn.jsdelivr.net/npm/vue@2.5.17/dist/vue.min.js"></script>

<script type="text/x-template" id="childarea">
    <div class="child">
        <p>begin</p>
        <slot></slot>
        <p>
            It's a veritable slot machine!<br> 
            Ha ha aw
        </p>
    </div>
/script>

<script>
    const Child = {
        template: '#childarea'
    };

    new Vue({
        el: '#app',
        components: {
            appChild: Child
        }
    })
</script>
```

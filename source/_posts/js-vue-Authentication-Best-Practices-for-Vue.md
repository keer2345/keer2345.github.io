---
title: Authentication Best Practices for Vue
date: 2018-02-08 23:39:37
categories: javascript
tags: [vue]
---
Vue之认证最佳实践
> https://blog.sqreen.io/authentication-best-practices-vue/

# 介绍
当我们开始一个项目，必会面对如何处理基于token客户端的认证。

我们将解答这些问题：
- 如何存储用户token
- 如何重定向用户认证后的行为（Login/ Logout）
- 如何防止路由访问认证与未认证的用户

然而，要知道所有的项目有着不同的认证行为。项目在你登录Gmail之后加载页面，或者在没有登录是访问到一些特性（比如Amazon)，所以我们将在文中调整一些叙述。

# 开始之前
代码存放在[sqreen/vue-authentication-example](https://github.com/sqreen/vue-authentication-example)。我们使用Vuex作为状态管理库，Vuex尤其适合作为应用范围的权限管理。如果不想使用Vuex，别担心，我们也提供了非Vuex的代码示例，我们使用[axios/axios](https://github.com/axios/axios)库用于AJAX调用。

# 登录
让我们从一个简单的登录表单开始：


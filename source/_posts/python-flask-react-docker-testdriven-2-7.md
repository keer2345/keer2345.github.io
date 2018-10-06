---
title: Flask React Docker in Testdriven - Part II - 7
date: 2018-09-25 20:49:30
tags: [testdriven, flask, react, docker]
---
# React Forms
本节，我们创建一个功能组件来添加用户。

添加两个文件：
```
touch services/client/src/components/AddUser.jsx
touch services/client/src/components/__tests__/AddUser.test.jsx
```

<!-- more -->

开始测试：
```javascript
import React from 'react';
import { shallow } from 'enzyme';
import renderer from 'react-test-renderer';

import AddUser from '../AddUser';

test('AddUser renders properly', () => {
  const wrapper = shallow(<AddUser/>);
  const element = wrapper.find('form');
  expect(element.find('input').length).toBe(3);
  expect(element.find('input').get(0).props.name).toBe('username');
  expect(element.find('input').get(1).props.name).toBe('email');
  expect(element.find('input').get(2).props.type).toBe('submit');
});
```

运行测试：
```
yarn test
```

> 如果测试的时候出现 `watch ENOSPC` 之类的错误，主要是因为 gulp 的 watch 需要监听很多文件的改动，但是 fedora、ubuntu 系统的文件句柄其实是有限制的。可以使用如下命令：
> ```
> echo fs.inotify.max_user_watches=524288 | sudo tee -a /etc/sysctl.conf && sudo sysctl -p
> ```



现在测试是失败的，接着来添加组件：
```javascript
import React from 'react';

const AddUser = (props) => {
  return (
    <form>
      <div className="field">
        <input
          name="username"
          className="input is-large"
          type="text"
          placeholder="Enter a username"
          required
        />
      </div>
      <div className="field">
        <input
          name="email"
          className="input is-large"
          type="email"
          placeholder="Enter an email address"
          required
        />
      </div>
      <input
        type="submit"
        className="button is-primary is-large is-fullwidth"
        value="Submit"
      />
    </form>
  )
};

export default AddUser;
```

在 `index.js` 中导入：
```
import AddUser from './components/AddUser';
```

然后更新 `render` 方法：
```javascript
render() {
  return (
    <section className="section">
      <div className="container">
        <div className="columns">
          <div className="column is-half">  {/* new */}
            <br/>
            <h1 className="title is-1">All Users</h1>
            <hr/><br/>
            <AddUser/>  {/* new */}
            <br/><br/>  {/* new */}
            <UsersList users={this.state.users}/>
          </div>
        </div>
      </div>
    </section>
  )
}
```

确保`users` 服务已启动并正在运行且 `REACT_APP_USERS_SERVICE_URL` 已设置环境变量：
```
$ export REACT_APP_USERS_SERVICE_URL=http://localhost
$ docker-compose -f docker-compose-dev.yml up -d --build
```
```
yarn start
```
如果一切顺利，您应该看到表单和用户。

<center>
![](https://raw.githubusercontent.com/keer2345/storehouse/master/hexo/images/2018/0923/008.png)
</center>

确保测试过去：
```
PASS  src/components/__tests__/UsersList.test.jsx
PASS  src/components/__tests__/AddUser.test.jsx

Test Suites: 2 passed, 2 total
Tests:       3 passed, 3 total
Snapshots:   1 passed, 1 total
Time:        0.593s, estimated 1s
Ran all test suites related to changed files.
```
有了它，让我们为 `AddUser.test.jsx` 添加一个快照测试：
```javascript
test('AddUser renders a snapshot properly', () => {
  const tree = renderer.create(<AddUser/>).toJSON();
  expect(tree).toMatchSnapshot();
});
```

现在，由于这是一个单页面应用程序，我们希望在提交表单时阻止正常的浏览器行为以避免页面刷新。

步骤：
1. 处理表单提交事件
1. 获取用户输入
1. 发送AJAX请求
1. 更新页面

## 处理表单提交事件
处理 submit 事件，只需要更新 *AddUser.jsx* 的 `form` 元素：
```javascript
<form onSubmit={(event) => event.preventDefault()}>
```


输入虚拟用户名和电子邮件地址，然后尝试提交表单。什么都不应该发生，这正是我们想要的。

接下来，将以下方法添加到 `App` 组件：
```javascript
addUser(event) {
  event.preventDefault();
  console.log('sanity check!');
};
```

由于 `AddUser` 是一个功能组件，我们需要通过 `props` 将此方法传递给它。像这样更新方法中的 `AddUser` 元素 `render` ：
```javascript
<AddUser addUser={this.addUser}/>
```

然后，再次更新构造器：
```javascript
constructor() {
  super();
  this.state = {
    users: []
  };
  this.addUser = this.addUser.bind(this);  // new
};
```

在这里，我们 `this` 通过以下方式绑定上下文 `bind()` ：
```javascript
this.addUser = this.addUser.bind(this);
```

没有它的话，`this` 内部的上下文是不正确的。想测试一下吗？只需添加 `console.log(this)` 到 `addUser()` 并提交。上下文是什么？移除 `bind` 再次测试，上下文又是什么？

> 有关更多信息，请查看官方React文档中的[处理事件](https://reactjs.org/docs/handling-events.html)。

在浏览器中测试它。您应该 `sanity check!` 在表单提交的 JavaScript 控制台中看到。


<center>
![](https://raw.githubusercontent.com/keer2345/storehouse/master/hexo/images/2018/0923/009.png)
</center>


## 获取用户输入
我们将使用受控组件来获取用户提交的输入。首先向 `App` 组件中的状态对象添加两个新属性：
```javascript
  this.state = {
    users: [],
    username: '',
    email: '',
  };
```

然后，将它们传递给组件：
```javascript
<AddUser
  username={this.state.username}
  email={this.state.email}
  addUser={this.addUser}
/>
```

现在可以通过 `props` 对象访问它们，它可以用作输入的当前值，如下所示：

```javascript
<div className="field">
  <input
    name="username"
    className="input is-large"
    type="text"
    placeholder="Enter a username"
    required
    value={props.username}  // new
  />
</div>
<div className="field">
  <input
    name="email"
    className="input is-large"
    type="email"
    placeholder="Enter an email address"
    required
    value={props.email}  // new
  />
</div>
```

因此，这定义了父组件的输入值。立即测试表格。如果您尝试添加用户名会怎样？您不应该看到任何类型的内容，因为该值是从父级“推”下来的 — 而且该值是 `''` 。

如果将这些值的初始状态设置为 `test` 字符串而不是空字符串，您认为会发生什么？尝试一下。

我们如何更新父组件中的状态，以便在用户在输入框中输入文本时更新？

首先，`handleChange` 向 `App` 组件添加一个方法：
```javascript
handleChange(event) {
  const obj = {};
  obj[event.target.name] = event.target.value;
  this.setState(obj);
};
```

将绑定添加到构造函数：
```javascript
this.handleChange = this.handleChange.bind(this);
```
然后，将方法传递给组件：
```javascript
<AddUser
  username={this.state.username}
  email={this.state.email}
  addUser={this.addUser}
  handleChange={this.handleChange}  // new
/>
```
将其添加到表单输入：

```javascript
<div className="field">
  <input
    name="username"
    className="input is-large"
    type="text"
    placeholder="Enter a username"
    required
    value={props.username}
    onChange={props.handleChange}  // new
  />
</div>
<div className="field">
  <input
    name="email"
    className="input is-large"
    type="email"
    placeholder="Enter an email address"
    required
    value={props.email}
    onChange={props.handleChange}  // new
  />
</div>
```
现在测试表格。它应该工作。如果好奇，您可以通过在 `addUser` 方法中将其记录到控制台来查看状态的值：
```javascript
addUser(event) {
  event.preventDefault();
  console.log('sanity check!');
  console.log(this.state);
};
```


<center>
![](https://raw.githubusercontent.com/keer2345/storehouse/master/hexo/images/2018/0923/010.png)
</center>


现在我们有了这些值，让我们触发AJAX请求，将数据添加到数据库，然后更新DOM。


## 发送AJAX请求
将注意力转回 `users` 服务。我们需要在 JSON 有效负载中发送什么来添加用户 — 用户名和电子邮件，是这样吧？
```
db.session.add(User(username=username, email=email))
```

使用 Axios 发送 POST 请求：
```javascript
addUser(event) {
  event.preventDefault();
  // new
  const data = {
    username: this.state.username,
    email: this.state.email
  };
  // new
  axios.post(`${process.env.REACT_APP_USERS_SERVICE_URL}/users`, data)
  .then((res) => { console.log(res); })
  .catch((err) => { console.log(err); });
};
```

测试一下。请务必使用唯一的用户名和电子邮件。虽然现在这并不重要，但由于将在数据库表中添加唯一约束，因此将来会出现问题。

<center>
![](https://raw.githubusercontent.com/keer2345/storehouse/master/hexo/images/2018/0923/011.png)
</center>


> 如果您遇到问题，请从 Chrome 开发者工具的“[网络](https://developers.google.com/web/tools/chrome-devtools/#network)”标签中分析响应对象。您还可以在 Docker 之外启动 `users` 服务并使用 Flask 调试器或 `print` 语句进行调试。

## 更新页面

最后，让我们更新成功表单提交的用户列表，然后清除表单：

```javascript
addUser(event) {
  event.preventDefault();
  const data = {
    username: this.state.username,
    email: this.state.email
  };
  axios.post(`${process.env.REACT_APP_USERS_SERVICE_URL}/users`, data)
  .then((res) => {
    this.getUsers();  // new
    this.setState({ username: '', email: '' });  // new
  })
  .catch((err) => { console.log(err); });
};
```


就是这样。手动测试它。然后，运行测试套件。更新快照测试（按 `u` 键盘上的）。

```
 PASS  src/components/__tests__/AddUser.test.jsx
 PASS  src/components/__tests__/UsersList.test.jsx

Test Suites: 2 passed, 2 total
Tests:       4 passed, 4 total
Snapshots:   2 passed, 2 total
Time:        0.16s, estimated 1s
Ran all test suites related to changed files.
```
然后提交您的代码。

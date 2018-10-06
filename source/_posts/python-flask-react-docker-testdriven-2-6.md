---
title: Flask React Docker in Testdriven - Part II - 6
date: 2018-09-25 20:49:29
tags: [testdriven, flask, react, docker]
---

# Testing React
*Create React App* [默认](https://github.com/facebookincubator/create-react-app/blob/master/packages/react-scripts/template/README.md#running-tests)使用 [Jest](https://facebook.github.io/jest/) 进行测试，所以我们可以开始编写测试规范,而无需安装跑步。与 Jest 一起，我们将使用 [Enzyme](https://github.com/airbnb/enzyme) ，这是一个专门用于测试 React 组件的出色实用程序库。

安装 enzyme-adapter-react-16 ：
```
$ yarn add enzyme enzyme-adapter-react-16 -D
```

<!-- more -->

要配置 Enzyme 以使用 React 16 适配器，请将新文件添加到 `src/setupTests.js`:
```javascript
import { configure } from 'enzyme';
import Adapter from 'enzyme-adapter-react-16';

configure({ adapter: new Adapter() });
```

> 有关设置Enzyme的更多信息，请查看[官方文档](http://airbnb.io/enzyme/#installation)。


运行测试：
```
$ yarn test
```

你应该看到：
```
No tests found related to files changed since last commit.
```
默认情况下，测试以 [watch](http://facebook.github.io/jest/docs/en/cli.html#watch) 模式运行，因此每次保存文件时都会重新运行测试。


## 测试组件
`components/__tests__/UsersList.test.jsx`:
```javascript
import React from 'react';
import { shallow } from 'enzyme';

import UsersList from '../UsersList';

const users = [
  {
    'active': true,
    'email': 'hermanmu@gmail.com',
    'id': 1,
    'username': 'michael'
  },
  {
    'active': true,
    'email': 'michael@mherman.org',
    'id': 2,
    'username': 'michaelherman'
  }
];

test('UsersList renders properly', () => {
  const wrapper = shallow(<UsersList users={users}/>);
  const element = wrapper.find('h4');
  expect(element.length).toBe(2);
  expect(element.get(0).props.children).toBe('michael');
});
```

在这个测试中，我们使用 [shallow](http://airbnb.io/enzyme/docs/api/shallow.html) 辅助方法来创建UsersList组件，然后我们检索输出并对其进行断言。重要的是要注意，通过 [shallow rendering](https://reactjs.org/docs/shallow-renderer.html) ，我们可以完全隔离测试组件，这有助于确保子组件不会间接影响断言。

> 欲了解更多关于 *shallow rendering* ，以再现部件测试等方法一起，`mount` 和 `render` ，请查看 [Stack Overflow article](https://stackoverflow.com/a/38747914/1799408) 。

运行测试以确保它通过。
```
PASS  src/components/__tests__/UsersList.test.jsx
 ✓ UsersList renders properly (4ms)

Test Suites: 1 passed, 1 total
Tests:       1 passed, 1 total
Snapshots:   0 total
Time:        0.118s, estimated 1s
Ran all test suites related to changed files
```

## 快照测试
接下来，添加 Snapshot](http://facebook.github.io/jest/docs/en/snapshot-testing.html) 测试以确保 UI 不会更改：
```javascript
test('UsersList renders a snapshot properly', () => {
  const tree = renderer.create(<UsersList users={users}/>).toJSON();
  expect(tree).toMatchSnapshot();
});
```
将导入添加到顶部：
```javascript
import renderer from 'react-test-renderer';
```

运行测试：
```
PASS  src/components/__tests__/UsersList.test.jsx
 ✓ UsersList renders properly (3ms)
 ✓ UsersList renders a snapshot properly (9ms)

Snapshot Summary
› 1 snapshot written in 1 test suite.

Test Suites: 1 passed, 1 total
Tests:       2 passed, 2 total
Snapshots:   1 added, 1 total
Time:        0.468s, estimated 2s
Ran all test suites related to changed files.
```

在第一次测试运行时，组件输出的快照将保存到“__snapshots__”文件夹中：
```
// Jest Snapshot v1, https://goo.gl/fbAQLP

exports[`UsersList renders a snapshot properly 1`] = `
<div>
  <h4
    className="box title is-4"
  >
    michael
  </h4>
  <h4
    className="box title is-4"
  >
    michaelherman
  </h4>
</div>
`;
```


在后续测试运行期间，新输出将与保存的输出进行比较。如果它们不同，测试将失败。

让我们快速进行健全检查！

随着观看方式的测试，更改 `{user.username}` 到 `{user.email}` 的 *UsersList* 组件。保存更改以触发新的测试运行。您应该看到两个测试都失败了，这正是我们想要的：

```
FAIL  src/components/__tests__/UsersList.test.jsx
  ● UsersList renders a snapshot properly

    expect(value).toMatchSnapshot()

    Received value does not match stored snapshot 1.

    - Snapshot
    + Received

     <div>
       <h4
         className="box title is-4"
       >
    -    michael
    +    hermanmu@gmail.com
       </h4>
       <h4
         className="box title is-4"
       >
    -    michaelherman
    +    michael@mherman.org
       </h4>
     </div>

      at Object.<anonymous>.test (src/components/__tests__/UsersList.test.jsx:24:16)
          at new Promise (<anonymous>)
      at Promise.resolve.then.el (node_modules/p-map/index.js:46:16)
      at process._tickCallback (internal/process/next_tick.js:68:7)

  ● UsersList renders properly

    expect(received).toBe(expected)

    Expected value to be (using ===):
      "michael"
    Received:
      "hermanmu@gmail.com"

      at Object.<anonymous>.test (src/components/__tests__/UsersList.test.jsx:31:41)
          at new Promise (<anonymous>)
      at Promise.resolve.then.el (node_modules/p-map/index.js:46:16)
      at process._tickCallback (internal/process/next_tick.js:68:7)

  ✕ UsersList renders a snapshot properly (5ms)
  ✕ UsersList renders properly (3ms)

Snapshot Summary
 › 1 snapshot test failed in 1 test suite. Inspect your code changes or press `u` to update them.

Test Suites: 1 failed, 1 total
Tests:       2 failed, 2 total
Snapshots:   1 failed, 1 total
Time:        0.909s, estimated 1s
Ran all test suites related to changed files.
```
现在，如果这是有意的更改，则需要[更新快照](http://facebook.github.io/jest/docs/en/snapshot-testing.html#updating-snapshots)。要这样做，只需按下u键：
```
Watch Usage
 › Press a to run all tests.
 › Press u to update failing snapshots.
 › Press p to filter by a filename regex pattern.
 › Press t to filter by a test name regex pattern.
 › Press q to quit watch mode.
 › Press Enter to trigger a test run.
```

尝试一下 —— 按 `u` 。测试将再次运行，快照测试应该通过：
```
 FAIL  src/components/__tests__/UsersList.test.jsx
  ● UsersList renders properly

    expect(received).toBe(expected)

    Expected value to be (using ===):
      "michael"
    Received:
      "hermanmu@gmail.com"

      at Object.<anonymous>.test (src/components/__tests__/UsersList.test.jsx:31:41)
          at new Promise (<anonymous>)
      at Promise.resolve.then.el (node_modules/p-map/index.js:46:16)
      at process._tickCallback (internal/process/next_tick.js:68:7)

  ✓ UsersList renders a snapshot properly (2ms)
  ✕ UsersList renders properly (2ms)

Snapshot Summary
 › 1 snapshot updated in 1 test suite.

Test Suites: 1 failed, 1 total
Tests:       1 failed, 1 passed, 2 total
Snapshots:   1 updated, 1 total
Time:        0.093s, estimated 1s
Ran all test suites related to changed files.
```

完成后，还原我们刚刚在组件中进行的更改并更新测试。确保它们在继续前通过。


## 测试覆盖率
```
$ react-scripts test --coverage
```

> 您可能需要全局安装React Scripts：`$ npm install react-scripts -g`

```
PASS  src/components/__tests__/UsersList.test.jsx
  ✓ UsersList renders a snapshot properly (11ms)
  ✓ UsersList renders properly (5ms)

Test Suites: 1 passed, 1 total
Tests:       2 passed, 2 total
Snapshots:   1 passed, 1 total
Time:        1.122s
Ran all test suites.
---------------------------|----------|----------|----------|----------|-------------------|
File                       |  % Stmts | % Branch |  % Funcs |  % Lines | Uncovered Line #s |
---------------------------|----------|----------|----------|----------|-------------------|
All files                  |     7.14 |        0 |     8.33 |     12.9 |                   |
 src                       |     1.89 |        0 |        0 |     3.57 |                   |
  index.js                 |        0 |        0 |        0 |        0 |... 18,19,20,23,40 |
  registerServiceWorker.js |        0 |        0 |        0 |        0 |... 36,137,138,139 |
  setupTests.js            |      100 |      100 |      100 |      100 |                   |
 src/components            |      100 |      100 |      100 |      100 |                   |
  UsersList.jsx            |      100 |      100 |      100 |      100 |                   |
---------------------------|----------|----------|----------|----------|-------------------|
```

## 测试交互
Enzyme 可用于测试用户交互。我们可以模拟操作和事件，然后测试实际结果是否与预期结果相同。我们将在未来的课程中看到这一点。

> 值得注意的是，我们将把 React 测试的大部分内容集中在单个测试单个组件上。我们将让端到端测试处理测试用户交互以及客户端和服务器之间的交互。

## `requestAnimationFrame` polyfill错误
测试运行时是否收到此错误？
```
console.error node_modules/fbjs/lib/warning.js:33
    Warning: React depends on requestAnimationFrame.
    Make sure that you load a polyfill in older browsers.
    http://fb.me/react-polyfills
```

如果是这样，将新文件 `__mocks__` 的 “services/client/src/components” ，然后将文件添加到名为 react.js 的文件夹中：
```
const react = require('react');
// Resolution for requestAnimationFrame not supported in jest error :
// https://github.com/facebook/react/issues/9102#issuecomment-283873039
global.window = global;
window.addEventListener = () => {};
window.requestAnimationFrame = () => {
  throw new Error('requestAnimationFrame is not supported in Node');
};

module.exports = react;
```

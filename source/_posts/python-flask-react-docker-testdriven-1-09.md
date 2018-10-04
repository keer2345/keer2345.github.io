---
title: Flask React Docker in Testdriven - Part I - 09
date: 2018-09-23 20:49:19
tags: [testdriven, flask, react, docker]
---

# RESTful Routes
这一节，我们通过 TDD 设置三个新的路由，遵循 RESTful 最佳实践。

Endpoint|HTTP Method|CRUD Method|Result
----|----|----|----
/users|GET|READ|get all users
/users/:id|GET|READ|get single user
/users|POST|CREATE|add a user

我们要实现：
1. 写一个测试
1. 运行测试，看着它失败（**红色**）
1. 编写足够的代码以使测试通过（**绿色**）
1. **重构**（如有必要）


我们开始写 POST 路由……

<!-- more -->

## Post
添加测试到 `TestUserService()`。

`project/test/test_users.py`:
```python
def test_add_user(self):
    """Ensure a new user can be added to the database."""
    with self.client:
        response = self.client.post(
            '/users',
            data=json.dumps({
                'username': 'michael',
                'email': 'michael@mherman.org'
            }),
            content_type='application/json',
        )
        data = json.loads(response.data.decode())
        self.assertEqual(response.status_code, 201)
        self.assertIn('michael@mherman.org was added!', data['message'])
        self.assertIn('success', data['status'])
```

运行测试，可以看到它是失败的：
```
$ docker-compose -f docker-compose-dev.yml run users python manage.py test
```

然后将路由处理程序添加到 `project/api/users.py` :
```python
@users_blueprint.route('/users', methods=['POST'])
def add_user():
    post_data = request.get_json()
    username = post_data.get('username')
    email = post_data.get('email')
    db.session.add(User(username=username, email=email))
    db.session.commit()
    response_object = {
        'status': 'success',
        'message': f'{email} was added!'
    }
    return jsonify(response_object), 201
```


同时更新导入：
```python
from flask import Blueprint, jsonify, request

from project.api.models import User
from project import db
```

运行测试。他们都应该通过：
```
Ran 5 tests in 0.092s

OK
```

那么错误和异常呢？类似：
1. 不发送有效负载
1. 负载无效 - 即JSON对象为空或包含错误的密钥
1. 用户已存在于数据库中

添加一些测试：
```python
def test_add_user_invalid_json(self):
    """Ensure error is thrown if the JSON object is empty."""
    with self.client:
        response = self.client.post(
            '/users',
            data=json.dumps({}),
            content_type='application/json',
        )
        data = json.loads(response.data.decode())
        self.assertEqual(response.status_code, 400)
        self.assertIn('Invalid payload.', data['message'])
        self.assertIn('fail', data['status'])

def test_add_user_invalid_json_keys(self):
    """
    Ensure error is thrown if the JSON object does not have a username key.
    """
    with self.client:
        response = self.client.post(
            '/users',
            data=json.dumps({'email': 'michael@mherman.org'}),
            content_type='application/json',
        )
        data = json.loads(response.data.decode())
        self.assertEqual(response.status_code, 400)
        self.assertIn('Invalid payload.', data['message'])
        self.assertIn('fail', data['status'])

def test_add_user_duplicate_email(self):
    """Ensure error is thrown if the email already exists."""
    with self.client:
        self.client.post(
            '/users',
            data=json.dumps({
                'username': 'michael',
                'email': 'michael@mherman.org'
            }),
            content_type='application/json',
        )
        response = self.client.post(
            '/users',
            data=json.dumps({
                'username': 'michael',
                'email': 'michael@mherman.org'
            }),
            content_type='application/json',
        )
        data = json.loads(response.data.decode())
        self.assertEqual(response.status_code, 400)
        self.assertIn(
            'Sorry. That email already exists.', data['message'])
        self.assertIn('fail', data['status'])
```

测试失败，然后更新路由处理程序：

```python
@users_blueprint.route('/users', methods=['POST'])
def add_user():
    post_data = request.get_json()
    response_object = {
        'status': 'fail',
        'message': 'Invalid payload.'
    }
    if not post_data:
        return jsonify(response_object), 400
    username = post_data.get('username')
    email = post_data.get('email')
    try:
        user = User.query.filter_by(email=email).first()
        if not user:
            db.session.add(User(username=username, email=email))
            db.session.commit()
            response_object['status'] = 'success'
            response_object['message'] = f'{email} was added!'
            return jsonify(response_object), 201
        else:
            response_object['message'] = 'Sorry. That email already exists.'
            return jsonify(response_object), 400
    except exc.IntegrityError as e:
        db.session.rollback()
        return jsonify(response_object), 400
```

添加导入：
```python
from sqlalchemy import exc
```


确保测试通过，然后转到下一个路线......


## GET single user
从测试开始：

```python
from project import db
from project.api.models import User

# ...

def test_single_user(self):
    """Ensure get single user behaves correctly."""
    user = User(username='michael', email='michael@mherman.org')
    db.session.add(user)
    db.session.commit()
    with self.client:
        response = self.client.get(f'/users/{user.id}')
        data = json.loads(response.data.decode())
        self.assertEqual(response.status_code, 200)
        self.assertIn('michael', data['data']['username'])
        self.assertIn('michael@mherman.org', data['data']['email'])
        self.assertIn('success', data['status'])
```


在编写视图之前确保测试中断：

```python
@users_blueprint.route('/users/<user_id>', methods=['GET'])
def get_single_user(user_id):
    """Get single user details"""
    user = User.query.filter_by(id=user_id).first()
    response_object = {
        'status': 'success',
        'data': {
            'id': user.id,
            'username': user.username,
            'email': user.email,
            'active': user.active
        }
    }
    return jsonify(response_object), 200
```


测试应该通过。现在来看一些错误处理：
1. 未提供 `id`
1. `id` 不存在

```python
def test_single_user_no_id(self):
    """Ensure error is thrown if an id is not provided."""
    with self.client:
        response = self.client.get('/users/blah')
        data = json.loads(response.data.decode())
        self.assertEqual(response.status_code, 404)
        self.assertIn('User does not exist', data['message'])
        self.assertIn('fail', data['status'])

def test_single_user_incorrect_id(self):
    """Ensure error is thrown if the id does not exist."""
    with self.client:
        response = self.client.get('/users/999')
        data = json.loads(response.data.decode())
        self.assertEqual(response.status_code, 404)
        self.assertIn('User does not exist', data['message'])
        self.assertIn('fail', data['status'])
```

升级视图：
```python
@users_blueprint.route('/users/<user_id>', methods=['GET'])
def get_single_user(user_id):
    """Get single user details"""
    response_object = {
        'status': 'fail',
        'message': 'User does not exist'
    }
    try:
        user = User.query.filter_by(id=int(user_id)).first()
        if not user:
            return jsonify(response_object), 404
        else:
            response_object = {
                'status': 'success',
                'data': {
                    'id': user.id,
                    'username': user.username,
                    'email': user.email,
                    'active': user.active
                }
            }
            return jsonify(response_object), 200
    except ValueError:
        return jsonify(response_object), 404
```


## GET all users
再次，让我们从测试开始。由于我们必须首先添加一些用户，所以让我们在 `project/tests/test_users.py`， 文件的顶部添加一个快速帮助函数，就在 `TestUserService()` 类的上方。

```python
def add_user(username, email):
    user = User(username=username, email=email)
    db.session.add(user)
    db.session.commit()
    return user
```

现在，重构 `test_single_user()` 测试，如下所示：

```python
def test_single_user(self):
    """Ensure get single user behaves correctly."""
    user = add_user('michael', 'michael@mherman.org')
    with self.client:
        response = self.client.get(f'/users/{user.id}')
        data = json.loads(response.data.decode())
        self.assertEqual(response.status_code, 200)
        self.assertIn('michael', data['data']['username'])
        self.assertIn('michael@mherman.org', data['data']['email'])
        self.assertIn('success', data['status'])
```


有了它，让我们添加新的测试：
```python
def test_all_users(self):
    """Ensure get all users behaves correctly."""
    add_user('michael', 'michael@mherman.org')
    add_user('fletcher', 'fletcher@notreal.com')
    with self.client:
        response = self.client.get('/users')
        data = json.loads(response.data.decode())
        self.assertEqual(response.status_code, 200)
        self.assertEqual(len(data['data']['users']), 2)
        self.assertIn('michael', data['data']['users'][0]['username'])
        self.assertIn(
            'michael@mherman.org', data['data']['users'][0]['email'])
        self.assertIn('fletcher', data['data']['users'][1]['username'])
        self.assertIn(
            'fletcher@notreal.com', data['data']['users'][1]['email'])
        self.assertIn('success', data['status'])
```


测试失败。然后添加视图：
```python
@users_blueprint.route('/users', methods=['GET'])
def get_all_users():
    """Get all users"""
    response_object = {
        'status': 'success',
        'data': {
            'users': [user.to_json() for user in User.query.all()]
        }
    }
    return jsonify(response_object), 200
```

将 `to_json` 方法添加到模型：

```python
def to_json(self):
    return {
        'id': self.id,
        'username': self.username,
        'email': self.email,
        'active': self.active
    }
```

测试过去了吗？

在继续之前，让我们在浏览器中测试路由 —— http://localhost:5001/users 。你应该看到：
```python
{
  "data": {
    "users": [ ]
  },
  "status": "success"
}
```



将一个种子命令添加到 `manage.py` 文件，以使用一些初始数据填充数据库：
```python
@cli.command()
def seed_db():
    """Seeds the database."""
    db.session.add(User(username='michael', email="hermanmu@gmail.com"))
    db.session.add(User(username='michaelherman', email="michael@mherman.org"))
    db.session.commit()
```

试试看：
```
$ docker-compose -f docker-compose-dev.yml run users python manage.py seed_db
```

现在，再次访问 http://localhost:5001/users ，就可以查看到用户：

```json
{
    "data": {
        "users": [
            {
                "active": true,
                "email": "hermanmu@gmail.com",
                "id": 1,
                "username": "michael"
            },
            {
                "active": true,
                "email": "michael@mherman.org",
                "id": 2,
                "username": "michaelherman"
            }
        ]
    },
    "status": "success"
}
```


> 考虑如何使用共享设置代码减少一些测试。如果你决定重构，尽量不要牺牲可读性。

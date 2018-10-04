---
title: Flask React Docker in Testdriven - Part I - 11
date: 2018-09-23 20:49:21
tags: [testdriven, flask, react, docker]
---

# Jinja Templates
与 JSON API 相反，我们在服务端添加模板。

<!-- more -->

添加路由处理到 `services/users/project/api/users.py` :
```
@users_blueprint.route('/', methods=['GET'])
def index():
    return render_template('index.html')
```

更新 Blueprint :
```python
users_blueprint = Blueprint('users', __name__, template_folder='./templates')
```

`services/users/project/api/templates/index.html`:
```html
<!DOCTYPE html>
<html>

<head>
    <meta charset="utf-8">
    <title>Flask on Docker</title>
    <!-- meta -->
    <meta name="description" content="">
    <meta name="author" content="">
    <meta name="viewport" content="width=device-width,initial-scale=1">
    <!-- styles -->
    <link href="//cdnjs.cloudflare.com/ajax/libs/bulma/0.7.1/css/bulma.min.css" rel="stylesheet">
    {% block css %}{% endblock %}
</head>

<body>
    <div class="container">
        <div class="column is-one-third">
            <br>
            <h1 class="title">All Users</h1>
            <hr><br>
            <form action="/" method="POST">
                <div class="field">
                    <input name="username" class="input" type="text" placeholder="Enter a username" required>
                </div>
                <div class="field">
                    <input name="email" class="input" type="email" placeholder="Enter an email address" required>
                </div>
                <input type="submit" class="button is-primary is-fullwidth" value="Submit">
            </form>
            <br>
            <hr>
            {% if users %}
            <ol>
                {% for user in users %}
                <li>{{user.username}}</li>
                {% endfor %}
            </ol>
            {% else %}
            <p>No users!</p>
            {% endif %}
        </div>
    </div>
    </div>
    </script>
    {% block js %}{% endblock %}
</body>

</html>
```

> 我们使用 [Bulma](https://bulma.io/) CSS 框架来快速设置应用的样式。

准备测试？我们先来访问 http://localhost 。

<center>
![](https://raw.githubusercontent.com/keer2345/storehouse/master/hexo/images/2018/0923/003.png)
</center>


怎么测试呢？
```python
def test_main_no_users(self):
    """Ensure the main route behaves correctly when no users have been
    added to the database."""
    response = self.client.get('/')
    self.assertEqual(response.status_code, 200)
    self.assertIn(b'All Users', response.data)
    self.assertIn(b'<p>No users!</p>', response.data)
```

```
$ docker-compose -f docker-compose-dev.yml run users python manage.py test
```

让我们更新路由并从数据库获取所有用户，而且发送到模板，开始测试：
```python
def test_main_with_users(self):
    """Ensure the main route behaves correctly when users have been
    added to the database."""
    add_user('michael', 'michael@mherman.org')
    add_user('fletcher', 'fletcher@notreal.com')
    with self.client:
        response = self.client.get('/')
        self.assertEqual(response.status_code, 200)
        self.assertIn(b'All Users', response.data)
        self.assertNotIn(b'<p>No users!</p>', response.data)
        self.assertIn(b'michael', response.data)
        self.assertIn(b'fletcher', response.data)
```

可以看到测试失败。更新视图：
```python
@users_blueprint.route('/', methods=['GET'])
def index():
    users = User.query.all()
    return render_template('index.html', users=users)
```

现在应该可以测试通过了！

表单怎样处理呢？我们应该能够添加用户并通过表单提交，然后将用户添加到数据库中。继续从测试开始：
```python
def test_main_add_user(self):
    """Ensure a new user can be added to the database."""
    with self.client:
        response = self.client.post(
            '/',
            data=dict(username='michael', email='michael@sonotreal.com'),
            follow_redirects=True
        )
        self.assertEqual(response.status_code, 200)
        self.assertIn(b'All Users', response.data)
        self.assertNotIn(b'<p>No users!</p>', response.data)
        self.assertIn(b'michael', response.data)
```

接着更新视图：
```python
@users_blueprint.route('/', methods=['GET', 'POST'])
def index():
    if request.method == 'POST':
        username = request.form['username']
        email = request.form['email']
        db.session.add(User(username=username, email=email))
        db.session.commit()

    users = User.query.all()

    return render_template('index.html', users=users)
```


最后，让我们更新代码到 AWS ：
1. 切换到 AWS 环境：`eval $(docker-machine env testdriven-prod)`
1. `docker-compose -f docker-compose-prod.yml up -d --build`
1. 访问 http://DOCKER_MACHINE_IP 、 http://DOCKER_MACHINE_IP/users
1. 切换回本地环境：`eval $(docker-machine env -u)`

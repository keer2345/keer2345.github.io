---
title: Flask Web投票应用03：模板，认证和静态页面
date: 2018-01-25 22:42:46
categories: python
tags: [python, flask]
---

欢迎回来，这一节我们将为投票应用设计登录页面和认证系统，允许用户创建主题并投票。

# 登录页面
我们使用[bootstrap](https://getbootstrap.com/)来自定义符合我们需求的页面。在此之前，我们创建一些目录来存储模板和一些静态文件（css, images 和 javascript）。

## 创建必须的目录
```shell
mkdir -p templates static/css static/images static/js
```
## 下载bootstrap
```shell
wget -pk https://getbootstrap.com/docs/3.3/examples/jumbotron-narrow
```
## 复制相关文件到指定的目录
```shell
mv getbootstrap.com/docs/3.3/examples/jumbotron-narrow/index.html templates
mv getbootstrap.com/docs/3.3/examples/jumbotron-narrow/jumbotron-narrow.css static/css
mv getbootstrap.com/docs/3.3/dist/css/bootstrap.min.css static/css
mv getbootstrap.com/docs/3.3/dist/fonts static
mv getbootstrap.com/docs/3.3/assets/css/ie10-viewport-bug-workaround.css static/css
mv getbootstrap.com/docs/3.3/assets/js/* static/js
rm -rf getbootstrap.com
```

我们也从[bootstrap](https://getbootstrap.com/docs/3.3/examples/signin/)下载登录页面的示例，并重命名登录页面模板为*signup.html*。重复之前的步骤，而且不要忘记将这里的*index.html*重命名为*signup.html*。
```shell
wget -pk https://getbootstrap.com/docs/3.3/examples/signin/
```

```shell
mv getbootstrap.com/docs/3.3/examples/signin/index.html templates/signup.html
mv getbootstrap.com/docs/3.3/examples/signin/signin.css static/css
rm -rf getbootstrap.com
```

整理完成之后，我们看到的效果大概是这样的：
```shell
(.venv) > $ tree templates static
templates
├── index.html
└── signup.html
static
├── css
│   ├── bootstrap.min.css
│   ├── ie10-viewport-bug-workaround.css
│   ├── jumbotron-narrow.css
│   └── signin.css
├── fonts
│   ├── glyphicons-halflings-regular.eot
│   ├── glyphicons-halflings-regular.eot?
│   ├── glyphicons-halflings-regular.svg
│   ├── glyphicons-halflings-regular.ttf
│   ├── glyphicons-halflings-regular.woff
│   └── glyphicons-halflings-regular.woff2
├── images
│   ├── background.jpg
│   └── logo.png
└── js
    ├── ie10-viewport-bug-workaround.js
    └── ie-emulation-modes-warning.js
```
包含了两个自定义的图片，`background.jpg`和`logo.png`，可以在我的`Github repo`中找到。

# 渲染模板
当客户访问制定的路由URL时，Flask提供了`render_template`方法来展示模板。

编辑`manage.py`:

```python
from flask import Flask, render_template
from flask_sqlalchemy import SQLAlchemy

from config import Config

vote = Flask(__name__)
# load config from the config file we created earlier
vote.config.from_object(Config)

db = SQLAlchemy(vote)

import models

# initialize and create the database
#  db.init_app(vote)
#  db.create_all(app=vote)


@vote.route('/')
def home():
    return render_template('index.html')


@vote.route('/signup')
def signup():
    return render_template('signup.html')


if __name__ == '__main__':
    vote.run()
```
这时访问`localhost:5000/signup`，将会看到`signup.html`在浏览器中被渲染。

# 用户注册与认证
为了实现用户注册，我们需要创建一个模型来存储用户，我们在`models.py`中创建模型为`Users`。
```python
class Users(Base):
    email = db.Column(db.String(100), unique=True)
    username = db.Column(db.String(50), unique=True)
    password = db.Column(db.String(200))
```

## 注册
`manage.py`:
```python
@vote.route('/signup', methods=['GET', 'POST'])
def signup():
    if request.method == 'POST':
        # get user details from the form
        email = request.form['email']
        username = request.form['username']
        password = request.form['password']

        # hash the Password
        password = generate_password_hash(password)

        user = models.Users(email=email, username=username, password=password)
        db.session.add(user)
        db.session.commit()

        flash('Thanks for signing up. Please login.')

        return redirect(url_for('home'))

    return render_template('signup.html')
```
修改`siginup.html`如下：
```html
<!DOCTYPE html>
<html lang="en">

<head>
  <meta charset="utf-8">
  <meta http-equiv="X-UA-Compatible" content="IE=edge">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <!-- The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags -->
  <meta name="description" content="">
  <meta name="author" content="">
  <link rel="icon" href="https://getbootstrap.com/docs/3.3/favicon.ico">

  <title>Signin Template for Bootstrap</title>

  <!-- Bootstrap core CSS -->
  <link href="{{ url_for('static', filename='css/bootstrap.min.css') }}" rel="stylesheet">

  <!-- IE10 viewport hack for Surface/desktop Windows 8 bug -->
  <link href="{{ url_for('static', filename='css/ie10-viewport-bug-workaround.css') }}" rel="stylesheet">

  <!-- Custom styles for this template -->
  <link href="{{ url_for('static', filename='css/signin.css') }}" rel="stylesheet">

  <!-- Just for debugging purposes. Don't actually copy these 2 lines! -->
  <!--[if lt IE 9]><script src="../../assets/js/ie8-responsive-file-warning.js"></script><![endif]-->
  <script src="{{ url_for('static', filename='js/ie-emulation-modes-warning.js') }}"></script>

  <!-- HTML5 shim and Respond.js for IE8 support of HTML5 elements and media queries -->
  <!--[if lt IE 9]>
      <script src="https://oss.maxcdn.com/html5shiv/3.7.3/html5shiv.min.js"></script>
      <script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js"></script>
    <![endif]-->
</head>

<body>

  <div class="container">

    <form class="form-signin" method="post" action="{{url_for('signup')}}">
      <h2 class="form-signin-heading">Sign Up</h2>

      <div class='form-group has-succes'>
        <label for='email' class='sr-only'>Email address</label>
        <input type="text" id='email' name='email' class='form-control' placeholder='Email address' required autofocus>
      </div>

      <div class='form-group has-succes'>
        <label for='password' class='sr-only'>Username</label>
        <input type="text" id='password' name='username' class='form-control' placeholder='Username' required autofocus>
      </div>

      <div class='form-group has-succes'>
        <label for='password' class='sr-only'>Password</label>
        <input type="password" id='password' name='password' class='form-control' placeholder='Password' required autofocus>
      </div>

      <button class="btn btn-lg btn-primary btn-block" type="submit">Sign Up</button>
    </form>

  </div>
  <!-- /container -->


  <!-- IE10 viewport hack for Surface/desktop Windows 8 bug -->
  <script src="../../assets/js/ie10-viewport-bug-workaround.js"></script>
</body>

</html>
```

## 编辑index.html和login.html


> 本文的源代码可以在[Github](https://github.com/keer2345/Flask_Vote/tree/v.03)中找到。

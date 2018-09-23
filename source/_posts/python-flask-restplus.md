---
title: Flask Restplus
date: 2018-09-18 21:45:35
tags: [flask]
---

# Get Start
## Install
```
pip install flask-restplus
```

## Quickstart

```python
from flask import Flask
from flask_restplus import Api, Resource

app = Flask(__name__)
api = Api(app)


@api.route('/hello')
class HelloWorld(Resource):
    def get(self):
        return {'Hello': 'world'}


if __name__ == '__main__':
    app.run(debug=True)
```

```
$ python demo01.py 
 * Serving Flask app "demo01" (lazy loading)
 * Environment: production
   WARNING: Do not use the development server in a production environment.
   Use a production WSGI server instead.
 * Debug mode: on
 * Running on http://127.0.0.1:5000/ (Press CTRL+C to quit)
 * Restarting with stat
 * Debugger is active!
 * Debugger PIN: 328-673-670
```
```
$ http :5000/hello
HTTP/1.0 200 OK
Content-Length: 25
Content-Type: application/json
Date: Tue, 18 Sep 2018 13:52:20 GMT
Server: Werkzeug/0.14.1 Python/3.6.6

{
    "Hello": "world"
}
```

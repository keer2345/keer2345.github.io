---
title: Flask React Docker in Testdriven - Part II - 2
date: 2018-09-25 20:49:25
tags: [testdriven, flask, react, docker]
---
# Code Coverage and Quality

本节，我们通过 Coverage.py 将代码覆盖添加到项目中。

<!-- more -->

确保我们的 Docker 切换到本地：
```
$ eval $(docker-machine env -u)
```


更新容器：
```
$ docker-compose -f docker-compose-dev.yml up -d
```

确保应用通过测试：
```
$ docker-compose -f docker-compose-dev.yml run users python manage.py test
```

## 代码覆盖率
代码覆盖率（[Code Coverage](https://en.wikipedia.org/wiki/Code_coverage)）是查找未被测试覆盖的代码的过程。*Coverage.py* 是用于测量 Python 代码覆盖率的流行工具。安装 *Coverage.py* ：
```
pip install coverage
```

接着，我们需要在 `manage.py` 配置覆盖率报告，在导入后添加配置：
```python
import coverage

COV = coverage.coverage(
    branch=True,
    include='project/*',
    omit=[
        'project/tests/*',
        'project/config.py',
    ])
COV.start()
```


添加 CLI 命令：
```python
@cli.command()
def cov():
    """Runs the unit tests with coverage."""
    tests = unittest.TestLoader().discover('project/tests')
    result = unittest.TextTestRunner(verbosity=2).run(tests)
    if result.wasSuccessful():
        COV.stop()
        COV.save()
        print('Coverage Summary:')
        COV.report()
        COV.html_report()
        COV.erase()
        return 0
    return 1
```

更新容器：
```
$ docker-compose -f docker-compose-dev.yml up -d --build
```

使用覆盖率运行测试：
```
$ docker-compose -f docker-compose-dev.yml run users python manage.py cov
```

我们应该会看到类似这样的结果：
```
Coverage Summary:
Name                    Stmts   Miss Branch BrPart  Cover
---------------------------------------------------------
project/__init__.py        14      6      0      0    57%
project/api/models.py      14     11      0      0    21%
project/api/users.py       48      0     10      0   100%
---------------------------------------------------------
TOTAL                      76     17     10      0    80%
```

可以在新创建的 “htmlcov” 目录中查看HTML版本。现在，您可以快速查看代码的哪些部分，而不是测试所涵盖的部分。

将此目录添加到 `.gitignore` 和 `.dockerignore` 文件中。

> 请记住，尽管代码覆盖率是一个很好的指标，但它并不能衡量测试套件的整体有效性。换句话说，100％ 的覆盖率意味着每行代码都在测试中; 这并不意味着测试可以处理每个场景。

> “仅仅因为你有100％的测试覆盖率并不意味着你正在测试正确的东西。”

## 代码质量
[Linting](https://stackoverflow.com/a/8503586/1799408) 是检查代码风格或者变成错误的过程。虽然 Python 中有很多常用的 linter，但是我们将使用 [flake8](https://gitlab.com/pycqa/flake8) ，因为它绑定了 [pep8](https://pypi.python.org/pypi/pep8) 和 [pyflakes](https://pypi.python.org/pypi/pyflakes) 。

安装 flake8 :
```
pip install flake8
```

更新容器：
```
$ docker-compose -f docker-compose-dev.yml up -d --build
```

运行 flake8 :
```
$ docker-compose -f docker-compose-dev.yml run users flake8 project
```

如果有错误，会一行一行的提示我们。

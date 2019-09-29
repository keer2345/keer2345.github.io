---
title: Building Practical Web Applications with React & Django
date: 2019-09-24 14:15:15
tags: [django, react]
---

# [Tutorial 1 — Git and Visual Studio Code Setup](https://blog.usejournal.com/building-practical-web-applications-with-react-django-git-and-vs-code-setup-a87aecfc8b9e)


## Git Installation and Set-up
## Install a Git Desktop Client
We recommend **[Gitkraken](https://www.gitkraken.com/download)** as it is well-maintained and comes for both Windows and Ubuntu.

## Choosing Web-based Repository Hosting Service (Bitbucket or Github)
## Visual Studio Code Setup
The power of VS Code lies in it’s simplicity and the number of plugins. Some of the extensions we use are:
- GitLens
- Sublime Text KeyMap
- Python & Pylint (for syntax highlighting & indentation)

# [Tutorial 2 — Setting up Python Virtual Environment and pip packages for Django Application](https://blog.usejournal.com/tutorial-2-setting-up-python-virtual-environment-and-pip-packages-for-django-application-d8d201e96bc7)

## Installing Miniconda to Setup Virtual Environment
```
$ wget https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh
$ chmod +x Miniconda-latest-Linux-x86_64.sh
$ ./Miniconda-latest-Linux-x86_64.sh
```

## Installing basic pip packages to run Django Application
```
pip install django django-import-export djangorestframework django-rest-swagger psycopg2 Pillow PyJWT django-cors-headers
```

# [Tutorial 3— PostgreSQL Database Setup, Backup and Restore](https://medium.com/@srplabs.in/tutorial-3-postgresql-database-setup-backup-and-restore-f633e7a85d02)

## Installation
```
$ sudo -s
$ echo 'deb http://apt.postgresql.org/pub/repos/apt/ xenial-pgdg main' >> /etc/apt/sources.list.d/pgdg.list
$ wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | sudo apt-key add -
$ sudo apt-get update
$ sudo apt-get install postgresql-10
```
## Set Up Password
```
$ su — postgres
$ psql
(in psql prompt) # \password postgres (Set password to postgres)
```

## Database Creation and Privileges
```
$ su — postgres
    <your_password>
$ psql
    <your_password>
# \l [ List out the databases for user postgres ]
# CREATE DATABASE <your_dbname>;
GRANT ALL PRIVILEGES ON DATABASE <your_dbname> TO postgres;
# \l [ You will see your newly created database ]
# \q [ Exit the psql shell ]
exit [ Exit the postgres shell ]
```
## Database Backup and Restore
- Backup
```
$ pg_dump -U postgres — format=c — blobs <your_dbname> — file <dump_name>
```

- Restore
```
$ su - postgres 
    <your_password>
$ psql
    <your_password>
# DROP DATABASE <your_dbname>
# CREATE DATABASE <your_dbname>
# \q [ Exit psql shell ]
$ exit [ Exit postgres shell ]
$ pg_restore -U postgres -d <your_dbname> -1 <dump_name>
```

# [Tutorial 4 — Live Project Discussion and Front-End Setup](https://medium.com/@srplabs.in/tutorial-4-live-project-discussion-and-front-end-setup-ba3bc0e0f1c)
## Blog Management System
## Setting up the Client Side
## Important npm scripts & packages

# [Tutorial 5— Introduction to Django Project and Applications and Understanding Directory Structure](https://blog.usejournal.com/tutorial-5-introduction-to-django-project-and-applications-and-understanding-directory-structure-7886d753f442)

## Django Project Creation and Understanding Generated Files
```
$ cd BMS
$ conda activate py35
$ django-admin startproject server
```
## Creating Your Django Applications
```
$ cd BMS
$ cd server
$ django-admin startapp utility
$ django-admin startapp blogs
$ django-admin startapp readers
```

`settings.py`
```py
DATABASES = {
    'default': {
        'ENGINE': 'django.db.backends.postgresql',
        'NAME': 'db_name', #created at the time of postgres installation
        'USER': 'postgres',
        'PASSWORD': 'your_password', #created at the time of password setup
        'HOST': 'localhost',
        'PORT': '5432',
    }
}
```

```
python manage.py migrate
python manage.py runserver
```

Go to link — http://127.0.0.1:8000/

# [Tutorial 6 — Introduction to Django Model Design and it’s optional keyword arguments](https://blog.usejournal.com/tutorial-6-introduction-to-django-model-design-and-its-optional-keyword-arguments-77091cd9de24)

## Frequently Used Django Fields Options
## Frequently Used Django Model Fields
```py
from django.db import models


class Difficulty(models.Model):
    difficulty_id = models.CharField(max_length=10, primary_key=True)
    name = models.CharField(max_length=200)


class Blog(models.Model):
    blog_id = models.CharField(max_length=25, unique=True)
    title = models.TextField(verbose_name='Blog Title')
    reactdj_score = models.IntegerField(default=10)
    published = models.BooleanField(default=False)
    avg_rating = models.DecimalField(max_digits=3, decimal_places=2)
    thumbnail = models.ImageField(upload_to='images/thumbnails')
    medium_url = models.URLField()
    published_on = models.DateTimeField(auto_now=True)
    difficulty = models.ForeignKey(Difficulty, on_delete=models.CASCADE)
```

```
python manage.py makemigrations
python manage.py migrate
```

# [Tutorial 7 — Designing and Visualising Django database models for Blog Management System](https://blog.usejournal.com/tutorial-7-designing-and-visualising-the-django-database-models-for-blog-management-system-49a37b9745)

# [Tutorial 8 — Registering Models and Import-Export in Django Administration](https://blog.usejournal.com/tutorial-8-registering-models-and-import-export-in-django-administration-abcbeddbd6fb)

# [Tutorial 9 — Introduction to Django Rest Framework and writing serializers for Blog Management System](https://blog.usejournal.com/tutorial-9-introduction-to-django-rest-framework-and-writing-serializers-for-blog-management-17d2ed2c209e)

# [Tutorial 10 — Designing our first API to get all the Blog Posts](https://medium.com/@srplabs.in/tutorial-10-designing-our-first-api-to-get-all-the-blog-posts-732a96bd6a8d)

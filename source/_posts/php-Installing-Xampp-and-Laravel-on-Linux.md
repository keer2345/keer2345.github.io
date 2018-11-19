---
title: Installing Xampp and Laravel on Linux
date: 2018-09-14 21:20:17
categories: php
tags: [laravel]
---
# Installing Xampp and Laravel on Linux (Ubuntu , mint , OpenSuse …)

This tutorial show you how to Install Xampp ,Laravel and Lumen on Linux (Ubuntu , mint , OpenSuse …).If you are involved in building web apps using PHP, MySQL / MariaDB and Apache the Xampp is the ultimate choice for Development .

XAMPP is a completely free, easy to install Apache distribution containing MySQL / MariaDB, PHP, and Perl. The XAMPP open source package has been set up to be incredibly easy to install and to use. Best part is Xampp is cross-platform tool available for Windows , Mac and Linux.

<!-- more -->

# Xampp Installation
## Install
Visit Xampp official Website [apachefriends.org](https://www.apachefriends.org) and download  latest and stable version of xampp and save in Home Folder.
```
sudo chmod 755 xampp-linux-*-installer.run
sudo ./xampp-linux-*-installer.run
```

## Command
```
sudo /opt/lampp/lampp start
sudo /opt/lampp/lampp stop
sudo /opt/lampp/lampp restart
```
## Setting Enviroment Variable
- `sudo vim /etc/environment`
- `:/opt/lampp/bin/php` add in last and save
- `sudo ln -s /opt/lampp/bin/php /usr/local/bin/php`

# Laravel
## Installing Composer Globally
```
curl -sS https://getcomposer.org/installer | php
sudo mv composer.phar /usr/local/bin/composer

composer global require "laravel/installer"
```

## Create Laravel Project
```
laravel new blog
```
or
```
composer create-project --prefer-dist laravel/laravel blog
```
## Run Project
```
cd blog
php artisan serve
```
Access to http://127.0.0.1:8000

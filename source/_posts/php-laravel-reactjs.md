---
title: Laravel and ReactJS
date: 2018-10-15 20:23:39
tags: [laravel, react]
---

# Install
[Installing Xampp and Laravel on Linux](../../../09/14/php-Installing-Xampp-and-Laravel-on-Linux/)
```
composer create-project --prefer-dist laravel/laravel larave-react
cd larave-react
php artisan serve --port=8000
```

[Using React](https://laravel.com/docs/5.7/frontend)
```
php artisan preset react
npm install && npm run dev
php artisan serve
```

Watch in development:
```
npm run watch
```

# 数据库
## 工具：
- [Sequel Pro](http://www.sequelpro.com/) (for Mac OS)
- phpmyadmin (for all OS)

- [修改 phpmyadmin 通过密码登陆](https://blog.csdn.net/chuang8_/article/details/79158730)：先在“账户”修改 `root@localhost` 的密码。然后将 `/opt/lamp/phpmyadmin/config.inc.php` 的 `$cfg['Servers'][$i]['auth_type'] = 'config';` 改成 `cookie` 。
- [创建用户及赋予权限(https://www.cnblogs.com/lemon-flm/p/7597879.html)
```sql
create database if not exists laravel_reactjs default character set utf8 collate utf8_general_ci;
create user 'laravel'@'localhost';
grant all on laravel_reactjs.* to 'laravel'@'localhost' identified by '123456';
```

## 同步数据库
```
php artisan migrate
```
> 如果出现如下提示：
```
SQLSTATE[42000]: Syntax error or access violation: 1071 Specified key was too long;
max key length is 767 bytes (SQL: alter table `users` add unique `users_email_unique`(`email`))
```
原因：laravel改变了默认的数据库字符集，现在使用utf8mb4，如果你使用的MySQL数据库高于5.7就不会报错，如果低于该版本就会报错


> 解决方案：在`AppServiceProvider.php` 添加代码：`Schema::defaultStringLength(191);`
完整的代码：
```
use Illuminate\Support\Facades\Schema;//一定要记得引入Schema
/**
 * Bootstrap any application services.
 *
 * @return void
 */
public function boot()
{
    Schema::defaultStringLength(191);
}
```

将刚才建的表删除，重新运行：

```
$ php artisan migrate
Migration table created successfully.
Migrating: 2014_10_12_000000_create_users_table
Migrated:  2014_10_12_000000_create_users_table
Migrating: 2014_10_12_100000_create_password_resets_table
Migrated:  2014_10_12_100000_create_password_resets_table
```

## 创建新表
```
php artisan make:migration create_blogs_table --create=blogs
php artisan migrate
php artisan migrate:rollback
```

## 配置路由
`routes/web.php`:
```php
Route::get('/', function () {
    // return view('welcome');

    $blogArticles=DB::table('blogs')->get();

    return $blogArticles;
});
```

# CRUD
```
php artisan make:model Blog
php artisan make:controller Api/BlogController --resource
```


> https://www.youtube.com/watch?v=WYKSDrgk0fE&index=1&list=PLTXFz3WKxvNJZo1T0-ypVBWD0MfJtshav

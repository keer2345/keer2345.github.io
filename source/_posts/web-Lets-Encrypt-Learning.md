---
title: Let's Encrypt Learning
date: 2018-09-01 22:59:54
tags: [https]
---

# 参考资料
- [使用Let's Encrypt加密你的小站](https://blog.123456.cloud/2017/12/20/%E4%BD%BF%E7%94%A8Let's%20Encrypt%E5%8A%A0%E5%AF%86%E4%BD%A0%E7%9A%84%E5%B0%8F%E7%AB%99/)
- [从letsencrypt申请https证书踩坑记](https://blog.csdn.net/kimicsdn/article/details/78948429)
- [免费SSL证书 之Let’s Encrypt申请与部署](https://www.cnblogs.com/teamblog/p/6219204.html)
- [使用LetsEncrypt配置网站https](https://www.jianshu.com/p/ee5c589950d1)

# 安装
```
sudo add-apt-repository ppa:certbot/certbot
sudo apt update
sudo apt install python nginx certbot
```

# 配置
## 生成证书
```
sudo service nginx stop
sudo certbot certonly --standalone --email your@email.com -d yourdomain.com
```

## 配置文件
`vim /etc/nginx/sites-available/ssl.conf` :

```
server {

   # SSL configuration
   #
   listen 443 ssl;
   listen [::]:443 ssl;

   ssl on;

   ssl_certificate /etc/letsencrypt/live/harrypotterfans.top/fullchain.pem;
   ssl_certificate_key /etc/letsencrypt/live/harrypotterfans.top/privkey.pem;
   ssl_session_timeout 5m;
   ssl_ciphers ECDHE-RSA-AES128-GCM-SHA256:ECDHE:ECDH:AES:HIGH:!NULL:!aNULL:!MD5:!ADH:!RC4;
   ssl_protocols TLSv1 TLSv1.1 TLSv1.2;
   ssl_prefer_server_ciphers on;

   root /var/www/html;

   # Add index.php to the list if you are using PHP
   index index.html index.htm index.nginx-debian.html;

   location / {
      # First attempt to serve request as file, then
      # as directory, then fall back to displaying a 404.
      proxy_set_header X-Real-IP $remote_addr;
              proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
              proxy_set_header Host $http_host;
      try_files $uri $uri/ =404;
   }
}


server {

  listen 80 default_server;
  listen [::]:80 default_server;

  server_name harrypotterfans.top; # www.harrypottefans.top;
  rewrite ^(.*) https://$server_name$1 permanent;
}

```

```
sudo ln -s /etc/nginx/sites-available/ssl.conf /etc/nginx/sites-enabled/ssl.conf
```

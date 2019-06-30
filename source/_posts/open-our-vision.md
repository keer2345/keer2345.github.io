---
title: Open our vision
date: 2019-06-30 20:42:27
tags: [system]
---

# 首先确保时间同步

配置科学上网折腾了我一天，windows和手机都很容易就连上了， 可是archlinux老是连接补上，配置改了又改，还是不行。最后发现本机时间和服务器时间有40秒钟的误差， 就抱着希望最后尝试一次，同步archlinux的时间， 结果奇迹发生了，可以连上了。

如果觉得配置麻烦，在这里生成配置：
https://intmainreturn0.com/v2ray-config-gen/

# 服务端配置

```
{
    "log": {
        "access": "/var/log/v2ray/access.log",
        "error": "/var/log/v2ray/error.log",
        "loglevel": "warning"
    },
    "inbound": {
        "port": 12345,
        "protocol": "vmess",
        "settings": {
            "clients": [
                {
                    "id": "xxxx-ef13-7ca5-343cee3e268a",
                    "level": 1,
                    "alterId": 100
                }
            ]
        }
    },
    "outbound": {
        "protocol": "freedom",
        "settings": {}
    },
    "inboundDetour": [],
    "outboundDetour": [
        {
            "protocol": "blackhole",
            "settings": {},
            "tag": "blocked"
        }
    ],
    "routing": {
        "strategy": "rules",
        "settings": {
            "rules": [
                {
                    "type": "field",
                    "ip": [
                        "0.0.0.0/8",
                        "10.0.0.0/8",
                        "100.64.0.0/10",
                        "127.0.0.0/8",
                        "169.254.0.0/16",
                        "172.16.0.0/12",
                        "192.0.0.0/24",
                        "192.0.2.0/24",
                        "192.168.0.0/16",
                        "198.18.0.0/15",
                        "198.51.100.0/24",
                        "203.0.113.0/24",
                        "::1/128",
                        "fc00::/7",
                        "fe80::/10"
                    ],
                    "outboundTag": "blocked"
                }
            ]
        }
    }
}
```



服务器端的重启
```
sudo systemctl stop v2ray
sudo systemctl start v2ray
sudo systemctl restart v2ray
```
服务端设置系统自启动
```
sudo systemctl  enable v2ray
```

# 客户端配置

```
{
    "log": {
        "loglevel": "warning"
    },
    "inbound": {
        "listen": "127.0.0.1",
        "port": 1080,
        "protocol": "socks",
        "settings": {
            "auth": "noauth",
            "udp": true,
            "ip": "127.0.0.1"
        }
    },
    "outbound": {
        "protocol": "vmess",
        "settings": {
            "vnext": [
                {
                    "address": "xxx.xxx.xxx.xxx",
                    "port": 12345,
                    "users": [
                        {
                            "id": "xxxx-ef13-7ca5-343cee3e268a",
                            "level": 1,
                            "alterId": 100
                        }
                    ]
                }
            ]
        }
    },
    "outboundDetour": [
        {
            "protocol": "freedom",
            "settings": {},
            "tag": "direct"
        }
    ],
    "routing": {
        "strategy": "rules",
        "settings": {
            "rules": [
                {
                    "type": "field",
                    "port": "54-79",
                    "outboundTag": "direct"
                },
                {
                    "type": "field",
                    "port": "81-442",
                    "outboundTag": "direct"
                },
                {
                    "type": "field",
                    "port": "444-65535",
                    "outboundTag": "direct"
                },
                {
                    "type": "field",
                    "domain": [
                        "gc.kis.scr.kaspersky-labs.com"
                    ],
                    "outboundTag": "direct"
                },
                {
                    "type": "chinasites",
                    "outboundTag": "direct"
                },
                {
                    "type": "field",
                    "ip": [
                        "0.0.0.0/8",
                        "10.0.0.0/8",
                        "100.64.0.0/10",
                        "127.0.0.0/8",
                        "169.254.0.0/16",
                        "172.16.0.0/12",
                        "192.0.0.0/24",
                        "192.0.2.0/24",
                        "192.168.0.0/16",
                        "198.18.0.0/15",
                        "198.51.100.0/24",
                        "203.0.113.0/24",
                        "::1/128",
                        "fc00::/7",
                        "fe80::/10"
                    ],
                    "outboundTag": "direct"
                },
                {
                    "type": "chinaip",
                    "outboundTag": "direct"
                }
            ]
        }
    }
}
```

# Docker部署：
```
docker pull v2ray/official
docker run --name v2ray -d -v $PWD:/etc/v2ray -p 10808:1080 v2ray/official
```
以后可以直接运行容器：

```
docker start v2ray
```

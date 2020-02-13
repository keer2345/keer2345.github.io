---
title: Scala Play Framework Introduction
date: 2020-01-11 14:55:42
tags: [PlayFramework]
---
This is just a quick introduction to the play framework using the Scala programming language.


# Introduction

> https://www.youtube.com/watch?v=t620S8-cwWg&list=PLBeQxJQNprbhb6kOJS477I_laHQTGgjg8&index=1

## Create Project
```
sbt new playframework/play-scala-seed.g8
```
Enter projdet name `scala-example`.

## HomeController.scala

`app/controllers/HomeController.scala`:
```scala
package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import play.api.libs.json._

@Singleton
class HomeController @Inject()(val controllerComponents: ControllerComponents)
    extends BaseController {

  def index() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index())
  }

  def ping = Action { implicit request =>
    Ok("String works.")
  }
  def anotherOne = Action { _ =>
    Ok(Json.obj("Yes" -> true))
  }

  def nameParam(name: String = "world") = Action { _ =>
    Ok(Json.obj("hi" -> name))
  }

  def posted = Action(parse.json) { implicit request =>
    Ok(Json.obj("received" -> Json.toJson(request.body)))
  }
}
```

`conf/routes`:

```
# An example controller showing a sample home page
GET     /               controllers.HomeController.index
GET     /ping           controllers.HomeController.ping
GET     /json           controllers.HomeController.anotherOne
GET     /hello/:name    controllers.HomeController.nameParam(name:String)
POST    /post           controllers.HomeController.posted

# Map static resources from the /public folder to the /assets URL path
GET     /assets/*file               controllers.Assets.versioned(path="/public", file: Asset)
```

**Run**:
```
sbt run
```

## Test

Test with **[httpie](https://github.com/jakubroztocil/httpie)** :

```
http 127.0.0.1:9000
http 127.0.0.1:9000/ping
http 127.0.0.1:9000/json
http 127.0.0.1:9000/hello/vovo
http POST 127.0.0.1:9000/post phone="vovo"
http POST 127.0.0.1:9000/post < test.json
```


# Data Model
## Rebuild

> https://www.youtube.com/watch?v=XD_X3BMsRhg&list=PLBeQxJQNprbhb6kOJS477I_laHQTGgjg8&index=4

```
scala-example> rm -rf app/views
scala-example> mv app/controllers/HomeController.scala app/controllers/AppController.scala
```
`app/controllers/AppController.scala`

```scala
package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import play.api.libs.json._

@Singleton
class AppController @Inject()(val controllerComponents: ControllerComponents)
    extends BaseController {

  def index() = Action { implicit request: Request[AnyContent] =>
    Ok("App works!")
  }

}
```
`route`
```
GET /   controllers.AppController.index
```

```
scala-example> http 127.0.0.1:9000
```
Result:
```
HTTP/1.1 200 OK
Content-Length: 10
Content-Type: text/plain; charset=UTF-8
Date: Sat, 11 Jan 2020 07:36:47 GMT
Referrer-Policy: origin-when-cross-origin, strict-origin-when-cross-origin
X-Content-Type-Options: nosniff
X-Frame-Options: DENY
X-Permitted-Cross-Domain-Policies: master-only
X-XSS-Protection: 1; mode=block

App works!
```

## MongoDB
Search keyword `mongo` and `reactivemongo`:

- https://index.scala-lang.org/
- https://index.scala-lang.org/reactivemongo/play-reactivemongo

```build.sbt
libraryDependencies += "org.reactivemongo" %% "play2-reactivemongo" % "0.20.1-play27"

play.sbt.routes.RoutesKeys.routesImport += "play.modules.reactivemongo.PathBindables._"
```
`application.conf`
```
play.modules.enabled += "play.modules.reactivemongo.ReactiveMongoModule"
mongodb.uri = "mongodb://localhost:27017/scalarest"
```
## Models
```
mkdir app/models
touch app/models/Post.scala
```

# Service Layer

> https://www.youtube.com/watch?v=_C5IlQgJbTo&list=PLBeQxJQNprbhb6kOJS477I_laHQTGgjg8&index=2

## Create two files
```
scala-example> mkdir app/models
scala-example> touch app/models/Post.scala
scala-example> mkdir app/repositories
scala-example> touch app/repositories/PostRepository.scala
```

# API Controller

> https://www.youtube.com/watch?v=G6But_7ug9Q&list=PLBeQxJQNprbhb6kOJS477I_laHQTGgjg8&index=3


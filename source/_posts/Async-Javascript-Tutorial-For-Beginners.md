---
title: Async Javascript Tutorial For Beginners
date: 2021-05-25 14:51:36
tags: [javascript]
---

Async Javascript Tutorial For Beginners

- Callbacks
- Promises
- Async
- Await

<!-- more -->

# Sync code example

`app.js`:

```javascript
// Sync code example

function otherFunc() {
  console.log('We are in another function')
  console.log('Do some stuff')
}

console.log('Start')

otherFunc()

console.log('End')
```

```shell
> node app.js
Start
We are in another function
Do some stuff
End
```

# ASync code example

## Sample 1

```javascript
// ASync code example

console.log('Start')

setTimeout(() => {
  console.log('We are in the 2 senconds timeout ')
}, 2000)

console.log('End')
```

```shell
> node app.js
Start
End
We are in the 2 senconds timeout
```

## Sample 2

```javascript
// ASync code example

console.log('Start')

function loginUser(email, password, callback) {
  setTimeout(() => {
    console.log('Now we have the data at 2 seconds:')
    callback({ userEmail: email, userPassword: password })
  }, 2000)
}

const user = loginUser('sample@gmail.com', 123456, (x) => {
  console.log(x)
})

console.log('End')
```

Result:

```shell
Start
End
Now we have the data at 2 seconds:
{
  userEmail: "sample@gmail.com" ,
  userPassword: 123456
}
```

## Sample 3

```javascript
// ASync code example

console.log('Start')

function loginUser(email, password, callback) {
  setTimeout(() => {
    console.log('Now we have the data at 5 seconds:')
    callback({ userEmail: email, userPassword: password })
  }, 5000)
}

function getUserVideos(email, callback) {
  setTimeout(() => {
    console.log('Now we have the data at 2 seconds:')
    callback(['video1', 'video2', 'video3'])
  }, 2000)
}

const user = loginUser('sample@gmail.com', 123456, (x) => {
  console.log(x)
  getUserVideos(x.userEmail, (videos) => {
    console.log(videos)
  })
})

console.log('End')
```

Result:

```shell
Start
End
Now we have the data at 5 seconds:
{
  userEmail: "sample@gmail.com" ,
  userPassword: 123456
}
Now we have the data at 2 seconds:
["video1" , "video2" , "video3"]
```

## Sample 4

```javascript
// ASync code example

console.log('Start')

function loginUser(email, password, callback) {
  setTimeout(() => {
    console.log('Now we have the data at 5 seconds:')
    callback({ userEmail: email, userPassword: password })
  }, 5000)
}

function getUserVideos(email, callback) {
  setTimeout(() => {
    console.log('Now we have the data at 2 seconds:')
    callback(['video1', 'video2', 'video3'])
  }, 2000)
}

function videoDetails(video, callback) {
  setTimeout(() => {
    console.log('Now we have the data at 1 seconds:')
    callback('title of the video')
  }, 1000)
}

const user = loginUser('sample@gmail.com', 123456, (x) => {
  console.log(x)
  getUserVideos(x.userEmail, (videos) => {
    console.log(videos)
    videoDetails(videos[0], (title) => {
      console.log(title)
      videoDetails(videos[0], (title) => {
        console.log(title)
        videoDetails(videos[0], (title) => {
          console.log(title)
        })
      })
    })
  })
})

console.log('End')
```

Result:

```shell
Start
End
Now we have the data at 5 seconds:
{
userEmail: "sample@gmail.com" ,
userPassword: 123456
}
Now we have the data at 2 seconds:
[
"video1" ,
"video2" ,
"video3"
]
Now we have the data at 1 seconds:
title of the video
Now we have the data at 1 seconds:
title of the video
Now we have the data at 1 seconds:
title of the video
```

# Promise

## Sample 1

```javascript
const promise = new Promise((resolve, reject) => {
  setTimeout(() => {
    console.log('got the user at 2 seconds:')
    resolve({ user: 'ed' })
  }, 2000)
})

promise.then((user) => {
  console.log(user)
})
```

Result:

```shell
"got the user at 2 seconds:"

{"user": "ed"}
```

## Sample 2

```javascript
const promise = new Promise((resolve, reject) => {
  setTimeout(() => {
    console.log('got the user')
    // resolve({ user: "ed" });
    reject(new Error('User not logged in'))
  }, 2000)
})

promise
  .then((user) => {
    console.log(user)
  })
  .catch((err) => {
    console.log(err.message)
  })
```

```shell
"got the user"

"User not logged in"
```

## To improve sample of last section

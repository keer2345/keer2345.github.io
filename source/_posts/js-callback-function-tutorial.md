---
title: Callback Function Tutorial
date: 2018-05-14 21:36:30
tags: [javascript]
---

> https://www.youtube.com/watch?v=pTbSfCT42_M

# Sample 1
This is a simple JavaSript callback function usage:
```javascript
let x = function(){
    console.log('this is a')
}

let y = function(call){
    console.log('this is b');
    call();
}

y(x)
```

Output:
```
this is b
this is a
```
<!-- more -->

# Sample 2

Before callback:

```javascript
let calc = function(num1, num2, calcType) {
    if (calcType === "add") {
        return num1 + num2;
    } else if (calcType === "multiply") {
        return num1 * num2;
    }
};

console.log(calc(2, 3, 'add'));
console.log(calc(2, 3, 'multiply'));
```


After callback:
```javascript
let add = function(a, b) {
    return a + b;
};

let multiply = function(a, b) {
    return a * b;
};

let calc = function(num1, num2, callback) {
    if (typeof callback === "function") {
        return callback(num1, num2);
    } else {
        return 'Error...';
    }
};

console.log(calc(2, 3, add));
console.log(calc(2, 3, 'add'));  // Error...
console.log(calc(2, 3, multiply));

console.log(calc(2, 3, function(a, b) {
    return a - b;
}));
console.log(calc(2, 3, (a, b) => {
    return a - b;
}));
```

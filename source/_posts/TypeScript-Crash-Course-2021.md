---
title: TypeScript Crash Course 2021
date: 2021-09-11 22:44:54
tags: [javascript]
---

Learn the basics of TypeScript in this beginner-friendly crash course

<!-- more -->

## Intro & Sponsor

## Slides

## TypeScript Setup

```sh
sudo npm i -g typescript
tsc -v
```

## TSC (TypeScript Compiler)

```js
let id: number = 5
```

```sh
tsc index
tsc --watch index
```

## Config File

```sh
tsc --init
```

Config, replace `es5` to `es6` on `target`:

```json
{
  "compilerOptions": {
    "target": "es6"
  }
}
```

## Folder Structure

## Basic Types

```js
let id: number = 5
let company: string = 'Traversy Media'
let isPublished: boolean = true
let x: any = 'hello'

let ids: number[] = [1, 2, 3, 4, 5]
let arr: any[] = [1, true, 'hello']
```

## Arrays & Tuples

```js
// Tuple
let person: [number, string, boolean] = [1, 'Brad', true]
// Tuple Array
let employee: [number, string][]
employee = [
  [1, 'Brad'],
  [2, 'John'],
  [3, 'Jill']
]
```

## Unions & Enum

```js
// Union
let pid: string | number
pid = '22'

// Enum
enum Direction1 {
  Up = 'Up',
  Down = 'Down',
  Left = 'Left',
  Right = 'Right'
}

console.log(Direction1.Up)
```

## Objects

```js
type User: {
  id: number,
  name: string
}

const user: User = {
  id: 1,
  name: 'John'
}
```

## Type Assertion

```js
let cid: any = 1
// let customerId = <number>cid
let customerId - cid as number
```

## Functions

```js
function addNum(x: number, y: number): number {
  return x + y
}

function log(message: string | number): void {
  console.log(message)
}
```

## Interfaces

```js
interface UserInterface {
  readonly id: number
  name: string
  age?: number
}

const user1: UserInterface = {
  id: 1,
  name: 'John',
}
```

## Function Interface

```js
interface MathFunc {
  (x: number, y: number): number;
}

const add: MathFunc = (x: number, y: number): number => x + y
const sub: MathFunc = (x: number, y: number): number => x - y
```

## Classes

```js
class Person {
  id: number
  name: string

  constructor(id: number, name: string) {
    this.id = id
    this.name = name
  }
}

const brad = new Person(1, 'Brad')
const mike = new Person(2, 'Mike')
```

## Data Modifiers

```js
class Person {
  private id: number
  protected name: string

  constructor(id: number, name: string) {
    this.id = id
    this.name = name
  }

  register() {
    return `${this.name} is now registered`
  }
}

const brad = new Person(1, 'Brad')
const mike = new Person(2, 'Mike')

const.log(brad.register())
```

## Implement Interface in Class

```js
interface PersonInterface {
  id: number
  name: string
  register(): string
}

class Person implements PersonInterface {
  id: number
  name: string

  constructor(id: number, name: string){
    this.id = id
    this.name = name
  }

  register() {
    return `${this.name} is now registered`
  }
}
```

## Extending Classes (Subclasses)

```js
class Employee extends Person {
  position: string

  constructor(id: number, name: string, position: string) {
    super(id, name)
    this.position = position
  }
}

const emp = new Employee(3, 'Shawn', 'Developer')

console.log(emp.register())
```

## Generics

```js
function getArray(items: any[]): any[] {
  return new Array().concat(items)
}

let numArray = getArray([1, 2, 3, 4])
let strArray = getArray(['brad', 'John', 'Jill'])

numArray.push('hello')
```

```js
function getArray(items: T[]): T[] {
  return new Array().concat(items)
}

let numArray = getArray < number > [1, 2, 3, 4]
let strArray = getArray < string > ['brad', 'John', 'Jill']

numArray.push(5)
```

## TypeScript With React

```sh
mkdir react-ts
cd react-ts

npx create-react-app . --template typescript
```

> https://youtu.be/BCg4U1FzODs
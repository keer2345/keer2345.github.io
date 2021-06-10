---
title: React full course 2021 - Basic
date: 2021-06-02 22:50:14
tags: [react]
---

<div align="center">

![](https://raw.githubusercontent.com/keer2345/storehouse/master/2021/0604-01.png)

</div>

<!-- more -->

# Introduce

> [My Source Code](https://github.com/keer2345/react-full-course-2021-basic)

**Alternative JavaScript Framework:**

- Angular, Vue, Svelte
- [socket.io](https://github.com/socketio/socket.io), [ember.js](https://github.com/emberjs/ember.js), [mithril.js](https://github.com/MithrilJS/mithril.js), [move.js](https://github.com/visionmedia/move.js), [keystone-classic](https://github.com/keystonejs/keystone-classic) ...

## Goals of the course

- Comfortable
- Apply Theory
- Build your own project

## Structure

- Dev Environment
- Tutorial
- Projects
- Redux

## Requirement

Html, CSS, JavaScript (ES6), [Coding Addict -JavaScript Nuggets](https://www.youtube.com/c/codingaddict)

## Dev Environment

- Node (--version, minimum 5.2.0 with _npx_), Visual Studio Code, Chrome, React Developer Tools

```
> npm -v
7.14.0
> node -v
v16.1.0
> code -v
1.56.2
```

- Visual Studio Code [settings.json](https://github.com/john-smilga/VS-CODE-SETUP)

# Create new project

**Create with [create-react-app](https://github.com/facebook/create-react-app):**

```shell
npx create-react-app react-full-course-2021-basic
```

**Project structure:**

```shell
react-full-course-2021-basic
├── README.md
├── node_modules
├── package.json
├── .gitignore
├── public
│   ├── favicon.ico
│   ├── index.html
│   ├── logo192.png
│   ├── logo512.png
│   ├── manifest.json
│   └── robots.txt
├── src
│   ├── App.css
│   ├── App.js
│   ├── App.test.js
│   ├── index.css
│   ├── index.js
│   ├── logo.svg
│   ├── reportWebVitals.js
│   └── setupTests.js
└── yarn.lock
```

**Clean boilerplate:**

```
cd src
rm -rf App.css App.js App.test.js index.css index.js logo.svg reportWebVitals.js setupTests.js
```

# Getting start

## First Component

`src/index.js`:

```js
import React from 'react'
import ReactDom from 'react-dom'

function Greeting() {
  return (
    <div>
      <h4>Hello React</h4>
    </div>
  )
}

ReactDom.render(<Greeting />, document.getElementById('root'))
```

## First Component in detail

`src/index.js`:

```js
import React from 'react'
import ReactDom from 'react-dom'

const Greeting = () => {
  return React.createElement(
    'div',
    {},
    React.createElement('h4', {}, 'Hello React')
  )
}

ReactDom.render(<Greeting />, document.getElementById('root'))
```

Or, short is:

```js
import React from 'react'
import ReactDom from 'react-dom'

const Greeting = () => <h4>Hello React</h4>

ReactDom.render(<Greeting />, document.getElementById('root'))
```

## Mini BookList Project

`index.js` and `index.css`:

```js
import React from 'react'
import ReactDom from 'react-dom'

// CSS
import './index.css'

const BookList = () => (
  <section className='booklist'>
    <Book />
    <Book />
    <Book />
    <Book />
    <Book />
    <Book />
    <Book />
    <Book />
  </section>
)

const Book = () => (
  <article className='book'>
    <Image />
    <Title />
    <Author />
  </article>
)

const Image = () => (
  <img
    src='https://images-na.ssl-images-amazon.com/images/I/81eB%2B7%2BCkUL._AC_UL200_SR200,200_.jpg'
    alt=''
  />
)

const Title = () => <h1>I Love You to the Moon and Back</h1>
const Author = () => (
  <h4 style={{ color: '#617d98', fontSize: '0.75rem', marginTop: '0.25rem' }}>
    Amelia Hepworth
  </h4>
)

ReactDom.render(<BookList />, document.getElementById('root'))
```

```css
* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen,
    Ubuntu, Cantarell, 'Open Sans', 'Helvetica Neue', sans-serif;
  background: #f1f5f8;
  color: #222;
}

.booklist {
  width: 90vw;
  max-width: 1170px;
  margin: 5rem auto;
  display: grid;
  gap: 2rem;
}

@media screen and (min-width: 768px) {
  .booklist {
    grid-template-columns: repeat(3, 1fr);
    /* align-items: start; */
  }
}

.book {
  background: #fff;
  border-radius: 1rem;
  padding: 1rem 2rem;
}

.book h1 {
  margin-top: 0.5rem;
}

.book h4 {
  color: #f17d98;
  letter-spacing: 3px;
  font-size: 0.75rem;
}

.book p {
  margin-top: 0.5rem;
}
```

## JSX and Props

```js
import React from 'react'
import ReactDom from 'react-dom'

// CSS
import './index.css'

const firstBook = {
  img: 'https://images-na.ssl-images-amazon.com/images/I/81eB%2B7%2BCkUL._AC_UL200_SR200,200_.jpg',
  title: 'I Love You to the Moon and Back',
  author: 'Amelia Hepworth'
}
const secondBook = {
  img: 'https://images-na.ssl-images-amazon.com/images/I/71aLultW5EL._AC_UL200_SR200,200_.jpg',
  title: 'Our Class is a Family',
  author: 'Shannon Olsen'
}

const BookList = () => (
  <section className='booklist'>
    <Book img={firstBook.img} title={firstBook.title} author={firstBook.author}>
      <p>
        Lorem ipsum dolor sit amet consectetur adipisicing elit. Iure quos
        adipisci minus.
      </p>
    </Book>
    <Book
      img={secondBook.img}
      title={secondBook.title}
      author={secondBook.author}
    />
  </section>
)

const Book = (props) => {
  const { img, title, author, children } = props

  return (
    <article className='book'>
      <img src={img} alt='' />
      <h1>{title}</h1>
      <h4>{author.toUpperCase()}</h4>
      {children}
    </article>
  )
}

ReactDom.render(<BookList />, document.getElementById('root'))
```

## Props List

`src/index.js`:

```js
import React from 'react'
import ReactDom from 'react-dom'

// CSS
import './index.css'

const books = [
  {
    img: 'https://images-na.ssl-images-amazon.com/images/I/81eB%2B7%2BCkUL._AC_UL200_SR200,200_.jpg',
    title: 'I Love You to the Moon and Back',
    author: 'Amelia Hepworth'
  },
  {
    img: 'https://images-na.ssl-images-amazon.com/images/I/71aLultW5EL._AC_UL200_SR200,200_.jpg',
    title: 'Our Class is a Family',
    author: 'Shannon Olsen'
  },
  {
    img: 'https://images-na.ssl-images-amazon.com/images/I/71e5m7xQd0L._AC_UL200_SR200,200_.jpg',
    title: 'The Vanishing Half: A Novel',
    author: 'Brit Bennett'
  }
]

const BookList = () => (
  <section className='booklist'>
    {books.map((book) => {
      return <Book book={book} />
    })}
  </section>
)

const Book = (props) => {
  const { img, title, author } = props.book

  return (
    <article className='book'>
      <img src={img} alt='' />
      <h1>{title}</h1>
      <h4>{author}</h4>
    </article>
  )
}

ReactDom.render(<BookList />, document.getElementById('root'))
```

## Key prop And Spread Operator

**Key prop**

Now, we can see a _Warnning_ in Console of browser:

```js
Warning: Each child in a list should have a unique "key" prop.

Check the render method of `BookList`. See https://reactjs.org/link/warning-keys for more information.
Book@http://localhost:3000/static/js/main.chunk.js:181:7
BookList
```

Append `id` prop in list `books`, and append `key={book.id}` in _books.map_ :

```js
// ...

const books = [
  {
    id: 1
    // ...
  },
  {
    id: 2
    // ...
  },
  {
    id: 3
    // ...
  }
]

const BookList = () => (
  <section className='booklist'>
    {books.map((book) => {
      return <Book key={book.id} book={book} />
    })}
  </section>
)

// ...
```

Or, replace with `index`:

```js
const BookList = () => (
  <section className='booklist'>
    {books.map((book, index) => {
      return <Book key={index} book={book} />
    })}
  </section>
)
```

**Spread Operator**

```js
// ...

const BookList = () => (
  <section className='booklist'>
    {books.map((book) => {
      return <Book key={book.id} {...book} />
    })}
  </section>
)

const Book = ({ img, title, author }) => {
  return (
    <article className='book'>
      <img src={img} alt='' />
      <h1>{title}</h1>
      <h4>{author}</h4>
    </article>
  )
}

// ...
```

## Event basic

**onClick**

```js
const Book = ({ img, title, author }) => {
  const clickHandler = (e) => {
    console.log(e)
    console.log(e.target)
    console.log('Example')
  }
  const clickContent = (content) => {
    console.log(content)
  }
  return (
    <article className='book'>
      <img src={img} alt='' />
      <h1>{title}</h1>
      <h4>{author}</h4>
      <button type='button' onClick={clickHandler}>
        Button1
      </button>
      <button type='button' onClick={() => clickContent(title)}>
        Button2
      </button>
      <button type='button' onClick={() => clickContent(author)}>
        Button3
      </button>
    </article>
  )
}
```

**onMouseOver**

```js
const Book = ({ img, title, author }) => {
  // ...

  return (
    <article className='book' onMouseOver={() => console.log(title)}>
      // ...
    </article>
  )
}
```

## Import and Export Statements

```
touch src/books.js src/Book.js
```

`src/index.js`:

```js
import React from 'react'
import ReactDom from 'react-dom'

// CSS
import './index.css'
// books
import { data } from './books'
import Book from './Book'

const BookList = () => (
  <section className='booklist'>
    {data.map((book) => {
      return <Book key={book.id} {...book} />
    })}
  </section>
)

ReactDom.render(<BookList />, document.getElementById('root'))
```

`src/books.js`:

```js
export const data = [
  // ...
]
```

`src/Book.js`:

```js
const Book = ({ img, title, author }) => {
  //  ...
}

export default Book
```

# Deploy on Free Hosting

Our server with `yarn start` default port is `3000`. Now, we build and deploy project from ourself Github to  https://www.netlify.com/ .


For example, my online project is https://react-full-course-2021-basic.netlify.app/ .

<div align="center">

![](https://raw.githubusercontent.com/keer2345/storehouse/master/2021/0604-02.png)

</div>

---
title: React full course 2021 - Advance
date: 2021-06-04 14:50:56
tags: [react]
---

<div align="center">

![](https://raw.githubusercontent.com/keer2345/storehouse/master/2021/0604-03.png)

</div>

<!-- more -->

# Getting Start

`src/App.js`:

```js
import React from 'react'

const App = () => {
  return (
    <div className='container'>
      <h2>React Advanced Tutorial</h2>
    </div>
  )
}

export default App
```

# useState

## useState Basic

**Error Case:**

```js
import React from 'react'
import Setup from './tutorial/01-useState/setup/1.error-example'
import Final from './tutorial/01-useState/final/1-error-example'

const App = () => {
  return (
    <div className='container'>
      <Setup />
      <Final />
    </div>
  )
}

export default App
```

**Right Case:**

```js
import React from 'react'
import Setup from './tutorial/01-useState/setup/2-useState-basics'
import Final from './tutorial/01-useState/final/2-useState-basics'

const App = () => {
  return (
    <div className='container'>
      <Setup />
      <Final />
    </div>
  )
}

export default App
```

`src/tutorial/01-useState/final/2-useState-basics.js`:

```js
import React, { useState } from 'react'
// starts with use
// component must be uppercase
// invoke inside function/component body
// don't call hooks conditonally

const UseStateBasics = () => {
  // console.log(useState());
  // const value = useState()[0];
  // const handler = useState()[1];
  // console.log(value, handler);

  const [text, setText] = useState('random title')

  const handleClick = () => {
    if (text === 'random title') {
      setText('hello world')
    } else {
      setText('random title')
    }
  }

  return (
    <React.Fragment>
      <h1>{text}</h1>
      <button type='button' className='btn' onClick={handleClick}>
        Change title
      </button>
    </React.Fragment>
  )
}

export default UseStateBasics
```

## General rules of Hooks

- starts with use
- component must be uppercase
- invoke inside function/component body
- don't call hooks conditonally

## useState with Array Example

`src/tutorial/01-useState/final/3-useState-array.js`:

```js
import React, { useState } from 'react'
import { data } from '../../../data'

const UseStateArray = () => {
  const [people, setPeople] = useState(data)
  const removeItem = (id) => {
    let newPeople = people.filter((p) => p.id !== id)
    setPeople(newPeople)
  }

  return (
    <>
      {people.map((p) => {
        const { id, name } = p
        return (
          <div key={id} className='item'>
            <h4>{name}</h4>
            <button onClick={() => removeItem(id)}>Remove</button>
          </div>
        )
      })}
      <button className='btn' onClick={() => setPeople([])}>
        Clear items
      </button>
      <button className='btn' onClick={() => setPeople(data)}>
        Recall items
      </button>
    </>
  )
}

export default UseStateArray
```

## useState with Object Example

`src/tutorial/01-useState/final/4-useState-object.js`:

```js
import React, { useState } from 'react'

const UseStateObject = () => {
  const [person, setPerson] = useState({
    name: 'peter',
    age: 24,
    message: 'random message'
  })
  const changeMessage = () => {
    setPerson({ ...person, message: 'hello world' })
  }
  return (
    <>
      <h3>{person.name}</h3>
      <h3>{person.age}</h3>
      <h3>{person.message}</h3>
      <button className='btn' onClick={changeMessage}>
        chang message
      </button>
    </>
  )
}

export default UseStateObject
```

## Simple Counter and Function update form

`src/tutorial/01-useState/final/5-useState-counter.js`:

```js
import React, { useState } from 'react'

const UseStateCounter = () => {
  const [value, setValue] = useState(0)

  const reset = () => {
    setValue(0)
  }

  const complexIncrease = () => {
    setTimeout(() => {
      // setValue(value+1)
      setValue((prevState) => {
        return prevState + 1
      })
    }, 2000)
  }

  return (
    <>
      <section style={{ margin: '4rem 0' }}>
        <h2>regular counter</h2>
        <h1>{value}</h1>
        <button className='btn' onClick={() => setValue(value - 1)}>
          decrease
        </button>
        <button className='btn' onClick={reset}>
          reset
        </button>
        <button className='btn' onClick={() => setValue(value + 1)}>
          increase
        </button>
      </section>
      <section style={{ margin: '4rem 0' }}>
        <h2>more complex counter</h2>
        <h1>{value}</h1>
        <button className='btn' onClick={complexIncrease}>
          increase later
        </button>
      </section>
    </>
  )
}

export default UseStateCounter
```

# useEffect Basic

Some materials:

- [How the useEffect Hook Works (with Examples)](https://daveceddia.com/useeffect-hook-examples/)

`src/tutorial/02-useEffect/final/1-useEffect-basics.js`:

```js
import React, { useState, useEffect } from 'react'

const UseEffectBasics = () => {
  const [value, setValue] = useState(0)
  useEffect(() => {
    console.log('call useEffect')
    if (value > 0) {
      document.title = `New Message(${value})`
    }
  })
  console.log('render component')
  return (
    <>
      <h1>{value}</h1>
      <button className='btn' onClick={() => setValue(value + 1)}>
        increase
      </button>
    </>
  )
}

export default UseEffectBasics
```

## useEffect with Dependency List

Add list to second argument,`[]` or `[something]`.

- `[]`: If the list is blank, it means the `useEffect` is only run on the initial render.
- `[something]`: it means the `useEffect` will be toggled when the value of `something` is changed.

```js
// ...
useEffect(() => {
  console.log('call useEffect')
  if (value > 0) {
    document.title = `New Message(${value})`
  }
}, [])
// ...
```

```js
// ...
useEffect(() => {
  console.log('call useEffect')
  if (value > 0) {
    document.title = `New Message(${value})`
  }
}, [value])
// ...
```

## useEffect with Cleanup Function

`src/tutorial/02-useEffect/final/2-useEffect-cleanup.js`:

```js
import React, { useState, useEffect } from 'react'

const UseEffectCleanup = () => {
  const [size, setSize] = useState(window.innerWidth)

  const checkSize = () => {
    setSize(window.innerWidth)
  }

  useEffect(() => {
    // This gets called after every render, by default
    // (the first one, and every one after that)
    console.log('useEffect')
    window.addEventListener('resize', checkSize)

    // If you want to implement componentWillUnmount,
    // return a function from here, and React will call
    // it prior to unmounting.
    return () => {
      console.log('cleanup')
      window.removeEventListener('resize', checkSize)
    }
  }, [])

  console.log('render')

  return (
    <>
      <h1>Window</h1>
      <h2>{size} PX</h2>
    </>
  )
}

export default UseEffectCleanup
```

## useEffect with Fetch Data

`src/tutorial/02-useEffect/final/3-useEffect-fetch-data.js`:

```js
import React, { useState, useEffect } from 'react'

const url = 'https://api.github.com/users'

const UseEffectFetchData = () => {
  const [users, setUsers] = useState([])
  const getUsers = async () => {
    const response = await fetch(url)
    const fetchUsers = await response.json()
    setUsers(fetchUsers)
  }

  useEffect(() => {
    getUsers()
  }, [])

  return (
    <>
      <h3>Github users</h3>
      <ul className='users'>
        {users.map((user) => {
          const { id, login, avatar_url, html_url } = user
          return (
            <li key='id'>
              <img src={avatar_url} alt={login} />
              <div>
                <h4>{login}</h4>
                <a href={html_url}>profile</a>
              </div>
            </li>
          )
        })}
      </ul>
    </>
  )
}

export default UseEffectFetchData
```

# Conditional Rendering

## Multiple Returns and Fetch Data

`src/tutorial/03-conditional-rendering/final/1-multiple-returns.js`:

```js
import React, { useState, useEffect } from 'react'

const url = 'https://api.github.com/users/QuincyLarson'

const MultipleReturns = () => {
  const [isLoading, setIsLoading] = useState(true)
  const [isError, setIsError] = useState(false)
  const [user, setUser] = useState('default user')

  useEffect(() => {
    fetch(url)
      .then((resp) => {
        if (resp.status >= 200 && resp.status <= 200) {
          return resp.json()
        } else {
          setIsLoading(false)
          setIsError(true)
          throw new Error(resp.statusText)
        }
      })
      .then((user) => {
        const { login } = user
        setUser(login)
        setIsLoading(false)
      })
      .catch((error) => console.log(error))
  }, [])

  if (isLoading) {
    return (
      <div>
        <h1>Loading...</h1>
      </div>
    )
  }
  if (isError) {
    return (
      <div>
        <h1>Error...</h1>
      </div>
    )
  }
  return (
    <div>
      <h1>{user}</h1>
    </div>
  )
}

export default MultipleReturns
```

## Short Circuit Evaluation

`src/tutorial/03-conditional-rendering/final/2-short-circuit.js`:

```js
import React, { useState } from 'react'

const ShortCircuit = () => {
  const [text, setText] = useState('')
  const [isError, setIsError] = useState(false)
  // const firstValue = text || 'hello world';
  // const secondValue = text && 'hello world';

  return (
    <>
      {/* <h1>{firstValue}</h1>
      <h1>value : {secondValue}</h1> */}
      {/* {if(){console.log('hello world')}} */}
      <h1>{text || 'john doe'}</h1>
      <button className='btn' onClick={() => setIsError(!isError)}>
        toggle error
      </button>
      {isError && <h1>Error...</h1>}
      {isError ? (
        <p>there is an error...</p>
      ) : (
        <div>
          <h2>there is no error</h2>
        </div>
      )}
    </>
  )
}

export default ShortCircuit
```

## Show or Hide Component

`src/tutorial/03-conditional-rendering/final/3-show-hide.js`:

```js
import React, { useState, useEffect } from 'react'

const ShowHide = () => {
  const [show, setShow] = useState(false)
  return (
    <>
      <button className='btn' onClick={() => setShow(!show)}>
        Show / Hide
      </button>
      {show && <Item />}
    </>
  )
}
const Item = () => {
  const [size, setSize] = useState(window.innerWidth)
  const checkSize = () => {
    setSize(window.innerWidth)
  }
  useEffect(() => {
    window.addEventListener('resize', checkSize)
    return () => {
      window.removeEventListener('resize', checkSize)
    }
  }, [])
  return (
    <div style={{ marginTop: '2rem' }}>
      <h1>Window</h1>
      <h2>size:{size}</h2>
    </div>
  )
}

export default ShowHide
```

# Forms

## Controll Input

`src/tutorial/04-forms/final/1-controlled-inputs.js`:

```js
import React, { useState } from 'react'

const ControlledInputs = () => {
  const [firstName, setFirstName] = useState('')
  const [email, setEmail] = useState('')
  const [people, setPeople] = useState([])

  const handleSubmit = (e) => {
    e.preventDefault()
    if (firstName && email) {
      const person = { id: new Date().getTime().toString(), firstName, email }
      console.log(person)
      setPeople((p) => {
        return [...p, person]
      })
      setFirstName('')
      setEmail('')
    } else {
      console.log('empty values')
    }
  }

  return (
    <>
      <article>
        <form className='form' onSubmit={handleSubmit}>
          <div className='form-control'>
            <label htmlFor='firstName'>Name:</label>
            <input
              type='text'
              id='firstName'
              name='firstName'
              value={firstName}
              onChange={(e) => setFirstName(e.target.value)}
            />
          </div>
          <div className='form-control'>
            <label htmlFor='email'>Email : </label>
            <input
              type='email'
              id='email'
              name='email'
              value={email}
              onChange={(e) => setEmail(e.target.value)}
            />
          </div>
          <button type='submit'>Add Person</button>
        </form>

        {people.map((p, index) => {
          const { id, firstName, email } = p
          return (
            <div className='item' key={id}>
              <h4>{firstName}</h4>
              <p>{email}</p>
            </div>
          )
        })}
      </article>
    </>
  )
}

export default ControlledInputs
```

## Multiple Input

`src/tutorial/04-forms/final/2-multiple-inputs.js`:

```js
import React, { useState } from 'react'

const MultipleInputs = () => {
  const [person, setPerson] = useState({ firstName: '', email: '', age: '' })
  const [people, setPeople] = useState([])

  const handleChange = (e) => {
    const name = e.target.name
    const value = e.target.value
    setPerson({ ...person, [name]: value })
  }

  const handleSubmit = (e) => {
    e.preventDefault()
    if (person.firstName && person.email && person.age) {
      const newPerson = { ...person, id: new Date().getTime().toString() }
      setPeople([...people, newPerson])
      setPerson({ firstName: '', email: '', age: '' })
    }
  }

  return (
    <>
      <article className='form'>
        <form>
          <div className='form-control'>
            <label htmlFor='firstName'>Name:</label>
            <input
              type='text'
              id='firstName'
              name='firstName'
              value={person.firstName}
              onChange={handleChange}
            />
          </div>
          <div className='form-control'>
            <label htmlFor='email'>Email:</label>
            <input
              type='email'
              id='email'
              name='email'
              value={person.email}
              onChange={handleChange}
            />
          </div>
          <div className='form-control'>
            <label htmlFor='age'>Age:</label>
            <input
              type='number'
              id='age'
              name='age'
              value={person.age}
              onChange={handleChange}
            />
          </div>
          <button type='submit' className='btn' onClick={handleSubmit}>
            Add Person
          </button>
        </form>
      </article>
      {people.map((p) => {
        const { id, firstName, email, age } = p
        return (
          <div key={id} className='item'>
            <h4>{firstName}</h4>
            <p>{email}</p>
            <p>{age}</p>
          </div>
        )
      })}
    </>
  )
}

export default MultipleInputs
```

# useRef

## useRef Basic

`src/tutorial/05-useRef/final/1-useRef-basics.js`:

```js
import React, { useEffect, useRef } from 'react'

const UseRefBasics = () => {
  const refContainer = useRef(null)

  const handleSubmit = (e) => {
    e.preventDefault()
    console.log(refContainer.current)
    console.log(refContainer.current.value)
  }
  useEffect(() => {
    console.log(refContainer.current)
    refContainer.current.focus()
  })
  return (
    <>
      <form className='form' onSubmit={handleSubmit}>
        <div>
          <input type='text' ref={refContainer} />
        </div>
        <button type='submit'>submit</button>
      </form>
    </>
  )
}

export default UseRefBasics
```

# useReducer

## Step 1 userState Setup

`src/tutorial/06-useReducer/final/index.js`:

```js
import React, { useState } from 'react'
import Modal from './Modal'
import { data } from '../../../data'

const Index = () => {
  const [name, setName] = useState('')
  const [people, setPeople] = useState(data)
  const [showModal, setShowModal] = useState(false)

  const handleSubmit = (e) => {
    e.preventDefault()
    if (name) {
      setPeople([
        ...people,
        { id: new Date().getTime().toString(), name: name }
      ])
      setName('')
    } else {
      setShowModal(true)
    }
  }
  return (
    <>
      {showModal && <Modal />}
      <form onSubmit={handleSubmit} className='form'>
        <div>
          <input
            type='text'
            value={name}
            onChange={(e) => setName(e.target.value)}
          />
        </div>
        <button type='submit'>Add</button>
      </form>
      {people.map((p) => {
        const { id, name } = p
        return (
          <div key={id}>
            <h4>{name}</h4>
          </div>
        )
      })}
    </>
  )
}

export default Index
```

## Step 2 useReducer Refactor

`src/tutorial/06-useReducer/final/index.js`:

```js
import React, { useState, useReducer } from 'react'
import Modal from './Modal'
import { data } from '../../../data'

const reducer = (state, action) => {
  if (action.type === 'TESTING') {
    return {
      ...state,
      people: data,
      isModalOpen: true,
      modalContent: 'item add success'
    }
  }
  throw new Error('no matching action type')
}
const defaultState = {
  people: [],
  isModalOpen: false,
  modalContent: 'hello modal'
}

const Index = () => {
  const [name, setName] = useState('')

  const [state, dispatch] = useReducer(reducer, defaultState)

  const handleSubmit = (e) => {
    e.preventDefault()
    if (name) {
      dispatch({ type: 'TESTING' })
    } else {
      dispatch({ type: 'RANDOM' })
    }
  }
  return (
    <>
      {state.isModalOpen && <Modal modalContent={state.modalContent} />}
      <form onSubmit={handleSubmit} className='form'>
        <div>
          <input
            type='text'
            value={name}
            onChange={(e) => setName(e.target.value)}
          />
        </div>
        <button type='submit'>Add</button>
      </form>
      {state.people.map((p) => {
        const { id, name } = p
        return (
          <div key={id}>
            <h4>{name}</h4>
          </div>
        )
      })}
    </>
  )
}

export default Index
```

## Step 3 useReducer Add Item

`src/tutorial/06-useReducer/final/index.js`:

```js
import React, { useState, useReducer } from 'react'
import Modal from './Modal'

const reducer = (state, action) => {
  if (action.type === 'ADD_ITEM') {
    const newPeople = [...state.people, action.payload]
    return {
      ...state,
      people: newPeople,
      isModalOpen: true,
      modalContent: 'item add success'
    }
  }
  if (action.type === 'NO_VALUE') {
    return {
      ...state,
      isModaleOpen: true,
      modalContent: 'please enter value'
    }
  }
  throw new Error('no matching action type')
}
const defaultState = {
  people: [],
  isModalOpen: false,
  modalContent: 'hello modal'
}

const Index = () => {
  const [name, setName] = useState('')

  const [state, dispatch] = useReducer(reducer, defaultState)

  const handleSubmit = (e) => {
    e.preventDefault()
    if (name) {
      const newPeople = { id: new Date().getTime().toString(), name }
      dispatch({ type: 'ADD_ITEM', payload: newPeople })
      setName('')
    } else {
      dispatch({ type: 'NO_VALUE' })
    }
  }
  return (
    <>
      {state.isModalOpen && <Modal modalContent={state.modalContent} />}
      <form onSubmit={handleSubmit} className='form'>
        <div>
          <input
            type='text'
            value={name}
            onChange={(e) => setName(e.target.value)}
          />
        </div>
        <button type='submit'>Add</button>
      </form>
      {state.people.map((p) => {
        const { id, name } = p
        return (
          <div key={id}>
            <h4>{name}</h4>
          </div>
        )
      })}
    </>
  )
}

export default Index
```

## Step 4 CLOSE_MODAL

`index.js`:

```js
// ...

const reducer = (state, action) => {
  // ...

  if (action.type === 'CLOSE_MODAL') {
    return { ...state, isModalOpen: false }
  }
  throw new Error('no matching action type')
}
const defaultState = {
  people: [],
  isModalOpen: false,
  modalContent: 'hello modal'
}

const Index = () => {
  // ...

  const closeModal = () => {
    dispatch({ type: 'CLOSE_MODAL' })
  }
  return (
    <>
      {state.isModalOpen && (
        <Modal closeModal={closeModal} modalContent={state.modalContent} />
      )}
      // ...
    </>
  )
}

export default Index
```

`Modal.js`:

```js
import React, { useEffect } from 'react'

const Modal = ({ modalContent, closeModal }) => {
  useEffect(() => {
    setTimeout(() => {
      closeModal()
    }, 3000)
  })
  return <div className='modal'>{modalContent}</div>
}

export default Modal
```

## Setp 5 Remove Item and Refactor reducer.js

`index.js`:

```js
import React, { useState, useReducer } from 'react'
import Modal from './Modal'
import { reducer } from './reducer'

const defaultState = {
  people: [],
  isModalOpen: false,
  modalContent: 'hello modal'
}

const Index = () => {
  const [name, setName] = useState('')

  const [state, dispatch] = useReducer(reducer, defaultState)

  const handleSubmit = (e) => {
    // ...
  }
  const closeModal = () => {
    // ...
  }
  return (
    <>
      // ...
      {state.people.map((p) => {
        const { id, name } = p
        return (
          <div key={id} className='item'>
            <h4>{name}</h4>
            <button
              onClick={() => dispatch({ type: 'REMOVE_ITEM', payload: id })}
            >
              Remove
            </button>
          </div>
        )
      })}
    </>
  )
}

export default Index
```

`reducer.js`:

```js
export const reducer = (state, action) => {
  if (action.type === 'ADD_ITEM') {
    // ...
  }
  if (action.type === 'NO_VALUE') {
    // ...
  }
  if (action.type === 'CLOSE_MODAL') {
    // ...
  }
  if (action.type === 'REMOVE_ITEM') {
    const newPeople = state.people.filter((p) => p.id !== action.payload)
    return { ...state, people: newPeople }
  }
  throw new Error('no matching action type')
}
```

# Prop Drilling

`src/tutorial/07-prop-drilling/final/1-prop-drilling.js`:

```js
import React, { useState } from 'react'
import { data } from '../../../data'

const PropDrilling = () => {
  const [people, setPeople] = useState(data)

  const removePerson = (id) => {
    setPeople(people.filter((p) => p.id !== id))
  }

  return (
    <div>
      <List people={people} removePerson={removePerson} />
    </div>
  )
}

const List = ({ people, removePerson }) => {
  return (
    <>
      {people.map((p) => {
        return <SinglePerson key={p.id} {...p} removePerson={removePerson} />
      })}
    </>
  )
}

const SinglePerson = ({ id, name, removePerson }) => {
  return (
    <div className='item'>
      <h4>{name}</h4>
      <button onClick={() => removePerson(id)}>Remove</button>
    </div>
  )
}

export default PropDrilling
```

# Context API and useContext

**Before** Context API:

`src/tutorial/08-useContext/final/1-context-api.js`:

```js
import React, { useState } from 'react'
import { data } from '../../../data'

const ContextAPI = () => {
  const [people, setPeople] = useState(data)
  const removePeople = (id) => {
    setPeople(people.filter((p) => p.id !== id))
  }
  return (
    <section>
      <h3>Context API / useContext</h3>
      <List people={people} removePeople={removePeople} />
    </section>
  )
}

const List = ({ people, removePeople }) => {
  return (
    <>
      {people.map((p) => {
        return <SinglePerson key={p.id} {...p} removePerson={removePeople} />
      })}
    </>
  )
}
const SinglePerson = ({ id, name, removePerson }) => {
  return (
    <div className='item'>
      <h4>{name}</h4>
      <button onClick={() => removePerson(id)}>remove</button>
    </div>
  )
}

export default ContextAPI
```

**After** Context API:

```js
import React, { useState, useContext } from 'react'
import { data } from '../../../data'

const PersonContext = React.createContext()

// two components - Provider, Consumer

const ContextAPI = () => {
  const [people, setPeople] = useState(data)
  const removePerson = (id) => {
    setPeople(people.filter((p) => p.id !== id))
  }
  return (
    <PersonContext.Provider value={{ removePerson, people }}>
      <h3>Context API / useContext</h3>
      <List />
    </PersonContext.Provider>
  )
}

const List = () => {
  const peopleData = useContext(PersonContext)
  return (
    <>
      {peopleData.people.map((p) => {
        return <SinglePerson key={p.id} {...p} />
      })}
    </>
  )
}
const SinglePerson = ({ id, name }) => {
  const { removePerson } = useContext(PersonContext)

  return (
    <div className='item'>
      <h4>{name}</h4>
      <button onClick={() => removePerson(id)}>remove</button>
    </div>
  )
}

export default ContextAPI
```

# Custom Hooks - useFetch

**Before** Custom Hooks:

`src/tutorial/09-custom-hooks/setup/1-fetch-example.js`:

```js
import React, { useState, useEffect } from 'react'

// ATTENTION!!!!!!!!!!
// I SWITCHED TO PERMANENT DOMAIN
const url = 'https://course-api.com/javascript-store-products'

const Example = () => {
  const [loading, setLoading] = useState(true)
  const [products, setProducts] = useState([])

  const getProducts = async () => {
    const response = await fetch(url)
    const products = await response.json()
    setProducts(products)
    setLoading(false)
  }

  useEffect(() => {
    getProducts()
  }, [url])

  console.log('products : ' + products)

  return (
    <div>
      <h2>{loading ? 'loading...' : 'data'}</h2>
    </div>
  )
}

export default Example
```

**After** Custom Hooks:

`src/tutorial/09-custom-hooks/final/1-fetch-example.js`:

```js
import React from 'react'
import { useFetch } from './2-useFetch'

// ATTENTION!!!!!!!!!!
// I SWITCHED TO PERMANENT DOMAIN
const url = 'https://course-api.com/javascript-store-products'

const Example = () => {
  const { loading, products } = useFetch(url)
  console.log('products : ' + products)

  return (
    <div>
      <h2>Final: {loading ? 'loading...' : 'data'}</h2>
    </div>
  )
}

export default Example
```

`src/tutorial/09-custom-hooks/final/2-useFetch.js`:

```js
import { useState, useEffect } from 'react'

export const useFetch = (url) => {
  const [loading, setLoading] = useState(true)
  const [products, setProducts] = useState([])

  const getProducts = async () => {
    const response = await fetch(url)
    const products = await response.json()
    setProducts(products)
    setLoading(false)
  }

  useEffect(() => {
    getProducts()
  }, [url])

  return { loading, products }
}
```

# PropTypes

**Setup**

`src/tutorial/10-prop-types/setup/index.js`:

```js
import React from 'react'
import Product from './Product'
import { useFetch } from '../../09-custom-hooks/final/2-useFetch'

const url = 'https://course-api.com/react-prop-types-example'

const Index = () => {
  const { products } = useFetch(url)
  return (
    <div>
      <h2>Products</h2>
      <section className='products'>
        {products.map((p) => {
          return <Product key={p.id} {...p} />
        })}
      </section>
    </div>
  )
}

export default Index
```

`src/tutorial/10-prop-types/setup/Product.js`:

```js
import React from 'react'

const Product = () => {
  return <article className='product'>single product</article>
}

export default Product
```

**Final**
`src/tutorial/10-prop-types/final/index.js`:

```js
import React from 'react'
import Product from './Product'
import { useFetch } from '../../09-custom-hooks/final/2-useFetch'

const url = 'https://course-api.com/react-prop-types-example'

const Index = () => {
  const { products } = useFetch(url)

  return (
    <div>
      <h2>Final: Products</h2>
      {/* <img src={defaultImage} /> */}
      <section className='products'>
        {products.map((product) => {
          return <Product key={product.id} {...product} />
        })}
      </section>
    </div>
  )
}

export default Index
```

`src/tutorial/10-prop-types/final/Product.js`:

```js
import React from 'react'
import PropTypes from 'prop-types'
import defaultImage from '../../../assets/default-image.jpeg'

const Product = ({ image, name, price }) => {
  const url = image && image.url
  return (
    <article className='product'>
      <img src={url || defaultImage} alt={name || 'default name'} />
      <h4>{name}</h4>
      <p>${price || 3.99}</p>
    </article>
  )
}

Product.propTypes = {
  image: PropTypes.object.isRequired,
  name: PropTypes.string.isRequired,
  price: PropTypes.number.isRequired
}
// Product.defaultProps = {
//   name: 'default name',
//   price: 3.99,
//   image: defaultImage,
// };

export default Product
```

# React Router

- https://reactrouter.com/web/guides/quick-start

```
yarn add react-router-dom
```

Source:

`index.js`:

```js
import React from 'react'
// react router
import { BrowserRouter as Router, Route, Switch } from 'react-router-dom'
// pages
import Home from './Home'
import About from './About'
import People from './People'
import Error from './Error'
import Person from './Person'
// navbar
import Navbar from './Navbar'

const Index = () => {
  return (
    <Router>
      <Navbar />
      <Switch>
        <Route exact path='/'>
          <Home />
        </Route>
        <Route path='/about'>
          <About />
        </Route>
        <Route path='/people'>
          <People />
        </Route>
        <Route path='/person/:id' children={<Person />}></Route>
        <Route path='*'>
          <Error />
        </Route>
      </Switch>
    </Router>
  )
}

export default Index
```

Other Javascript file:

- [Navbar.js](https://github.com/keer2345/react-full-course-2021-advance/blob/main/src/tutorial/11-react-router/final/Navbar.js)
- [Home.js](https://github.com/keer2345/react-full-course-2021-advance/blob/main/src/tutorial/11-react-router/final/Home.js)
- [About.js](https://github.com/keer2345/react-full-course-2021-advance/blob/main/src/tutorial/11-react-router/final/About.js)
- [Error.js](https://github.com/keer2345/react-full-course-2021-advance/blob/main/src/tutorial/11-react-router/final/Error.js)
- [People.js](https://github.com/keer2345/react-full-course-2021-advance/blob/main/src/tutorial/11-react-router/final/People.js)
- [Person.js](https://github.com/keer2345/react-full-course-2021-advance/blob/main/src/tutorial/11-react-router/final/Person.js)

# Performace Optimization

- React.memo
- useCallback
- useMemo
- Memoizing - caching results (remember)
- React is fast by default !!!
- Optimizations add their own costs !!!

## React.memo

## useCallback

## useMemo

## useCallback - Fetch Example

---
title: React Axios Demo
date: 2018-07-09 21:02:20
tags: [react]
---

# Install
```
yarn add axios
```

# src/App.js
```javascript
import React, { Component } from 'react';
import axios from 'axios';

import UserForm from './components/UserForm';

import './App.css';

class App extends Component {
  state = {
    repos: null,
  }
  getUser = (e) => {
    e.preventDefault();
    const user = e.target.elements.username.value;

    if (user) {
      axios.get(`https://api.github.com/users/${user}`).then(
        (res) => {
          const repos = res.data.public_repos;
          this.setState({repos:repos});
        }
      )
    } else return;
  }
  render() {
    return (
      <div className="App">
        <header className="App-header">
          <h1 className="App-title">HTTP Calls in React</h1>
        </header>

        <UserForm getUser={this.getUser}/> 

        { this.state.repos ? <p>Number of repos: { this.state.repos }</p> : 
            <p>Please enter a right username</p>}
      </div>
    );
  }
}

export default App;
```

# src/components/UserForm.js
```javascript
import React from 'react';

const UserForm = (props) => {
    return (
        <form onSubmit={props.getUser}>
            <input style={{margin:"20px auto", display:"block"}} 
                type="text" name="username"/>
            <button>Submit</button>
        </form>
    )
}

export default UserForm;
```

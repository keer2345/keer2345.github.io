---
title: React Router Demo
date: 2018-07-08 19:54:30
tags: [react]
---
## Create project
```
create-react-app react-router

cd react-router
yarn start
```

## Add react-router-dom
```
yarn add react-router-dom
```

## Edit Files
### src/App.js
```js
import React, { Component } from 'react';
import { BrowserRouter, Route, Switch} from 'react-router-dom';

import Navigation from './components/Navigation';

import Home from './components/Home';
import About from './components/About';
import Contact from './components/Contact';
import Error from './components/Error';

class App extends Component {
  render() {
    return (
      <BrowserRouter>
        <div>
          <Navigation/>

          <Switch>
            <Route exact path="/" component={Home}/>  
            <Route path="/about" component={About}/>  
            <Route path="/contact" component={Contact}/>  

            <Route component={Error}/>
          </Switch>
        </div>
      </BrowserRouter>
    );
  }
}

export default App;
```

### Other files
src/components/Home.js
```js
import React from 'react';

const Home = () => {
    return (
        <div>Home page</div>
    )
}

export default Home;
```

src/components/About.js
```js
import React from 'react';

const About = () => {
    return (
        <div>About page</div>
    )
}

export default About;
```


src/components/Contact.js
```js
import React from 'react';

const Contact = () => {
    return (
        <div>Contact page</div>
    )
}

export default Contact;
```

src/components/Error.js
```js
import React from 'react';

const Error = () => {
    return (
        <div>404 - Page not found</div>
    )
}

export default Error;
```

src/components/Navigation.js
```js
import React from 'react';
import { NavLink } from 'react-router-dom';

const Navigation = () => {
    return (
        <div>
            <NavLink to="/">Home</NavLink> | 
            <NavLink to="/about">About</NavLink> | 
            <NavLink to="/contact">Contact</NavLink>
        </div>
    )
}

export default Navigation;
```

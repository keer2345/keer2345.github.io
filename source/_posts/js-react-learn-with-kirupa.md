---
title: Learn React with Kirupa.com
date: 2018-04-20 11:50:03
categories: react
tags: react
---

> https://www.kirupa.com/react/

# [Introducing React](https://www.kirupa.com/react/introducing_react.htm)
![](https://www.kirupa.com/react/images/single_page_model_144.png)

## History
1. Old way for Multi-Page Design
1. Now, new way for Single-Page Design (SPD mode)

## Meet React
- Automatic UI State Management
- Lightning-fast DOM Manipulation
- APIs to Create Truly Composable UIs
- Visuals Defined Entirely in JavaScript

# [Building your first React App](https://www.kirupa.com/react/building_your_first_react_app.htm)
![](https://www.kirupa.com/react/images/code_editor_web_app_144.png)
## Step 1
Basic framework:
```html
<!DOCTYPE html>
<html>

    <head>
        <meta charset="utf-8">
        <title>react react</title>
    </head>

    <body>

    </body>

</html>
```

## Step 2
Add script: 
```html
<script src="https://unpkg.com/react@16/umd/react.development.js"></script>
<script src="https://unpkg.com/react-dom@16/umd/react-dom.development.js"></script>
<script src="https://unpkg.com/babel-standalone@6.15.0/babel.min.js"></script>
```

## Step 3
Add core content int *body* element:
```javascript
<body>
    <div id="container"></div>

    <script type="text/babel">
        ReactDOM.render(
            <h1>hello react</h1>, 
            document.getElementById("container")
        );
    </script>
</body>
```

## Step 4
Add CSS style:
```css
<head>

    ...

    <style>
        #container {
            padding: 50px;
            background-color: #eee;
        }

        #container h1 {
            font-size: 60px;
            font-family: sans-serif;
            color: #0080a8;
        }
    </style>
</head>
```

# [Components in React](https://www.kirupa.com/react/components.htm)
![](https://www.kirupa.com/react/images/c_app_144.png)

## Common Component
```javascript
class HelloWorld extends React.Component {
    render() {
       return <p>Hello, componentized world!</p>
    }
}

ReactDOM.render(
  <div>
      <HelloWorld/>
      <HelloWorld/>
      <HelloWorld/>
  </div>,
  document.querySelector("#container")
);
```

## Specifying Properties
```javascript
class HelloWorld extends React.Component {
    render() {
       return <p>Hello, {this.props.greetTarget}!</p>
    }
}

ReactDOM.render(
  <div>
    <HelloWorld greetTarget="Batman"/>
    <HelloWorld greetTarget="Iron Man"/>
    <HelloWorld greetTarget="Nicolas Cage"/>
    <HelloWorld greetTarget="Mega Man"/>
    <HelloWorld greetTarget="Bono"/>
    <HelloWorld greetTarget="Catwoman"/>
  </div>,
  document.querySelector("#container")
);
```

## Dealing with Children
```JavaScript
class Buttonify extends React.Component {
    render() {
      return(
      <div>
        <button type={this.props.behavior}>{this.props.children}</button>
      </div>
      );
  }
}

ReactDOM.render(
  <div>
    <Buttonify behavior="submit">SEND DATA</Buttonify>
  </div>,
  document.querySelector("#container")
);
```

# [Styling in React](https://www.kirupa.com/react/styling_in_react.htm)
![](https://www.kirupa.com/react/images/colorful_vowels.png)
## Just Style It Already!
```javascript
class Letter extends React.Component {
  render() {
    return (
      <div className="letter">
        {this.props.children}
      </div>
    );
  }
}
```

```css
.letter {
  padding: 10px;
  margin: 10px;
  background-color: #ffde00;
  color: #333;
  display: inline-block;
  font-family: monospace;
  font-size: 32px;
  text-align: center;
}
```

## Styling Content the React Way
```javascript
class Letter extends React.Component {
    render() {
        var letterStyle = {
            padding: 10,
            margin: 10,
            backgroundColor: "#ffde00",
            color: "#333",
            display: "inline-block",
            fontFamily: "monospace",
            fontSize: 32,
            textAlign: "center"
        };
 
        return(
            <div style={letterStyle}>
                {this.props.children}
            </div>
        );
    }
}

ReactDOM.render(
    <div>
        <Letter bgcolor="#58B3FF">A</Letter>
        <Letter bgcolor="#FF605F">E</Letter>
        <Letter bgcolor="#FFD52E">I</Letter>
        <Letter bgcolor="#49DD8E">O</Letter>
        <Letter bgcolor="#AE99FF">U</Letter>
    </div>,
    destination
);
```

## Making the Background Color Customizable
```javascript
ReactDOM.render(
    <div>
        <Letter bgcolor="#58B3FF">A</Letter>
        <Letter bgcolor="#FF605F">E</Letter>
        <Letter bgcolor="#FFD52E">I</Letter>
        <Letter bgcolor="#49DD8E">O</Letter>
        <Letter bgcolor="#AE99FF">U</Letter>
    </div>,
    destination
);
```
```javascript
var letterStyle = {
  padding: 10,
  margin: 10,
  backgroundColor: this.props.bgcolor,
  color: "#333",
  display: "inline-block",
  fontFamily: "monospace",
  fontSize: 32,
  textAlign: "center"
};
```

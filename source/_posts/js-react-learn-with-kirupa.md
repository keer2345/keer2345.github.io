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

# [Creating Complex Components](https://www.kirupa.com/react/creating_complex_components.htm)
React allow for **composability**. You can combine components to create more complex components.

In this tutorial, we will look at what all of this means.	More specifically, we will look at two things:
1. The boring technical stuff that you need to know.
1. The boring stuff you need to know about how to identify components when you look at a bunch of visual elements.

![](https://www.kirupa.com/react/images/individual_components_part2_144.png)

```javascript
class Square extends React.Component {
  render() {
    let squareStyle = {
      height: 150,
      backgroundColor: this.props.color
    };
    return <div style={squareStyle} />;
  }
}
class Label extends React.Component {
  render() {
    let labelStyle = {
      fontFamily: "sans-serif",
      fontWeight: "bold",
      padding:13,
      margin:0
    };
    return <div style={labelStyle}>{this.props.color}</div>;
  }
}
class Card extends React.Component {
  render() {
    let cardStyle = {
      height: 200,
      width: 150,
      padding: 0,
      backgroundColor: "#fff",
      WebkitFilter: "drop-shadow(0px 0px 5px #666)",
      filter: "drop-shadow(0px 0px 5px #666)"
    };
    return (
      <div style={cardStyle}>
        <Square color={this.props.color}/>
        <Label color={this.props.color}/>
      </div>
    );
  }
}

ReactDOM.render(
  <div>
    <Card color="#FF6663"/>
    <Card color="#FFf666"/>
    <Card color="#FF9999"/>
    <Card color="#ddeeff"/>
  </div>,
  document.getElementById("container")
);
```

# [Transferring Properties](https://www.kirupa.com/react/transferring_properties.htm)
![](https://www.kirupa.com/react/images/two_properties_144.png)

# [Meet JSX...Again!(https://www.kirupa.com/react/meet_jsx_again.htm)
## What Happens with JSX?
## JSX Quirks to Remember
### Evaluating Expressions
```javascript
class Stuff extends React.Component {
  render() {
    return (
      <h1>Boring static content!</h1>
    );
  }
};
```
```javascript
class Stuff extends React.Component {
  render() {
    return (
      <h1>Boring {Math.random() * 100} content!</h1>
    );
  }
};
```

### Returning Multiple Elements
In a lot of our examples, we've always returned one top-level element (often a `div`) that then had many other elements under it. You aren't technically limited to following that pattern. You can actually return multiple elements. There are two ways you can do that.


One way is by using an array-like syntax:
```javascript
class Stuff extends React.Component {
  render() {
    return (
      [
        <p>I am</p>,
        <p>returning a list</p>,
        <p>of things!</p>
      ]
    );
  }
};
```
```javascript
class Stuff extends React.Component {
  render() {
    return (
      [
        <p key="1">I am</p>,
        <p key="2">returning a list</p>,
        <p key="3">of things!</p>
      ]
    );
  }
};
```
How will you know whether you need to add the `key` attribute or not? React will tell you! You will see a message similar to to following printed to your Dev Tools Console:

> **Warning: Each child in an array or iterator should have a unique "key" prop**.

Besides the array-like approach, you have another (arguably better!) way of returning multiple elements. This involves something known as fragments. The way you use it looks as follows:
```javascript

class Stuff extends React.Component {
  render() {
    return (
      <React.Fragment>
        <p>I am</p>
        <p>returning a list</p>
        <p>of things!</p>
      </React.Fragment>
    );
  }
};
```
### You Can't Specify CSS Inline
`style` in HTML:
```html
<div style="font-family:Arial;font-size:24px">
    <p>Blah!</p>
</div>
```

`style` in **JSX**:
```javascript
class Letter extends React.Component {
  render() {
    var letterStyle = {
      padding: 10,
      margin: 10,
      backgroundColor: this.props.bgcolor,
      color: "#333",
      display: "inline-block",
      fontFamily: "monospace",
      fontSize: "32",
      textAlign: "center"
    };
 
    return (
      <div style={letterStyle}>
        {this.props.children}
      </div>
    );
  }
}
```

### Comments
```javascript

ReactDOM.render(
  <div className="slideIn">
    <p className="emphasis">Gabagool!</p>
    {/* I am a child comment */}
    <Label/>
  </div>,
  document.querySelector("#container")
);
```
```javascript
ReactDOM.render(
  <div className="slideIn">
    <p className="emphasis">Gabagool!</p>
    <Label
      /* This comment
         goes across
         multiple lines */
         className="colorCard" // end of line
    />
  </div>,
  document.querySelector("#container")
);
```
### Capitalization, HTML Elements, and Components
## Your JSX Can Be Anywhere
```javascript
var swatchComponent = <Swatch color="#2F004F"></Swatch>;

ReactDOM.render(
  <div>
    {swatchComponent}
  </div>,
  document.querySelector("#container")
);  
```

# [Dealing With State in React](https://www.kirupa.com/react/dealing_with_state.htm)
```javascript
<script type="text/babel">
    class LightningCounter extends React.Component {
      constructor(props,context){
        super(props,context);
        this.state={
          stricks:0
        };
        
        this.timerTick = this.timerTick.bind(this)
      
      }
        componentDidMount(){
          setInterval(this.timerTick,1000);
        }
        
        timerTick(){
          this.setState({
            stricks: this.state.stricks + 1         
          });
        }
      
      render() {
        var strickStyle = {
          color:"#66ffff",
          fontSize:50
        }
        return (
          <h1 style={strickStyle}>{this.state.stricks}</h1>
        );
      }
    }
 
    class LightningCounterDisplay extends React.Component {
      render() {
        var divStyle = {
          width: 250,
          textAlign: "center",
          backgroundColor: "black",
          padding: 40,
          fontFamily: "sans-serif",
          color: "#999",
          borderRadius: 10
        };
        
        
 
        return (
          <div style={divStyle}>
            <LightningCounter/>
          </div>
        );
      }
    }
 
    ReactDOM.render(
      <LightningCounterDisplay/>,
      document.querySelector("#container")
    );
  </script>
```

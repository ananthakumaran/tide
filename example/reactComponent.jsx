var React = require("react")

class MyComponent extends React.Component {
  render() {
    return <span>{this.props.foo}</span>
  }
}

<MyComponent name="bar" />;
<MyComponent name={0} />;

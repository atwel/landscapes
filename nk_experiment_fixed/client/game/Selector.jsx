import React from "react";
import Toggle from './Toggle';
import Grid from '@material-ui/core/Grid';
import './Selector.css';


export default class Selector extends React.Component {
  constructor(props) {
    super(props);
    const { player } = this.props;
    this.props = props;
    this.state = { shapes: this.props.state.shapes,
                    curSelection : player.round.get("curSelection")};

    this.toggleShape = this.toggleShape.bind(this);
  }

  toggleShape(shape) {
    var index = this.state.shapes.indexOf(shape);
    var curValue = this.state.curSelection[index]
    if (curValue==1) {
      this.state.curSelection[index]=0
      this.props.stateUpdate(this.state.curSelection)
    } else {
      this.state.curSelection[index]=1
      this.props.stateUpdate(this.state.curSelection)
    }
  }

  renderShapes() {
    if (this.state.shapes.length<6) {
      return (
      <Grid container item spacing={16}  direction="row" justify="center" alignItems="center">
        {this.state.shapes.map((shape,index) => (
        <Toggle key={shape} src={shape} toggleShape={this.toggleShape} selected={this.state.curSelection[index]} />
        ))}
      </Grid>);
    }
    else {
      return (
      <div>
      <Grid container direction="row" justify="center" alignItems="center" wrap="nowrap">
        { this.state.shapes.slice(0,5).map((shape,index) => (
          <Toggle key={shape} src={shape} toggleShape={this.toggleShape} selected={this.state.curSelection[index]}/>
          ))
        }
      </Grid>
      <br />
      <br /> 
      <Grid container direction="row" justify="center" alignItems="center">
        { this.state.shapes.slice(5,this.state.shapes.length).map((shape,index) => (
          <Toggle key={shape} src={shape} toggleShape={this.toggleShape} selected={this.state.curSelection[index+5]}/>
          ))
        }
      </Grid> </div>  );
    }
  }

  render() {
    const { round, stage, player } = this.props;
    var gridOutput = this.renderShapes()

    return (
      <div className="selector">
      <Grid container>
          { gridOutput }
      </Grid>
    </div>
    );
  }
}

import React from "react";
import './ArtDisplay.css';

export default class ArtDisplay extends React.Component {
    constructor(props) {
      super(props);
      const { player } = this.props;
      this.random = player.round.get("random");
    }

  
    renderShapes() {
      var output = [];
      console.log(this.props.state.curSelection)
      this.props.state.curSelection.forEach((val,index) => {
        if (val) {
          var shape = this.props.state.shapes[index]
          var rand = this.random[index]
          output.push(<img src={shape} key={shape} style={rand}/>)
        } 
      });
      return (output);
    }

    render() {
      const { round, stage, player } = this.props;
      return (
        <div id="ArtDisplay" className="ArtDisplay">&nbsp;
          {this.renderShapes()}
      </div>
      );
    }
  }
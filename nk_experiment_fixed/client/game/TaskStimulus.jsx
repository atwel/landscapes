import React from "react";
import Selector from './Selector';
import ArtDisplay from './ArtDisplay'

export default class TaskStimulus extends React.Component {
  constructor(props) {
    super(props);
    const { player, round } = this.props;
    //var shapes = props.game.treatment.objects.split(","); OLD - set objects yourself
    var shapes  = [];
    _.times(10, i=> {
      var mod = ((round.index%3)*10)+1
      shapes.push("shape_"+(i+mod)+".png")
    })

    var curSelection = player.round.get("curSelection")
    this.state = { curSelection : curSelection,
                   shapes: shapes}

    this.stateUpdate = this.stateUpdate.bind(this);
  }

  stateUpdate(curSelection) {
    const { player } = this.props;
    this.setState({curSelection: curSelection})
    player.round.set("curSelection",curSelection)
    player.stage.set("selection",curSelection)
    this.render()
  }

  render() {
    const { round, stage, player } = this.props;
    return (
      <div className="task-stimulus">
        <ArtDisplay state={this.state} player={player}></ArtDisplay>
        <Selector stateUpdate={this.stateUpdate} state={this.state} player={player}></Selector>
      </div>
    );
  }
}

import React from "react";
import Button from '@material-ui/core/Button';

export default class TaskResponse extends React.Component {
  constructor(props) {
    super(props);
  }



  handleSubmit = event => {
    this.props.player.stage.submit();
  };

  renderSubmitted() {
    return (
      <div className="task-response">
        <div className="response-submitted">
          <h5>Waiting on other players...</h5>
          Please wait until all players are ready
        </div>
      </div>
    );
  }


  render() {
    const { player } = this.props;

    // If the player already submitted, don't show the slider or submit button
    if (player.stage.submitted) {
      return this.renderSubmitted();
    }

    return (
      <div className="task-response">
        <Button variant="contained" onClick={this.handleSubmit}>
          Submit
        </Button>
      </div>
    );
  }
}

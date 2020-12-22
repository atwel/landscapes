import React from "react";

import PlayerProfile from "./PlayerProfile.jsx";
import History from "./history.jsx";
import Task from "./Task.jsx";
import RoundQuiz from "./RoundQuiz.jsx"

export default class Round extends React.Component {
  componentDidMount() {
    window.addEventListener("focus", this.onFocus)
    window.addEventListener("blur", this.onBlur)
  }

  componentWilUnmount() {
    window.removeEventListener("focus", this.onFocus)
    window.removeEventListener("blur", this.onBlur)
  };

  onFocus = () => {
    console.log('The window is in focus')
    var timestamp = Date.now()/1000.0;

    // Append to Round History (in the single player case, the round history will feature just one player's data so player-round and round are the same)
    var histData = { playerid: this.props.player._id, // MongoDB ID of player
                     timestamp: timestamp,
                     action: 'focus', // could be submit, window blur/focus
                   };

    this.props.round.append("history",histData);
  };

  onBlur = () => {
    console.log('The window is out of focus')
    var timestamp = Date.now()/1000.0;

    // Append to Round History (in the single player case, the round history will feature just one player's data so player-round and round are the same)
    var histData = { playerid: this.props.player._id, // MongoDB ID of player
                     timestamp: timestamp,
                     action: 'blur', // could be submit, window blur/focus
                   };

    this.props.round.append("history",histData);
  }

  render() {
    const { round, stage, player, game } = this.props;

    return (
      <div className="round">
        <div className="content">
          <PlayerProfile player={player} stage={stage} game={game} round={round} />
          {stage.name=="Reflection" ? (
            <RoundQuiz game={game} round={round} stage={stage} player={player} />
          ) : (
            <Task game={game} round={round} stage={stage} player={player} />
          )}
          <History stage={stage} player={player} game={game} round={round} />
        </div>
      </div>
    );
  }
}

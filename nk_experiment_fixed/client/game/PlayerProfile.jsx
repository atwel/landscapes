import React from "react";

import Timer from "./Timer.jsx";

export default class PlayerProfile extends React.Component {
  renderProfile() {
    const { player } = this.props;
    return (
      <div className="profile-score">
        <h3>Your Profile</h3>
        <img src={player.get("avatar")} className="profile-avatar" />
      </div>
    );
  }

  renderScore() {
    const { player, stage, round } = this.props;

    return (
      <div className="profile-score">
        <h4>Current score</h4>
        <span>{((player.round.get("curScore") || 0)*1000).toFixed(2)}</span>
        <h4>Total Rewards</h4>
        <span>${(player.round.get("totalScore") || 0).toFixed(2)}</span>
      </div>
    );
  }

  render() {
    const { stage } = this.props;
    return (
      <aside className="player-profile">
        {this.renderScore()}
        <Timer stage={stage} />
      </aside>
    );
  }
}

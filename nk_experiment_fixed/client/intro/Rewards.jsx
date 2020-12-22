import React from "react";

import { Centered } from "meteor/empirica:core";

export default class Rewards extends React.Component {
  render() {
    const { hasPrev, hasNext, onNext, onPrev, game } = this.props;

    return (
      <Centered>
        <div className="instructions">
          <h1> Rewards </h1>
          <p>
          Your rewards for the task depend on the scores of all combinations, not just the final or best score. <em>Each point in the animals' scoring system is worth $0.001.</em>
          </p>
          <p>Scores range from 0 to 100, so if you found combinations worth 42, 67, 55 and 89 points, the score-based reward for those would be (42+67+55+89) x $0.001 = $0.25.
          </p>
          <p>If you got a score of 50 for each of the 25 rounds, your bonus reward would be $1.25. If you got scores of 100 for all 25 rounds, the reward would be $2.50. <strong>You must complete all tasks to get the rewards.</strong></p>
          <p>
          Your rewards for each of the three species you'll try to understand will be added to your final payment.
          </p>
          <p>
            <button type="button" onClick={onPrev} disabled={!hasPrev}>
              Previous
            </button>
            <button type="button" onClick={onNext} disabled={!hasNext}>
              Next
            </button>
          </p>
        </div>
      </Centered>
    );
  }
}

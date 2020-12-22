import React from "react";

import { Centered } from "meteor/empirica:core";

export default class InstructionStepOne extends React.Component {
  render() {
    const { hasPrev, hasNext, onNext, onPrev, game } = this.props;

    return (
      <Centered>
        <div className="instructions">
          <h1> Instructions </h1>
          <p>
            In the following tasks, you're going to help us understand how well humans can understand different animal species' visual preferences.
          </p>
          <p>
            You will select combinations of up to 10 different shapes. The shape selector will turn a darker version of its color when selected. When you want to know the species' <em>average visual pleasantness</em> score for a combination, click submit and you'll be shown the score for that combination.
          </p>
          <p>
            You will do this for 25 rounds and the goal is to find the highest value combination possible.
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

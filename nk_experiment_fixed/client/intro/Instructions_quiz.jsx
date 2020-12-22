import React from "react";

import { Centered } from "meteor/empirica:core";

export default class Quiz extends React.Component {
  state = { goal: "", selectorColor: "", bonus:"" };

  handleChange = event => {
    const el = event.currentTarget;
    this.setState({ [el.name]: el.value.trim().toLowerCase() });
  };

  handleSubmit = event => {
    event.preventDefault();
    const { player } = this.props;
    if (this.state.goal !== "true" || this.state.selectorColor !== "true" || this.state.bonus !== "true") {
      alert("You have at least one incorrect answer! Read the instructions, and please try again.");
    } else {
      player.set("introQuiz",this.state)
      this.props.onNext();
    }
  };

  setGoal(event) {
    this.state.goal = event.target.value;
  }
  setSelectorColor(event) {
    this.state.selectorColor = event.target.value;
  }
  setBonus(event) {
    this.state.bonus = event.target.value;
  }

  render() {
    const { hasPrev, hasNext, onNext, onPrev } = this.props;
    const { goal, selectorColor, bonus } = this.state;
    return (
      <Centered>
        <div className="quiz">
          <h1> Instruction checks </h1>
          <form onSubmit={this.handleSubmit}>
          <div onChange={this.setGoal.bind(this)}>
            <p><h4>Q1: The goal of the task is to find the highest value combination.</h4>
              <label htmlFor="goalTrue">
              <input type="radio" id="goalTrue" name="goal" value="true"/>
              True</label>
              <label htmlFor="goalFalse">
              <input type="radio" id="goalFalse" name="goal" value="false"/>
              False</label>
            </p>
          </div>
          <div onChange={this.setSelectorColor.bind(this)}>
            <p><h4>Q2: When you add a shape to the combination, it's color becomes...?</h4>
              <label htmlFor="selectLight">
              <input type="radio" id="selectLight" name="selectorColor" value="false"/>
              Light</label>
              <label htmlFor="selectDark">
              <input type="radio" id="selectDark" name="selectorColor" value="true"/>
              Dark</label>
            </p>
          </div>
          <div onChange={this.setBonus.bind(this)}>
            <p><h4>Q3: You'll get an extra rewards only if you complete all three tasks.</h4>
              <label htmlFor="bonusTrue">
              <input type="radio" id="bonusTrue" name="bonus" value="true"/>
              True</label>
              <label htmlFor="bonusFalse">
              <input type="radio" id="bonusFalse" name="bonus" value="false"/>
              False</label>
            </p>
          </div>
            <p>
              <button type="button" onClick={onPrev} disabled={!hasPrev}>
                Back to instructions
              </button>
              <button type="submit">Submit</button>
            </p>
          </form>
        </div>
      </Centered>
    );
  }
}

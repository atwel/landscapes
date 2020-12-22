import React from "react";

import { Centered } from "meteor/empirica:core";


export default class ExitSurvey extends React.Component {
  static stepName = "ExitSurvey";
  state = { task1: "0", task2: "0", task3: "0", feedback: "" };

  handleChangeTask1 = event => {
    this.state.task1 = event.target.value;
    console.log(this.state)
  };
  handleChangeTask2 = event => {
    this.state.task2 = event.target.value;
    console.log(this.state)

  };
  handleChangeTask3 = event => {
    this.state.task3 = event.target.value;
    console.log(this.state)

  };
  handleChangeComments = event => {
    this.state.feedback = event.target.value;
  };

  handleSubmit = event => {
    const { player } = this.props;
    if (this.state.task1 != "easy" && this.state.task2 != "easy" && this.state.task3 != "easy") {
      alert("Please choose one task as easy");
      event.preventDefault();

    } else if (this.state.task1 != "med" && this.state.task2 != "med" && this.state.task3 != "med"){
      alert("Please choose one task as medium difficulty");
      event.preventDefault();
    } else if (this.state.task1 != "hard" && this.state.task2 != "hard" && this.state.task3 != "hard"){
      alert("Please choose one task as hard difficulty");
      event.preventDefault();
    }
    else {
      player.set("exitQuiz",this.state)
      event.preventDefault();
      this.props.onSubmit(this.state);
    }
  };

  render() {
    const { player } = this.props;
    const { age, gender, strength, fair, feedback, education } = this.state;

    return (
      <Centered>
        <div className="exit-survey">
          <h1> Exit Survey </h1>
          <p>
            Please submit the following code to receive your bonus:{" "}
            <h4><strong>{player._id}</strong></h4>
          </p>
          <p>You <strong>MUST</strong> submit this code to receive your bonus</p>
          <br />
          <p>
            Please answer the following short survey. You do not have to provide
            any information you feel uncomfortable with.
          </p>
          <form onSubmit={this.handleSubmit}>
            <div className="form-line"/>
            <label><h4>Q1: Please rank the tasks from Easiest to Hardest:</h4></label>
            <table>
              <tbody>
              <tr>
                <td>Task</td>
                <td>Easy</td>
                <td>Medium</td>
                <td>Hard</td>
              </tr>
              <tr>
                <td>1</td>
                <td><input type="radio" id="task1_easy" name="task1" value="easy" onClick={this.handleChangeTask1.bind(this)}/></td>
                <td><input type="radio" id="task1_med" name="task1" value="med" onClick={this.handleChangeTask1.bind(this)}/></td>
                <td><input type="radio" id="task1_hard" name="task1" value="hard" onClick={this.handleChangeTask1.bind(this)}/></td>
              </tr>
              <tr>
                <td>2</td>
                <td><input type="radio" id="task2_easy" name="task2" value="easy" onClick={this.handleChangeTask2.bind(this)}/></td>
                <td><input type="radio" id="task2_med" name="task2" value="med" onClick={this.handleChangeTask2.bind(this)}/></td>
                <td><input type="radio" id="task2_hard" name="task2" value="hard" onClick={this.handleChangeTask2.bind(this)}/></td>
              </tr>
              <tr>
                <td>3</td>
                <td><input type="radio" id="task3_easy" name="task3" value="easy" onClick={this.handleChangeTask3.bind(this)}/></td>
                <td><input type="radio" id="task3_med" name="task3" value="med" onClick={this.handleChangeTask3.bind(this)}/></td>
                <td><input type="radio" id="task3_hard" name="task3" value="hard" onClick={this.handleChangeTask3.bind(this)}/></td>
              </tr>
              </tbody>
            </table>
            <div>
              <label>
                <h4>Q2: Do you have other comments about this experiment?</h4><br></br>
                <textarea id="Q2" name="Q2" rows="4" cols="100" onChange={this.handleChangeComments}/>
              </label>
            </div>
            <button type="submit">Submit</button>
          </form>
        </div>
      </Centered>
    );
  }
}

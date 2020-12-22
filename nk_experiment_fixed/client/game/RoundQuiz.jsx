import React from "react";

export default class Quiz extends React.Component {
  state = { desc: [], otherDesc: "", learn: "" };

  handleSubmit = event => {
    const { player } = this.props;
    if (this.state.desc.length == 0 && this.state.otherDesc=="") {
      alert("Please answer the first question to move on");
    } else {
      player.round.set("quiz",this.state)
      this.props.player.stage.submit();
    }
  };

  setDesc(event) {
    var index = this.state.desc.indexOf(event.target.value);

    if (index === -1) {
      this.state.desc.push(event.target.value);
    } else {
      this.state.desc.splice(index, 1);
    }
    console.log(this.state)
  }
  setOtherDesc(event) {
    this.state.otherDesc = event.target.value;
    console.log(this.state)
  }
  setLearn(event) {
    this.state.learn = event.target.value;
    console.log(this.state)
  }

  render() {
    const { hasPrev, hasNext, onNext, onPrev } = this.props;
    const { sum, horse } = this.state;
    return (
      <div className="quiz">
        <h1> Reflection </h1>
            <h4>Q1: What words describe your progress throughout the round? (Select any that apply):</h4>
            <label htmlFor="Reliable">
            <input type="checkbox" id="Reliable" name="Reliable" value="Reliable" onChange={this.setDesc.bind(this)}/>
            Reliable</label>
            <label htmlFor="Random">
            <input type="checkbox" id="Random" name="Random" value="Random" onChange={this.setDesc.bind(this)}/>
            Random</label>
            <label htmlFor="Improving">
            <input type="checkbox" id="Improving" name="Improving" value="Improving" onChange={this.setDesc.bind(this)}/>
            Improving</label>
            <label htmlFor="Not Improving">
            <input type="checkbox" id="Not Improving" name="Not Improving" value="Not Improving" onChange={this.setDesc.bind(this)}/>
            Not Improving</label>
            <label htmlFor="Cyclical">
            <input type="checkbox" id="Cyclical" name="Cyclical" value="Cyclical" onChange={this.setDesc.bind(this)}/>
            Cyclical</label>
            <label htmlFor="Fluctuating">
            <input type="checkbox" id="Fluctuating" name="Fluctuating" value="Fluctuating" onChange={this.setDesc.bind(this)}/>
            Fluctuating</label>
            <label htmlFor="Slow">
            <input type="checkbox" id="Slow" name="Slow" value="Slow" onChange={this.setDesc.bind(this)}/>
            Slow</label>
            <label htmlFor="Fast">
            <input type="checkbox" id="Fast" name="Fast" value="Fast" onChange={this.setDesc.bind(this)}/>
            Fast</label>
            <label htmlFor="Plateauing">
            <input type="checkbox" id="Plateauing" name="Plateauing" value="Plateauing" onChange={this.setDesc.bind(this)}/>
            Plateauing</label>
            <label htmlFor="Other">
            Other?
            <input type="text" id="Other" name="Other" onChange={this.setOtherDesc.bind(this)}/></label>

            <h4>Q2: How did you try to learn the sort of combinations the species like in this round?</h4>
            <textarea name="Text1" cols="50" rows="3" onChange={this.setLearn.bind(this)}></textarea>
            <br/>
            <button onClick={this.handleSubmit}>Submit</button>
        </div>
    );
  }
}

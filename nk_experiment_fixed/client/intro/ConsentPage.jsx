import React from "react";

import { Centered, ConsentButton } from "meteor/empirica:core";
import Button from '@material-ui/core/Button';

export default class Consent extends React.Component {
  constructor(props) {
    super(props);
    this.state = { consent: this.props.consent};
    this.no_consent = this.no_consent.bind(this);
  }
  no_consent(){
      this.props.toggleConsent("false")
      this.setState((state) => {
        return {consent: "false"}
      });
      this.render()
  }
  whatToRender(){
      if(this.state.consent == ""){
        return (
            <Centered>
        <div className="consent">
          <h1> Consent Form </h1>
          <p>
          This task is part of a Stanford University research study looking at the ability of
          people to understand the visual preferences of animals. Your decision to participate is
          entirely voluntary. The whole task should take around 15 minutes and you will be
          compensated for your time and good performance, as detailed in the instructions.
          <br/>
          <br/>

          There are no anticipated risks involved in doing this task. The only information
          we gather are your Prolific ID, your behavior in the tasks. Your data points will
          become one of hundreds we collect. The aggregated results of our research may be
          presented at scientific meetings or published in scientific journals.
          <br/>
          <br/>


          If you have questions or concerns, please contact the study director Jon Atwell
          at atwell@stanford.edu. If you have any complaints, concerns or general
          questions about the study or your rights as a participant, please contact the
          Stanford Institutional Review Board (IRB) at 650-723-2480, email at IRB2-
          Manager@lists.stanford.edu or toll free at 1-866-680-2906. You can also write to
          the Stanford IRB, Stanford University, 1705 El Camino Real, Palo Alto, CA
          94306.
          <br/>
          <br/>


          Selecting "AGREE" below indicates that you are at least 18 years of age, and
          agree to participate voluntary. If you select DISAGREE, you will exit
          </p>
          <br />
          <ConsentButton text="AGREE" />
          <Button variant="contained" onClick={this.no_consent}>DISAGREE</Button>
        </div>
      </Centered>
        )
      } else {
          return (
            <Centered>
            <div className="NotConsented">
            <p>Thank you for your response. Please exit this survey and return this HIT.</p>
            <br />
         </div>
         </Centered>
          )
      }
  }
  render() {
    var page_stuff = this.whatToRender()
    return (
        page_stuff
    )
  }
}
import React from "react";

import { Centered, ConsentButton } from "meteor/empirica:core";
import ConsentPage from "./ConsentPage"
export default class Consent extends React.Component {
  constructor(props) {
    super(props);
    this.state = { consent: ""};
    this.toggleConsent = this.toggleConsent.bind(this);
  }
  toggleConsent(consent){
    this.setState((state) => {
      return {consent: consent}
    });
    this.render()
  }
  render() {
    return (
    <ConsentPage consent={this.state.consent} toggleConsent={this.toggleConsent}/>
    );
  }
}

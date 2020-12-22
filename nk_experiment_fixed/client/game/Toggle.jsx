import React from "react";
import Grid from '@material-ui/core/Grid';
import Card from '@material-ui/core/Card';
import CardActionArea from '@material-ui/core/CardActionArea';
import CardMedia from '@material-ui/core/CardMedia';

import './Selector.css';


export default class Toggle extends React.Component {
    constructor (props) {
        super(props)
        const { selected } = this.props;
        this.props = props
        this.state = {selected : selected,
                      shape : props.src}
        this.toggleShape = this.toggleShape.bind(this);

    }

    render() {
        return (
            <Grid item >
                <Card className="shapeCard" elevation={0}>
                    <CardActionArea onClick={() => this.toggleShape()}>
                            <img className={this.state.selected ? 'selected' : 'unselected'} src={this.state.shape} alt=""  />
                    </CardActionArea>
                </Card>  
            </Grid>
        );
    }

    toggleShape() {
        this.props.toggleShape(this.state.shape);
        this.setState({selected: !this.state.selected});
      }
}
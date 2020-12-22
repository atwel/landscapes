import React from "react";
import Table from '@material-ui/core/Table';
import TableBody from '@material-ui/core/TableBody';
import TableCell from '@material-ui/core/TableCell';
import TableHead from '@material-ui/core/TableHead';
import TableRow from '@material-ui/core/TableRow';
import Grid from '@material-ui/core/Grid';
import Card from '@material-ui/core/Card';



export default class History extends React.Component {
  constructor(props) {
    super(props);
    const { round } = this.props;
    //var shapes = props.game.treatment.objects.split(","); OLD - set objects yourself
    var shapes  = [];
    _.times(10, i=> {
      var mod = ((round.index%3)*10)+1
      shapes.push("shape_"+(i+mod)+".png")
    })
    this.shapes = shapes;
  }

    renderImage(submission) {
      if (this.shapes.length<6) {
        return (
        <Grid container>
        <Grid  item spacing={8} >
          {this.shapes.map((shape,index) => (
            <Card className="shapeCard" elevation={0} key={shape}>
              <img key={shape} src={shape}
              className={submission["curSelection"][index]===0 ? 'gridImage-unselected' : 'gridImage-selected'}/>
            </Card>
          ))}
        </Grid> </Grid>);
      }
      else {
        return (
        <div>
        <Grid container item spacing={8} >
          { this.shapes.slice(0,5).map((shape,index) => (
            <Card className="shapeCard" elevation={0} key={shape}>
              <img key={shape} src={shape} className={submission["curSelection"][index]===0 ? 'gridImage-unselected' : 'gridImage-selected'}/>
            </Card>
            ))
          }
        </Grid>
        <Grid container item spacing={8}>
          { this.shapes.slice(5,this.shapes.length).map((shape,index) => (
            <Card className="shapeCard" elevation={0} key={shape}>
              <img className={submission["curSelection"][index+5]===0 ? 'gridImage-unselected' : 'gridImage-selected'} key={shape} src={shape}  />
            </Card>
            ))
          }
        </Grid> </div> );
      }
    }

    highlightFirstRow()
    {
        var trs = document.getElementById('history_list').getElementsByTagName('tr');
    trs[0].className = "currentRow";
    }

    renderSubmission(submission,index, array) {
      console.log(array)
      console.log(index)
        var output_index = array.length - index
      console.log(output_index)
        var submission = array[output_index-1]
        var gridOutput = this.renderImage(submission)
        return (
          <TableBody key={output_index}>
              <TableRow key={output_index}>

                <TableCell   className={output_index=== array.length ? 'most_recent' : 'not_recent'}
                ><h1>{output_index}</h1></TableCell>
                <TableCell key={output_index} className={output_index=== array.length ? 'most_recent_selection' : 'not_recent_selection'}>
                  {gridOutput}
                </TableCell>
                <TableCell key={output_index} className={output_index=== array.length ? 'most_recent' : 'not_recent'}  ><h1>{(submission["score"]*1000).toFixed(2)}</h1>   </TableCell>
              </TableRow>
            </TableBody>
        );
      }

    render() {
      const { game, player } = this.props;
      const history_list = player.round.get("submissions")

      return (
        <div className="history" style={{overflow:"auto", maxHeight:"600px", width:"500px"}}>
          <Table>
              <TableBody >
                <TableRow >
                  <TableCell><h1>Submission</h1></TableCell>
                  <TableCell><h1>Selection</h1></TableCell>
                  <TableCell><h1>Points</h1></TableCell>
                </TableRow>
              </TableBody>
            {history_list.map((submission, index, array) => this.renderSubmission(submission,index, array))}
          </Table>
        </div>
      );
    }
  }

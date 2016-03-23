var utils = require('./utils');
var regressionLine = require('./regressionLine');
var d3 = require('d3');

var drawRegressionLine = function(data){

  var columnData = utils.convertToColumns(data);
  var x = columnData[0];
  var y = columnData[1];

  var regressionL = regressionLine(x,y);

  var intercept = regressionL[0];
  var slope = regressionL[1];

  var domain = [utils.min(x), utils.max(x)];

  var point1 = [domain[0], slope*domain[0] + intercept];
  var point2 = [domain[1], slope*domain[1] + intercept];
  var linePoints = [point1, point2];

  var margin = {
    top: 20,
    right: 15,
    bottom: 60,
    left: 60
  }

  var width = 960 - margin.left - margin.right;
  var height = 500 - margin.top - margin.bottom;

  var x = d3.scale.linear()
            .domain([d3.min(data,function(d) {return d[0];}),
              d3.max(data, function(d) { return d[0]; })])
            .range([ 0, width ]);

  var y = d3.scale.linear()
    	        .domain([d3.min(data, function(d) { return d[1]; }),
                d3.max(data, function(d) { return d[1]; })])
  	          .range([ height, 0 ]);

  var chart = d3.select('body')
	              .append('svg:svg')
	              .attr('width', width + margin.right + margin.left)
	              .attr('height', height + margin.top + margin.bottom)
	              .attr('class', 'chart');

  var main = chart.append('g')
	                .attr('transform', 'translate(' + margin.left + ',' + margin.top + ')')
	                .attr('width', width)
	                .attr('height', height)
	                .attr('class', 'main');

    // draw the x axis
  var xAxis = d3.svg.axis()
                .scale(x)
	              .orient('bottom');

  main.append('g')
	    .attr('transform', 'translate(0,' + height + ')')
	    .attr('class', 'main axis')
	    .call(xAxis);

    // draw the y axis
  var yAxis = d3.svg.axis()
	              .scale(y)
                .orient('left');

  main.append('g')
	    .attr('transform', 'translate(0,0)')
      .attr('class', 'main axis')
	    .call(yAxis);

  var g = main.append("svg:g");

  g.selectAll("scatter-dots")
   .data(data)
   .enter()
   .append("svg:circle")
   .attr("cx", function (d,i) { return x(d[0]); } )
   .attr("cy", function (d) { return y(d[1]); } )
   .attr("r", 3)
   .attr("fill", "green");

   main.append('svg:line')
       .attr("x1", x(point1[0]))
       .attr("y1", y(point1[1]))
       .attr("x2", x(point2[0]))
       .attr("y2", y(point2[1]))
       .style("stroke", "blue")
       .style("stroke-width", 2);



}

module.exports = drawRegressionLine;

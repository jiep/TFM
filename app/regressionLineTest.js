var regressionLine = require('./regressionLine');

x = [-1,0,1,2,3,4,5,6];
y = [10,9,7,5,4,3,0,-1];
var regressionLine1 = regressionLine(x,y);
var intercept = regressionLine1[0];
var slope = regressionLine1[1];
console.log("The regressionLine should be y = -1.6071429+8.6428571x and it is y=" +intercept + "+" + slope + "x");

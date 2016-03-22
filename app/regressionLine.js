var sum = require('./utils').sum;
var pow = require('./utils').pow;
var prod = require('./utils').prod;


var regressionLine = function(x, y){
  var slope = 0;
  var intercept = 0;
  if(x.length == y.length){
    var n = x.length;
    var denominator = n*sum(pow(x,2)) - Math.pow(sum(x), 2);
    var slope_numerator = n*sum(prod(x,y)) - sum(x)*sum(y);
    var intercept_numerator = sum(pow(x,2))*sum(y) - sum(prod(x,y))*sum(x);
    var slope = slope_numerator/denominator;
    var intercept = intercept_numerator/denominator;
  }
  return [intercept, slope];
}

module.exports = regressionLine;

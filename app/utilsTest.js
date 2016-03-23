var utils = require('./utils');

var a = [1, 2, 5.56, 0.5, -8.834];
var b = [-1,0,2];
var c = [1,2,3];
var d = [[1,2,4],[2,34,5],[34,5,67],[1,2,4]];

console.log("The min should be -8.834 and it is " + utils.min(a));
console.log("The max should be 5.56 and it is " + utils.max(a));
console.log("The sum should be 0.226 and it is " + utils.sum(a));
console.log("The square should be [1,0,4] and it is " + utils.pow(b,2));
console.log("The product should be [-1,0,6] and it is " + utils.prod(b,c));
console.log("Convert to columns should be [[1,2,34,1],[2,34,5,2],[4,5,67,4]] and it is " + utils.convertToColumns(d));

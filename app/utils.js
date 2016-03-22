var max = function(array){
  var _max = array[0];
  for(var i=1; i < array.length; i++){
    if(array[i] > _max){
      _max = array[i];
    }
  }
  return _max;
}

var min = function(array){
  var _min = array[0];
  for(var i=1; i < array.length; i++){
    if(array[i] < _min){
      _min = array[i];
    }
  }
  return _min;
}

var sum = function(array){
  var _sum = 0;
  for(var i = 0; i < array.length; i++){
    _sum += array[i];
  }

  return _sum;
}

var pow = function(array, n){
  pow_array = [];
  for(var i = 0; i < array.length; i++){
    pow_array[i] = Math.pow(array[i],n);
  }
  return pow_array;
}

var prod = function(array1, array2){
  prod_array = [];
  if(array1.length = array2.length){
    for(var i = 0; i < array1.length; i++){
      prod_array[i] = array1[i]*array2[i];
    }
  }

  return prod_array;
}

module.exports = {
  max: max,
  min: min,
  sum: sum,
  pow: pow,
  prod: prod
}

model <- "
/************************/
/************************ Set-up
/************************/
var sizes = ['little', 'big']
var sizeDistr = Categorical({
ps: [1, 1],
//   ps:repeat(sizes.length, function(){return 0.1}),
vs:sizes})

var colors = ['red', 'blue', 'green', 'yellow', 'purple', 'cyan', 'black']
// var colors = ['red', 'blue']
var colorDistr = Categorical({
//   ps:[1, 1],
ps:[0.5, 1, 1, 1, 1, 1, 1],
//   ps:repeat(colors.length, function(){return 0.1}),
vs:colors})

var shapes = ['square', 'triangle']
var shapeDistr = Categorical({
ps:repeat(shapes.length, function(){return 0.1}),
vs:shapes})

var _createUtterance = function(size, color, shape) {
return size + ' ' + color + ' ' + shape;
}
var utterancePrior = function() {
return _createUtterance(sample(sizeDistr), sample(colorDistr), sample(shapeDistr));
}

var _makeObj = function(name, size, color, shape) {
return {name: name, size:size, color:color, shape:shape};
}

var makeObj = function(name) {
var size = sample(sizeDistr)
var color = sample(colorDistr)
var shape = sample(shapeDistr)
return _makeObj(name, size, color, shape);
}

var objPrior = function() {
return makeObj('obj');
}

var _isFeature = function(featureName) {
return function(m) {return any(function(w) {return m == w;}, featureName);}
}
var isColor = _isFeature(colors)
var isShape = _isFeature(shapes)
var isSize = _isFeature(sizes)
var _getFeatureType = function(m) {
return isColor(m) ? 'color' : isShape(m) ? 'shape' : 'size'
}
var getFeatureTypes = function(utterance) {
return map(_getFeatureType, utterance.split(' '))
}

// Noise functions
// Naive tranform to A
var transform2A = function(token) {
return 'A'
}

var _createNewUtterance = function(tokens, newToken, index) {
return tokens.slice(0, index).concat([newToken]).concat(tokens.slice(index+1, tokens.length)).join(' ')
}

var tokenMeaning = function(token) {
if (token[0] == 'X') {
return token.slice(1, token.length)
}
return token
}

var meaning = function(utterance) {
/* Assign a meaning to an utterance. */ 
var tokens = utterance.split(' ')
var meanings = map(tokenMeaning, tokens)
return meanings
}

var objMeaning = function(obj) {
return getVals(obj)
}

/************************/
/************************ Utils
/************************/
var getVals = function(obj) {
return filter(function(x) {x != ''}, map(function(key) {
return key != 'name' ? obj[key] : ''}, Object.keys(obj)))
}

var equalArrays = function(arr1, arr2) {  
return all(function(x) {return x[0] == x[1]}, zip(arr1, arr2))
}

var equalObjects = function(obj1, obj2) {
var obj1Vals = getVals(obj1)
var obj2Vals = getVals(obj2)
if (obj1Vals.length != obj2Vals.length) {
return false;
}
// NOTE (BP): below assumes feature orders are always <size, color, shape>
return equalArrays(obj1Vals, obj2Vals)
}

var stripUms = function(utterance) {
var tokens = utterance.split(' ')
return map(function(token) {return token[0] == 'X' ? token.slice(1, token.length) : token}, tokens).join(' ')
}

/************************/
/************************ Models - noise / um
/************************/

var transformUtterance = function(tokens, index, transformFn) {
var oldToken = tokens[index]
var newToken = transformFn(oldToken)
return _createNewUtterance(tokens, newToken, index)
}

// Randomly select token of same class
var randomClassPick = function(token) {
if (token[0] == 'X') {
var cleanedToken = token.slice(1, token.length)
return flip(0.9) ? token : isSize(cleanedToken) ? 
sample(sizeDistr) : (isColor(cleanedToken) ? 
sample(colorDistr) : (isShape(cleanedToken) ? 
sample(shapeDistr) : error(cleanedToken)))
}
return isSize(token) ? 
sample(sizeDistr) : (isColor(token) ? 
sample(colorDistr) : (isShape(token) ? 
sample(shapeDistr) : error(token)))
}

var singleDartNoiseModel = function(utterance, theta, noiseFn) {
// NOTE (BP): Here we simply remove 'X', later it can be a cue that reduces
// prob of error.
var tokens = utterance.split(' ')
var index = randomInteger(tokens.length)
return flip(theta) ? transformUtterance(tokens, index, noiseFn) : utterance
}

var noiseModel = function(utterance, noiseProb) {
Infer({
model() {
var intendedUtterance = utterancePrior()
var noisyUtterance = singleDartNoiseModel(utterance, noiseProb, randomClassPick)
factor(equalArrays(meaning(noisyUtterance), meaning(intendedUtterance)) ? 0 : -Infinity)
return intendedUtterance;
}
})
}

var addUm = function(token) {
return 'X' + token
}

var umModel = function(utterance, index, theta, addUmFn) {
var tokens = utterance.split(' ')
return flip(theta) ? transformUtterance(tokens, index, addUmFn) : utterance
}

/************************/
/************************ RSA
/************************/

// var NOISE_PROB = 0.65
var L0 = function(utterance, noiseProb) {
Infer({ 
model() {
var intendedUtterance = sample(noiseModel(utterance, noiseProb))
var m = meaning(intendedUtterance)
var obj = objPrior()
factor(equalArrays(meaning(intendedUtterance), getVals(obj)) ? 0 : -Infinity)
return obj;
}
})
}

// var UM_PROB = 0.65
var S1 = function(obj, umProb, noiseProb) {
Infer({model() {
var originalUtterance = utterancePrior()
var umIndex = randomInteger(originalUtterance.split(' ').length)
var umUtterance = umModel(originalUtterance, umIndex, umProb, addUm)
var L = L0(umUtterance, noiseProb)
factor(equalObjects(sample(L), obj) ? 0 : -Infinity)
return umUtterance;
}
})
}


/************************/ 
/************************ Run
/************************/

var _createFnForArbitraryArgs = function() {
return function() {
var args = Array.prototype.slice.call(arguments, 0);
return args
}
}

var createReturnDataObj = _createFnForArbitraryArgs()


var exUtt = 'little red triangle'
var exObj1 = {name:'obj1', size:'little', color:'red', shape:'triangle'}
var exObj2 = {name:'obj2', size:'little', color:'blue', shape:'square'}
var exObj3 = {name:'obj3', size:'big', color:'red', shape:'square'}
var exObj4 = {name:'obj4', size:'big', color:'blue', shape:'triangle'}
// map(S1, [exObj1, exObj2, exObj3, exObj4])
// [exObj1, JSON.stringify(map(S1, [exObj1, exObj2]))]
// createReturnDataObj(exObj1, exObj2)

var getExperimentObj = function(rData) {
  var data = rData[0]
  var newObj = {name:data['name'], size:data['size'], color:data['color'], shape:data['shape']}
  return newObj
}

var getExperimentUmProb = function(rData) {
  var data = rData[0]
  return data['umProb']
}

var getExperimentNoiseProb = function(rData) {
  var data = rData[0]
  return data['noiseProb']
}

var runSpeakerExperiment = function(speakerObj, umProb, noiseProb) {
  return S1(speakerObj, umProb, noiseProb)
}

var expObj = getExperimentObj(rData)
var noiseProb =  getExperimentNoiseProb(rData)
var umProb = getExperimentUmProb(rData)

runSpeakerExperiment(expObj, umProb, noiseProb)
"



  
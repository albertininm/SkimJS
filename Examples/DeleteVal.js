//delete val
function deleteVal (array, x) {
	var w = [];
	for (var i = 0; i < array.len; i = i + 1) {
		if ((array[i] == x) == false) {
			w = w.concat(array[i]);
		}
	}
	return w;
}
var array = [4,3,5,4,23,54,7,8,6,3,5,1];
var s = deleteVal(array, 5);
s;
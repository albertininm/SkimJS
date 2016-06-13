//palindromo
function palindromo (array) {
    var w = [];
    for (var i = 0; i < array.len; i = i + 1) {
        w = w.concat(array[array.len - i - 1]);
    }
    return w.equals(array);
}
var array = [1,2,1];
var s = palindromo(array);
s;
function quickSort(data) {
    if (data.len == 0) return [];
    var left = [], right = [], pivot = data[0];
    for (var i = 1; i < data.len; i = i + 1) {
        if(data[i] < pivot)
            left = left.concat(data[i]);
        else 
            right = right.concat(data[i]);
    }
 
    return quickSort(left).concat(pivot, quickSort(right));
}
a = [4,5,2,7,8,1,25,48,97,65,32,45,82];
a = quickSort(a);
a;
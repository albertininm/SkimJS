function quicksort(data) {
    if (data.len == 0) return [];
 
    var left = [], right = [], pivot = data[0];
 
    for (var i = 1; i < data.len; i = i + 1) {
        if(data[i] < pivot)
            left = left.concat(data[i]);
        else 
            right = right.concat(data[i]);
    }
 
    return quicksort(left).concat(pivot, quicksort(right));
}
function binarySearch(array, i){
    var inicio = 0;
    var fim  = array.len -1;
    while(inicio<=fim){
        var meio = (inicio+fim)/2;
        if(array[meio]==i){
            return meio;
        }
        if(array[meio]>i){
            fim = meio-1;
        }else{
            inicio = meio+1;
        }
    }
    return (-1);
}
a = [4,5,2,7,8,1,25,48,97,65,32,45,82];
a = quicksort(a);
binarySearch(a, 25);
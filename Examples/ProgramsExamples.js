/*
 //Crivo
primo = [];
//console.log(primo);
for(var x = 2;x<=1000;x++){
	primo = primo.concat([true]);
}
saida = [];
for(var i = 2;i<=1000;i++){
	if(primo[i]){
		saida = saida.concat([i])
		for(var j = 2;j*i<=1000;j++){
			primo[j*i] = false;				
		}
	}
}
saida;
*/


/*
//Bubble
vetor = [5,2,3,6,7,3,5,6,4,2,559,8,7,4,1,24,5,4,7,26,89];
for(var i = 0;i<vetor.len();i = i+1){
	for(var j = 0;j<vetor.len();j = j+1){
		if(vetor[i]<vetor[j]){
			aux = vetor[i];
			vetor[i] = vetor[j];
			vetor[j] = aux;
		}
	}
}
vetor;
*/

/*
//Fatorial
function fat(x){
	if(x<=1){
		return 1;
	}
	return (fat(x-1))*x;
}
//console.log(fat(4));
*/

/*
//Fibonacci
var y = 5;
function fib(y){
	if(y<=1){
		return 1;
	}
	return (fib(y-1))+(fib(y-2));
}
fib(y);
*/
/*
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
*/

/*
//Soma
function somaArray(array){
    var retorno = 0;
    while(array.len>0){
        retorno = retorno + array.head;
        array = array.tail;
    }
    return retorno;
}
var x = [1,2,3,4,5,6,7];
somaArray(x);
*/

/*
//kadane
function max(a, b){
    if(a>b)return a;
    return b;
}
function kadane(array){
    var soma = 0;
    var retorno = 0;
    for(var i = 0;i<array.len;i++){
        soma = soma + array[i];
        if(soma<0)soma = 0;
        retorno = max(retorno, soma);
    }
    return retorno;
}
var x = [2,4,-3,7,-2,-5, 10, 2, -1];
kadane(x);
*/

/*
//delete var
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
*/

/*
//palindromo
function palindromo (array) {
	var w = [];
	for (var i = 0; i < array.len; i = i + 1) {
		w = w.concat(array[array.len - i - 1]);
	}
	return w;
}

var array = [4,3,5,4,23,54,7,8,6,3,5,1];
var s = palindromo(array);
s;
*/
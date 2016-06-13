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
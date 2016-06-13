//SomaArray
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
//Fibonacci
var y = 5;
function fib(y){
    if(y<=2){
        return 1;
    }
    return (fib(y-1))+(fib(y-2));
}
fib(y);
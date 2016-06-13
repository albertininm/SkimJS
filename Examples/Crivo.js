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
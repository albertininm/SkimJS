//Fatorial
function fat(x){
	if(x<=1){
		return 1;
	}
	return (fat(x-1))*x;
}
fat(25);
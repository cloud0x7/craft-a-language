function fibonacci(n:number):number{
    if (n <= 1) {
        return n;
    }
    else{
        return fibonacci(n-1) + fibonacci(n-2);
    }
}

for (let i:number = 0; i< 10; i++){
    println(fibonacci(i));
}